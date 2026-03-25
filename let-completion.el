;;; let-completion.el --- Show let-binding values in Elisp completion -*- lexical-binding: t -*-

;; Author: Gino Cornejo <gggion123@gmail.com>
;; Maintainer: Gino Cornejo <gggion123@gmail.com>
;; URL: https://github.com/gggion/let-completion.el
;; Keywords: lisp, completion

;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `let-completion-mode' makes Emacs Lisp in-buffer completion aware of
;; lexically enclosing binding forms.  Local variables from `let',
;; `let*', `when-let*', `if-let*', `and-let*', `dolist', and `dotimes'
;; are promoted to the top of the candidate list, annotated with their
;; binding values when short enough or a [local] tag otherwise, and
;; shown in full via pretty-printed fontified expressions in
;; corfu-popupinfo or any completion UI that reads `:company-doc-buffer'.
;;
;; Names that the built-in `elisp--local-variables' misses (untrusted
;; buffers, macroexpansion failure) are injected into the completion
;; table directly so they always appear as candidates.  For `if-let'
;; and `if-let*', bindings are suppressed in the else branch where
;; they are not in scope.
;;
;; The package installs a single around-advice on
;; `elisp-completion-at-point' when enabled and removes it when
;; disabled.  Loading the file produces no side effects.
;;
;; Usage:
;;
;;     (add-hook 'emacs-lisp-mode-hook #'let-completion-mode)
;;
;; Customize `let-completion-inline-max-width' to control the maximum
;; printed width for inline value annotations, or set it to nil to
;; always show [local] instead.

;;; Code:

(require 'cl-lib)

(defgroup let-completion nil
  "Show let-binding values in Elisp completion."
  :group 'lisp
  :prefix "let-completion-")

(defcustom let-completion-annotation-format " [%s]"
  "The format string for inline annotation."
  :type 'string
  :group 'let-completion)

(defcustom let-completion-inline-max-width 5
  "Max printed width for inline value annotation, or nil to disable.
Only binding values whose `prin1-to-string' form fits within this
many characters appear inline next to the candidate as \" [VALUE]\".
Longer values show \" [local]\" instead.  The popupinfo buffer
always shows the full value regardless of this setting.

Also see `let-completion-mode'."
  :type '(choice natnum (const :tag "Disable" nil)))

(defvar let-completion--doc-buffer nil
  "Reusable buffer for pretty-printed binding values.
Created on first use by the function `let-completion--doc-buffer'.
Consumed by corfu-popupinfo via `:company-doc-buffer'.")

(defun let-completion--doc-buffer ()
  "Return reusable doc buffer with `emacs-lisp-mode' initialized.
The buffer is created once and reused across calls.  Mode setup
runs via function `delay-mode-hooks' to avoid triggering user hooks.

Called by `let-completion--advice' for `:company-doc-buffer'."
  (or (and (buffer-live-p let-completion--doc-buffer)
           let-completion--doc-buffer)
      (setq let-completion--doc-buffer
            (with-current-buffer (get-buffer-create " *let-completion-doc*")
              (delay-mode-hooks (emacs-lisp-mode))
              (current-buffer)))))

(defconst let-completion--empty-value (make-symbol "empty"))

(cl-defgeneric let-completion--local-variables-1 (vars sexp))

;; From `elisp--local-variables-1'
(cl-defmethod let-completion--local-variables-1 (vars sexp)
  "Return VARS locally bound around the witness, or nil if not found."
  (let (res)
    (while
        (unless
            (setq res
                  (pcase sexp
                    (`(,(or 'let 'let*) ,bindings)
                     (let ((vars vars))
                       (when (eq 'let* (car sexp))
                         (dolist (binding (cdr (reverse bindings)))
                           (push (cons (or (car-safe binding) binding)
                                       (car (cdr-safe binding)))
                                 vars)))
                       (let-completion--local-variables-1
                        vars (car (cdr-safe (car (last bindings)))))))
                    (`(,(or 'let 'let*) ,bindings . ,body)
                     (let ((vars vars))
                       (dolist (binding bindings)
                         (push (cons (or (car-safe binding) binding)
                                     (car (cdr-safe binding)))
                               vars))
                       (let-completion--local-variables-1 vars (car (last body)))))
                    (`(lambda ,_args)
                     ;; FIXME: Look for the witness inside `args'.
                     (setq sexp nil))
                    (`(lambda ,args . ,body)
                     (let-completion--local-variables-1
                      (let ((args (if (listp args) args)))
                        ;; FIXME: Exit the loop if witness is in args.
                        (append (remq '&optional (remq '&rest args)) vars))
                      (car (last body))))
                    (`(condition-case ,_ ,e) (let-completion--local-variables-1 vars e))
                    (`(condition-case ,v ,_ . ,catches)
                     (let-completion--local-variables-1
                      (cons v vars) (cdr (car (last catches)))))
                    (`(quote . ,_)
                     ;; FIXME: Look for the witness inside sexp.
                     (setq sexp nil))
                    (`(,(or 'with-slots 'cl-with-accessors)
                       ,spec-list ,_obj . ,body)
                     (let-completion--local-variables-1 vars `(let ,spec-list ,@body)))
                    (`(,(or 'dlet 'cl-symbol-macrolet) . ,_)
                     (let-completion--local-variables-1 vars (cons 'let (cdr sexp))))
                    (`(,(or 'when-let* 'and-let*) . ,_)
                     (let-completion--local-variables-1 vars (cons 'let* (cdr sexp))))
                    (`(if-let* ,bindings ,then . ,else)
                     (or (let-completion--local-variables-1 vars `(let* ,bindings ,then))
                         (let-completion--local-variables-1 vars (macroexp-progn else))))
                    (`(cl-letf ,bindings . ,body)
                     (let-completion--local-variables-1
                      vars `(let ,(seq-filter (lambda (b) (symbolp (car b))) bindings)
                              ,@body)))
                    (`(cl-letf* ,bindings . ,body)
                     (let-completion--local-variables-1
                      vars `(let* ,(seq-filter (lambda (b) (symbolp (car b))) bindings)
                              ,@body)))
                    (`(letrec ,bindings . ,body)
                     (let (lvars exps)
                       (pcase-dolist (`(,var ,exp) bindings)
                         (push var lvars)
                         (push exp exps))
                       (let-completion--local-variables-1 vars `(let ,lvars
                                                                  ,@exps
                                                                  ,@body))))
                    ;; FIXME: Handle `cond'.
                    (`(,_ . ,_)
                     (let-completion--local-variables-1 vars (car (last sexp))))
                    ('elisp--witness--lisp (or vars '(nil)))
                    (_ nil)))
          ;; We didn't find the witness in the last element so we try to
          ;; backtrack to the last-but-one.
          (setq sexp (ignore-errors (butlast sexp)))))
    res))

(cl-defmethod let-completion--local-variables-1 (vars
                                                 (sexp (head define-inline)))
  (let-completion--local-variables-1 vars (cons 'defun (cdr sexp))))

(defun let-completion--walk-pcase-pat (vars pat)
  (let ((pat (pcase--macroexpand pat))
        (pvars nil))
    (cl-labels ((walk (pat)
                  (pcase pat
                    (`(app ,_ ,pat)
                     (walk pat))
                    (`(,(or 'and 'or) . ,pats)
                     (mapc #'walk pats))
                    ('elisp--witness--lisp
                     (throw 'pcase-return
                            (nconc pvars vars)))
                    ((pred symbolp)
                     (push pat pvars)))))
      (walk pat))
    pvars))

(cl-defmethod let-completion--local-variables-1 :extra "pcase" (vars sexp)
  (catch 'pcase-return
    (pcase sexp
      (`(pcase ,form . ,cases)
       (or (let-completion--local-variables-1 vars form)
           (pcase-dolist (`(,pat . ,body) cases)
             (let ((pvars (let-completion--walk-pcase-pat vars pat)))
               (when-let* ((res (let-completion--local-variables-1
                                 vars (macroexp-progn body))))
                 (throw 'pcase-return (nconc pvars res)))))))
      (`(pcase-let ,bindings . ,body)
       (let ((pat-vars nil))
         (pcase-dolist (`(,pat ,exp) bindings)
           (let ((pvars (let-completion--walk-pcase-pat vars pat)))
             (when-let* ((res (let-completion--local-variables-1 vars exp)))
               (throw 'pcase-return res))
             (cl-callf2 nconc pvars pat-vars)))
         (let-completion--local-variables-1
          (nconc pat-vars vars)
          (macroexp-progn body))))
      (`(pcase-let* ,bindings . ,body)
       (pcase-dolist (`(,pat ,exp) bindings)
         (let ((pvars (let-completion--walk-pcase-pat vars pat)))
           (when-let* ((res (let-completion--local-variables-1 vars exp)))
             (throw 'pcase-return res))
           (cl-callf2 nconc pvars vars)))
       (let-completion--local-variables-1
        vars (macroexp-progn body)))
      (`(pcase-lambda ,arglist . ,body)
       (let-completion--local-variables-1
        (nconc (mapcan (lambda (p) (let-completion--walk-pcase-pat vars p))
                       arglist)
               vars)
        (macroexp-progn body)))
      (`(pcase-dolist (,pat ,exp) . ,body)
       (let ((pvars (let-completion--walk-pcase-pat vars pat)))
         (or (let-completion--local-variables-1 vars exp)
             (let-completion--local-variables-1 (nconc pvars vars)
                                                (macroexp-progn body)))))
      (_ (cl-call-next-method)))))

(cl-defmethod let-completion--local-variables-1 (vars
                                                 (sexp (head dolist)))
  (pcase sexp
    (`(dolist (,var ,val . ,_) . ,body)
     (let-completion--local-variables-1 (cons (cons var val) vars)
                                        (macroexp-progn body)))))

(cl-defmethod let-completion--local-variables-1 (vars
                                                 (sexp (head cl-do)))
  (pcase sexp
    (`(,_ ,bindings ,test . ,body)
     (let-completion--local-variables-1
      vars
      `(let ,(mapcar (lambda (binding) (take 2 binding)) bindings)
         ,@test
         ,@body)))))

(cl-defmethod let-completion--local-variables-1 (vars
                                                 (sexp (head cl-do*)))
  (pcase sexp
    (`(,_ ,bindings ,test . ,body)
     (let-completion--local-variables-1
      vars
      `(let* ,(mapcar (lambda (binding) (take 2 binding)) bindings)
         ,@test
         ,@body)))))

(cl-defmethod let-completion--local-variables-1 (vars
                                                 (sexp (head defun)))
  (pcase sexp
    (`(defun ,_name ,arglist . ,body)
     (let-completion--local-variables-1 (append arglist vars)
                                        (macroexp-progn body)))))

(cl-defmethod let-completion--local-variables-1 (vars
                                                 (sexp (head cl-function)))
  (let-completion--local-variables-1 vars (macroexpand-1 sexp)))

(cl-defmethod let-completion--local-variables-1 (vars
                                                 (sexp (head cl-defun)))
  (pcase sexp
    (`(cl-defun ,name ,arglist . ,body)
     (let-completion--local-variables-1
      vars
      (car (last (cl--transform-lambda (cons arglist body) name)))))))

(cl-defmethod let-completion--local-variables-1 (vars
                                                 (sexp (head cl-defsubst)))
  (let-completion--local-variables-1 vars (cons 'cl-defun (cdr sexp))))

(cl-defmethod let-completion--local-variables-1 (vars
                                                 (sexp (head cl-defmacro)))
  (let-completion--local-variables-1 vars (cons 'cl-defun (cdr sexp))))

(cl-defmethod let-completion--local-variables-1 (vars
                                                 (sexp (head cl-defmethod)))
  (pop sexp)
  (while (cl-generic--method-qualifier-p (car sexp))
    (pop sexp))
  (let-completion--local-variables-1
   (nconc (cdr (cl--generic-split-args (pop sexp)))
          vars)
   (macroexp-progn sexp)))

;; Is `macroexpand-1'ing a `cl-loop' dangerous?
(cl-defmethod let-completion--local-variables-1 (vars
                                                 (sexp (head cl-loop)))
  (let-completion--local-variables-1 vars (macroexpand-1 sexp)))

;; From `elisp--local-variables'
(defun let-completion--local-variables (extract &optional macroexpand)
  "Return a list of locally let-bound variables at point."
  (save-excursion
    (skip-syntax-backward "w_")
    (let* ((ppss (syntax-ppss))
           (txt (buffer-substring-no-properties (or (car (nth 9 ppss)) (point))
                                                (or (nth 8 ppss) (point))))
           (closer ()))
      (dolist (p (nth 9 ppss))
        (push (cdr (syntax-after p)) closer))
      (setq closer (apply #'string closer))
      (let* ((sexp (condition-case nil
                       (car (read-from-string
                             (concat txt "elisp--witness--lisp" closer)))
                     ((invalid-read-syntax end-of-file) nil)))
             (vars
              (funcall extract nil (if (not macroexpand) sexp
                                     (elisp--safe-macroexpand-all sexp)))))
        (delete-dups
         (delq nil
               (mapcar (lambda (form)
                         (pcase-let (((or `(,var . ,val) var) form))
                           (and (symbolp var)
                                (not (string-match (symbol-name var) "\\`[&_]"))
                                ;; Eliminate uninterned vars.
                                (intern-soft var)
                                (propertize (symbol-name var)
                                            'let-completion-value
                                            (or val let-completion--empty-value)))))
                       vars)))))))

;; From `elisp--local-variables-completion-table'
(defconst let-completion--local-variables-completion-table
  (let ((lastpos nil)
        (lastvars nil)
        (hook-sym (make-symbol "hook")))
    (fset hook-sym (lambda ()
                     (setq lastpos nil)
                     (remove-hook 'post-command-hook hook-sym)))
    (completion-table-dynamic
     (lambda (_string)
       (save-excursion
         (skip-syntax-backward "_w")
         (let ((newpos (cons (point) (current-buffer))))
           (unless (equal lastpos newpos)
             (add-hook 'post-command-hook hook-sym)
             (setq lastpos newpos)
             (setq lastvars (let-completion--local-variables
                             #'let-completion--local-variables-1 t)))))
       lastvars))))

(cl-defgeneric let-completion--local-functions-1 (vars sexp))

(cl-defmethod let-completion--local-functions-1 (vars sexp)
  "Return VARS locally bound around the witness, or nil if not found."
  (let (res)
    (while
        (unless
            (setq res
                  (pcase sexp
                    (`(,(or 'let 'let*) ,bindings)
                     (let-completion--local-functions-1
                      vars (car (cdr-safe (car (last bindings))))))
                    (`(,(or 'let 'let*) ,_bindings . ,body)
                     (let-completion--local-functions-1 vars (car (last body))))
                    (`(lambda ,_args)
                     ;; FIXME: Look for the witness inside `args'.
                     (setq sexp nil))
                    (`(lambda ,_args . ,body)
                     (let-completion--local-functions-1 vars (car (last body))))
                    (`(condition-case ,_ ,e)
                     (let-completion--local-functions-1 vars e))
                    (`(condition-case ,_ ,_ . ,catches)
                     (let-completion--local-functions-1
                      vars (cdr (car (last catches)))))
                    (`(quote . ,_)
                     ;; FIXME: Look for the witness inside sexp.
                     (setq sexp nil))
                    ;; FIXME: Handle `cond'.
                    (`(,_ . ,_)
                     (let-completion--local-functions-1 vars (car (last sexp))))
                    ('elisp--witness--lisp (or vars '(nil)))
                    (_ nil)))
          ;; We didn't find the witness in the last element so we try to
          ;; backtrack to the last-but-one.
          (setq sexp (ignore-errors (butlast sexp)))))
    res))

(cl-defmethod let-completion--local-functions-1 (vars
                                                 (sexp (head cl-labels)))
  (pcase sexp
    (`(,_ ,fbindings . ,body)
     (let* ((fbodies nil)
            (fvars (mapcar (lambda (binding)
                             (cons (car binding)
                                   (car (pcase (cdr binding)
                                          (`(#',fn) `#',fn)
                                          (def
                                           (car (push (cons 'lambda def)
                                                      fbodies)))))))
                           fbindings)))
       (let-completion--local-functions-1
        (nconc fvars vars)
        `(progn ,@fbodies ,@body))))))

(cl-defmethod let-completion--local-functions-1 (vars
                                                 (sexp (head cl-flet)))
  (let-completion--local-functions-1
   (nconc (mapcar (lambda (binding)
                    (cons (car binding)
                          (pcase (cdr binding)
                            (`(#',fn) `#',fn)
                            (def (cons 'lambda def)))))
                  (cadr sexp))
          vars)
   (macroexp-progn (cddr sexp))))

(defconst let-completion--local-functions-completion-table
  (let ((lastpos nil)
        (lastvars nil)
        (hook-sym (make-symbol "hook")))
    (fset hook-sym (lambda ()
                     (setq lastpos nil)
                     (remove-hook 'post-command-hook hook-sym)))
    (completion-table-dynamic
     (lambda (_string)
       (save-excursion
         (skip-syntax-backward "_w")
         (let ((newpos (cons (point) (current-buffer))))
           (unless (equal lastpos newpos)
             (add-hook 'post-command-hook hook-sym)
             (setq lastpos newpos)
             (setq lastvars (let-completion--local-variables
                             #'let-completion--local-functions-1)))))
       lastvars))))

(defun let-completion--completion-local-symbols-advice (table)
  (let ((tables
         (list let-completion--local-functions-completion-table
               table)))
    (lambda (string pred action)
      (add-function :before-until (var pred)
                    (lambda (val) (stringp val)))
      (cond
       ((null action)
        (let ((retvals (mapcar (lambda (table)
                                 (try-completion string table pred))
                               tables)))
          (if (member string retvals)
              string
            (try-completion string
                            (mapcar (lambda (value)
                                      (if (eq value t) string value))
                                    (delq nil retvals))
                            pred))))
       ((eq action t)
        (apply #'append (mapcar (lambda (table)
                                  (all-completions string table pred))
                                tables)))
       (t
        (seq-some (lambda (table)
                    (complete-with-action action table string pred))
                  tables))))))

(defun let-completion--advice (orig-fn)
  "Enrich the capf result from ORIG-FN with let-binding metadata.
Wrap the completion table via `let-completion--make-table' to
merge extracted local names into the candidate pool and inject
`display-sort-function' into the table's metadata response,
promoting locals to the top.  Inject `:annotation-function' to
show values or \"[local]\" tags, and `:company-doc-buffer' to
provide full pretty-printed values.  All three fall back to the
original plist functions for non-local candidates.

Unbind `print-level' and `print-length' inside the doc-buffer
function to defeat truncation imposed by `corfu-popupinfo'.

Installed as `:around' advice on `elisp-completion-at-point' by
`let-completion-mode'.  Removed by disabling the mode."
  (let ((result
         (cl-letf (((symbol-function 'elisp--completion-local-symbols))
                   (elisp--local-variables-completion-table
                    let-completion--local-variables-completion-table))
           (advice-add 'elisp--completion-local-symbols :filter-return
                       #'let-completion--completion-local-symbols-advice)
           (funcall orig-fn))))
    (if (and result (listp result) (>= (length result) 3))
        (let* ((plist (drop 3 result))
               (orig-ann (plist-get plist :annotation-function))
               (orig-doc (plist-get plist :company-doc-buffer))
               (sort-fn (lambda (cands)
                          (let ((seen (make-hash-table :test #'equal))
                                local other)
                            (dolist (c cands)
                              (unless (gethash c seen)
                                (puthash c t seen)
                                (if (get-text-property 0 'let-completion-value c)
                                    (push c local)
                                  (push c other))))
                            (nconc (nreverse local) (nreverse other))))))
          (when (plist-get plist :predicate)
            (add-function :around (plist-get plist :predicate)
                          (lambda (fn str)
                            (if (stringp str)
                                (get-text-property 0 'let-completion-value str)
                              (funcall fn str)))))
          (setf (plist-get plist :display-sort-function) sort-fn
                (plist-get plist :annotation-function)
                (lambda (c)
                  (pcase (get-text-property 0 'let-completion-value c)
                    ('nil
                     (when orig-ann (funcall orig-ann c)))
                    ((pred (eq let-completion--empty-value))
                     (format let-completion-annotation-format "local"))
                    (val
                     (let ((short (and let-completion-inline-max-width
                                       (prin1-to-string val))))
                       (format let-completion-annotation-format
                               (or (and (<= (length short)
                                            let-completion-inline-max-width)
                                        short)
                                   "local"))))))
                (plist-get plist :company-doc-buffer)
                (lambda (c)
                  (pcase (get-text-property 0 'let-completion-value c)
                    ((pred (eq let-completion--empty-value))
                     (format let-completion-annotation-format "local"))
                    ('nil (when orig-doc (funcall orig-doc c)))
                    (val
                     (let ((buf (let-completion--doc-buffer)))
                       (with-current-buffer buf
                         (let ((inhibit-read-only t)
                               (print-level nil)
                               (print-length nil))
                           (erase-buffer)
                           (insert (pp-to-string val))
                           (font-lock-ensure)))
                       buf)))))
          (append (take 3 result) plist))
      result)))

;;;###autoload
(define-minor-mode let-completion-mode
  "Enrich Elisp completion with let-binding values.
When enabled, install `let-completion--advice' around
`elisp-completion-at-point'.  When disabled, remove it.

Also see `let-completion-inline-max-width'."
  :lighter nil
  :group 'let-completion
  :global t
  (if let-completion-mode
      (advice-add 'elisp-completion-at-point :around
                  #'let-completion--advice)
    (advice-remove 'elisp-completion-at-point #'let-completion--advice)))

(provide 'let-completion)
;;; let-completion.el ends here
