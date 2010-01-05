;;; company-abscope.el --- abscope bindings for company
;;
;; Copyright (C) 2009 
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'company)
(require 'abscope)
(eval-when-compile (require 'cl))

(defvar company-abscope-modes '(c-mode c++-mode jde-mode java-mode))

(defun company-abscope-doc-or-summary (tag)
  (or (abscope-documentation-for-tag tag)
      (funcall abscope-idle-summary-function tag nil t)))

(defun company-abscope-summary-and-doc (tag)
  (let ((doc (abscope-documentation-for-tag tag))
        (summary (funcall abscope-idle-summary-function tag nil t)))
    (and (stringp doc)
         (string-match "\n*\\(.*\\)$" doc)
         (setq doc (match-string 1 doc)))
    (concat (funcall abscope-idle-summary-function tag nil t)
            (when doc
                  (if (< (+ (length doc) (length summary) 4) (window-width))
                      " -- "
                    "\n"))
            doc)))

;; (defun company-abscope-doc-buffer (tag)
;;   (let ((doc (abscope-documentation-for-tag tag)))
;;     (when doc
;;       (with-current-buffer (company-doc-buffer)
;;         (insert (funcall abscope-idle-summary-function tag nil t)
;;                 "\n"
;;                 doc)
;;         (current-buffer)))))

(defsubst company-abscope-completions (prefix)
  "find a tag based on a best guess about the context, i.e. in a funcall, struct, etc.
e.g.
- foo->|    : query for the members of the struct of type foo
- foo(a,b,| : query for the arglist of the function foo "
  (let (lis li)
	(setq lis (abs-query nil "fd" (format "\\b%s" prefix)))
	(loop for i in lis
		  if (setq li (abs-tag-from-locinfo i))
		  collect (cdr li)
		  )
	)
  )

;; (defun company-abscope-completions-raw (prefix)
;;   (let (candidates)
;;     (dolist (tag (abscope-analyze-find-tags-by-prefix prefix))
;;       (unless (eq (abscope-tag-class tag) 'include)
;;         (push (abscope-tag-name tag) candidates)))
;;     (delete "" candidates)))

(defun company-abscope--pre-prefix-length (prefix-length)
  "Sum up the length of all chained symbols before POS.
Symbols are chained by \".\" or \"->\"."
  (save-excursion
    (let ((pos (point)))
      (goto-char (- (point) prefix-length))
      (while (looking-back "->\\|\\.")
        (goto-char (match-beginning 0))
        (skip-syntax-backward "w_"))
      (- pos (point)))))

(defun company-abscope--grab ()
  "Grab the abscope prefix, but return everything before -> or . as length."
  (let ((symbol (company-grab-symbol)))
    (when symbol
      (cons symbol (company-abscope--pre-prefix-length (length symbol))))))

;;;###autoload
(defun company-abscope (command &optional arg &rest ignored)
  "A `company-mode' completion back-end using abscope."
  (interactive (list 'interactive))
  (case command
    ('interactive (company-begin-backend 'company-abscope))
    ('prefix (and (memq major-mode company-abscope-modes)
                  (not (company-in-string-or-comment))
                  (or (company-abscope--grab) 'stop)))
    ('candidates ;; (if (and (equal arg "") (not (looking-back "->\\|\\.")))
                 ;;    (company-abscope-completions-raw arg)
                   (company-abscope-completions arg)) ;; )
    ('meta) ;; todo
    ('doc-buffer) ;; todo (company-abscope-doc-buffer (abscope-analyze-find-tag arg)))
    ;; because "" is an empty context and doesn't return local variables
    ('no-cache (equal arg ""))
    ('location) ;; todo
	)
)

(provide 'company-abscope)
;;; company-abscope.el ends here