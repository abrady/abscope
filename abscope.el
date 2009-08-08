

(setq abscope-dir "c:/src")
(setq abscope-file "abscope-queries.org")
(setq abscope-exe "c:/abs/abscope/abscope.exe")
(setq abscope-proc nil)
(setq abscope-tq nil)

(defun abscope-re-launch ()
  "launch/re-launch the exe"
  (if (not (and abscope-proc (equal (process-status abscope-proc) 'run)))
      (save-window-excursion
        (find-file (concat abscope-dir "/" abscope-file))
        (setq abscope-proc (start-process "abscope" (current-buffer) abscope-exe "-Qa" "-"))
        (setq abscope-tq (tq-create abscope-proc))
        )
    )
)
;; (setq foo-str "'(
;; (LocInfo 
;;   (File \"a b\")
;;   (Lineno 1)
;;   (Line \"a b c d;\")
;;   (Tag \"asdf\")
;;   (RefName \"refname\")
;;   (Ctxt \"ctxt\")
;;   (Ref (LocInfo 
;;     (File \"file ref\")
;;     (Lineno 3)
;;     (Line \"ref line str2;\")
;;     (Tag \"ref asdf2\")
;;     (RefName \"sub refname2\")
;;     (Ctxt \"sub ctxt2\")
;;   ))
;; )
;; (LocInfo 
;; (File \"file2\")
;; (Lineno 2)
;; (Line \"line str2;\")
;;       (Tag \"asdf2\")
;; (RefName \"refname2\")
;; (Ctxt \"ctxt2\")
;; )
;; )")
;; (setq foo (eval (read foo-str)
;; ))

(defun abs-print-locinfo (struct-block prefix)
  "print out a locinfo block of the form:
LocInfo
File foo
Lineno n
Ref r
Ctxt c"
  (let
      (
       (File "") (Lineno "1")
       (Tag "") (RefName "") (Ctxt "") (Line "") (flds)
       )
    (setq flds '(File Lineno Tag RefName Ctxt Line))
    (loop with sym for i in struct-block do
          (setq sym (car i))
          (if (and (eql 'Ref (car i)) (eql 'LocInfo (caadr i)))
              (abs-print-locinfo (cdadr i) (concat prefix "*"))
              (loop for j in flds do
                    (if (eql sym j)
                        (set j (cadr i))
                      )
                    )
              )
          )
    (insert (format "%s [[file:%s::%s][%s]]" prefix File Lineno (or (string-replace-match ".*? " Ctxt "") (file-name-nondirectory File))))
    (insert (format "\t%s\n" (stripLeadingWhitespace Line)))
    )
  )


;; (abs-print-locinfo (cdar foo) "**")
(defvar abscope-last-output nil
  "last form returned by abscope")

(defvar abscope-last-output-str nil
  "string last returned by abscope")

(defun abscope-proc-eval-output (proc str)
  (setq abscope-last-output (eval (read str)))
  )

(defun abscope-proc-print-output (proc str) 
  (with-current-buffer (process-buffer proc)
    (insert ":") ;; subtle notice of when the process finishes
    (save-excursion
      ;; (goto-char (process-mark proc)) todo: proper mark saving.
      (let
          (
           (forms)
           )
        (setq abscope-last-output-str str)
        (condition-case err
            (progn
              (setq forms (abscope-proc-eval-output proc str))
              (loop for i in forms do
                    (case (car i)
                      ('LocInfo (abs-print-locinfo (cdr i) "**"))
                      (t (message "unknown form")))
                    )
              )
          (insert (format "error in output %s" (princ err)))
          )
        )
      (set-marker (process-mark proc) (point))
      )
    (re-search-forward "\\[\\[" (line-end-position) t)
    )
  )

(defun abscope-query-data (type tag process-defun)
  (abscope-re-launch)
  (tq-enqueue abscope-tq (concat type " " tag "\n") "^QUERY_DONE)\n\n" abscope-proc process-defun)

  ;; *seems* to be more responsive if you do this
  (accept-process-output abscope-proc 0.1 0 1) 
  (tq-process-buffer abscope-tq)
)

(defun abscope-query (type tag)
  "query a tag by type. uses pcre syntax:
- ? : for shortest match.
   1.  \b Match a word boundary
   2. \B Match except at a word boundary
   3. \A Match only at beginning of string
   4. \Z Match only at end of string, or before newline at the end
   5. \z Match only at end of string
   6. \G Match only at pos() (e.g. at the end-of-match position of prior m//g)
"
  (interactive "s (s)truct (S)tructref (f)unc (F)uncref:
stag:")
  (if (find-buffer-visiting (concat abscope-dir"/" abscope-file))
      (iswitchb-visit-buffer (find-buffer-visiting (concat abscope-dir"/" abscope-file)))
    (progn
      (find-file-other-window abscope-dir)
      (find-file (concat abscope-dir "/" abscope-file))
      ))
  
  (end-of-buffer)
  (insert "\n\n* " tag)
  (push-mark (point))
  (abscope-query-data type tag 'abscope-proc-print-output)
  )


(defun abq (tag)
  "query for a tag from all types"
  (interactive (list (read-string "The tag to search for:" (readWordOrRegion))))
  (abscope-query "a" tag)
  )

(defun abc (tag)
  "find a src file"
  (interactive (list (read-string "src file to search for:" (readWordOrRegion))))
  (abscope-query "c" tag)
  )

(defun abstruct (tag)
  "find a src file"
  (interactive (list (read-string "src file to search for:" (readWordOrRegion))))
  (abscope-query "s" tag)
  )


(defun cdbabscope-testparse () 
  (interactive)
  (find-file "c:/abs/abscope")
  (cdb "cdb -2 c:/abs/abscope/abscope.exe -T")
  )

(defun cdbabscope-testquery () 
  (interactive)
  (find-file "c:/src")
  (cdb "cdb -2 c:/abs/abscope/abscope.exe -Qa reward.c")
  )

(defun abscope-next-match ()
  "find the next match and go to it"
  (interactive)
  (let
      ((buf))
    (find-file (concat abscope-dir "/" abscope-file))

    (setq buf (save-window-excursion
                      (org-open-at-point)
                      (current-buffer))
          )
    (forward-line 1)
    (iswitchb-visit-buffer buf)
    )
)

(defun abscope-prev-match ()
  "find the next match and go to it"
  (interactive)
  (let
      ((buf))
    (find-file (concat abscope-dir "/" abscope-file))
    (forward-line -1)
    (setq buf (save-window-excursion
                      (org-open-at-point)
                      (current-buffer))
          )
    (iswitchb-visit-buffer buf)
    )
)

(defun abscope-matches-replace ()
  (interactive)
  (while t 
    (abscope-next-match)
    (call-interactively 'query-replace-regexp)))

(defun abscope-follow-tag (tag)
  "try to find an existing tag from the word at point"
  (interactive (list (read-string "tag:" (readWordOrRegion)))) 
  (find-file (concat abscope-dir "/" abscope-file))
  (end-of-buffer)
  (if (re-search-backward (format "^\\* %s" tag) nil t)
      (abscope-next-match)
    (abq tag)))


(defun abscope-find-var-type (varname)
  "int a; ... should return int"
  (let
      (
       (pt-max (point))
       )
    (save-excursion
      (beginning-of-defun)
      (re-search-forward varname pt-max)
      (backward-word 2)
      (thing-at-point 'symbol)
      )))

(defun abscope-gather-vars (start end)
  "gather variable decls in a region"
  (save-excursion
    (goto-char start) 
    (loop while (re-search-forward ".*\\(\\b[a-z_]+\\b\\);" end t)
          collect (cons (match-string-no-properties 1) (match-string-no-properties 0)))))

(defun abscope-gather-struct-vars ()
  (save-excursion
    (abscope-gather-vars (point) (progn (end-of-defun) (point)))
    )
  )

(defvar abscope-find-members-history)
(defun abscope-find-members ()
  (interactive)
  (let
      (
       (varname)
       (vartype)
       (mbrs)
       (li)
       )
    (save-excursion
      (backward-word)
      (setq varname (thing-at-point 'symbol))
      (setq vartype (abscope-find-var-type varname))
      (if (not vartype)
          (error "couldn't find vartype for var %s" varname)
        )
      (setq vartype (concat "\\b" vartype "\\b"))
      (setq abscope-last-output nil)
      (abscope-query-data "s" vartype 'abscope-proc-eval-output)
      (loop for i from 1 to 1000 until abscope-last-output 
            do (accept-process-output abscope-proc 0.1 0 t)
            )
      (if (not abscope-last-output)
          (error "no info for type %s" vartype))
      
      (save-window-excursion
        (setq li (cdar abscope-last-output))
        (find-file (concat abscope-dir (cadr (assoc 'File li))))
        (goto-line (string-to-number (cadr (assoc 'Lineno li))))
        (setq mbrs (abscope-gather-struct-vars)))
      )
    (insert (completing-read "mbr:" mbrs nil t nil 'abscope-find-members-history nil nil))
    )
  )
          
          
    

(provide 'abscope)

