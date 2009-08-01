

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

(defun proc-msg-listener (proc str) 
  (with-current-buffer (process-buffer proc)
    (insert ":") ;; subtle notice of when the process finishes
    (save-excursion
      ;; (goto-char (process-mark proc)) todo: proper mark saving.
      (let
          (
           (forms)
           )
        (setq forms (eval (read str)))
        (loop for i in forms do
              (case (car i)
                ('LocInfo (abs-print-locinfo (cdr i) "**"))
                (t (message "unknown form")))
              )
        )
      (set-marker (process-mark proc) (point))
      )
    (re-search-forward "\\[\\[" (line-end-position) t)
    )
  )

(defun abscope-query (type tag)
  (interactive "s (s)truct (f)unc (r)ef:
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
  (push-mark (point))
  (abscope-re-launch)
    ;;(set-process-sentinel proc (make-proc-event-listener))
    ;;(set-process-filter proc  'proc-msg-listener)
  (tq-enqueue abscope-tq (concat type " " tag "\n") "^QUERY_DONE)\n\n" abscope-proc 'proc-msg-listener)
  ;; *seems* to be more responsive if you do this
  (accept-process-output abscope-proc 0.1 0 1) 
  (tq-process-buffer abscope-tq)
  )

(defun abq (tag)
  "query for a tag from all types"
  (interactive (list (read-string "The string to search for:"
 								  (readWordOrRegion))))
  (abscope-query "a" tag)
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

(provide 'abscope)

