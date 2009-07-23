;; ;;
;; process-buffer : the buffer for a process
;; process-mark   : 
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


;;        (insert "done.\n" event)
(defmacro make-proc-event-listener ()
  "called when process finishes"
  `(lambda (proc event)
     (let
         ((beg ,(point)))
       (with-current-buffer (process-buffer proc)
         (insert ": ")
         (align-regexp beg (process-mark proc) "\t" 0 -48)
;;         (goto-char beg)
         )
       )
     )
  )

(defun abs-print-locinfo (struct-block)
  "print out a locinfo block of the form:
LocInfo
File foo
Lineno n
Ref r
Ctxt c"
  (let
      (
       (lines)
       (fld "")
       (val "")
       (file "")
       (lineno "1")
       (tag "")
       (ref "")
       (ctxt "")
       (line "")
       (flds)
       (tmp)
       )

    (setq flds '(file val lineno tag ref ctxt line))
;;    (setq hval (car flds))
;;    (symbol-value hval)
    (setq lines (split-string struct-block "\n"))
    (loop for i in lines do
          (if (string-match "^\\(.*?\\) +\\(.*\\)" i)
              (progn
                (setq fld (match-string 1 i))
                (setq val (match-string 2 i))
                (loop for j in flds do
                      (if (equal (symbol-name j) (downcase fld))
                          (progn
                            (set j val)
                            (return)
                            )
                        )
                      
                      )
                )
            )
          )
    
    ;; *************************************************************************
    ;; insertion

    ;; tag: not really useful as a field to display.
;;    (insert (format "** [[file:%s::%s][%s:%s(%s)]] %s: %s\n" file lineno (file-name-nondirectory file) ref lineno ctxt line))

;;  ** storeCommon.c:func store_Validate(222) pDef: 	if (pDef->eContents != Store_All && pDef->bSellEnabled
;;    (insert (format "** [[file:%s::%s][%s:%s(%s)]] %s: %s\n" file lineno (file-name-nondirectory file) ctxt lineno ref line))

;; bSellEnabled:
;; struct ContactDialog: bool bSellEnabled; -- just the line
;; func  store_Validate: if (pDef->eContents != Store_All && pDef->bSellEnabled -- just the line
    (insert (format "** [[file:%s::%s][%s]]" file lineno (or (string-replace-match ".*? " ctxt "") (file-name-nondirectory file))))
;;    (loop for i from (length ctxt) to 16 do ;; can't use indent-to as the the whole expression confuses it until org-mode hides it
;;          (insert " "))
    (insert (format "\t%s\n" (stripLeadingWhitespace line)))
    )
  )

;; (abs-print-locinfo "LocInfo
;; File foo
;; Line n
;; Ref r
;; Ctxt c
;; ")   

(defun proc-msg-listener (proc str) 
  (with-current-buffer (process-buffer proc)
    (save-excursion
      ;; (goto-char (process-mark proc)) todo: proper mark saving.
      (insert ":") ;; subtle notice of when the process finishes
      (let
          (
           (struct-name)
           (lines)
           )
        (setq lines (split-string str "\nEnd\n" t)) ;; split into blocks of structs
        (loop for i in lines do
              (if (string-match "\\(.*?\\)\n" i)
                  (progn
                    (setq struct-name (downcase (match-string 1 i)))
                    (cond
                     ((equal struct-name "locinfo") (abs-print-locinfo i))))))
        ;;      (insert str)
        )
      (set-marker (process-mark proc) (point)))
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
  (tq-enqueue abscope-tq (concat tag "\n") "^QUERY_DONE\n\n" abscope-proc 'proc-msg-listener)
  )

(progn
(defun abscope-query-struct (struct)
  (interactive "s struct:")
  (abscope-query "s" struct))
;;(abscope-query-struct "ItemDef")
)

(defun abq (tag)
  (interactive (list (read-string "The string to search for:"
 								  (readWordOrRegion))))
  (abscope-query "a" tag))
 


(defun abt ()
  (interactive)
  (find-file "c:/abs/abscope/test")
  (find-file abscope-file)
  (end-of-buffer)
  (insert "\n\n* ItemDef \n")
  (shell-command "c:/abs/abscope/abscope.exe -f itemCommon.h")
  (shell-command "c:/abs/abscope/abscope.exe -Qs ItemDef" (current-buffer)))

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
;;   (iswitchb-visit-buffer
;;    (with-current-buffer (get-file-buffer (concat abscope-dir "/" abscope-file))
;;      ;;    (find-file (concat abscope-dir "/" abscope-file))
;;      (forward-line 1)
;;      ;;    (re-search-forward "\\*+ ")
;;      (save-window-excursion
;;        (org-open-at-point)
;;        (current-buffer)))
;;    )

(defun abscope-matches-replace ()
  (interactive)
  (while t 
    (abscope-next-match)
    (call-interactively 'query-replace-regexp)))

(defun abscope-follow-tag (tag)
  "in code try to jump to the tag matching the thing at point"
  (interactive (list (read-string "tag:" (readWordOrRegion)))) 
  (find-file (concat abscope-dir "/" abscope-file))
  (end-of-buffer)
  (if (re-search-backward (format "^\\* %s" tag) nil t)
      (abscope-next-match)
    (abq tag))) ;; todo: it would be nice to query a follow after process finishes


;;(defun abscope-query-replace ()

(provide 'abscope)

