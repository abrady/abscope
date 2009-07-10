;; ;;
;; process-buffer : the buffer for a process
;; process-mark   : 
(setq abscope-dir "c:/src")
(setq abscope-file "abscope-queries.org")
(setq abscope-exe "c:/abs/abscope/abscope.exe")

(defmacro make-proc-event-listener ()
  `(lambda (proc event) 
      (with-current-buffer (process-buffer proc)
;;        (insert "done.\n" event)
        (if (> (point) ,(point))
            (goto-char ,(point))))
        )
)

(defun abs-print-locinfo (struct-block)
  "print out a locinfo block of the form:
LocInfo
File foo
Line n
Ref r
Ctxt c"
  (let
      (
       (lines)

       (fld "")
       (val "")
       (file "")
       (line "")
       (ref "")
       (ctxt "")

       (flds)
       )

    (setq flds '(val file line ref ctxt))
;;    (setq hval (car flds))
;;    (symbol-value hval)
    (setq lines (split-string struct-block "\n"))
    (loop for i in lines do
          (if (string-match "^\\(.*?\\) \\(.*\\)" i)
              (progn
                (setq fld (downcase (match-string 1 i)))
                (setq val (downcase (match-string 2 i)))
                (loop for j in flds do
                      (if (equal (symbol-name (car flds)) fld)
                          (set (car flds) val)))
            )
          )
    )

    (insert (format "*** [[file:%s::%i][%s]] %s\n", file, line, referrer, ctxt))
    )
  )   

(defun proc-msg-listener (proc str) 
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (end-of-buffer)
      (let
          (
           (struct-name)
           (lines)
           )
        (setq lines (split-string str "End" t)) ;; split into blocks of structs
        (loop for i in lines do
              (if (string-match "^\\(.*\\)\n" i)
                  (progn
                    (setq struct-name (downcase (match-string 1 i)))
                    (cond
                     ((equal struct-name "locinfo") (abs-print-locinfo i))))))
        ;;      (insert str)
        )
      )
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
  (insert "\n\n* " tag ": ")
  (push-mark (point))
  (push-mark (point))
  (let
      ((proc))
    (setq proc (start-process "abscope" (current-buffer) abscope-exe (format "-Q%s" type) tag))    
    (set-process-sentinel proc (make-proc-event-listener))
    (set-process-filter proc  'proc-msg-listener)
    )
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

(provide 'abscope)