;; ;;
;; process-buffer : the buffer for a process
;; process-mark   : 
(setq abscope-dir "c:/src")
(setq abscope-file "abscope-queries.org")
(setq abscope-exe "c:/abs/abscope/abscope.exe")

;;        (insert "done.\n" event)
(defmacro make-proc-event-listener ()
  "called when process finishes"
  `(lambda (proc event) 
     (with-current-buffer (process-buffer proc)
       (goto-char ,(point))
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
    
    ;; tag: not really useful as a field to display.
    (insert (format "** [[file:%s::%s][%s]] %s: %s\n" file lineno ref ctxt line))
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
      (end-of-buffer)
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

(provide 'abscope)