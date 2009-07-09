;; ;;
;; process-buffer : the buffer for a process
;; process-mark   : 
(setq abscope-dir "c:/src")
(setq abscope-file "abscope-queries.org")
(setq abscope-exe "c:/abs/abscope/abscope.exe")

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
  (set-mark-command nil)
  (start-process "abscope" (current-buffer) abscope-exe
                 (format "-Q%s" type) tag)
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