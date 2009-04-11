;;
;; process-buffer : the buffer for a process
;; process-mark   : 

(setq abscope-dir "c:/src")
(setq abscope-file "abscope-queries.org")

(progn
(defun abscope-query-struct (struct)
  (interactive "s:struct name")
  (find-file abscope-dir)
  (find-file abscope-file)

  (end-of-buffer)
  (insert "\n\n* " struct "\n")
  (start-process "abscope" (current-buffer) "c:/abs/abscope/abscope.exe" 
                 "-Qs" struct)
  ) 
(abscope-query-struct "ItemDef"))


(defun abt ()
  (interactive)
  (find-file "c:/abs/abscope/test")
  (find-file abscope-file)
  (end-of-buffer)
  (insert "\n\n* ItemDef \n")
  (shell-command "c:/abs/abscope/abscope.exe -f itemCommon.h")
  (shell-command "c:/abs/abscope/abscope.exe -Qs ItemDef" (current-buffer)))

(provide 'abscope)