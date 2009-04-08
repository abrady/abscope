;;
;; process-buffer : the buffer for a process
;; process-mark   : 

(setq abscope-dir "c:/absrc/abscope/")
(setq abscope-file "abscope-queries.org")

(progn
(defun ab-query-struct (struct)
  (interactive "s:struct name")
  (find-file abscope-dir)
  (find-file abscope-file)

  (end-of-buffer)
  (insert "\n\n* " struct "\n")
  (start-process "abscope" (current-buffer) "c:/absrc/abscope/abscope.exe" 
                 "-Qs" struct)
  ) 
(ab-query-struct "Foo"))
