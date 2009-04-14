;;
;; process-buffer : the buffer for a process
;; process-mark   : 

(setq abscope-dir "c:/src")
(setq abscope-file "abscope-queries.org")
(setq abscope-exe (concat (file-name-directory (buffer-file-name)) "abscope.exe"))
(progn
  (defun ab-query-struct (struct)
    (interactive "sstruct name:")
    (find-file abscope-dir)
    (find-file abscope-file)
    
    (save-excursion
      (end-of-buffer)
      (insert "\n\n* " struct "\n")
      (start-process "abscope" (current-buffer) abscope-exe "-Qs" struct)
      )
    )
(ab-query-struct "ItemDef"))
