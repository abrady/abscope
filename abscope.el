;;
;; process-buffer : the buffer for a process
;; process-mark   : 
(setq abscope-dir "c:/src")
(setq abscope-file "abscope-queries.org")
(setq abscope-exe (concat (file-name-directory (buffer-file-name)) "abscope.exe"))

(defun abscope-query (type tag)
  (interactive "s (s)truct (f)unc (r)ef:
stag:")
  (find-file abscope-dir)
  (find-file abscope-file)
  (end-of-buffer)
  (insert "\n\n* " tag "\n")
  (start-process "abscope" (current-buffer) "c:/abs/abscope/abscope.exe" 
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
  (abscope-query "srf" tag))
 


(defun abt ()
  (interactive)
  (find-file "c:/abs/abscope/test")
  (find-file abscope-file)
  (end-of-buffer)
  (insert "\n\n* ItemDef \n")
  (shell-command "c:/abs/abscope/abscope.exe -f itemCommon.h")
  (shell-command "c:/abs/abscope/abscope.exe -Qs ItemDef" (current-buffer)))

(provide 'abscope)
