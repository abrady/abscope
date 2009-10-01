
;;
;; todos:
;; - local vars command => completing read
;; - function params
;; - list of files in project for auto complete
;; - global string pool
;; - shrink footprint.
;; - push mark properly:
;; -- org-follow-link-hook
;; -- org-open-file
;; - add strings.abs : for searching all text strings
;; - context should grab entire line
(setq abscope-file "abscope-queries.org")
(setq abscope-exe "c:/abs/abscope/abscope.exe")

(defun abs-follow-link-hook ()
  "what do do after an abscope link is followed by org mode"
  (push-mark)
  )


(add-hook 'org-follow-link-hook 'abs-follow-link-hook)

;;(setq abscope-dir "c:/src")
(defvar abscope-process nil "the process for each project")
(defvar abscope-tq     nil "the command-response queue for each process")
(make-variable-buffer-local 'abscope-tq)
(make-variable-buffer-local 'abscope-process)

(defun abscope-file ()
  "get the buffer for abscope given the context"
  (let
      ((bn default-directory))
    (if (string-match "\\(c:/src.*?\\)/" bn)
          (find-file-noselect (concat (match-string 1 bn) "/" abscope-file ))
      (find-file-noselect (concat "c:/src/" abscope-file)))))

(defun abscope-dir ()
  (with-current-buffer (abscope-file)
    (string-replace-match "/$" default-directory "")))

(defun abscope-proc ()
  "get the abscope process for a given project"
  (with-current-buffer (abscope-file)
    abscope-process))

(defun abscope-re-launch ()
  "launch/re-launch the exe"
  (with-current-buffer (abscope-file)
    (if (not (and abscope-process (equal (process-status abscope-process) 'run)))
        (save-window-excursion
          (setq abscope-process (start-process "abscope" (current-buffer) abscope-exe "-Qa" "-"))
          (setq abscope-tq (tq-create abscope-process))
          )
      )
    )
)

(defun abscope-init (project-dir)
  "fire up abscope for a given directory"
  (interactive "D project dir:")
  (save-window-excursion
    (with-current-buffer (find-file-noselect project-dir)
      (abscope-re-launch))))
    

(defun abscope-kill ()
  "kill abscope process in current project"
  (interactive)
  (with-current-buffer (abscope-file)
    (kill-process abscope-proc)))


;; (abs-print-locinfo (cdar foo) "**")
(defvar abscope-last-output nil
  "last form returned by abscope")

(defvar abscope-last-output-str nil
  "string last returned by abscope")

(make-variable-buffer-local 'abscope-last-output)
(make-variable-buffer-local 'abscope-last-output-str)

(defun abs-proc-eval-output (proc str)
  "helper for reading and evaluating any exprs in the output"
  (setq abscope-last-output (eval (read str)))
  )

;; (setq foo-str "'(
;; (LocInfo 
;;   (File . \"a b\")
;;   (Lineno . 1)
;;   (Line . \"a b c d;\")
;;   (Tag . \"asdf\")
;;   (RefName . \"refname\")
;;   (Ctxt . \"ctxt\")
;;   (Ref (LocInfo 
;;     (File . \"file ref\")
;;     (Lineno . 3)
;;     (Line . \"ref line str2;\")
;;     (Tag . \"ref asdf2\")
;;     (RefName . \"sub refname2\")
;;     (Ctxt . \"sub ctxt2\")
;;   ))
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
                        (set j (cdr i))
                      )
                    )
              )
          )
    ;; 
    (setq Ctxt
          (or 
           (string-replace-match ".*? " Ctxt "")
           (if (> (length Ctxt) 0) Ctxt nil)
           (file-name-nondirectory File)))
    (insert (format "%s [[file:%s::%s][%s]]" prefix File Lineno Ctxt))
    (insert (format "\t%s\n" (stripLeadingWhitespace Line)))
    )
  )

(defun abscope-proc-print-output (proc str) 
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (process-mark proc)) ;; todo: proper mark saving.
      (insert ":") ;; subtle notice of when the process finishes
      (let
          (
           (forms)
           )
        (setq abscope-last-output-str str)
        (condition-case err
            (progn
              (setq forms (abs-proc-eval-output proc str))
              (loop for i in forms do
                    (case (car i)
                      ('LocInfo (abs-print-locinfo (cdr i) "**"))
                      ('QUERY_DONE) ;; nothing to do
                      (t (message "unknown form %s" (print i))))
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

(defun abs-proc-wait-once ()
  "stall for just a short period to try to get the output
*seems* to be more responsive if you do this"
  (with-current-buffer (abscope-file)
    ;; 
    (accept-process-output abscope-process 0.1 0 1)))

(defun abs-proc-wait-until-done ()
  (with-current-buffer (abscope-file)
    (loop for i from 1 to 1000 until abscope-last-output 
          do (abs-proc-wait-once)
          )
    )
  )

(defun abs-query (type tag)
  "fundamental function for queueing a request."
  (with-current-buffer (abscope-file)
    (abscope-re-launch)
    (if (equal type "") (setq type "a"))
    (if (equal tag "") (error "invalid (empty) tag"))

    (end-of-buffer)
    (insert "\n\n* " tag)
    (set-marker (process-mark abscope-process) (point))

    (tq-enqueue abscope-tq (concat "Query " type " " tag " End\n") "^(QUERY_DONE))\n\n" abscope-process 'abscope-proc-print-output)
    (abs-proc-wait-once)
    (tq-process-buffer abscope-tq)
    )
)

(defun abscope-query (type tag)
  "query a tag by type. uses pcre syntax:
- ? : for shortest match.
   1. \\b Match a word boundary
   2. \\B Match except at a word boundary
   3. \\A Match only at beginning of string
   4. \\Z Match only at end of string, or before newline at the end
   5. \\z Match only at end of string
   6. \\G Match only at pos() (e.g. at the end-of-match position of prior m//g)
"
  (interactive "s (s)truct (S)tructref (f)unc (F)uncref:
stag:")
  (iswitchb-visit-buffer (abscope-file))
  (abs-query type tag)
  )


(defun abq (tag)
  "query for a tag from all types"
  (interactive (list (read-string "The tag to search for:" (readWordOrRegion))))
  (abscope-query "a" tag)
  )

(defun abqc (tag)
  "find a src file"
  (interactive (list (read-string "src file to search for:" (readWordOrRegion))))
  (abscope-query "c" tag)
  )
(defun abqf (tag)
  "find a function def"
  (interactive (list (read-string "func to search for:" (readWordOrRegion))))
  (abscope-query "f" tag)
  )

(defun abqF (tag)
  "find a function ref"
  (interactive (list (read-string "func to search for:" (readWordOrRegion))))
  (abscope-query "F" tag)
  )

(defun abqs (tag)
  "find a struct def"
  (interactive (list (read-string "struct to search for:" (readWordOrRegion))))
  (abscope-query "s" tag)
  )

(defun abqp (tag)
  "find a  cryptic def"
  (interactive (list (read-string "cryptic to search for:" (readWordOrRegion))))
  (abscope-query "p" tag)
  )

(defun abqd (tag)
  "find define"
  (interactive (list (read-string "The tag to search for:" (readWordOrRegion))))
  (abscope-query "ad" tag)
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
    (find-file (buffer-file-name (abscope-file)))

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
    (find-file (abscope-file))
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

(defun abscope-find-var-type (varname)
  "int a; ... should return int"
  (let
      (
       (pt-max (point))
	   (s)
       )
    (save-excursion
      (beginning-of-defun)
      (re-search-forward varname pt-max)
      (backward-sexp 2)
      (setq s (thing-at-point 'symbol))
	  (if (equal s "NOCONST")
		  (progn
			(forward-char 1)
			(thing-at-point 'symbol))
		s)
      )))

(defun abscope-gather-vars (start end)
  "gather variable decls in a region"
  (save-excursion
    (goto-char start) 
    (loop while (re-search-forward ".*?\\(\\b[a-z_]+\\b\\);" end t)
          collect (cons (match-string-no-properties 1) (match-string-no-properties 0)))))

(defun abscope-gather-struct-vars ()
  (save-excursion
    (abscope-gather-vars (point) (progn (end-of-defun) (point)))
    )
  )

(defun abscope-jump(tag flags)
  (interactive (list (read-string "tag:" (readWordOrRegion))
               (read-string "flags (s)truct (f)unc:" "a" 'abscope-find-members-history)))
  (interactive)
  (let
      (
       (li)
       (loc)
       (lis)
       )
    (save-window-excursion
      (with-current-buffer (abscope-file)
        (setq abscope-last-output nil)
        (abs-query flags tag) ;; this logs the history as well as provides the info
        (abs-proc-wait-until-done)      (if (not abscope-last-output) 
                                            (error "no info for type %s" vartype))      
        (setq lis (loop for i in abscope-last-output
                        if (eq (car i) 'LocInfo) collect (cons (cdr (assoc 'Tag (cdr i))) (cdr i))))
		(or (> (length lis) 0) (error "no locations found for %s" tag))
		(setq loc (or
				  (assoc tag lis)
				  (if (= (length lis) 1)
					  (car lis)
					(progn
					  (setq li (completing-read "jump to:" lis nil t nil 'abscope-find-members-history nil nil))
					  (if (not li)
						  (error "no location chosen"))
					  (assoc li lis))
					)
				  )
			  )
        )
      )
    (if (not loc)
        (error "no location found"))
    (find-file (concat (abscope-dir) (cdr (assoc 'File loc))))
    (goto-line (string-to-number   (cdr (assoc 'Lineno loc))))
    )
  )

(defalias 'abj 'abscope-jump
  "alias for jump")

(defun abjs (tag)
  "jump struct"
  (interactive (list (read-string "struct:" (readWordOrRegion))))
  (abscope-jump tag "s"))
          
(defun abjf (tag)
  "jump func"
  (interactive (list (read-string "func:" (readWordOrRegion))))
  (abscope-jump tag "f"))

(defun abjc (tag)
  "jump srcfile"
  (interactive (list (read-string "srcfile:" (readWordOrRegion))))
  (abscope-jump tag "c"))

(defun abjd (tag)
  "jump #define"
  (interactive (list (read-string "define:" (readWordOrRegion))))
  (abscope-jump tag "d"))
          
(defun abscope-insert(tag flags)
  (interactive (list (read-string "tag:" (readWordOrRegion))
                     (read-string "flags (s)truct (f)unc:" "a" 'abscope-find-members-history)))
  (interactive)
  (let
      (
       (li)
       (loc)
       (lis)
       )
    (save-window-excursion
      (with-current-buffer (abscope-file)
        (setq abscope-last-output nil)
        (abscope-query flags tag) ;; this logs the history as well as provides the info
        (abs-proc-wait-until-done)
        (if (not abscope-last-output) 
            (error "no info for type %s" vartype))      
        (setq lis (loop for i in abscope-last-output
                        if (eq (car i) 'LocInfo) collect (cons (cdr (assoc 'Tag (cdr i))) (cdr i))))
        (setq li (completing-read "insert:" lis nil t (caar lis) 'abscope-find-members-history nil nil))
        (if (not li)
            (error "no location chosen"))
        )
      )
    (insert li)
    )
  )

(defvar abs-find-members-history)
(defvar abs-find-members-last-type)

(defun abs-nearest-tag ()
  "helper for finding a previous tag"
  (save-excursion
	(backward-word)
	(thing-at-point 'symbol)))


(defun abscope-find-members (varname)
  (interactive (list (abs-nearest-tag)))
  (let
      (
       (vartype)
       (mbrs)
       (li)
       (mbr)
       (choice)
       )
    (save-excursion
	  (if (not varname)
		  (error "no var found"))
      (setq vartype (abscope-find-var-type (concat "\\b" varname "\\b")))
      (if (not vartype)
          (error "couldn't find vartype for var %s" varname)
        )
      (setq vartype (concat "\\b" vartype "\\b" ))
      (setq abscope-last-output nil)
      (save-window-excursion
        (with-current-buffer (abscope-file)
          (abs-query "s" vartype)
          (abs-proc-wait-until-done)
          (if (not abscope-last-output)
              (error "no info for type %s" vartype))
          
          (if (equal (car abscope-last-output) '(QUERY_DONE))
              (error "no matches found for type %s" vartype))
          
          (setq li (cdar abscope-last-output))
          (find-file (cdr (assoc 'File li)))
          (goto-line (string-to-number (cdr (assoc 'Lineno li))))
          (setq mbrs (abscope-gather-struct-vars)))
        )
      )
    (setq choice (completing-read "mbr:" mbrs nil t nil 'abs-find-members-history nil nil))
    (setq mbr (assoc choice mbrs))
    (insert choice)
    (if (string-match " *\b\\(.*?\\) " (cdr mbr))
        (setq abs-find-members-last-type (match-string 1 (cdr mbr))))
    )
  )
(defalias 'abfm 'abscope-find-members)
(defalias 'fm 'abscope-find-members)
(defalias 'fp 'abscope-find-params)
(defun abs-choose-result ()
  (let
	  (
	   (o)
	   (c)
	   )
	(setq o (loop for j from 0 for i in abscope-last-output 
				  if (eq (car i) 'LocInfo) collect (cons (format "%s" (cdr (assoc 'Tag (cdr i)))) (cdr i))))
	(setq c 
		  (if (> (length o) 1)
			  (completing-read "tag: " o nil t nil 'abs-find-members-history)
			(caar o)))
	(cdr (assoc c o))
	)
  )
  

(defun abscope-find-params (funcname)
  (interactive (list (abs-nearest-tag)))
  (let
	  (
	   (li) 
	   (parms-str)
	   (parms)
	   (p)
	   (tag)
	   )
	(if (not funcname)
		(error "no func found"))
	(with-current-buffer (abscope-file)	  
	  (abs-query "fd" (format "\\b%s\\b" funcname))
	  (abs-proc-wait-until-done)
	  (if (not abscope-last-output)
		  (error "no info for func %s" funcname))
	  (setq li (abs-choose-result))
	  (save-window-excursion
		(find-file (cdr (assoc 'File li)))
		(goto-line (string-to-number (cdr (assoc 'Lineno li))))
		(re-search-forward "(\\(.*\\))" nil t)
		(setq parms-str (match-string 1))
		)
	  )
	(setq tag (cdr (assoc 'Tag li)))
	(setq parms (split-string parms-str ","))
	(loop with first = nil for i in parms do
		  (if first
			  (insert ", "))
		  (setq first t)
		  (setq p (read-string (concat tag ": " i " :")))
		  (insert p))
	) 
  )
	  
	  


(provide 'abscope)
