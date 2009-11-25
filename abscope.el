;;; abscope.el --- functions for interacting with the abscope tags system
;;
;; Copyright (C) 2009
;; Author: Aaron Brady
;; Keywords: tags, tools
;; Homepage: http://github.com/abrady/abscope
;; Version: 0.0a
;;
;; USAGE: 
;; - make sure the abscope binary is in the path somewhere.
;; - abscope-query : basic query function
;; 
;; by default this prints to a .org file. you may want to set up the following
;; in your .emacs file:
;; (require 'org) 
;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;
;; todos:
;; - local vars command => completing read
;; - function params
;; - global string pool
;; - shrink footprint.
;; - push mark properly:
;; -- org-follow-link-hook
;; -- org-open-file
;; - add strings.abs : for searching all text strings
;; - context should grab entire line
;; - editors/MultiEditTable.h/(209):editors/MultiEditTable.h/(14):struct METable; : misparsed?
;; - recursive
(require 'cl)
(require 'ido)


;;----------------------------------------
;; my crazy alias section. I prefer mnemonics like
;; M-x abc to C-a C-b c commands, feel free to use or change.

(if t
	(progn
	  
	  (defalias 'abp 'abscope-pop-loc)
	  (defalias 'abq 'abscope-query)
	  (defalias 'abj 'abscope-jump
		"alias for jump")
	  
	  (defun abjs (tag)
		"jump struct"
		(interactive (list (abs-query-read "struct:" 'structs)))
		(abscope-jump tag "s"))
	  
	  (defun abjf (tag)
		"jump to func/define"
		(interactive (list (abs-query-read "func:" 'funcs)))
		(abscope-jump tag "fd"))
	  
	  (defun abjc (tag)
		"jump srcfile"
		(interactive (list (abs-query-read "srcfile:" 'srcfiles)))
		(abscope-jump tag "c"))
	  
	  (defun abjd (tag)
		"jump #define"
		(interactive (list (abs-query-read "define:" 'defines)))
		(abscope-jump tag "d"))
	  (defalias 'abfm 'abscope-find-members)
	  (defalias 'fm 'abscope-find-members)
	  (defalias 'fp 'abscope-find-params)
	  
	  ))

;;----------------------------------------
;; buffer local variables used

(defvar abscope-file		"abscope-queries.org"	"the org file where the output of abscope queries will go")
(defvar abscope-exe			"abscope.exe"			"the application that will run as a subprocess of this library")
(defvar abscope-process		nil						"the process for each project")
(defvar abscope-tq			nil						"the command-response queue for each process")
(defvar abscope-loc-history nil						"the process for each project")
(defvar abscope-tags		nil						"a list of tags exported when the abscope process starts by field")
(defvar abscope-tags-all	nil						"every tag string in a single list")

(make-variable-buffer-local 'abscope-tq)
(make-variable-buffer-local 'abscope-process)
(make-variable-buffer-local 'abscope-loc-history)
(make-variable-buffer-local 'abscope-tags)
(make-variable-buffer-local 'abscope-tags-all)

(defun abs-follow-link-hook ()
  "what do do after an abscope link is followed by org mode"
  )

(defun abscope-default-input ()
  "default input for user actions, e.g. the current word"
  (thing-at-point 'symbol))

(defun abscope-switch-buffer (buffer)
  "switch to the buffer"
  (switch-to-buffer buffer))

(add-hook 'org-follow-link-hook 'abs-follow-link-hook)

;;(setq abscope-dir "c:/src")

(defun abscope-push-loc ()
  "push the current point on the abscope list"
  (interactive)
  (let
	  ((m (point-marker)))
	(with-current-buffer (abscope-file)
	  (setq abscope-loc-history (cons m abscope-loc-history ))
	  )
	)
  )


(defun abscope-pop-loc ()
  "pop to the last pushed abscope location"
  (interactive)
  (let
	  ((m))
	(with-current-buffer (abscope-file)
	  (setq m (pop abscope-loc-history))
	  )
	(switch-to-buffer (marker-buffer m))
	(goto-char (marker-position m))
	)
  )



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
          (setq abscope-process (start-process "abscope" (current-buffer) abscope-exe "-t" "-Qa" "-"))
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
    (kill-process abscope-process)))


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

;;(setq foo '(1 2 3))
;;(pop (reverse foo))
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
          (if (string-match ".*? \\(.*\\)" Ctxt) ;; strip "struct " from "struct Foo"
	      (match-string 1 Ctxt)
	    (if (> (length Ctxt) 0) Ctxt ;; if no context, use file instead
	      (file-name-nondirectory File)
	      )
	    )
	  )
    (insert (format "%s [[file:%s::%s][%s]]" prefix File Lineno Ctxt))
    (insert (format "\t%s\n" Line))
    )
  )

(defun abs-reload-tags ()
  (let
	  ((b))
	  (if (file-exists-p "tags.el") 
		  (progn
			(if (not (setq b (find-buffer-visiting "tags.el")))
				(save-window-excursion
				  (setq abscope-tags nil) ;; clear this for reload
				  (find-file "tags.el")
				  (setq b (current-buffer)))) 
			(setq abscope-tags (if (and b (or (not abscope-tags) (not (verify-visited-file-modtime b))))
				(with-current-buffer b
				  (revert-buffer t t)
				  (beginning-of-buffer)
				  (eval (read (current-buffer)))
				  )
			  abscope-tags
			  ))
			(setq abscope-tags-all (loop for i in abscope-tags append (cadr i)))
			)
		)
	)
  )

(defun abscope-proc-print-output (proc str) 
  (with-current-buffer (process-buffer proc)
	(abs-reload-tags)
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

(defun abs-query (type fields tag)
  "fundamental function for queueing a request."
  (with-current-buffer (abscope-file)
    (abscope-re-launch)
    (if (equal type "") (setq type "a"))
    (if (equal tag "") (error "invalid (empty) tag"))

    (end-of-buffer)
    (insert "\n\n* " tag)
    (set-marker (process-mark abscope-process) (point))

    (tq-enqueue abscope-tq (concat "Query " type " " (or fields "a") " " tag " End\n") "^(QUERY_DONE))\n\n" abscope-process 'abscope-proc-print-output)
    (abs-proc-wait-once)
    (tq-process-buffer abscope-tq)
    )
)


(defun abscope-query (type tag &optional fields)
  "query a tag by type. uses pcre syntax:
- ? : for shortest match.
   1. \\b Match a word boundary
   2. \\B Match except at a word boundary
   3. \\A Match only at beginning of string
   4. \\Z Match only at end of string, or before newline at the end
   5. \\z Match only at end of string
   6. \\G Match only at pos() (e.g. at the end-of-match position of prior m//g)
"
  (interactive (list (read-string "s (s)truct (S)tructref (f)unc (F)uncref:" "a")
					 (completing-read "stag:" abscope-tags-all nil nil (abscope-default-input))))
  (abscope-switch-buffer (abscope-file))
  (abs-query type fields tag)
  )



(defun abs-query-read (prompt field)
  (with-current-buffer (abscope-file)
	(completing-read prompt (cadr (assoc field abscope-tags)) nil nil (abscope-default-input))))

(defun abqc (tag)
  "find a src file"
  (interactive (list (abs-query-read "src file to search for:" 'srcfiles)))
  (abscope-query "c" tag)
  )
(defun abqf (tag)
  "find a function def"
  (interactive (list (abs-query-read "func to search for:" 'funcs)))
  (abscope-query "f" tag)
  )

(defun abqF (tag)
  "find a function ref"
  (interactive (list (abs-query-read "func to search for:" 'funcs)))
  (abscope-query "F" tag)
  )

(defun abqs (tag)
  "find a struct def"
  (interactive (list (abs-query-read "struct to search for:" 'structs)))
  (abscope-query "s" tag)
  )

(defun abqp (tag)
  "find a  cryptic def"
  (interactive (list (abs-query-read "cryptic to search for:" 'cryptic)))
  (abscope-query "p" tag)
  )

(defun abqx (tag)
  "find everything with 'tag' in its context def"
  (interactive (list (read-string "context to search for:" (abscope-default-input))))
  (abscope-query "a" tag "x")
  )

(defun abqd (tag)
  "find define"
  (interactive (list (abs-query-read "The tag to search for:" 'defines)))
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
    (abscope-switch-buffer buf)
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
    (abscope-switch-buffer buf)
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

(defun abscope-jump(tag flags &optional flds)
  (interactive (list (read-string "tag:" (abscope-default-input))
               (read-string "flags (s)truct (f)unc:" "a" 'abscope-find-members-history)))
  (interactive)
  (let
      (
       (li)
       (loc)
       (lis)
       )
    (save-window-excursion
	  (abscope-push-loc)
      (with-current-buffer (abscope-file)
        (setq abscope-last-output nil)
        (abs-query flags nil tag) ;; this logs the history as well as provides the info
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

;; *************************************************************************
;; smart inserts/completes  
;; *************************************************************************          

(defun abscope-insert(tag flags)
  (interactive (list (read-string "tag:" (abscope-default-input))
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

(make-variable-buffer-local 'abs-find-members-history)
(make-variable-buffer-local 'abs-find-members-last-type)

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
          (abs-query "s" nil vartype)
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
	  (abs-query "fd" nil (format "\\b%s\\b" funcname))
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
