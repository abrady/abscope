;;; abscope.el --- functions for interacting with the abscope tags system
;;
;; Copyright (C) 2009
;; Author: Aaron Brady
;; Keywords: tags, tools
;; Homepage: http://github.com/abrady/abscope
;; Version: 0.0a
;;
;; Installation:
;; - make sure the abscope binary is in the path somewhere, or set `abscope-exe' to its location
;; - load the abscope library
;; - call abscope-scan-src on the directory you'd like to scan (customize abscope-scan-args if necessary. -E dir is the most likely thing to add for excluding certain directories)
;;
;; Running:
;; All tag queries are pcre regular expressions (http://www.pcre.org/), and are case insensitive.
;;
;; The results of tag queries are printed to a file in the project
;; directory named the value of `abscope-file'. usually
;; abscope-queries.org. as you run abscope-query and abscope-jump
;; functions this file will keep a handy history of everything you've
;; searched for so far in a format 'org-mode' can understand for
;; navigation.
;;
;; Most people will use one of the query or jump helper functions:
;; - abscope-query-function : run a query for a function and print the results in the `abscope-file' file.
;; - abscope-jump-function  : query and jump to the matching location (best guess for ambiguous)
;; - abscope-query-struct   : same as above, but for a struct and not a function
;; - abscope-jump-struct    : "", jump to the best-guess location
;; - ...
;;
;; General queries can be done with this function:
;; - abscope-query : takes a tag and a string of types to match, with
;; "a" being a special character for matching all
;; types. e.g. (abscope-query "a" "main") will look for any tag named
;; 'main'. you could also say (abscope-query "df" "main") to look for
;; all defines or functions named 'main'
;;
;; Navigation and history:
;; - abscope-pop-loc    : jump to the last location you used an abscope-jump function or link to get to
;; - abscope-next-match : next match from the last query run
;; - abscope-prev-match : prev match from the last query run
;; - abscope-query-replace
;; 
;; Autocomplete (work in progress):
;; - abscope-insert       : do a query, and then insert the results
;; - abscope-find-members : autocomplete struct members: if you call this with point after a-> it will try to get the type for 'a' and get you the members.
;; - abscope-find-params  : autocomplete for function params:
;;
;; 
;; Finally, you may want to set up the following:
;; in your .emacs file so the `abscope-file' buffer is in org-mode automatically:
;; (require 'org) 
;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;;----------------------------------------

;;----------------------------------------
;; todos:
;; - function params
;; - global string pool
;;
;; exe todos:
;; - shrink footprint.
;; - add strings.abs : for searching all text strings
;; - context should grab entire line.
;; - editors/MultiEditTable.h/(209):editors/MultiEditTable.h/(14):struct METable; : misparsed?
;; - recursive search/call graph
;; - faster exe load/load on start
;;----------------------------------------
(require 'cl)


;;----------------------------------------
;; my crazy alias section. I prefer mnemonics like
;; M-x abc to C-a C-b c commands, feel free to use or change.

(defcustom abscope-define-aliases t
  "variable that controls if the three and four letter aliases for the abscope set of functions are defined.")

(defcustom abscope-default-project-root "c:/src"
  "if no abscope files can be found in the current buffer's directory or any parent directories, jump to this directory to find them")

(defcustom abscope-scan-args '("-R" "." "-E" "AutoGen" "-E" "3rdparty")
  "parameters to an abscope scan, e.g. -E .svn for ignored directories ")

(if abscope-define-aliases
	(progn
	  
	  (defalias 'abp 'abscope-pop-loc)

	  ;; primary query/jump functions
	  (defalias 'abq 'abscope-query)
	  (defalias 'abj 'abscope-jump)
	  
	  ;; query helpers
	  (defalias 'abqc 'abscope-query-srcfile)
	  (defalias 'abqf 'abscope-query-function)
	  (defalias 'abqF 'abscope-query-funcref)
	  (defalias 'abqs 'abscope-query-struct)
	  (defalias 'abqp 'abscope-query-cryptic)
	  (defalias 'abqd 'abscope-query-define)
	  (defalias 'abqx 'abscope-query-context)

	  ;; jump helpers
	  (defalias 'abjc 'abscope-jump-srcfile)
	  (defalias 'abjf 'abscope-jump-function)
	  (defalias 'abjF 'abscope-jump-funcref)
	  (defalias 'abjs 'abscope-jump-struct)
	  (defalias 'abjp 'abscope-jump-cryptic)
	  (defalias 'abjd 'abscope-jump-define)

	  ;; auto complete
	  (defalias 'abfm 'abscope-find-members)
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

(defun abscope-scan-src (dir)
  "perform a scan on the passed directory using the default options. see `abscope-scan-args'"
  (interactive "Ddir:")
  (save-window-excursion
	(find-file dir)
	(set-process-sentinel (eval `(start-process (format "abscope-scan<%s>" dir) nil ,abscope-exe ,@abscope-scan-args)) 
						  (lambda (p e) (message (format "%s `%s'" p e)) ))))
			   

(defun abscope-follow-link-hook ()
  "what do do after an abscope link is followed by org mode"
  )

(defun abscope-default-input ()
  "default input for user actions, e.g. the current word"
  (if mark-active
	  (buffer-substring
	   (region-beginning) (region-end))
	(thing-at-point 'symbol)))

(defun abscope-switch-buffer (buffer)
  "switch to the buffer"
  (switch-to-buffer buffer))

(add-hook 'org-follow-link-hook 'abscope-follow-link-hook)

(defun abscope-buf ()
  "get the buffer for abscope given the context"
  (let
      ((dn)
	   (fn)
	   (tmp)
	   (tmp2)
	   (was-loaded)
	   (b)
	   )

	;; walk up the directory tree until a project is found
	(setq tmp default-directory)
	(while tmp
	  (setq fn (concat tmp "tags.el"))
	  (if (file-exists-p fn)
		  (progn 
			(setq dn tmp)
			(setq tmp nil))
		(if (equal tmp (setq tmp2 (file-name-as-directory (expand-file-name (concat tmp ".."))))) ;;(<= (length tmp) 3) ;; on win32 c:/.. => c:/
			(setq tmp nil)
		  (setq tmp tmp2))
		)
	  )
		
	(if (not dn)
		(setq dn abscope-default-project-root))
	  
	(setq fn (concat (file-name-as-directory dn) abscope-file))

	(if (not (find-buffer-visiting fn))
		(setq was-loaded t))
	(setq b (find-file-noselect fn))
;;	(if was-loaded 
;;		(with-current-buffer b (setq buffer-read-only t)))
	b
	))

(defmacro with-abscope-buffer (&rest body)
  "helper to set the context for abscope execution"
  `(with-current-buffer (abscope-buf)
	 (let
		 (
;;		  (was-read-only buffer-read-only)
		  (res)) 
;;	   (setq buffer-read-only nil)
	   (setq res (progn ,@body))
;;	   (if was-read-only
;;		   (setq buffer-read-only t))
	   res
	   )
	 ))


(defun abscope-push-loc ()
  "push the current point on the abscope list"
  (interactive)
  (let
	  ((m (point-marker)))
	(with-abscope-buffer
	  (setq abscope-loc-history (cons m abscope-loc-history ))
	  )
	)
  )


(defun abscope-pop-loc ()
  "pop to the last pushed abscope location"
  (interactive)
  (let
	  ((m))
	(with-abscope-buffer
	  (setq m (pop abscope-loc-history))
	  )
	(if (not m) (message "location stack is empty")
	  (progn
		(switch-to-buffer (marker-buffer m))
		(goto-char (marker-position m))
		))
	)
  )

(defun abscope-dir () (with-abscope-buffer default-directory))

(defun abscope-proc ()
  "get the abscope process for a given project"
  (with-abscope-buffer
    abscope-process))

(defun abscope-re-launch ()
  "launch/re-launch the exe"
  (with-abscope-buffer
    (if (not (and abscope-process (equal (process-status abscope-process) 'run)))
        (save-window-excursion
          (setq abscope-process (start-process "abscope" (current-buffer) abscope-exe "-t" "-Qa" "-"))
          (setq abscope-tq (tq-create abscope-process))
		  (message "%s not running. launching..." abscope-exe)
		  (sleep-for 0 750) ;; stall for a bit to let the message show/process launch.
          )
      )
    )
)

(defun abscope-init (project-dir)
  "fire up abscope for a given directory. you may want to add this to your startup for commonly used large projects where abscope load time may be a problem"
  (interactive "D project dir:")
  (save-window-excursion
    (with-current-buffer (find-file-noselect project-dir)
      (abscope-re-launch))))
    

(defun abscope-kill ()
  "kill abscope process in current project"
  (interactive)
  (with-abscope-buffer
   (if abscope-process
	   (kill-process abscope-process)
	 (message "no process to kill"))))

(defun abscope-clear ()
  "erase the abscope buffer"
  (interactive)
  (with-abscope-buffer
    (erase-buffer)))


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
			(if (and b (or (not abscope-tags) (not (verify-visited-file-modtime b)))) 
				(progn 
				  (setq abscope-tags (with-current-buffer b
									   (revert-buffer t t)
									   (beginning-of-buffer)
									   (eval (read (current-buffer)))
									   )) 
				  (setq abscope-tags-all (loop for i in abscope-tags append (cadr i)))
				  )
			  )
			)
		)
	)
  )

(defun abscope-proc-print-output (proc str) 
  "the abscope process filter"
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
  (with-abscope-buffer
    ;; 
    (accept-process-output abscope-process 0 100 1)))

(defun abs-proc-wait-until-done ()
  (with-abscope-buffer
    (loop for i from 1 to 1000 until abscope-last-output 
          do (abs-proc-wait-once)
          )
    )
  )

(defun abs-query (type fields tag)
  "fundamental function for queueing a request. fields a string with any of:
t.ag default if fields is nil.
r.ef : context-specific referrer to the tag e.g. int foo, ref is int. a->b ref is a.
c.ontext: where the tag was parsed. e.g. function, struct, global
f.ile: file where tag was parsed
l.ine: actual text line where the tag was parsed"
  (with-abscope-buffer
    (abscope-re-launch)
    (if (equal type "") (setq type "a"))
    (if (equal tag "") (error "invalid (empty) tag"))

    (end-of-buffer)
    (insert "\n\n* " tag)
    (set-marker (process-mark abscope-process) (point))

    (tq-enqueue abscope-tq (concat "Query " type " " (or fields "t") " " tag " End\n") "^(QUERY_DONE))\n\n" abscope-process 'abscope-proc-print-output) ;; search for (t)ags by default
    (abs-proc-wait-once)
    (tq-process-buffer abscope-tq)
    )
)

(defun abs-querytype-from-char (c)
  "get they query type from the passed character"
  (case c
	(?c 'srcfiles)
	(?f 'funcs)
	(?F 'funcs)
	(?s 'structs)
	(?p 'cryptic)
	(?d 'defines)
	))

(defun abs-query-read (prompt field)
  (completing-read prompt 
				   ;; jump to abscope buf to get list of tags
				   (with-abscope-buffer
					(abs-reload-tags)
					(if field (cadr (assoc field abscope-tags)) abscope-tags-all))
				   nil nil 
				   ;; get symbol at point
				   (abscope-default-input)))

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
					 (abs-query-read "tag:" nil)))
  (abscope-switch-buffer (abscope-buf))
  (abs-query type fields tag)
  )

(defun abscope-query-srcfile (tag)
  "find a src file"
  (interactive (list (abs-query-read "src file to search for:" 'srcfiles)))
  (abscope-query "c" tag)
  )
(defun abscope-query-function (tag)
  "find a function"
  (interactive (list (abs-query-read "func to search for:" 'funcs)))
  (abscope-query "f" tag)
  )

(defun abscope-query-funcref (tag)
  "find a function ref"
  (interactive (list (abs-query-read "func to search for:" 'funcs)))
  (abscope-query "F" tag)
  )

(defun abscope-query-struct (tag)
  "find a struct def"
  (interactive (list (abs-query-read "struct to search for:" 'structs)))
  (abscope-query "s" tag)
  )

(defun abscope-query-cryptic (tag)
  "find a  cryptic def"
  (interactive (list (abs-query-read "cryptic to search for:" 'cryptic)))
  (abscope-query "p" tag)
  )

(defun abscope-query-define (tag)
  "find define"
  (interactive (list (abs-query-read "The tag to search for:" 'defines)))
  (abscope-query "ad" tag)
  )

(defun abscope-query-context (tag)
  "find everything with 'tag' in its context def"
  (interactive (list (read-string "context to search for:" (abscope-default-input))))
  (abscope-query "a" tag "x")
  )

;; *************************************************************************
;; smart jump directly to query
;; *************************************************************************          


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
      (with-abscope-buffer
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
	  
(defun abscope-jump-srcfile (tag)
  "jump srcfile"
  (interactive (list (abs-query-read "srcfile:" 'srcfiles)))
  (abscope-jump tag "c"))

(defun abscope-jump-function (tag)
  "jump to func/define"
  (interactive (list (abs-query-read "func:" 'funcs)))
  (abscope-jump tag "fd"))

(defun abscope-jump-struct (tag)
  "jump struct"
  (interactive (list (abs-query-read "struct:" 'structs)))
  (abscope-jump tag "s"))

(defun abscope-jump-cryptic (tag)
  "jump cryptic"
  (interactive (list (abs-query-read "cryptic:" 'cryptic)))
  (abscope-jump tag "s"))	  
	  
(defun abscope-jump-define (tag)
  "jump #define"
  (interactive (list (abs-query-read "define:" 'defines)))
  (abscope-jump tag "d"))


;; *************************************************************************
;; navigating matches
;; *************************************************************************          

(defun abscope-next-match ()
  "find the next match and go to it"
  (interactive)
  (let
      ((buf))
    (find-file (buffer-file-name (abscope-buf)))

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
    (find-file (abscope-buf))
    (forward-line -1)
    (setq buf (save-window-excursion
                      (org-open-at-point)
                      (current-buffer))
          )
    (abscope-switch-buffer buf)
    )
)


;; *************************************************************************
;; smart inserts/completes (work in progress)
;; *************************************************************************          


(defun abscope-query-replace ()
  "do a query-replace on each match in the current query from the current point in the query buffer."
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



(defun abscope-insert(tag flags)
  "do a query, and then insert the results"
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
      (with-abscope-buffer
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
  "autocomplete struct members: if you call this with point after a-> it will try to get the type for 'a' and get you the members."
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
        (with-abscope-buffer
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
  "autocomplete for function params. call this with point after opening parens"
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
	(with-abscope-buffer	  
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

;; *************************************************************************
;; autocomplete intergration. very handy
;; *************************************************************************

(defun abscope-autocomplete-candidate-words ()
  "helper for auto-completing a word"
  (cond 
   ((re-search-backward "//" (line-beginning-position) t) nil)
   ((string-match "($" ac-prefix) (abscope-autocomplete-candidate-functions))
   (t
	(with-abscope-buffer
	 (all-completions ac-prefix abscope-tags-all))
	)))
   

(defvar abscope-autocomplete-sources '((candidates . abscope-autocomplete-candidate-words)))

(defun abscope-autocomplete-init ()
  (interactive)
  "set up the auto complete variables"
  (setq ac-sources '(
					 abscope-autocomplete-sources
					 ac-source-words-in-buffer
					 )))
(provide 'abscope)
