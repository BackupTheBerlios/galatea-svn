;;; psql-mode.el --- Mode for editing postgres sql
;;
;; ~/lib/emacs/lisp/psql-mode.el ---
;;
;; $Id: psql-mode.el,v 1.1.1.1 2003/03/10 20:23:15 michael Exp $
;;

;; Author:    James H Gorrell     harley@bcm.tmc.edu
;; Keywords:  postgres sql-mode
;; URL:       http://www.hgsc.bcm.tmc.edu/~harley/elisp/psql-mode.el

;;; Commentary:
;;
;; psql-mode was written to edit the postgres dialect of sql
;; and work with the psql command in a subshell.  Mostly
;; fontifcation of text with some customization of comint
;; mode.
;;
;; It was developed under 19.34.
;;

;;

;; I have these lines in my .emacs:
;;
;; (autoload 'psql-mode "psql-mode" "Mode for editing postgress sql." t)
;; (autoload 'psql-run  "psql-mode" "Mode for editing postgress sql." t)
;; (if (eq window-system 'x)
;;     (progn
;;       (add-hook 'psql-mode-hook 'turn-on-font-lock)
;;       (add-hook 'psql-run-mode-hook 'turn-on-font-lock)
;;       ))
;; (add-to-list 'auto-mode-alist '("\\.p?sql$" . psql-mode ))
;; (setq psql-run-host "psql.bcm.tmc.edu")
;; (setq psql-run-port "5432")
;; (setq psql-program-name "/genome7/home/harley/Stuff/PG/postgres95/bin/psql")

;; To Do:
;;  * auto indent
;;  * tempo support
;;  * multiple buffers
;;
;; Bugs:
;;  * the fontification of the psql-run buffer is too aggressive.

;;; Thanks to:
;;  Olin Shivers - comint.el
;;  Jolly Chen & Andew Yu - Postgres
;;  The PG95 team - Postgres95 

;;; Variables:

(defvar psql-mode-abbrev-table nil "")
(define-abbrev-table 'psql-mode-abbrev-table ())

;; Editing...
(defvar psql-mode-hook nil
  "*Hook for customising psql mode.")

(defvar psql-comment-start "--"
  "Start of a psql comment.")

(defvar psql-comment-column 40
  "Column to indent comments to.")

(defun jhg-psql-indent-comment ()
  "Indent a psql comment to psql-comment-column."
  (interactive)
  (save-excursion
    (let (bol eol p com-start com-col)
      (setq p (point))
      (beginning-of-line)
      (setq bol (point))
      (end-of-line)
      (setq eol (point))
      (goto-char bol)
      (setq com-start (- (search-forward psql-comment-start eol t)
			 (length psql-comment-start)))
      (goto-char com-start)
      (setq com-col (current-column))
      (cond
       ;; n
       ;; make it flush left
       ((= psql-comment-column com-col)
	(goto-char com-start)
	(delete-horizontal-space))
       ;; indent it
       ((< com-start eol)
	(goto-char com-start)
	(delete-horizontal-space)
	(indent-to-column psql-comment-column))
       ))))

;; Running...
(defvar psql-program-name "psql"
  "*Name of psql executable")
(defvar psql-program-args '()
  "*List of arguments to pass to psql at startup.
For example:   (\"-a\" \"foobar\")")

(defvar psql-run-unset-pager t
  "*Unset the pager enviornment variable before starting psql.")

(defvar psql-run-initial-command nil ;"\\o |cat\n"
  "*An intital command to sent at the start of each psql process.
This is typicaly used to turn off the pager.
The the command must end with a '\\n'.
")

(defvar psql-run-host (or (getenv "PGHOST") "localhost")
  "*Hostname of default psql connection.")
(defvar psql-run-port (or (getenv "PGPORT") "5432")
  "*Default portname of psql connection.")
(defvar psql-run-database nil
  "*Default database of psql connection.")

(defvar psql-run-hook nil
  "*Hook for customising psql run mode.")

(defvar psql-run-prompt-regexp "\\sw+=>"
  "Regexp to match the psql prompt")

;; Both...
(defvar psql-load-hook nil
  "*Hooks to run after loading psql-mode.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; psql-mode : editing psql
;;

(defvar psql-mode-syntax-table nil
  "Syntax table for psql-mode."
  )

(if psql-mode-syntax-table
    nil
  (setq psql-mode-syntax-table (make-syntax-table))
  ;; Comments
  (modify-syntax-entry ?-  "<124b"  psql-mode-syntax-table)
  (modify-syntax-entry ?\n ">b"  psql-mode-syntax-table)
  ;; Change "_" to be part of a word.
  (modify-syntax-entry ?\_ "w"  psql-mode-syntax-table)
  ;; Change the meanings of ' and "
  (modify-syntax-entry ?\' "\"" psql-mode-syntax-table) ; quote
  (modify-syntax-entry ?\" "\." psql-mode-syntax-table) ; not quote
  )


(defvar psql-font-lock-keywords
  (list
   ;; lc
   (concat
    "\\<\\(a\\(bort\\|cl\\|dd\\|fter\\|ggregate\\|l\\(l\\|ter\\)\\|"
    "nd\\|ppend\\|rch\\(_store\\|ive\\)\\|sc?\\)\\|b\\(ackward\\|"
    "e\\(fore\\|gin\\)\\|inary\\|y\\)\\|c\\(ast\\|hange\\|"
    "l\\(ose\\|uster\\)\\|o\\(lumn\\|mmit\\|py\\)\\|reate\\|"
    "ur\\(rent\\|sor\\)\\)\\|d\\(atabase\\|e\\(clare\\|l\\(ete\\|"
    "imiters\\)\\|sc\\)\\|istinct\\|o\\|rop\\)\\|e\\(nd\\|"
    "x\\(ecute\\|plain\\|tend\\)\\)\\|f\\(etch\\|or\\(\\|"
    "ward\\)\\|rom\\|unction\\)\\|gr\\(ant\\|oup\\)\\|h\\(aving\\|"
    "eavy\\)\\|i\\(n\\(\\|dex\\|herits\\|s\\(ert\\|tead\\)\\|to\\)\\|"
    "snull\\)\\|l\\(anguage\\|i\\(ght\\|ke\\|sten\\)\\|oad\\)\\|"
    "m\\(erge\\|ove\\)\\|n\\(ew\\|o\\(ne\\|t\\(\\|hing\\|ify\\|"
    "null\\)\\)\\|ull\\)\\|o\\([nr]\\|ids\\|p\\(erator\\|tion\\)\\|"
    "rder\\)\\|p\\(rivileges\\|u\\(blic\\|rge\\)\\)\\|r\\(e\\(cipe\\|"
    "name\\|place\\|t\\(rieve\\|urns\\)\\|voke\\)\\|"
    "ollback\\|ule\\)\\|s\\(e\\(lect\\|t\\(\\|"
    "of\\)\\)\\|t\\(d\\(in\\|out\\)\\|ore\\)\\)\\|t\\(able\\|o\\|"
    "ransaction\\|ype\\)\\|u\\(pdate\\|sing\\)\\|v\\(a\\(cuum\\|"
    "lues\\)\\|ersion\\|iew\\)\\|w\\(here\\|ith\\|ork\\)\\)\\>"  
    )

   (cons
    (concat "\\<\\("
	    "char\\(\\|[248]\\|16\\)\\|"
	    "float[48]\\|"
	    "int[24]?\\|"
	    "number\\|date\\|" ;; oracle
	    "text\\|varchar2?\\|oid"
	    "\\)\\>"
	    )
    'font-lock-type-face)
    )
  "Keywords to hilight in font-lock-mode"
  )

;;;###autoload
(defun psql-mode ()
  "Major mode for editing psql."
  (interactive)
  ;;
  (kill-all-local-variables)
  (setq mode-name "psql")
  (setq major-mode 'psql-mode)
  (set-syntax-table psql-mode-syntax-table)
  ;; shared stuff
  (psql-mode-shared-setup)

  ;;
  (make-local-variable 'font-lock-defaults)
  ;; keywords key-words-only case-fold
  (setq font-lock-defaults '(psql-font-lock-keywords nil t))
  ;;
  (run-hooks 'psql-mode-hook)
  )

(defun psql-mode-shared-setup ()
  "Code shared between psql-mode and psql-run-mode."
  ;;
  (set-syntax-table psql-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start psql-comment-start)
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "--+ *")
  (setq local-abbrev-table psql-mode-abbrev-table)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; psql-run-mode : running psql
;;
(require 'comint)


(defconst psql-run-font-lock-keywords
  (append
   psql-font-lock-keywords
   (list
    (cons psql-run-prompt-regexp 'font-lock-type-face)
    )
   )
  "Keywords to highligh in psql-run-mode.")

(defvar psql-run-mode-map nil)
(if psql-run-mode-map
    nil
  (setq psql-run-mode-map (copy-keymap comint-mode-map))
  (define-key psql-run-mode-map "\e;" 'psql-run-magic-semicolon)
  )


(defun psql-prompt-host-port ()
  "Prompt for the particulars of the database to connect to.
Save the data in the global variables psql-run-host,
psql-run-port and psql-run-database."
  (let (h p d)
    (setq h (read-string "Host:" (or psql-run-host "localhost")))
    (setq p (read-string "Port:" (or psql-run-port "5432")))
    (setq d (read-string "Database:" (or psql-run-database "")))
    ;; Read all without an error.
    (setq 
     psql-run-host h
     psql-run-port p
     psql-run-database d
     )
    ))

(defun psql-run-make-args ()
  "Make the list of args to pass to the psql subprocess."
  (append
   (if (and psql-run-host (< 1 (length psql-run-host)))
       (list "-h" psql-run-host))
   (if (and psql-run-port (< 1 (length psql-run-port)))
       (list "-p" psql-run-port))
   (if (and psql-run-database (< 1 (length psql-run-database)))
       (list "-d" psql-run-database))
   psql-program-args
   ))

(defun psql-run-mode ()
  "Apply psql customizations to comint mode."
  (interactive)
  (comint-mode)
  ;;
  (setq comint-prompt-regexp psql-run-prompt-regexp)
  (setq comint-get-old-input 'psql-run-get-old-input)
  ;;
  (setq major-mode 'psql-run-mode)
  (setq mode-name (format "psql:%s" psql-run-database))
  (use-local-map psql-run-mode-map)
  ;; shared stuff
  (psql-mode-shared-setup)
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(psql-run-font-lock-keywords nil t)) ; kw kw-only fold
  ;;
  (run-hooks  'psql-run-mode-hook)
  )

;;;###autoload
(defun psql-run (&optional arg)
  "*Start or continue a interactive psql session.

To have multiple sessions, rename the '*psql*' buffer and
start another session.

If a prefix is given, prompt for the host, port and name of
the database to connect to.
"
  (interactive "P")

  ;; Based on shell from shell.el.
  (if (not (comint-check-proc "*psql*"))
      (let ((orig-pager-env (getenv "PAGER")))

	(if psql-run-unset-pager
	    (setenv "PAGER" nil))

	;; prompt for connection info?
	(if (or arg (not (and psql-run-host psql-run-port)))
	    (psql-prompt-host-port))

	;;
	(set-buffer 
	 (apply 'make-comint 
		"psql" psql-program-name nil (psql-run-make-args)))
	(setq psql-buffer (current-buffer))
	(psql-run-mode)
	(pop-to-buffer "*psql*") ; flip to just started

	;; send the initial psql comands. Used to turn off the pager.
	(if psql-run-initial-command 
	    (comint-send-string 
	     (get-buffer-process (current-buffer))
	     psql-run-initial-command
	     ))

	;; return the pager to its original state
	(setenv "PAGER" orig-pager-env)
	)
    (pop-to-buffer "*psql*") ; already running
    )
  )

;;;###autoload
(defun psql-run-sqlplus (&optional arg)
  (interactive "P")
  (let ((b-name "*sqlplus*"))
    (if (not (comint-check-proc b-name))
	(progn
	  ;;
	  (setq psql-run-database (getenv "ORACLE_USER"))
	  (set-buffer 
	   (make-comint 
	    "sqlplus" "sqlplus" nil (getenv "ORACLE_USER")
	    (concat "@" (expand-file-name "~/.sqlplus.sql"))))
	   (setq psql-buffer (current-buffer))
	   (psql-run-mode)
	   (pop-to-buffer b-name) ; flip to just started
	   )
      (pop-to-buffer b-name) ; already running
      )
    ))


(defun psql-run-get-old-input ()
  (interactive)
  (save-excursion
    (let (s e)
      ;; Find start
      (if (search-backward-regexp "\\sw+=>" (point-min) t)
	  (setq s (match-end 0))
	(error "No leading prompt.")
	)
      ;; Find end
      (if (search-forward-regexp ";$" (point-max) t)
	  (setq e (match-end 0))
	(error "No trailing semicolon.")
	)
      ;; Cut it out
      ;(message "S%d  E%d  %s" s e (buffer-substring-no-properties s e))
      (buffer-substring-no-properties s e)
      )))


(defun psql-run-magic-semicolon ()
  "Send the current command to psql process."
  (interactive)
  (end-of-line)
  (if (re-search-backward ";\\s-*\\=" (point-min) t)
      nil
    (insert ";"))
  (comint-send-input)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions and variables used in generating the keyword list.
;;

;; Keywords to fontify.  Taken from parser/keywords.c
;; These are also converted to uppercase.
(defvar psql-fl-keywords-1
      '(
	"abort" "acl" "add" "after" "aggregate" "all" "alter"
	"and" "append" "arch_store" "archive" "as"
	"asc" "backward" "before" "begin" "binary" "by"
	"cast" "change" "close" "cluster" "column" "commit"
	"copy" "create" "current" "cursor" "database"
	"declare" "delete" "delimiters" "desc" "distinct" "do"
	"drop" "end" "execute" "explain" "extend" "fetch"
	"for" "forward" "from" "function" "grant" "group"
	"having" "heavy" "in" "index" "inherits" "insert"
	"instead" "into" "isnull" "language" "light" "like"
	"listen" "load" "merge" "move" "new" "none" "not"
	"nothing" "notify" "notnull" "null" "oids" "on"
	"operator" "option" "or" "order" "privileges" "public"
	"purge" "recipe" "rename" "replace" "retrieve"
	"returns" "revoke" "rollback" "rule" "select" "set"
	"setof" "stdin" "stdout" "store" "table" "to"
	"transaction" "type" "update" "using" "vacuum"
	"values" "version" "view" "where" "with" "work"
	)
      "*List of psql keywords.")

(defvar psql-fl-types-1
  '(
    "char" "char2" "char4" "char8" "char16" 
    "float4" "float8" 
    "int" "int2" "int4" 
    "oid"
    "text" "varchar"
    )
  "*List of psql types.")



;; These two functions are use to generate the the
;; font-lock-keyword list as I didnt want to type it in.
;; The do-uc is taken care of by font-lock.
(defun psql-generate-flkw (fl-k fl-t do-uc)
  "Generate psql-font-lock-keywords using the make-regexp function."
  ;; library
  (if (fboundp 'make-regexp)
      ()
    (load-library "make-regexp"))
  ;;
  (list
   (concat "\\<\\(" (make-regexp fl-k) "\\)\\>")
   (if do-uc (concat "\\<\\(" (make-regexp (mapcar 'upcase fl-k)) "\\)\\>"))
   (cons (make-regexp fl-t) font-lock-type-face)
   ))

(defun psql-insert-flkw (fl-k fk-t do-uc)
  "Insert an expression into the current buffer to define the keywords."
  (setq psql-font-lock-keywords (psql-generate-flkw fl-k fk-t do-uc))
  (save-excursion
    (end-of-buffer)
    (insert
     "\n(defvar psql-font-lock-keywords\n"
     "   '" (format "%S" psql-font-lock-keywords)
     "\n"
     "   \"Keywords to hilight in font-lock-mode\")\n"
     )))

;; For those of you with 'make-regexp'
;(defvar psql-font-lock-keywords
;  (eval-when-compile
;    (psql-generate-flkw psql-fl-keywords-1 psql-fl-types-1 t)
;    )
;  "Keywords to highlight."
;  )

;; Eval this line to regenerate the defvar below.
;;(psql-insert-flkw psql-fl-keywords-1 psql-fl-types-1 t)


;; Psql-mode is now ready to go.
(provide 'psql-mode)
(run-hooks 'psql-load-hook)

;;; psql-mode.el ends here

