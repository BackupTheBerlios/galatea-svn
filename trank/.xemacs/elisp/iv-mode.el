;; Inventor file editing commands for Emacs
;;	Extracted from c++-mode.el

(defvar iv-mode-abbrev-table nil
  "Abbrev table in use in Iv-mode buffers.")
(define-abbrev-table 'iv-mode-abbrev-table ())

(defvar iv-mode-map ()
  "Keymap used in Iv mode.")
(if iv-mode-map
    ()
  (setq iv-mode-map (make-sparse-keymap))
  (define-key iv-mode-map "\C-j" 'reindent-then-newline-and-indent)
  (define-key iv-mode-map "{" 'electric-iv-brace)
  (define-key iv-mode-map "}" 'electric-iv-brace)
  (define-key iv-mode-map "[" 'electric-iv-brace)
  (define-key iv-mode-map "]" 'electric-iv-brace)
  (define-key iv-mode-map "\e\C-q" 'indent-iv-exp)
  (define-key iv-mode-map "\177" 'backward-delete-char-untabify)
  (define-key iv-mode-map "\t" 'iv-indent-command)
  (define-key iv-mode-map "\C-c\C-c" 'iv-comment-region)
  (define-key iv-mode-map "\C-c\C-u" 'iv-uncomment-region)
  (define-key iv-mode-map "\e\C-a" 'iv-beginning-of-defun)
  (define-key iv-mode-map "\e\C-e" 'iv-end-of-defun)
  (define-key iv-mode-map "\e\C-x" 'iv-indent-defun))

(defvar iv-mode-syntax-table nil
  "Syntax table in use in Iv-mode buffers.")

(if iv-mode-syntax-table
    ()
  (setq iv-mode-syntax-table (copy-syntax-table c-mode-syntax-table))

  ;; Don't recognize C or C++ comments
  (modify-syntax-entry ?/ "." iv-mode-syntax-table)
  (modify-syntax-entry ?* "." iv-mode-syntax-table)

  ;; Recognize comments after "#" up to newline
  (modify-syntax-entry ?#  "<" iv-mode-syntax-table)
  (modify-syntax-entry ?\n ">" iv-mode-syntax-table)

  ;; Single quote has no special meaning
  (modify-syntax-entry ?\' "." iv-mode-syntax-table))

(defvar iv-indent-level 4
  "*Indentation amount in Iv-mode." )

(defconst iv-auto-newline nil
  "*Non-nil means automatically newline before and after braces
 inserted in Inventor code.")

(defconst iv-tab-always-indent t
  "*Non-nil means TAB in Iv mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")

(defun iv-mode ()
  "Major mode for editing Inventor code.  Very much like editing C/C++ code.
Expression and list commands understand all Inventor brackets.
Tab at left margin indents for Inventor code
Comments are delimited with # ... <newline>
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{iv-mode-map}
Variables controlling indentation style:
 iv-tab-always-indent
    Non-nil means TAB in Inventor mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
    Default is t.
 iv-auto-newline
    Non-nil means automatically newline before and after braces
    inserted in Inventor code.
 iv-indent-level
    Indentation of Inventor statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.

Standard settings are
  iv-indent-level 4

Turning on Iv mode calls the value of the variable iv-mode-hook with
no args,if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map iv-mode-map)
  (setq major-mode 'iv-mode)
  (setq mode-name "Iv")
  (setq local-abbrev-table iv-mode-abbrev-table)
  (set-syntax-table iv-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'iv-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "# *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'iv-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (run-hooks 'iv-mode-hook))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in Iv code
;; based on its context.
(defun iv-comment-indent ()
  (if (looking-at "^#")
      0					;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max
       ;; leave at least one space on non-empty lines.
       (if (zerop (current-column)) 0 (1+ (current-column)))
       (let ((cur-pt (point)))
	 (beginning-of-line 0)
	 ;; If previous line had a comment, use it's indent
	 (if (re-search-forward comment-start-skip cur-pt t)
	     (progn
	       (goto-char (match-beginning 0))
	       (current-column))
	   comment-column))))))		;otherwise indent at comment column.

(defun electric-iv-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (if iv-auto-newline (progn (iv-indent-line) (newline) t) nil)))
	(progn
	  (insert last-command-char)
	  (iv-indent-line)
	  (if iv-auto-newline
	      (progn
		(newline)
		;; (newline) may have done auto-fill
		(setq insertpos (- (point) 2))
		(iv-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun iv-indent-command (&optional whole-exp)
  "Indent current line as Iv code, or in some cases insert a tab character.
If iv-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as C
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (iv-indent-line))
	    beg end)
	(save-excursion
	  (if iv-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-sexp 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt "^$")))
    (if (and (not iv-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (iv-indent-line))))

(defun iv-indent-line ()
  "Indent current line as Iv code.
Return the amount the indentation changed by."
  (let ((indent (calculate-iv-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  ((eq indent t)
	   (setq indent (calculate-c-indent-within-comment)))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (if (or
		(= (following-char) ?})
		(= (following-char) ?\]))
	       (setq indent (- indent iv-indent-level)))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun calculate-iv-indent (&optional parse-start)
  "Return appropriate indentation for current line as Iv code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     (current-indentation))
	    (
	     (and
	      (/= (char-after containing-sexp) ?{)
	      (/= (char-after containing-sexp) ?\[))
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open
	     (goto-char (1+ containing-sexp))
	     (current-column))
	    (t
	     ;; Statement.  Find previous non-comment character.
	     (goto-char indent-point)
	     (iv-backward-to-noncomment containing-sexp)
	     ;; Indent this line same as previous.
	     (progn
	       (iv-backward-to-start-of-continued-exp containing-sexp)
	       (current-column))
	     ;; This line starts a new statement.
	     ;; Position following last unclosed open.
	     (goto-char containing-sexp)
	     ;; Is line first statement after an open-brace?
	     (or
	      ;; If no, find that first statement and indent like it.
	      (save-excursion
		(forward-char 1)
		(while (progn (skip-chars-forward " \t\n")
			      (looking-at "#"))
		  ;; Skip over comments following openbrace.
		  (cond ((= (following-char) ?\#)
			 (forward-line 1))
			((looking-at "#")
			 (forward-line 1))
			(t 'move)))
		;; The first following code counts
		;; if it is before the line we want to indent.
		(and (< (point) indent-point)
		     (current-column)))
	      ;; If no previous statement,
	      ;; indent it relative to line brace is on.
	      ;; For open brace in column zero, don't let statement
	      ;; start there too.  If iv-indent-offset is zero,
	      ;; use 0.
	      (+ (if (and (bolp) (zerop iv-indent-level))
		     0
		   iv-indent-level)
		 ;; Move back over whitespace before the openbrace.
		 ;; If openbrace is not first nonwhite thing on the line,
		 ;; add the c-brace-imaginary-offset.
		 (progn (skip-chars-backward " \t")
			0)
		 ;; If the openbrace is preceded by a parenthesized exp,
		 ;; move to the beginning of that;
		 ;; possibly a different line
		 (progn
		   (if (eq (preceding-char) ?\))
		       (forward-sexp -1))
		   ;; Get initial indentation of the line we are on.
		   (current-indentation)))))))))

(defun iv-backward-to-start-of-continued-exp (lim)
  (if (memq (preceding-char) '(?\) ?\"))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun iv-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\r\f" lim)
      (setq opoint (point))
      (cond ((and
	      (search-backward "#" (max (point-bol) lim) 'move)
	      (not (within-string-p (point) opoint))))
	    (t (beginning-of-line)
	       (skip-chars-forward " \t")
	       (setq stop t)
	       (goto-char opoint))))))

(defun indent-iv-exp ()
  "Indent each line of the Iv grouping following point."
  (interactive)
  (let ((indent-stack (list nil))
	(contain-stack (list (point)))
	(case-fold-search nil)
	restart outer-loop-done inner-loop-done state ostate
	this-indent last-sexp
	(opoint (point))
	(next-depth 0))
    (save-excursion
      (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp)) (not outer-loop-done))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq innerloop-done nil)
	(while (and (not innerloop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (and (car (cdr (cdr state)))
		   (>= (car (cdr (cdr state))) 0))
	      (setq last-sexp (car (cdr (cdr state)))))
	  (if (or (nth 4 ostate))
	      (iv-indent-line))
	  (if (or (nth 3 state))
	      (forward-line 1)
	    (setq innerloop-done t)))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  ;; If this line had ..))) (((.. in it, pop out of the levels
	  ;; that ended anywhere in this line, even if the final depth
	  ;; doesn't indicate that they ended.
	  (while (> last-depth (nth 6 state))
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  ;; Add levels for any parens that were started in this line.
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  contain-stack (cons nil contain-stack)
		  last-depth (1+ last-depth)))
	  (if (null (car contain-stack))
	      (setcar contain-stack (or (car (cdr state))
					(save-excursion (forward-sexp -1)
							(point)))))
	  (forward-line 1)
	  (skip-chars-forward " \t")
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Line is on an existing nesting level.
		;; Lines inside parens are handled specially.
		(if (and
		     (/= (char-after (car contain-stack)) ?{)
		     (/= (char-after (car contain-stack)) ?\[))
		    (setq this-indent (car indent-stack))
		  ;; Line is at statement level.
		  ;; Is it a new statement?  Is it an else?
		  ;; Find last non-comment character before this line
		  (save-excursion
		    (iv-backward-to-noncomment opoint)
		    ;; Use the standard indent for this level.
		    (setq this-indent (car indent-stack))))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (calculate-iv-indent
			  (if (car indent-stack)
			      (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))))
	    ;; Adjust line indentation according to its contents
	    (if (or
		 (= (following-char) ?})
		 (= (following-char) ?\]))
		(setq this-indent (- this-indent iv-indent-level)))
	    ;; Put chosen indentation into effect.
	    (or (= (current-column) this-indent)
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to this-indent)))
	    ;; Indent any comment following the text.
	    (or (looking-at comment-start-skip)
		(if (re-search-forward comment-start-skip (save-excursion (end-of-line) (point)) t)
		    (progn (indent-for-comment) (beginning-of-line)))))))))
  )

(defun fill-iv-comment ()
  (interactive)
  (save-excursion
    (let ((save fill-prefix))
      (beginning-of-line 1)
      (save-excursion
	(re-search-forward comment-start-skip
			   (save-excursion (end-of-line) (point))
			   t)
	(goto-char (match-end 0))
	(set-fill-prefix))
      (while (looking-at fill-prefix)
	(previous-line 1))
      (next-line 1)
      (insert-string "\n")
      (fill-paragraph nil)
      (delete-char -1)
      (setq fill-prefix save))))

(defun point-bol ()
  "Returns the value of the point at the beginning of the current
line."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun within-string-p (point1 point2)
  "Returns true if number of double quotes between two points is odd."
  (let ((s (buffer-substring point1 point2)))
    (not (zerop (mod (count-char-in-string ?\" s) 2)))))

(defun count-char-in-string (c s)
  (let ((count 0)
	(pos 0))
    (while (< pos (length s))
      (setq count (+ count (if (\= (aref s pos) c) 1 0)))
      (setq pos (1+ pos)))
    count))


;;; This page covers commenting out multiple lines.

(defun iv-comment-region ()
  "Comment out all lines in a region between mark and current point by
inserting \"# \" (comment-start)in front of each line."
  (interactive)
  (let* ((m      (if (eq (mark) nil) (error "Mark is not set!") (mark)))
	 (start  (if (< (point) m) (point) m))
	 (end    (if (> (point) m) (point) m))
	 (mymark (copy-marker end)))
    (save-excursion
	(goto-char start)
	(while (< (point) (marker-position mymark))
	    (beginning-of-line)
	    (insert comment-start)
	    (beginning-of-line)
	    (next-line 1)
	    ))))


(defun iv-uncomment-region ()
  "Uncomment all lines in region between mark and current point by deleting
the leading \"# \" from each line, if any."
  (interactive)
  (let* ((m      (if (eq (mark) nil) (error "Mark is not set!") (mark)))
	 (start  (if (< (point) m) (point) m))
	 (end    (if (> (point) m) (point) m))
	 (mymark (copy-marker end))
	 (len    (length comment-start))
	 (char   (string-to-char comment-start))
	 )
    (save-excursion
	(goto-char start)
	(while (< (point) (marker-position mymark))
	    (beginning-of-line)
	    (if (looking-at (concat " *" comment-start))
		(progn
		  (zap-to-char 1 char)
		  (delete-char len)))
	    (beginning-of-line)
	    (next-line 1)
	    ))))

;; This part has to do with recognizing defuns.

(defconst iv-defun-header "^{\\|^[_a-zA-Z].*{"
  "Regexp to match beginning of definition.  ")

(defun iv-beginning-of-defun (arg)
  (interactive "p")
  (let ()
    (cond ((or (= arg 0) (and (> arg 0) (bobp))) nil)
	  ((and (not (looking-at iv-defun-header))
		(let ((curr-pos (point))
		      (open-pos (if (search-forward "{" nil 'move)
				    (point)))
		      (beg-pos
			(if (re-search-backward iv-defun-header nil 'move)
			    (match-beginning 0))))
		  (if (and open-pos beg-pos
			   (< beg-pos curr-pos)
			   (> open-pos curr-pos))
		      (progn
			(goto-char beg-pos)
			(if (= arg 1) t nil));; Are we done?
		    (goto-char curr-pos)
		    nil))))
	  (t
	    (if (and (looking-at iv-defun-header) (not (bobp)))
		(forward-char (if (< arg 0) 1 -1)))
	    (and (re-search-backward iv-defun-header nil 'move (or arg 1))
		 (goto-char (match-beginning 0)))))))


(defun iv-end-of-defun (arg)
  (interactive "p")
  (let ()
    (if (and (eobp) (> arg 0))
	nil
      (if (and (> arg 0) (looking-at iv-defun-header)) (forward-char 1))
      (let ((pos (point)))
	(iv-beginning-of-defun 
	 (if (< arg 0)
	     (- (- arg (if (eobp) 0 1)))
	   arg))
	(if (and (< arg 0) (bobp))
	    t
	  (if (re-search-forward iv-defun-header nil 'move)
	      (progn (forward-char -1)
		     (forward-sexp)
		     (beginning-of-line 2)))
	  (if (and (= pos (point)) 
		   (re-search-forward iv-defun-header nil 'move))
	      (iv-end-of-defun 1))))
      t
      ))
  )

(defun iv-indent-defun ()
  "Indents the current function def, struct or class decl."
  (interactive)
  (let ((restore (point)))
    (iv-end-of-defun 1)
    (beginning-of-line 1)
    (let ((end (point)))
      (iv-beginning-of-defun 1)
      (while (<= (point) end)
	(iv-indent-line)
	(next-line 1)
	(beginning-of-line 1)))
    (goto-char restore)))
