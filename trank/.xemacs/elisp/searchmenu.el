;;; search-menu.el --- pop up a window for control search and replace --- FSF Emacs 19:

;; Author: Bryan M. Kramer   <kramer@ai.toronto.edu>
;; Created: April 22, 1994
;; Version: for Xemcas 19.14, Nov. 6, 1996
;; Keywords: search, replace, form


;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;  ----------
;;
;;  This is useful for certain kinds of editing. i-search is usually more
;;  appropriate for searching.
;;
;;  usage: (load-library "searchmenu")
;;         M-x search-menu
;;
;;    (it's more convenient from a menu or a key sequence)
;;
;; Optional Customization:
;;
;;    Specify the size and location of the control menu by specifying
;;    frame parameters in searchm-frame-parameters
;;
;;    Specify the colours and fonts using the variables:
;;
;;    	searchm-button-face - font, foreground, and background for
;;    	            	    	buttons
;;    	searchm-button-highlight - font, foreground, and background for
;;    	                            buttons when under mouse
;;    	searchm-buffer-name-face - font, foreground and background for target buffer
;;    	    	    	    	   name
;;    	searchm-field-face - font, foreground, and background used for fields
;;    	searchm-field-label-face - font, foreground, and background used for field labels
;;
;;    eg: (setq searchm-button-face '("9x15" "white" "blue"))
;;
;;    Setting these to nil is possible and very safe (uses your emacs' defaults.)
;;
;;
;;    In addition
;;      searchm-button-font set to a font name will change only the button fonts
;;
;;  searchm-auto-size - if set to non-nil, the control buffer will size itself to the
;;  	    	    	buffer size at each call (ignoring the height and width
;;  	    	    	frame parameters)
;;
;;  TO CHANGE BUFFER BEING SEARCHED, invoke M-x search-menu from the desired buffer
;;
;;  buttons are activated using the middle mouse button
;;
;;  type a regexp beside pattern:, the click on "Search" and the expected will occur;
;;        ditto "Search Backward"
;;  "Replace" will replace the pattern with the "Substitution:" if the current
;;        buffer location is looking-at the pattern.
;;  "Replace and Search" will follow the replacement with a search
;;  "Replace All" will replace all subsequent matches a la replace-regexp
;;  "Undo" undoes the last change
;;  "Execute Emacs-Lisp" interprets the contents of of the Emacs-Lisp: field as an s-expression
;;      and evaluates it. Presumably it does something different at different locations
;;      in the target buffer
;;  "Grep" invokes grep with Pattern: on either the file of the target buffer (default) or the
;;      specified files.
;;
;;
;;   CHANGING THE LAYOUT OF THE CONTROL BUFFER
;;
;;   The variable searchm-commands describes the layout. The value is a list of
;;   specifications. Each specification is a list. The car of this list is some
;;   text that is inserted into the buffer. Basically the buffer is filled by
;;   sequentially inserting the car of each specification.
;;
;;   The cdr of a specification is a list of alternating keywords and values
;;   like a plist. The keywords specify properties of the region of text specified
;;   by the car of the specification. The keywords are:
;;
;;   	face - set the face of the inserted text to the value
;;   	highlight - set the highlight of the inserted text to the value
;;   	field - the inserted text is interpreted as a field with key eq to the value
;;   	        At the moment fields work best if the text is "  \n" and the following
;;   	        line contains at least one character.
;;   	command - if searchm-respond-to-click is invoked from a mouse click over
;;   	    	  this piece of text, the action specified by the value is evaluated.
;;   	    	  The value is a list of the form (command arg arg arg ...).
;;   	    	  The arguments are interpreted as follows - if there is a field in the
;;   	    	  control buffer with key eq to the arg, the text in that field is
;;   	    	  substituted for arg. Otherwise arg is passed as is. The command is evaluated
;;   	    	  in the context of the buffer being searched.
;;   	local-command - like command except that the command is interpreted in
;;   	    	   the context of the search control buffer
;;
;;   The whole buffer, except the fields, is made read only.
;;
;;   (searchm-setup-buffer <buffer> <spec>) is a function that can be used to help
;;   set up other menus using the same specification mechanism.
;;
;;
;;;  Change Log
;;
;;	1996/11/06 - incorporated some changes suggested by
;;			Thomas Feuster <Thomas.Feuster@theo.physik.uni-giessen.de>
;;


(defvar searchm-commands
    '(("Buffer: \n\n" face searchm-buffer)
      ("Pattern:\t" face searchm-field-label)
      (" \n" field pattern face searchm-field)
      ("Substitution:\t" face searchm-field-label)
      (" \n" field substitution face searchm-field)
      ("Emacs Lisp:\t" face searchm-field-label)
      (" \n" field lisp face searchm-field)
      (" \n")
      ("Search" command (searchm-search pattern) face searchm-button highlight searchm-highlight)
      ("\t")
      ("Search Backward" command (searchm-search-back pattern)
       face searchm-button highlight searchm-highlight)
      ("\t\t")
      ("Beginning of Buffer" command (beginning-of-buffer)
       face searchm-button highlight searchm-highlight)
      ("\n\n")
      ("Replace" command (searchm-replace pattern substitution)
       face searchm-button highlight searchm-highlight)
      ("\t")
      ("Replace and Search" command (searchm-rep-search pattern substitution)
       face searchm-button highlight searchm-highlight)
      ("\t")
      ("Replace All" command (searchm-replace-all pattern substitution)
       face searchm-button highlight searchm-highlight)
      ("\n\n")
      ("Undo" command (searchm-undo) face searchm-button highlight searchm-highlight)
      ("\t")
      ("Execute Emacs Lisp" command (searchm-emacs-lisp lisp)
       face searchm-button highlight searchm-highlight)
      ("\t")
      ("Dismiss" local-command (searchm-dismiss) face searchm-button highlight searchm-highlight)
      ("\n\n")
      ("Grep" command (searchm-grep pattern files)
       face searchm-button highlight searchm-highlight)
      ("\t")
      ("Files:\t" face searchm-field-label)
      (" \n" field files face searchm-field)
      )
  "Specification of the layout and commands in the search control buffer")



(defvar searchm-setup-hook nil
  "Hook to run after searchm-setup-buffer")


(defvar searchm-auto-size t "If non-nil, control window is sized to fit buffer")


(defvar searchm-frame-parameters '((modeline . nil) (top . 100) (left . -200) (height . 14)
				   (width . 60) (menu-bar-lines . 0) (minibuffer . nil)
				   (vertical-scroll-bars . nil)))

(defvar searchm-use-black-and-white nil "Force black and white display of search menu")

(defvar searchm-button-font "-adobe-courier-bold-o-normal--12-120-75-75-m-70-iso8859-1"
  "Font to use for buttons")


(defvar searchm-current-buffer nil "Buffer search-menu is looking at")
(defvar searchm-current-directory nil "Directory of buffer that search-menu is looking at")

(defvar searchm-control-buffer nil "Buffer containing search menu")


(defvar searchm-button-face '(searchm-button-font "white" "CadetBlue")
  "font, foreground, and background for search menu buttons")

(defvar searchm-buffer-name-face '(searchm-button-font "red" "white")
  "font, foreground, and background for search menu buffer name display")

(defvar searchm-button-highlight '(searchm-button-font "black" "darkseagreen2")
  "font, foreground, and background for search menu buttons under mouse")

(defvar searchm-field-face '(nil nil "grey89"))

(defvar searchm-field-label-face '(searchm-button-font nil nil))

;; set the face font only if there is no error

(defun searchm-set-face-font (name font-spec)
  (condition-case nil
      (and font-spec (set-face-font name font-spec))
    (error (set-face-font name nil))
    )
  )


;; Process colour spec (foreground background) and make appropriate
;; choices depending on whether the colours exist and if the fields
;; should be inverted

(defun searchm-verify-colours (name spec invert-on-black-and-white)
  (cond ((and (not searchm-use-black-and-white) (eq (device-class) 'color))
	 (let ((fore (car spec))
	       (back (car (cdr spec)))
	       (invalid nil))
	   (if (or (null fore) (and (stringp fore) (valid-color-name-p fore)))
	       (and fore (set-face-foreground name fore))
	     (setq invalid t))
	   (if (or (null back) (and (stringp back) (valid-color-name-p back)))
	       (and back (set-face-background name back))
	     (setq invalid t))
	   (if invalid
	       (if invert-on-black-and-white
		   (progn
		     (set-face-foreground name "white")
		     (set-face-background name "black"))
		 (progn
		   (set-face-background name "white")
		   (set-face-foreground name "black"))))))
	((and spec (equal (car spec) "white"))
	 (set-face-foreground name "white")
	 (set-face-background name "black"))
	((and spec (equal (car spec) "black"))
	 (set-face-foreground name "black")
	 (set-face-background name "white"))
	(invert-on-black-and-white
	 (set-face-foreground name "white")
	 (set-face-background name "black"))
	(t
	 (set-face-background name "white")
	 (set-face-foreground name "black"))))


;; create or modify the face as specified by spec

(defun searchm-create-face (name spec invert-on-black-and-white)
  (if (not (member name (face-list)))
      (make-face name))
  (cond ((and (symbolp spec) (member spec (face-list)))
	 (copy-face name spec))
	((listp spec)
	 (if (symbolp (car spec))
	     (searchm-set-face-font name (eval (car spec)))
	   (searchm-set-face-font name (car spec)))
	 (searchm-verify-colours name (cdr spec) invert-on-black-and-white))
	(t (error "Invalid face specification"))))

(searchm-create-face 'searchm-button searchm-button-face t)
(searchm-create-face 'searchm-buffer searchm-buffer-name-face t)
(searchm-create-face 'searchm-highlight searchm-button-highlight nil)
(searchm-create-face 'searchm-field-label searchm-field-label-face t)
(searchm-create-face 'searchm-field searchm-field-face t)





;; invoke the grep command as specified by the search control window

(defun searchm-grep (form files)
  (if (null form) (error "No grep pattern."))
  (grep (concat "grep -n -i -e '" form "' "
		(or files
		    (file-name-nondirectory (buffer-file-name searchm-current-buffer))))
	)
  )


;; execute some emacs lisp form

(defun searchm-emacs-lisp (form)
  (eval (car (read-from-string form))))


;; get rid of control window

(defun searchm-dismiss ()
  (let* ((window (get-buffer-window searchm-control-buffer t))
	 (frame (and (windowp window) (window-frame window))))
    (kill-buffer searchm-control-buffer)
    (if (framep frame)
	(delete-frame frame))))


;; what can I say?

(defun searchm-undo ()
  (undo 1))


;; replace text at point with substitution

(defun searchm-replace (pattern substitution)
  (if (null pattern) (error "No search pattern specified"))
  (if (looking-at pattern)
    (progn (undo-boundary) (replace-match (or substitution "") t))
    (error "Text at current position does not match pattern. Have you moved the point?")))


;; replace all remaining occurrences of pattern with substitution

(defun searchm-replace-all (pattern substitution)
  (if (null pattern) (error "No search pattern specified"))
  (undo-boundary)
  (while (re-search-forward pattern nil t)
    (replace-match (or substitution "") t  nil)))


;; replace and search again in the controlled buffer

(defun searchm-rep-search (pattern substitution)
  (if (null pattern) (error "No search pattern specified"))
  (if (looking-at pattern)
      (progn
	(undo-boundary)
	(replace-match (or substitution "") t)
	(searchm-search pattern))
    (error "Text at current position does not match pattern. Have you moved the point?")))


;; Search forward in the controlled buffer for pattern

(defun searchm-search (pattern)
  (if (null pattern) (error "No search pattern specified"))
  (forward-char)
  (if (re-search-forward pattern)
      (progn
	;(deactivate-mark)
	(goto-char (match-end 0))
	(push-mark (match-beginning 0) t t searchm-current-buffer)
	(exchange-point-and-mark)
	(activate-region))))


;; Search backward in the controlled buffer for pattern

(defun searchm-search-back (pattern)
  (if (null pattern) (error "No search pattern specified"))
  (if (re-search-backward pattern)
      (progn
	;(deactivate-mark)
	(goto-char (match-end 0))
	(push-mark (match-beginning 0) t t searchm-current-buffer)
	(exchange-point-and-mark)
	(activate-region))))


;; translate \n into newline and \t into tab in a string

(defun searchm-process-substitution (str)
  (if str
      (let ((copy (copy-sequence str))
	    (i 0)
	    (j 0)
	    (l2 (length str))
	    (l (length str)))
	(while (< i l)
	  (if (eq ?\\ (aref str i))
	      (if (< (+ i 1) l)
		  (cond ((eq ?n (aref str (+ 1 i)))
			 (setq i (+ i 1))
			 (setq l2 (- l2 1))
			 (aset copy j ?\n))
			((eq ?t (aref str (+ 1 i)))
			 (setq i (+ i 1))
			 (setq l2 (- l2 1))
			 (aset copy j ?\t))
			(t (aset copy j (aref str i))))
		(aset copy j (aref str i)))
	    (aset copy j (aref str i)))
	  (setq i (+ i 1))
	  (setq j (+ j 1))
	  )
	(substring copy 0 l2))))


;; debugging function to look at property tags in buffer
;; output ends up in message log

(defun searchm-lookat-read-only ()
       (save-excursion
	 (let ((p1 (point-min))
	       p2)
	   (while p1
	     (setq p2 (next-single-property-change p1 'read-only))
	     (if p2
		 (progn
		   (message "%d %d %s %s '%s'" p1 p2 (get-text-property p1 'read-only) (get-text-property p2 'read-only) (buffer-substring p1 p2))
		   (setq p1 p2))
	       (setq p1 nil))))))

;; Extract a field value (identified by having read-only property key) from the control buffer

(defun searchm-pattern-field (key)
  (let ((buf (get-buffer "*sm control*")))
    (save-excursion
      (set-buffer buf)
      (let ((p1 (point-min))
	    p2)
	(while (and p1 (not p2))
	  (setq p1 (next-single-property-change p1 'field))
	  (if (and p1 (eq key (get-text-property p1 'field)))
	      (progn
		(setq p2 (next-single-property-change p1 'field)))))
	(if (and p1 p2)
	    (buffer-substring p1 (- p2 1)))))))


;; find values (from the buffer) of the arguments specified by the command

(defun searchm-get-args (args)
  (if args
      (cons (or (searchm-pattern-field (car args)) (car args))
	    (searchm-get-args (cdr args)))
    nil))


;; Execute a command in the context of the controlled buffer

(defun searchm-do-command (command local buffer)
  (let ((expr (searchm-get-args (cdr command))))
    (set-buffer buffer)
    (if local
	(save-excursion
	  (apply (car command) expr))
      (save-window-excursion
	(save-excursion
	  (if searchm-current-buffer
	      (progn
		(set-buffer searchm-current-buffer)
		(let ((window (get-buffer-window searchm-current-buffer t)))
		  (if (and (windowp window) (window-live-p window))
		      (progn
			(select-window window)
			(apply (car command) expr))
		    (error "Window is no longer live."))))
	    (error "No target buffer")))))))




;; Find and execute the command, if any, specified by a mouse click

(defun searchm-respond-to-click (click)
  "Find and execute the command, if any, specified by a mouse click"
  (interactive "e")
  (setq e click)
  (let* ((window (event-window click))
	 (buffer (and window (window-buffer window)))
	 (pos (event-point click))
	 (command (and buffer (get-text-property pos 'searchm-command buffer)))
	 (lcommand (and buffer (get-text-property pos 'searchm-local-command buffer))))
    ;; (my-message "Command: %s" (list click window buffer pos command lcommand))
    (if command
	(searchm-do-command command nil buffer)
      (if lcommand
	(searchm-do-command lcommand t buffer)
      ;;(mouse-drag-region click)
      ;;(mouse-set-point click)
      ))))

;; function to do search from key stroke

(defun searchm-call-search (&optional args)
  "Invoke the search menu search forward function"
  (interactive "P")
  (searchm-do-command '(searchm-search pattern) nil (get-buffer "*sm control*")))

;; no-op to shadow global key bindings

(defun searchm-noop (&optional args)
  "Interactive beep"
  (interactive "P")
  (beep))

;; no-op to shadow global key bindings

(defun searchm-mouse-noop (click)
  "Do nothing on a mouse click"
  (interactive "e")
  nil)



;; treat pattern as a list of alternating keywords and values
;; (like plists)
;; return the value corresponding to key

(defun searchm-lookup-key (key pattern)
  (let ((found (memq key pattern)))
    (and found
	 (cdr found)
	 (car (cdr found)))))


;; calculate size of buffer

(defun searchm-buffer-dimensions (buffer)
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (end-of-line)
    (let ((max (current-column)))
      (while (equal (forward-line 1) 0)
	(end-of-line)
	(if (> (current-column) max)
	    (setq max (current-column))))
      (list 'width (+ max 2) 'height (+ 2 (count-lines (point-min) (point-max)))))))


;; create the search buffer

(defun searchm-setup-buffer (buf commands)
  (save-window-excursion
    (save-excursion
      (set-buffer buf)
      (local-set-key "\C-y" 'yank)
      (let ((inhibit-read-only t)
	    (search-map (make-sparse-keymap)))
	(define-key search-map [button1down] 'searchm-mouse-noop)
	(define-key search-map [button1] 'searchm-mouse-noop)
	(define-key search-map [button2] 'searchm-respond-to-click)
	(make-local-variable 'highlight-nonselected-windows)
	(setq highlight-nonselected-windows t)
	(erase-buffer)
	(insert-string "\n")
	(goto-char (point-min))
	(let ((ptr commands)
	      (last-read-only (point-min))
	      desc)
	  (while ptr
	    (setq desc (car ptr))
	    (setq ptr (cdr ptr))
	    (let ((p1 (point))
		  p2
		  (field-keys (searchm-lookup-key 'field (cdr desc)))
		  (local-command (searchm-lookup-key 'local-command (cdr desc)))
		  (command (searchm-lookup-key 'command (cdr desc)))
		  (face (searchm-lookup-key 'face (cdr desc)))
		  (highlight (searchm-lookup-key 'highlight (cdr desc))))
	      (insert-string (car desc))
	      (if (or command local-command)
		  (setq p2 (point))
		(setq p2 (- (point) 1)))
	      (if field-keys
		  (progn
		    (put-text-property last-read-only p1 'read-only t)
		    (put-text-property p1 p2 'read-only nil)
		    (put-text-property p1 p2 'field field-keys)
		    (setq last-read-only (+ p2 1))))
	      (if face (put-text-property p1 p2 'face face))
	      (if highlight (put-text-property p1 p2 'highlight t))
	      (if command
		  (progn
		    (put-text-property p1 p2 'searchm-command command)
		    (put-text-property p1 p2 'keymap search-map)))
	      (if local-command
		  (progn
		    (put-text-property p1 p2 'searchm-local-command local-command)
		    (put-text-property p1 p2 'keymap search-map))))
	    (put-text-property last-read-only (point-max) 'read-only 'end-field)))
	(run-hooks 'searchm-setup-hook)))))



(defun searchm-test ()
  (searchm-setup-buffer "*sm control*" searchm-commands)
  (global-set-key [M-C-mouse-2] 'searchm-look-at-prop)
  )

(defun searchm-look-at-prop (click)
  (interactive "e")
  (let* ((window (event-window click))
	 (buffer (and window (window-buffer window)))
	 (pos (event-point click)))
    (message "%s" (list window buffer pos (text-properties-at pos)))
    )
  )




;; return the search control buffer, creating it if it is not present in a
;; reasonable form

(defun searchm-get-control-buffer ()
  (if (or (not searchm-control-buffer)
	  (not (bufferp searchm-control-buffer))
	  (null (buffer-name searchm-control-buffer))) ; deleted
      (progn
	(setq searchm-control-buffer (get-buffer-create "*sm control*"))
	(searchm-setup-buffer searchm-control-buffer searchm-commands)
	))
  searchm-control-buffer)


;; display the search control menu first inserting the name of the controlled buffer

(defun searchm-display-control-menu ()
  (let* ((buf (searchm-get-control-buffer))
	 (window (get-buffer-window buf t))
	 (frame (and window (window-live-p window) (window-frame window))))
    (if (null frame)
	(setq frame (make-frame searchm-frame-parameters)))
    (select-frame frame)
    (set-window-buffer (selected-window) buf)
    (switch-to-buffer buf)
    (set-buffer-menubar nil)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (end-of-line)
      (delete-region (point-min) (point) buf)
      (insert-string "Buffer: \t" buf)
      (insert-string (buffer-name searchm-current-buffer) buf)
      (goto-char (point-min))
      (end-of-line)
      (put-text-property (point-min) (point) 'face 'searchm-buffer)
      (put-text-property (point-min) (point) 'read-only 'end-field)
      )
    (if searchm-auto-size (set-frame-properties frame (searchm-buffer-dimensions buf)))
    (goto-char (point-min buf) buf)
    (re-search-forward "^Pattern:\t")
    (make-frame-visible frame)
    (raise-frame frame)))


(defun search-menu ()
  "Pop up a frame containing fields and buttons for doing search."
  (interactive)
  (setq searchm-current-buffer (current-buffer))
  (searchm-display-control-menu)
  )


(defun debug-properties (buffer)
  (let ((p (point-min buffer)) q)
    (print (list 1 (text-properties-at 1 buffer)))
    (while (setq q (next-property-change p buffer))
      (print (list p (buffer-substring p q buffer) (text-properties-at p buffer)))
      (setq p q)
      )))

(provide 'search-menu)

;;; search-menu.el ends here
