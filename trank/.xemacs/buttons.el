;;; buttons.el --- all menu and buttons add-ons

;; Copyright (C) 2002 by Free Software Foundation, Inc.

;; Author: Michael Pavlovsky  <mishka@FREE-BKDBMGRUV4>
;; Keywords: gui, gui

(defun myCPP-mode-setup () 
  "customization of c++ mode"
  (interactive)
  (require 'easymenu)
  (easy-menu-add-item nil '("C++") "--")
;;; line numbers
  (require 'setnu)
  (easy-menu-add-item  nil '("C++" "misc") 
		       ["Show line numbers" setnu-mode t])
  (require 'hideif)
  (easy-menu-add-item  nil '("C++" "misc") 
		       ["hide ifdef" hide-ifdef-mode t])
  (require 'doxymacs)
  (easy-menu-add-item  nil '("C++")
		       (list "doxygen" 
			     ["lookup" doxymacs-lookup t]
			     ["grouping-comments" 
			      doxymacs-insert-grouping-comments t]
			     ["function-comment"
			      doxymacs-insert-function-comment t]
			     ["file-comment" 
			      doxymacs-insert-file-comment t]
			     ["blank-multiline-comment" 
			      doxymacs-insert-blank-multiline-comment t]
			     ["rescan-tags" doxymacs-rescan-tagst]
			     ["blank-singleline-comment" 
			      doxymacs-insert-blank-singleline-comment t]))
  )

(add-hook 'c++-mode-hook 'myCPP-mode-setup )
  ;;; html-ization
(require 'htmlize)
(easy-menu-add-item  nil '("Tools") 
		     ["Htmlize file" htmlize-file t])


;; it has the menue of its own
;(require 'follow)
;(easy-menu-add-item  nil '("Tools" "Mods") 
;		     ["follow mode" follow-mode t])
;;{{{ search-menu
(require 'search-menu)
(easy-menu-add-item  nil '("Edit") 
		     ["search-menu" search-menu t])

;;}}}
;;{{{ ruler menu item
(defun my-column-ruler (width)
  "Display temp ruler at point."
  (interactive `(,(+ (window-hscroll)(window-width))))
  (momentary-string-display
   (if (< width 10)
       "1   5   10\n|...|....|\n"
     (let* ((iterations (/ width 10))
	    (short (- width (* 10 iterations)))
	    (result1 "|...|....|")
	    (result2 "1   5   10")
	    (inc1 "....|....|")
	    (inc2 "        %d0")
	    (i 1))
       (while  (< i iterations)
	 (setq i (1+ i))
	 (setq result1 (concat result1 inc1))
	 (setq result2 (concat result2 (substring (format inc2 i) -10))))
       (concat result2 "\n" result1 (substring inc1 0 short) "\n")))
   (comint-line-beginning-position)
   nil "[space] Clears ruler"))
 
(easy-menu-add-item  nil '("Cmds" "Utils") 
		     ["Ruler" my-column-ruler t])
;(global-set-key [f9] 'my-column-ruler)
;;}}}
