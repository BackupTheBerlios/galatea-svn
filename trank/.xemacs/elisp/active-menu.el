;;  -*- Emacs-Lisp -*-

;;; active-menu.el --- Show menubar only when the mouse is at the top of
;;;                    the frame.

;;
;; Copyright (C) 2002 Claus Brunzema <mail@cbrunzema.de>
;;                    Stefan Kamphausen <mail@skamphausen.de>

;; Version: 1.0.4
;; $Id: active-menu.el,v 1.14 2002/09/30 19:16:15 chb Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;; -----------------------------------------------------------------------


;;; Commentary:

;; Prerequisites:
;;
;; I made active-menu with the following xemacs:
;; XEmacs 21.4 (patch 6) "Common Lisp" [Lucid] (i386-debian-linux) of Sat Apr  6 2002 on eeyore"
;; Please let me know if other versions work.


;; Installation:
;;
;; put this file in your load-path and the following in your init
;; file (~/.emacs or ~/.xemacs/init.el) if you want to use the
;; customisation facility:

;; (require 'active-menu)
;;
;; If you turn active-menu on and off frequently, you might want to use
;;
;; (autoload 'active-menu
;;           "active-menu"
;;           "Show menu only when mouse is at the top of the frame."
;;           t)
;;
;; instead. Now you can toggle active-menu with M-x active-menu. If
;; your frames change the height when the menu is toggled, try to find
;; a suitable value for `active-menu-frame-compensation'. 


;; History:
;; 2002-09-30  Claus Brunzema
;;         * Correct menu-hiding when frame is not focused on startup.
;;         * Fixed behavior with accelerate-menu (Thanks to Thomas
;;           Link for the suggestion).
;;         * Version 1.0.4
;; 2002-08-11  Claus Brunzema
;;         * Version 1.0.3
;; 2002-08-08  Claus Brunzema
;;         * Added active-menu-deselect-frame-hook-function. The
;;           menubar being visible only when it is needed is the whole
;;           point of active-menu. Obviously the menubar is not needed
;;           when the xemacs window isn't even selected.
;; 2002-07-18  Claus Brunzema
;;         * Modeline redraw fix
;; 2002-07-17  Claus Brunzema
;;         * Customizable active-menu-sensitive-height
;;           (a suggestion from Stefan Kamphausen)
;;         * Documentation fixes
;;         * Version 1.0.1
;; 2002-07-13  Claus Brunzema
;;         * More customisation stuff
;;         * Version 1.0.0
;; 2002-07-12  Claus Brunzema
;;         * Restricted toggling of the menubar to the
;;           currently selected frame
;;         * Customisation fixes
;;         * Redraw fixes
;; 2002-06-15  Claus Brunzema
;;         * Small cleanups
;; 2002-06-07  Claus Brunzema
;;         * Fixed and documented the require-way
;; 2002-06-06  Claus Brunzema
;;         * Code cleanup.
;;         * Version 0.9.7 released into public
;; 2002-06-02  Claus Brunzema
;;         * Frame compensation
;; 2002-05-30  Claus Brunzema
;;         * Big rewrite by Claus Brunzema
;; 2002-05-28  Stefan Kamphausen
;;         * Idea and initial implementation

;; ToDo:
;;
;; - save and restore menubar-visible-p
;; - maybe show different menus if you hit different areas on the top
;;   border.
;; - make it a real package

;; Bugs:
;;
;; - the menubar stays visible if you pop up some menus and click
;;   outside all menus to cancel any selection. It is removed after
;;   the first mouse move or key press.


;;; Code:

;; Customisation ----------------------------------------------------------
(defgroup active-menu nil
  "Show menubar only if the mouse is at the top of the frame."
  :link '(url-link :tag "Homepage" 
                   "http://www.cbrunzema.de/software.html#active-menu")
  :link '(emacs-commentary-link :tag "Commentary in active-menu.el"
				"active-menu.el")
  :prefix "active-menu-"
  :group 'mouse
  :group 'gui)

(defcustom active-menu-activated nil
  "*When t, active-menu is activated."
  :type 'boolean
  :initialize #'(lambda (symbol value)
		  (setq active-menu-activated nil))
  :set #'(lambda (symbol value)
	   (if value
	       (turn-on-active-menu)
	     (turn-off-active-menu)))
  :group 'active-menu)

(defcustom active-menu-sensitive-height 5
  "*The pixelrange of the sensitive area for active-menu."
  :type 'integer
  :group 'active-menu)

(defcustom active-menu-frame-compensation 1
  "*Number of extra textlines to add when the menu is hidden.
When the menubar is hidden, the frame will be expanded by that many
lines to compensate for the height change. This is, I admit
it, an ugly hack. There has to be a way to calculate this number
automatically. If you know how to do it, send mail to
mail@cbrunzema.de quick, please."
  :type 'integer
  :group 'active-menu)


;; Variables --------------------------------------------------------------
(defvar active-menu-original-mouse-motion-handler nil)
(defvar active-menu-suspend nil)
(defvar active-menu-command-skip 0)


;; Functions (internal) --------------------------------------------------
(defun active-menu-redraw ()
  "Redraw selected frame and all modelines if not suspended."
  (unless active-menu-suspend
    (redraw-frame (selected-frame))
    (redraw-modeline t)))

(defun active-menu-show-menubar ()
  "Show the menubar."
  (let ((frame (selected-frame)))
    (unless (specifier-instance menubar-visible-p frame)
      (set-specifier menubar-visible-p t frame)
      (set-frame-height frame
			(- (frame-height)
			   active-menu-frame-compensation))
      (active-menu-redraw)))
  t)

(defun active-menu-hide-menubar ()
  "Hide the menubar."
  (let ((frame (selected-frame)))
    (when (specifier-instance menubar-visible-p frame)
      (set-specifier menubar-visible-p nil frame)
      (set-frame-height frame
			(+ (frame-height)
			   active-menu-frame-compensation))
      (active-menu-redraw))))
  
(defun active-menu-menubar-maybe-show (event) 
  "Hide or show menubar according to the mouse position."
  (if (null event)
      (active-menu-hide-menubar)
    (when (motion-event-p event)
      (if (<= (event-y-pixel event) active-menu-sensitive-height)
	  (active-menu-show-menubar)
	(active-menu-hide-menubar)))))

(defun active-menu-post-command-hook-function ()
  ;; this is needed to eventually remove the menu after an item
  ;; is selected (and the mouse isn't moved after the click).
  (when active-menu-suspend
    (if (zerop active-menu-command-skip)
	(setq active-menu-suspend nil)
      (decf active-menu-command-skip)))
  (unless active-menu-suspend  
    (active-menu-menubar-maybe-show
     (mouse-position-as-motion-event))))
  
(defun active-menu-mouse-motion-handler (event)
  (unless active-menu-suspend
    (active-menu-menubar-maybe-show event)
    (funcall active-menu-original-mouse-motion-handler event)))

(defun active-menu-deselect-frame-hook-function ()
  (unless active-menu-suspend
    (active-menu-hide-menubar)))

(defun active-menu-activate-menubar-hook-function ()
  (unless active-menu-suspend
    ;; we must ignore the command that invoked the menubar and
    ;; thus the activate-menubar-hook. The next command will be the
    ;; one coming from the menubar, so we will end the suspension in
    ;; active-menu-post-command-hook-function on that one. See
    ;; active-menu-post-command-hook-function, too.
    (setq active-menu-command-skip 1)
    (setq active-menu-suspend t)
    (active-menu-show-menubar))
  t)


(defun active-menu-install-handler-and-hook ()
  "Install mouse handler and hook functions for active-menu.
Don't use this, use `turn-on-active-menu' instead"
  (setq active-menu-suspend nil)
  (setq active-menu-original-mouse-motion-handler mouse-motion-handler)
  (setq mouse-motion-handler #'active-menu-mouse-motion-handler)
  (add-hook 'post-command-hook
	    #'active-menu-post-command-hook-function)
  (add-hook 'deselect-frame-hook
	    #'active-menu-deselect-frame-hook-function)
  (add-hook 'activate-menubar-hook
	    #'active-menu-activate-menubar-hook-function)
  (active-menu-menubar-maybe-show (mouse-position-as-motion-event))
  (setq active-menu-activated t))

(defun active-menu-remove-handler-and-hook ()
  "Remove mouse handler and hook functions for active-menu.
Don't use this, use `turn-off-active-menu' instead"
  (setq mouse-motion-handler active-menu-original-mouse-motion-handler)
  (remove-hook 'post-command-hook
	       #'active-menu-post-command-hook-function)
  (remove-hook 'deselect-frame-hook
	       #'active-menu-deselect-frame-hook-function)
  (remove-hook 'activate-menubar-hook
	       #'active-menu-activate-menubar-hook-function)
  (active-menu-show-menubar)
  (setq active-menu-activated nil))


;;; Functions (external) --------------------------------------------------
(defun turn-on-active-menu ()
  "Turn on active-menu (you guessed it)."
  (interactive)
  (unless active-menu-activated
    (active-menu-install-handler-and-hook)))

(defun turn-off-active-menu ()
  "Turn off active-menu."
  (interactive)
  (when active-menu-activated
    (active-menu-remove-handler-and-hook)))

;;;###autoload
(defun active-menu (&optional arg)
  "Toggle active menu.
With arg, turn active-menu on iff arg is positive."
  (interactive "P")
  (if (or (and arg (> (prefix-numeric-value arg) 0))
	  (and (null arg) (not active-menu-activated)))
      (turn-on-active-menu)
    (turn-off-active-menu)))

(provide 'active-menu)

;;; active-menu.el ends here
