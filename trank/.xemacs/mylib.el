;;; mylib.el --- 

;; Copyright 2003 Michael Pavlovsky 
;;
;; Author: spavlov@techst02.technion.ac.il
;; Version: $Id: mylib.el,v 1.1.1.1 2003/03/10 20:23:15 michael Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'mylib)

;;; Code:

(provide 'mylib)
(eval-when-compile

(defvar safe-init-errors-list () )

(defun safe-require (f)
  (condition-case error ;
      (progn
	(message "# {{{ (safe-require \'%s )" f)
	(require f)
	(message "# }}} " ))
    (error ; catch an error
     (setq safe-init-errors-list (cons safe-init-errors-list (list "safe-require:" f  (cdr error))))
     (error 'error (princ (format "[EXEPTION] in (safe-require \'%s) : %s"  f  (error-message-string error))))))) 
     

(defun safe-init-func (func)
  (condition-case error
      (progn
	(message "# {{{ (safe-init-func \'%s)\n" func )
	(funcall func)
	(message "# }}} "  )
	t)
    (error 
     (setq safe-init-errors-list (cons safe-init-errors-list (list "safe-init-errors-list:" func  (cdr error))))
     (message "[EXEPTION] in (safe-init-func \'%s) : %s"  func  (error-message-string error)))))

   
;;{{{ safe-load

(defvar safe-load-error-list ""
  "*List of files that reported errors when loaded via safe-load")

(defun safe-load (file &optional noerror nomessage nosuffix)
  "Load a file.  If error when loading, report back, wait for
   a key stroke then continue on"
  (interactive "f")
  (condition-case nil
      (progn
	(message "# {{{ loading file %s " file)
	(load file noerror nomessage nosuffix) 
	(message "# }}} " )
	t)
    (error 
     (progn 
       (setq safe-load-error-list  (concat safe-load-error-list  " " file))
       (message 
	"****** [Return to continue] Error loading %s" safe-load-error-list )
       (sleep-for 1)
       nil))))

(defun safe-load-check ()
  "Check for any previous safe-load loading errors.  (safe-load.el)"
  (interactive)
  (if (string-equal safe-load-error-list "") () 
    (message (concat "****** error loading: " safe-load-error-list))))

;;}}}
;;{{{ my-load

(defun my_load (file)
  "give it one parameter as your customization file path
and it will check it for existence and for readablity.
all warnings will be in the *Warnings* buffer "
  (interactive "p"  )
  (if (and (file-exists-p file) 
	   (file-readable-p file))
      (progn 
	(load-file file)
	(message "%s loaded succesfully." file))
    (warn "Failed to load %s" file)))

;;}}}
(defun my-set-face-background-pixmap (FACE ROOTDIR FILE)
  "FACE is the same like in set-face-background-pixmap
ROOTDIR is a string with path to directory where  FILE is.
for example (my-set-face-background-pixmap 'default \"~/.xemacs/pixmaps/\" \"default.xpm\""   
)

;(defun my_debug_code (func)
;  " incapsulate your code in some function
;   kinda of try and catch"
;;;  (interactive "p"  )
;  (if (and (file-exists-p file) 
;	   (file-readable-p file))
;      (progn 
;	(load-file file)
;	(message "%s loaded succesfully." file))
;    (warn "Failed to load %s" file)))

;(defun Init-Safe-Require (Feat)
;"Try To Require The Specified Feature.  Errors Occurring Are Silenced.
;\(Perhaps In The Future There Will Be A way to get at the error.)
;Returns t if the feature was successfully required."
;  (condition-case nil
;      (progn (require feat) t)
;    (error nil)))



  (require 'cl))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################





;;; mylib.el ends here
