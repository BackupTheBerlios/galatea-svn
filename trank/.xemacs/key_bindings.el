;; -*- Mode: Emacs-Lisp -*-

(global-set-key [(f8)] 'compile )
(global-set-key [(f9)] 'recompile)
(global-set-key '(shift f5) 'find-library)
(global-set-key '(control f5) 'find-function)
(global-set-key '(meta f5) 'find-variable)
(global-set-key '(f2)  'kill-this-buffer)
(global-set-key '[(control c) (control c)] 'comment-region)
(global-set-key '(control kp-delete)  'bury-buffer)
(defun my-toggle-toolbar ()
  (interactive)
  (set-specifier default-toolbar-visible-p
		 (not (specifier-instance default-toolbar-visible-p))))
(global-set-key '[(control  x ) T ] 'my-toggle-toolbar)
 
;;; It works like this: ctr-x ctr-b and you are in the buffers list
;;; push v on selected 
(global-set-key [(control x) (control b)] '(lambda () 
					     (interactive)
					     (list-buffers)
					     (pop-to-buffer "*Buffer List*")))
;(add-hook 'buffer-menu-mode-hook (lambda ()
;				   (local-set-key '(v) '(message "hello"))))
						  ;(lambda ()							    ()
;							   (progn
;							     ;(Buffer-menu-select)
;							     (message "Hello") nil)
;							   ))))
				   
	  
;;(global-set-key '(shift f11) 'describe-foo-at-point)

;;
;; Walk between the windows
;;

(defun my-previous-window ()
  "Previous window"
  (interactive) 
  (other-window -1))
(global-set-key "\C-xp" 'my-previous-window)

(global-set-key "\C-xn" 'other-window)

;;; c++ mode
(require 'setnu)
(add-hook 'c++-mode-hook (lambda () (local-set-key "\C-c\M-n" #'setnu-mode)))


(add-hook 'css-mode-hook (lambda () (local-set-key "\M-space" #'cssm-complete-property)))



(define-key global-map (read-kbd-macro "M-RET") 'hippie-expand)

(add-hook 'latex-mode-hook (lambda () (local-set-key "\M->" #'TeX-complete-symbol)))
