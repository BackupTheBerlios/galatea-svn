;; -*- Mode: Emacs-Lisp -*-

;;; all custom or additional  *.el files should be stored in the
;;; ~/.xemacs/elisp/ 	
(setq load-path (cons (expand-file-name "~/.xemacs/") load-path))
(setq load-path (cons (expand-file-name "~/.xemacs/elisp/") load-path))

;;; You have to load this first
(load "mylib") ;;  I hope this is the only Achilles vulnerability I have

(safe-load "~/.xemacs/custom.el")
(safe-load "~/.xemacs/file_types.el")
(safe-load "~/.xemacs/packages.el");
(safe-load "~/.xemacs/key_bindings.el")
(safe-load "~/.xemacs/buttons.el")
(safe-load "faces")

(message "Errors log : %s" safe-init-errors-list)
(safe-load-check )
(if (not (eq safe-init-errors-list nil))
    (progn
      (show-message-log)
      (turn-on-folding-mode))
  ())





