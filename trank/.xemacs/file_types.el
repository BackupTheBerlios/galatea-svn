;; -*- Mode: Emacs-Lisp -*-
(setq auto-mode-alist
      (append '(("\\.C$"  . c++-mode)
		("\\.cc$" . c++-mode)
		("\\.cpp$". c++-mode)		
		("\\.hh$" . c++-mode)
		("\\.inl$". c++-mode) ;; inline files  		
		("\\.c$"  . c-mode)
		("\\.h$"  . c-mode)
		("\\.stp$"  . sql-mode)
		("\\.scan$"  . autoconf-mode))
	      auto-mode-alist))