;; -*- Mode: Emacs-Lisp -*-

;;{{{ auctex

(defun my-auctex-init-hook ()
  (safe-require 'tex-site)
  (if window-system
      (safe-require 'font-latex))
  (add-hook 'LaTeX-mode-hook 
	    '(lambda ()
	       (define-key LaTeX-mode-map [(control c) $] 
		 '(lambda ()
		    (interactive )
		    (insert "$$")
		    (forward-char -1)))))
  )
(safe-init-func 'my-auctex-init-hook)

;;}}}
;;{{{ active-menu
(defun my-active-menu-init-hook ()
  (safe-require 'active-menu)
  ;;; nice button onto Options-> Active menu -> Toggle menu bar
  (safe-require 'easymenu)
  (easy-menu-add-item nil '("Options") "--")
  (easy-menu-add-item  nil '("Options" "Active menu") 
		       ["Toggle menu bar" active-menu t])
  ;;; uncomment this line, if you 
  ;;; want it be activated from the beginning.
  ;(active-menu)
  )
(safe-init-func 'my-active-menu-init-hook)
;;}}}
;;{{{ autoinsert

;;; autoinsert
(defun my-auto-insert-init-hook ()
  (safe-require 'autoinsert)
  (add-hook 'find-file-hooks 'auto-insert))
(safe-init-func 'my-auto-insert-init-hook)

;;}}}
;;{{{ auto-revert-mode

(defun my-auto-revert-mode-init-hook ()
  (autoload 'auto-revert-mode "autorevert" nil t)
  (autoload 'turn-on-auto-revert-mode "autorevert" nil nil)
  (autoload 'global-auto-revert-mode "autorevert" nil t)
  (add-hook 'c-mode-hook 'turn-on-auto-revert-mode)
  (add-hook 'c++-mode-hook 'turn-on-auto-revert-mode))
(safe-init-func 'my-auto-revert-mode-init-hook)
;;(add-hook 'dired-mode-hook 'turn-on-auto-revert-mode)
;; To activate Global Auto-Revert Mode every time Emacs is started the
;; following line could be added to your ~/.emacs:
;;   (global-auto-revert-mode 1)
;; (autoload 'auto-revert-mode "autorevert" nil t)
;; (autoload 'turn-on-auto-revert-mode "autorevert" nil nil)
;; (autoload 'global-auto-revert-mode "autorevert" nil t)

;;}}}
;;{{{ auto-save

(defun my-auto-safe-init-hook ()
;;; ********************
;;; Load the auto-save.el package, which lets you put all of your autosave
;;; files in one place, instead of scattering them around the file system.
;;;
  (setq auto-save-directory (expand-file-name "~/.xemacs/.autosave/")
	auto-save-directory-fallback auto-save-directory
	auto-save-hash-p nil
	efs-auto-save t
	efs-auto-save-remotely nil
	;; now that we have auto-save-timeout, let's crank this up
	;; for better interactive response.
	auto-save-interval 2000 )
  ;; We load this afterwards because it checks to omake sure the
  ;; auto-save-directory exists (creating it if not) when it's loaded.
  (safe-require 'auto-save))
(safe-init-func 'my-auto-safe-init-hook)

;;}}}
;;{{{ backup-dir 

(defun my-backup-dir-init-hook ()
  (safe-require 'backup-dir)
  (setq bkup-backup-directory-info
	'(("~/.*" "~/.xemacs/.backups/" ok-create full-path prepend-name)
	  ("^/[^/:]+:"     ".backups/") ; handle EFS files specially: don't 
	  ("^/[^/:]+:"     "./")        ; search-upward... its very slow
	  (t               ".backups/" full-path prepend-name search-upward))))

(safe-init-func 'my-backup-dir-init-hook)

;;}}}
;;{{{ bookmarks   
(defun my-bookmarks-init-hook ()
  (safe-require 'bookmark)
  (if (and (file-exists-p bookmark-default-file) 
	   (file-readable-p bookmark-default-file))
      (progn
	(bookmark-load bookmark-default-file) t)
    (warn "WARNING> Bookmark file: %s is NOT present or NOT readable " bookmark-default-file)))

(safe-init-func 'my-bookmarks-init-hook)
;;}}}
;;{{{ big-menubar
(defun my-big-menubar-init-hook()
  (safe-require 'big-menubar)
  (delete-menu-item '("Top"))
  (delete-menu-item '("<<"))
  (delete-menu-item '(" | "))
  (delete-menu-item '(">>"))
  (delete-menu-item '("Bot"))
  (delete-menu-item '("Buffers")))

(safe-init-func 'my-big-menubar-init-hook)
;;}}}
;;{{{ color-themes

;;; adds nice menu at tools->ColorThemes.
(require 'color-theme)
(color-theme-billw)

;;}}}
;;{{{ color-mate

;;}}}
;;{{{ ctypes

;;; ctypes
(defun my-ctypes-init-hook ()
  (defun my-c-mode-hook ()
    (safe-require 'ctypes)
    (turn-on-font-lock)
    (setq ctypes-write-types-at-exit t)
    (ctypes-read-file nil nil t t)
    (ctypes-auto-parse-mode 1))

  (add-hook 'c-mode-hook 'my-c-mode-hook)
  (add-hook 'c++-mode-hook 'my-c-mode-hook)

  (defun my-ctypes-load-hook ()
    (ctypes-read-file "~/.xemacs/.ctypes_std_c" nil t t))
  (add-hook 'ctypes-load-hook 'my-ctypes-load-hook))

(safe-init-func 'my-ctypes-init-hook)

;;}}}
;;{{{ csharp

(defun my-csharp-init-hook ()
  (add-to-list 'load-path "~/.xemacs/elisp/csharp")
  (autoload 'csharp-mode "cc-mode")
  (c-add-style "myC#Style"
	       '("C#"
		 (c-basic-offset . 2)
		 (c-comment-only-line-offset . (0 . 0))
		 (c-offsets-alist . (
				     (c                     . c-lineup-C-comments)
				     (inclass		   . 0)
				     (namespace-open	   . +)
				     (namespace-close	   . +)
				     (innamespace	   . 0)
				     (class-open		   . +)
				     (class-close	   . +)
				     (inclass		   . 0)
				     (defun-open		   . +)
				     (defun-block-intro     . 0)
				     (inline-open	   . +)
				     (inline-close	   . 0)
				     (statement-block-intro . 0)
				     (statement-cont	   . +)
				     (brace-list-intro      . +)
				     (topmost-intro-cont    . 0)
				     (block-open		   . +)
				     (block-close	   . 0)
				     (arglist-intro	   . +)
;    (arglist-cont	   . 0)
				     (arglist-close	   . 0)
				     ))
		 ))
  
  (defun my-csharp-mode-hook ()
    (cond (window-system
	   (turn-on-font-lock)
	   (c-set-style "myC#Style")
	   )))
  (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
  (setq auto-mode-alist
	(append '(
		  ("\\.cs$" . csharp-mode)
		  ) auto-mode-alist ))
  
;  (setq compilation-error-regexp-alist
;	(append '(
;;C# Compiler
;;t.cs(6,18): error SC1006: Name of constructor must match name of class
;;
;		  ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): \\(error\\|warning\\) CS[0-9]+:" 1 3 4)
;		  )
;		compilation-error-regexp-alist))
  )
(safe-init-func 'my-csharp-init-hook)

;;}}}
;;{{{ default-dir.el  

;;; ********************
;;; Load the default-dir.el package which installs fancy handling
;;;  of the initial contents in the minibuffer when reading
;;; file names.
(defun my-default-dir-init-hook ()
  (if (and running-xemacs
	   (or (and (= emacs-major-version 20) (>= emacs-minor-version 1))
	       (and (= emacs-major-version 19) (>= emacs-minor-version 15))))
      (safe-require 'default-dir)))
(safe-init-func 'my-default-dir-init-hook)

;;}}}
;;{{{ dos2unix

(defun my-dos2unix-init-hook ()
  (load-library "dos2unix")
  (safe-require 'easymenu)
  (easy-menu-add-item  nil '("Tools") ["dos2unix" dos2unix t])
  )
(safe-init-func 'my-dos2unix-init-hook)

;;}}}
;;{{{ dired Ox

(require 'dired)
;; we want dired not not make always a new buffer if visiting a directory
;; but using only one dired buffer for all directories.
(defadvice dired-advertised-find-file (around dired-subst-directory activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer))
	(filename (dired-get-filename)))
    ad-do-it
    (when (and (file-directory-p filename)
	       (not (eq (current-buffer) orig)))
      (kill-buffer orig))))

;;}}}
;;{{{ dired-single

;(require 'dired-single)

;(defun my-dired-init ()
;  "Bunch of stuff to run for dired, either immediately or when it's
;         loaded."
;  ;; <add other stuff here>
;  (define-key dired-mode-map [return] 'joc-dired-single-buffer)
;  (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
;  (define-key dired-mode-map "^"
;    (function
;     (lambda nil (interactive) (joc-dired-single-buffer "..")))))

;;; if dired's already loaded, then the keymap will be bound
;(if (boundp 'dired-mode-map)
;    ;; we're good to go; just add our bindings
;    (my-dired-init)
;  ;; it's not loaded yet, so add our bindings to the load-hook
;  (add-hook 'dired-load-hook 'my-dired-init))
;;;  NOTE: This should only be done for the dired-mode-map (NOT globally!).

;;;;; The following code will work whether or not dired has been loaded already.

;(defun my-dired-init ()
;  "Bunch of stuff to run for dired, either immediately or when it's
;         loaded."
;  ;; <add other stuff here>
;  (define-key dired-mode-map [return] 'joc-dired-single-buffer)
;  (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
;  (define-key dired-mode-map "^"
;    (function
;     (lambda nil (interactive) (joc-dired-single-buffer "..")))))

;;; if dired's already loaded, then the keymap will be bound
;(if (boundp 'dired-mode-map)
;    ;; we're good to go; just add our bindings
;    (my-dired-init)
;  ;; it's not loaded yet, so add our bindings to the load-hook
;  (add-hook 'dired-load-hook 'my-dired-init))

;;}}}
;;{{{ default-dir.el

;;; ********************
;;; Load the default-dir.el package which installs fancy handling
;;;  of the initial contents in the minibuffer when reading
;;; file names.

(if (and running-xemacs
	 (or (and (= emacs-major-version 20) (>= emacs-minor-version 1))
	     (and (= emacs-major-version 19) (>= emacs-minor-version 15))))
    (require 'default-dir))

;;}}}
;;{{{ doxywizard

(defun my-doxymax-init-hook ()
  (safe-require 'doxymacs)
  (defun my-doxymacs-font-lock-hook ()
    (if (or (eq major-mode 'c-mode) 
	    (eq major-mode 'c++-mode)
	    (eq major-mode 'plsql-mode)
	    (eq major-mode 'sql-mode))
	(doxymacs-font-lock)))
  
  (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
  (add-hook 'c-mode-common-hook 'doxymacs-mode))
(safe-init-func 'my-doxymax-init-hook)
;- Default key bindings are:
;  - C-c d ? will look up documentation for the symbol under the point.
;  - C-c d r will rescan your Doxygen tags file.
;  - C-c d f will insert a Doxygen comment for the next function.
;  - C-c d i will insert a Doxygen comment for the current file.
;  - C-c d ; will insert a Doxygen comment for the current member.
;  - C-c d m will insert a blank multiline Doxygen comment.
;  - C-c d s will insert a blank singleline Doxygen comment.
;  - C-c d @ will insert grouping comments around the current region.

;;}}}
;;{{{ eshell

(defun my-eshell-init-hook ()
  (safe-require 'eshell)
  ;; eshell cotrol files
  (setq shell-directory-name "~/.xemacs/eshell_control_files/")
  ;; You may type alt-x eshell to invoke eshell. To set the path and
  ;;environment correctly, you may want to put this into your .emacs file.
  (add-hook 'eshell-mode-hook
	    '(lambda nil
	       (eshell/export "EPOCROOT=\\Paragon\\")
	       (let ((path))
		 ;; TODO: This is paltform specific separator, replace this
		 (setq path ".;c:/program files/microsoft visual studio/vb98/")
		 (setq path (concat path ";c:/programs/perl/bin"))
		 (setenv "PATH" path))
		;(local-set-key "\C-u" 'eshell-kill-input)
	       )
	    )
  
  ;; To make eshell understand .pl perl extensions.
  (add-hook 'eshell-named-command-hook 'n-eshell-exec-perl)
  (defun n-eshell-exec-perl (command args)
    "04Dec2001 - sailor, to make eshell understand perl scripts."
    (if (string-match "^.+\.pl[ \t]*" command)
	(progn
	  (setq args (cons command args))
	  (setq args (cons "-S" args))
	  (throw 'eshell-replace-command
		 (eshell-parse-command "perl" args)))))
  (defun eshell/clear ()
    "04Dec2001 - sailor, to clear the eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  )
(safe-init-func 'my-eshell-init-hook)
;;}}}
;;{{{ func-menu

(defun my-function-menu-init-hook ()
;;; ********************
;;; func-menu is a package that scans your source file for function
;;; definitions and makes a menubar entry that lets you jump to any
;;; particular function definition by selecting it from the menu.  The
;;; following code turns this on for all of the recognized languages.
;;; Scanning the buffer takes some time, but not much.
;;;
;;; Send bug reports, enhancements etc to:
;;; David Hughes <ukchugd@ukpmr.cs.philips.nl>
;;;
  (cond (running-xemacs
	 (safe-require 'func-menu)
	 ;; The Hyperbole information manager package uses (shift button2) and
	 ;; (shift button3) to provide context-sensitive mouse keys.  If you
	 ;; use this next binding, it will conflict with Hyperbole's setup.
	 ;; Choose another mouse key if you use Hyperbole.
	 (define-key global-map '(shift button3) 'mouse-function-menu)
	 
	 ;; For descriptions of the following user-customizable variables,
       ;; type C-h v <variable>
	 (setq fume-max-items 25
	       fume-fn-window-position 3
	       fume-auto-position-popup t
	       fume-display-in-modeline-p t
	       fume-menubar-menu-location "File"
	       fume-buffer-name "*Function List*"
	       fume-no-prompt-on-valid-default nil) )))

(safe-init-func 'my-function-menu-init-hook)

;;}}}
;;{{{ flame
(defun my-flame-init-hook ()
  (safe-require 'flame)
  (safe-require 'friedman-flame))
(safe-init-func 'my-flame-init-hook)

;;}}}
;;{{{ folding mode package 
(defun my-folding-mode-init-hook ()
  (autoload 'folding-mode          "folding" "Folding mode" t)
  (autoload 'turn-off-folding-mode "folding" "Folding mode" t)
  (autoload 'turn-on-folding-mode  "folding" "Folding mode" t)
  (if (load "folding" 'nomessage 'noerror)
      (folding-mode-add-find-file-hook))
  (add-hook 'sh-mode-hook '(lambda ()
			     (turn-on-folding-mode)))
  (add-hook 'autoconf-mode-hook '(lambda ()
				   (turn-on-folding-mode)))
  
  (safe-require 'autoconf-mode)
  (folding-add-to-marks-list 'autoconf-mode "dnl {{{" "dnl }}}" )
  (folding-add-to-marks-list 'plsql-mode "-- {{{" "-- }}}" )
  (folding-add-to-marks-list 'sql-mode "-- {{{" "-- }}}" )
  (folding-add-to-marks-list 'psql-mode "-- {{{" "-- }}}" )
  )
(safe-init-func 'my-folding-mode-init-hook)

;;; don't forget that I have changed the default values 
;;; in folding.el file -> c++-mode -> ///
;;{{{ Examples

;;  Example: personal setup
;;
;;      To define your own key binding instead of using the standard ones,
;;      you can do like this:
;;
;;           (setq folding-mode-prefix-key "\C-c")
;;           ;;
;;           (setq folding-default-keys-function
;;               '(folding-bind-backward-compatible-keys))
;;           ;;
;;           (setq folding-load-hook 'my-folding-load-hook)
;;
;;
;;           (defun my-folding-load-hook ()
;;             "Folding setup."
;;
;;             (folding-install)  ;; just to be sure
;;
;;             ;; ............................................... markers ...
;;
;;             ;;  Change text-mode fold marks. Handy for quick
;;             ;;  sh/perl/awk code
;;
;;             (defvar folding-mode-marks-alist nil)
;;
;;             (let* ((ptr (assq 'text-mode folding-mode-marks-alist)))
;;               (setcdr ptr (list "# {{{" "# }}}")))
;;
;;             ;; ............................................... bindings ...
;;
;;             ;;  Put `folding-whole-buffer' and `folding-open-buffer'
;;             ;;  close together.
;;
;;             (defvar folding-mode-prefix-map nil)
;;
;;             (define-key folding-mode-prefix-map "\C-w" nil)
;;             (define-key folding-mode-prefix-map "\C-s"
;;                         'folding-show-current-entry)
;;             (define-key folding-mode-prefix-map "\C-p"
;;                         'folding-whole-buffer))
;;
;;  Example: changing default fold marks
;;
;;      In case you're not happy with the default folding marks, you
;;      can change them easily. Here is an example
;;
;;          (setq folding-load-hook 'my-folding-load-hook)
;;
;;          (defun my-folding-load-hook ()
;;            "Folding vars setup."
;;            (let* ((ptr (assq 'text-mode folding-mode-marks-alist)))
;;              (setcdr ptr (list "# {{{" "# }}}"))))
;;
;;
;;  Example: choosing different fold marks for mode
;;
;;      Suppose you sometimes want to use different fold marks for the major
;;      mode: e.g. to alternate between "# {{{" and "{{{" in `text-mode'
;;      Call `M-x' `my-folding-text-mode-setup' to change the marks.
;;
;;            (defun my-folding-text-mode-setup (&optional use-custom-folding-marks)
;;              (interactive
;;                (list (y-or-n-p "Use Custom fold marks now? ")))
;;              (let* ((ptr (assq major-mode folding-mode-marks-alist))
;;                     (default-begin "# {{{")
;;                     (default-end   "# }}}")
;;                     (begin "{{{")
;;                     (end   "}}}"))
;;                (when (eq major-mode 'text-mode)
;;                  (unless use-custom-folding-marks
;;                    (setq  begin default-begin  end default-end)))
;;                (setcdr ptr (list begin end))
;;                (folding-set-marks begin end)))
;;
;;  Example: AucTex setup
;;
;;      Suppose you're using comment.sty with AucTeX for editing LaTeX2e
;;      documents and you have these comment types. You would like to be
;;      able to set which of these 3 is to be folded at any one time, using
;;      a simple key sequence: move back and forth easily between the
;;      different comment types, e.g., "unfold everything then fold on \x".
;;
;;          \O   ...  \endO
;;          \L   ...  \endL
;;          \B   ...  \endB
;;
;;          (setq folding-load-hook 'my-folding-load-hook)
;;
;;          (defun my-folding-load-hook ()
;;            "Folding vars setup."
;;            (let ((ptr (assq 'text-mode folding-mode-marks-alist)))
;;              (setcdr ptr (list "\\O" "\\endO"))
;;              (define-key folding-mode-prefix-map "C"
;;                         'my-folding-marks-change)))
;;
;;          (defun my-folding-marks-change (&optional selection)
;;            "Select folding marks: prefixes nil, C-u and C-u C-u."
;;            (interactive "P")
;;            (let ((ptr (assq major-mode folding-mode-marks-alist))
;;                  input)
;;              (when (string-match "^\\(plain-\\|la\\|auc\\)?tex-"
;;                                  (symbol-name  major-mode))
;;                (setq input
;;                      (read-string "Latex \\end(X) Marker (default O): "
;;                                   nil nil "O" nil))
;;                (setq input (upcase input))
;;                (turn-off-folding-mode)
;;                (folding-add-to-marks-list
;;                 major-mode (concat "\\" input) (concat "\\end" input) nil nil t)
;;                ;; (setcdr ptr (list (concat "\\" input) (concat "\\end" input)))
;;                (turn-on-folding-mode))))
;;          ;;  End of example
;;
;;  Bugs: Lazy-shot.el conflict in XEmacs
;;
;;      [XEmacs 20.4 lazy-shot-mode]
;;      1998-05-28 Reported by Solofo Ramangalahy <solofo@mpi-sb.mpg.de>
;;
;;          % xemacs -q folding.el
;;          M-x eval-buffer
;;          M-x folding-mode
;;          M-x font-lock-mode
;;          M-x lazy-shot-mode
;;          C-s mouse
;;
;;      then search for mouse again and again. At some point you will see
;;      "Deleting extent" in the minibuffer and XEmacs freezes.
;;
;;      The strange point is that I have this bug only under Solaris 2.5 sparc
;;      (binaries from ftp.xemacs.org) but not under Solaris 2.6 x86. (XEmacs
;;      20.4, folding 2.35). I will try to access more machines to see if it's
;;      the same.
;;
;;      I suspect that the culprit is lazy-shot as it is beta, but maybe you
;;      will be able to describe the bug more precisely to the XEmacs people I
;;      you can reproduce it.

;;}}}

;;}}}
;;{{{ follow mode
(defun my-follow-mode-init-hook ()
  (safe-require 'follow))
(safe-init-func 'my-follow-mode-init-hook)

;;}}}
;;{{{ font-lock

;; Must be defined before font lock is loaded.
(setq font-lock-maximum-decoration t)
(add-hook 'font-lock-mode-hook 'my-font-lock-mode-hook)

(defun my-font-lock-mode-hook ()
  ;; My mode line is already full...
  (setcar (cdr (assq 'font-lock-mode minor-mode-alist)) "")
  ;; I belive that the need for fontfication increases with
  ;; the size of the file. 
  (setq font-lock-maximum-size (* 1024 1024)))

(add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
;(global-font-lock-mode t)

(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1); Emacs
  (setq font-lock-auto-fontify t)); XEmacs

;;}}}
;;{{{ face-list
(defun my-face-list-init-hook ()
  (safe-require 'face-list))
(safe-init-func 'my-face-list-init-hook)

;;}}}
;;{{{ gnuserv
(defun my-gnuserv-init-hook ()
  (gnuserv-start))
(safe-init-func 'my-gnuserv-init-hook)
;;}}}
;;{{{ gtags
(defun my-gtags-init-hook ()
  (setq c++-mode-hook
	'(lambda ()
	   (gtags-mode 1) ))
  (setq c-mode-hook
	'(lambda ()
	   (gtags-mode 1) )))
(safe-init-func 'my-gtags-init-hook)

;;}}}
;;{{{ highlight-current-line

(defun my-highlight-current-line-init-hook ()
  (safe-require 'highlight-current-line)
  (easy-menu-add-item  nil '("View") 
		       ["highlight-current-line" highlight-current-line-on t])

  )

(safe-init-func 'my-highlight-current-line-init-hook)
;; ;; If you want to mark only to the end of line:
;; (highlight-current-line-whole-line-on nil)
;; ;; switch highlighting on
;; (highlight-current-line-on t)
;;
;; ;; If you want to change default-foreground/background color add something
;; ;; like:
;; (highlight-current-line-set-fg-color "red")
;; (highlight-current-line-set-bg-color "white")
;; ;; There's a special color "none" defined to set no color.
;;
;; ;; Ignore no buffer
;; (setq highlight-current-line-ignore-regexp nil) ; or set to ""
;; ;; alternate way to ignore no buffers
;; (fmakunbound 'highlight-current-line-ignore-function)
;; ;; Ignore more buffers
;; (setq highlight-current-line-ignore-regexp
;;      (concat "Dilberts-Buffer\\|"
;;	      highlight-current-line-ignore-regexp))

;;}}}
;;{{{ highlight-completion
(defun my-highlight-completion-hook ()
  (safe-require 'highlight-completion)
  (highlight-completion-mode 1))
(safe-init-func 'my-highlight-completion-hook)

;;}}}
;;{{{ hideshow
(defun my-hideshow-init-hook ()
  (load-library "hideshow")
  (defun my-hs-setup () 
    "enables hideshow and binds some commands"
    (hs-minor-mode 1)
    (define-key hs-minor-mode-map "\C-ch" 'hs-hide-block)
    (define-key hs-minor-mode-map "\C-cs" 'hs-show-block)
    (define-key hs-minor-mode-map "\C-cH" 'hs-hide-all)
    (define-key hs-minor-mode-map "\C-cS" 'hs-show-all)
    (define-key hs-minor-mode-map "\C-cR" 'hs-show-region))
  (add-hook 'c-mode-common-hook 'my-hs-setup)
  (add-hook 'c++-mode-hook 'my-hs-setup)
  (add-hook 'c-mode-hook 'my-hs-setup) 
  )

(safe-init-func 'my-hideshow-init-hook)

;;}}}
;;{{{ info cleaning hack
;;;Problem description: Info-directory-list includes some non existant directories
;;;I have no time to find why is that happens and this small hack cleans it out
(defun my-info-cleanup-init-hook ()
  (defun rec (list )
    (if (eq nil list)
	()	
      (if (file-exists-p (car list))
	  (cons (car list) (rec (cdr list))) 
	(rec (cdr list)))))
  (setq Info-directory-list (rec Info-directory-list)))

(safe-init-func 'my-info-cleanup-init-hook)
;;}}}
;;{{{ icomplete-mode
(defun my-icomplete-mode-init-hook ()
  (safe-require 'icomplete)
  (icomplete-mode))
(safe-init-func 'my-icomplete-mode-init-hook)

;;}}}
;;{{{ latex-toolbar

(defun my-latex-toolbar-init-hook ()
  (add-to-list 'load-path "~/.xemacs/elisp/latex-toolbar")
;;      ;; Load AucTeX ...
  (safe-require 'tex-site)
;;      ...
  (add-hook 'LaTeX-mode-hook
                (function (lambda()
			    (setq toolbar-visible-p t)
			    ;; Add some commands to `TeX-command-list'
			    (add-to-list
			     'TeX-command-list
			     '("PDFLaTeX" "pdflatex '\\nonstopmode\\input{%t}'"
			       TeX-run-command nil nil))
			    (add-to-list
			     'TeX-command-list
      		       '("Acroread" "acroread %s.pdf"
      			 TeX-run-silent t nil))
			    (add-to-list
			     'TeX-command-list
			     '("xpdf" "xpdf %s.pdf" TeX-run-silent t nil))
			    (add-to-list
			     'TeX-command-list
      		       '("gv" "gv %s.ps" TeX-run-silent t nil))
			    ;; ...
                            ;; LaTeX toolbar
			    (safe-require 'latex-toolbar)
			    (latex-toolbar-install)))))
(safe-init-func 'my-latex-toolbar-init-hook)

;;}}}
;;{{{ mail stuff
(defun my-mail-init-hook ()
  ;;; read more there
  (setq vm-init-file "~/.xemacs/vm-config.el"))

(safe-init-func 'my-mail-init-hook)

;;}}}
;;{{{ member-functions Expand C++ member function declarations
(defun my-member-functions-init-hook ()
  (autoload 'expand-member-functions "member-functions" "Expand C++ member function declarations" t)

  (defun my-expand-setup () "enables expand menu"
    (local-set-key "\C-cm" #'expand-member-functions)
    (safe-require 'easymenu)
    (easy-menu-add-item nil '("C++") "--")
    (easy-menu-add-item  nil '("C++" "Utils") 
			 ["Expand mem. funct" expand-member-functions t]))
  (add-hook 'c++-mode-hook 'my-expand-setup t)
  )
(safe-init-func 'my-member-functions-init-hook)

;;}}}
;;{{{ mwheel
(defun my-mwhell-init-hook ()
  (safe-require 'mwheel)
  (mwheel-install))
(safe-init-func 'my-mwhell-init-hook)
;;}}}
;;{{{ partial-completion mechanism

;;; ********************
;;; Load a partial-completion mechanism, which makes minibuffer completion
;;; search multiple words instead of just prefixes; for example, the command
;;; `M-x byte-compile-and-load-file RET' can be abbreviated as `M-x b-c-a RET'
;;; because there are no other commands whose first three words begin with
;;; the letters `b', `c', and `a' respectively.
;;;
(defun my-completion-init-hook ()
  (safe-require 'completion)
  ;;  (load-library "completion")
  (initialize-completions))
(safe-init-func 'my-completion-init-hook)

;;}}}
;;{{{ pl-sql editing mode

(defun my-plsql-init-hook ()
  (safe-require 'plsql)
  (setq plsql-indent 2)
  (setq auto-mode-alist
	(append
	 '(("\\.\\(p\\(?:k[bg]\\|ls\\)\\|sql\\)\\'" . plsql-mode))
	 auto-mode-alist)))
(safe-init-func 'my-plsql-init-hook)

;;}}}
;;{{{ templates
(defun my-template-init-hook ()
  (safe-require 'template)
  (template-initialize)
  (add-menu-button '("File")
		   ["Insert and Expand Template..." template-expand-template  :active (not buffer-read-only)]
		   "Insert File..."))
(safe-init-func 'my-template-init-hook)
;;}}}
;;{{{ tempo 

(defun my-tempo-init-hook ()
  (safe-require 'tempo)
  ;; This is a way to hook tempo into cc-mode
  (defvar c-tempo-tags nil
    "Tempo tags for C mode")

  (defvar c++-tempo-tags nil
    "Tempo tags for C++ mode")
  
;;; C-Mode Templates and C++-Mode Templates (uses C-Mode Templates also)
 
  (add-hook 'c-mode-hook    '(lambda ()
			       (tempo-use-tag-list 'c-tempo-tags)
			       ))
  
  (add-hook 'c++-mode-hook  '(lambda ()
			       (tempo-use-tag-list 'c-tempo-tags)
			       (tempo-use-tag-list 'c++-tempo-tags)
			       ))
  
  (add-hook 'plsql-mode-hook '(lambda ()
				(tempo-use-tag-list 'c-tempo-tags)
				))
;;; doxygen templates
  (tempo-define-template "doxygen-function-template"
			 '("/** " > n
			   " *  @fn " (p "function name : ") > n>
			   " *  @param " > n> 
			   " *  @return " > n>  
			   " *  @throw" > n> 
			   " *  @brief" > n>
			   " *  @par " > n >
			   " *  " > n >
			   " *  CDECLARE " > n >
			   " */" n>
			   )
			 "doxygen-function-template" ;; tag for completion
			 "Insert doxygen's function template" ;;; doc string
			 'c-tempo-tags)
  (tempo-define-template "doxygen-class-template"
			 '("/** " > n
			   " *  @class " (p "class name : ") > n>
			   " *  @brief" > n>
			   " *  @par   " > n>
			   " */" n>
			   )
			 "doxygen-class-template" ;; tag for completion
			 "Insert doxygen's class template" ;;; doc string
			 'c-tempo-tags)  
 (tempo-define-template "doxygen-sql-record-template"
			'("/** " > n
			  " *  @class " (p "class name : ") > n>
			  " *  @brief " > n>
			  " *  @par   RECORD FIELDS" > n>
			  " *  @warning" > n> 
                          " *  <b>You have no promise of the columns order (and maybe for colums names)</b>" > n >
                          " *" > n >
			  " *  @htmlonly" > n >
			  " *  <table border=0 cellpadding=1 cellspacing=2> " > n >
			  " *     <tr  bgcolor=#3366ff> " > n >
			  " *        <td ><font color=#ffffff><b> Label </td> " > n >
			  " *        <td ><font color=#ffffff><b> Type </td> " > n >
			  " *        <td ><font color=#ffffff><b> Description </td> " > n >
			  " *     </tr > " > n >
			  " *     <tr > " > n >
			  " *        <td bgcolor=#ffcc99>  </td><td bgcolor=#ccccff>  </td><td>  </td> " > n >
			  " *     </tr> " > n >
			  " *  </table> " > n >
			  " *  @endhtmlonly" > n >
			  " *  CDECLARE " > n >
			  " */" n>
			   )
			 "doxygen-sql-record-template" ;; tag for completion
			 "Insert doxygen's sql record template" ;;; doc string
			 'c-tempo-tags)
;;; Preprocessor Templates (appended to c-tempo-tags)
  
  (tempo-define-template "c-include"
			 '("#include <" r ".h>" > n
			   )
			 "#include"
			 "Insert a #include <> statement"
			 'c-tempo-tags)

  (tempo-define-template "c-ifdef"
			 '("#ifdef " (p "ifdef-clause: " clause) > n> p n
			   "#else /* !(" (s clause) ") */" n> p n
			   "#endif /* " (s clause)" */" n>
			   )
			 "#ifdef"
			 "Insert a #ifdef #else #endif statement"
			 'c-tempo-tags)

  (tempo-define-template "c-ifndef"
			 '("#ifndef " (p "ifndef-clause: " clause) > n 
			   "#define " (s clause) n> p n
			   "#endif /* " (s clause)" */" n>
			   )
			 "#ifndef"
			 "Insert a #ifndef #define #endif statement"
			 'c-tempo-tags)

;;; C-Mode Templates

  (tempo-define-template "c-if"
			 '(> "if(" (p "if-clause: " clause) ")"  n> 
			     "{" > n> r n 
			     "} /* end of if(" (s clause) ") */" > n> 
			     )
			 "if"
			 "Insert a C if statement"
			 'c-tempo-tags)

  (tempo-define-template "c-else"
			 '(> "else" n> 
			     "{" > n> r n 
			     "} /* end of else */" > n>
			     )
			 "else"
			 "Insert a C else statement"
			 'c-tempo-tags)

  (tempo-define-template "c-if-else"
			 '(> "if(" (p "if-clause: " clause) ")"  n> 
			     "{" > n> r n 
			     "} /* end of if(" (s clause) ") */" > n>
			     > "else" n> 
			     "{" > n> r n 
			     "} /* end of if(" (s clause) ")else */" > n> 
			     )
			 "ifelse"
			 "Insert a C if else statement"
			 'c-tempo-tags)

  (tempo-define-template "c-while"
			 '(> "while(" (p "while-clause: " clause) ")" >  n> 
			     "{" > n> r n 
			     "} /* end of while(" (s clause) ") */" > n>
			     )
			 "while"
			 "Insert a C while statement"
			 'c-tempo-tags)

  (tempo-define-template "c-for"
			 '(> "for(" (p "for-clause: " clause) ")" >  n> 
			     "{" > n> r n 
			     "} /* end of for(" (s clause) ") */" > n>
			     )
			 "for"
			 "Insert a C for statement"
			 'c-tempo-tags)

  (tempo-define-template "c-for-i"
			 '(> "for(" (p "variable: " var) " = 0; " (s var)
			     " < "(p "upper bound: " ub)"; " (s var) "++)" >  n> 
			     "{" > n> r n 
			     "} /* end of for(" (s var) " = 0; "
			     (s var) " < " (s ub) "; " (s var) "++) */" > n>
			     )
			 "fori"
			 "Insert a C for loop: for(x = 0; x < ..; x++)"
			 'c-tempo-tags)

  (tempo-define-template "c-for"
			 '(> "for(" (p "for-clause: " clause) ")" >  n> 
			     "{" > n> r n 
			     "} /* end of for(" (s clause) ") */" > n>
			     )
			 "for"
			 "Insert a C for statement"
			 'c-tempo-tags)

  (tempo-define-template "c-main"
			 '(> "main(int argc, char *argv[])" >  n> 
			     "{" > n> r n 
			     "} /* end of main() */" > n>
			     )
			 "main"
			 "Insert a C main statement"
			 'c-tempo-tags)

  (tempo-define-template "c-if-malloc"
			 '(> "if((" (p "variable: " var) " = ("
			     (p "type: " type) " *) malloc(sizeof(" (s type) 
			     "))) == (" (s type) " *) NULL)" n> 
			     "{" > n> r n 
			     "} /* end of if((" (s var) " = (" (s type) 
			     " *) malloc...) == NULL) */" > n>
			     )
			 "ifmalloc"
			 "Insert a C if(malloc...) statement"
			 'c-tempo-tags)

  (tempo-define-template "c-switch"
			 '(> "switch(" (p "switch-condition: " clause) ")" >  n> 
			     "{" > n 
			     "case " (p "first value: ") ":" > n> p n
			     "break;" > n> p n
			     "default:" > n> p n
			     "break;" > n
			     "} /* end of switch(" (s clause) ") */" > n>
			     )
			 "switch"
			 "Insert a C switch statement"
			 'c-tempo-tags)

  (tempo-define-template "c-case"
			 '(n "case " (p "value: ") ":" > n> p n
			     "break;" > n> p
			     )
			 "case"
			 "Insert a C case statement"
			 'c-tempo-tags)


;;;C++-Mode Templates


  (tempo-define-template "c++-class"
			 '("class " (p "classname: " class) p n "{" n "public:" n>

			   (s class) "();" 
			   (indent-for-comment) "the default constructor" n>

			   (s class) 
			   "(const " (s class) "&rhs);"
			   (indent-for-comment) "the copy constructor" n>

			   (s class)
			   "& operator=(const " (s class) "&rhs);"
			   (indent-for-comment) "the assignment operator" n>

			   n> "// the default address-of operators" n>
			   "// "(s class)
			   "* operator&()             { return this; };" n>
			   "// const "(s class)
			   "* operator&() const { return this; };" n


			   n > "~" (s class) "();"
			   (indent-for-comment) "the destructor" n n>
			   p n
			   "protected:" n> p n
			   "private:" n> p n
			   "};\t// end of class " (s class) n>
			   )
			 "class"
			 "Insert a class skeleton"
			 'c++-tempo-tags)
  )
(safe-init-func 'my-tempo-init-hook)

;;}}}
;;{{{ toolbar 

(defun my-toolbar-icons-init-hook ()
  (setq load-path 
	(cons "~/.xemacs/elisp/toolbar-custom" load-path))
  (load-file "~/.xemacs/elisp/toolbar-custom/my-toolbar.el")
  (safe-require 'mytoolbar)
  )
(safe-init-func 'my-toolbar-icons-init-hook)

;;}}}
;;{{{ psql-mode

;;;---------------------------------
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

;;}}}
;;{{{ python

(defun my-python-mode-init-hook ()
  (safe-require 'python-mode)
  (setq python-modes-package t)
  (setq py-imenu-show-method-args-p t)
  )
(safe-init-func 'my-python-mode-init-hook)
  
;;}}}
;;{{{ resize-minibuffer-mode

;;; ********************
;;; resize-minibuffer-mode makes the minibuffer automatically
;;; resize as necessary when it's too small to hold its contents.
(defun my-resize-minibuffer-mode-init-hook ()
  (when (fboundp 'resize-minibuffer-mode)
    (resize-minibuffer-mode)
    (setq resize-minibuffer-window-exactly nil)))
(safe-init-func 'my-resize-minibuffer-mode-init-hook)

;;}}}
;;{{{ save-history

(defun my-save-history-init-hook ()
  (safe-require 'savehist)
  (savehist-load))
(safe-init-func 'my-save-history-init-hook)

;;}}}
;;{{{ shell command

(require 'shell-command)

;;}}}
;;{{{ misc

;; Replace "yes or no" with y or n
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))

;;}}}
;;{{{ search-menu

(load-library "searchmenu")

;;}}}
;;{{{ view-less

;; If you would like all write-protected files to be visited in view-mode,
;; then add the following to your .emacs file:

(add-hook 'find-file-hooks 'auto-view-mode)

;;}}}
;;{{{ clearcase

;(unless (noninteractive)
;  (require 'clearcase))

;;}}}
;;{{{ file-confirm.el
(require 'file-confirm)
;;}}}
;;{{{ bbdb

(require 'bbdb)
(bbdb-initialize)

;;}}}
;;{{{ jka-compr Handle compressed files just like normal files

(require 'jka-compr)

;;}}}
;;{{{ recent-files

(defun my-recent-files-init-hook ()
  (safe-require 'recent-files)
  (recent-files-initialize))

(safe-init-func 'my-recent-files-init-hook)

;;}}}
;;{{{ session

;;; session
(defun my-session-init-hook ()
  (safe-require 'session)
  (add-hook 'after-init-hook 'session-initialize))
(safe-init-func 'my-session-init-hook)

;;}}}
;;{{{ shell-command completion
(defun my-shell-command-init-hook ()
  (safe-require 'shell-command))
(safe-init-func 'my-shell-command-init-hook)
;;}}}
;;{{{ sql configuration
(defun my-sql-init-hook ()
  (safe-require 'sql)
  (setq sql-user "postgres"))

(safe-init-func 'my-sql-init-hook)
 

;;}}}
;;{{{ tar-mode

(defun my-tar-mode-init-hook ()
  (setq auto-mode-alist (cons '("\\.tar$" . tar-mode) auto-mode-alist))
  (safe-require 'tar-mode))

;;}}}
;;{{{ ultratex

;(defun my-ultratex-mode-init-hook ()
;  (setq load-path 
;	(cons "~/.xemacs/elisp/ultratex-0.71/lisp" load-path))
;  (safe-require 'light)
;  (safe-require 'ultex-setup)
;  )
;(safe-init-func 'my-ultratex-mode-init-hook)

;;}}}
;;{{{ xrdb

(defun my-xrdb-mode-init-hook ()
  (when (not (eq system-type 'windows-nt) )
    (safe-require 'xrdb-mode)
    (setq auto-mode-alist
	  (append '(("\\.Xdefaults$"    . xrdb-mode)
		    ("\\.Xenvironment$" . xrdb-mode)
		    ("\\.Xresources$"   . xrdb-mode)
		    ("*.\\.ad$"         . xrdb-mode))
		  auto-mode-alist))
  ;;; very nice trick, that I have learned some time ago.
    (setq xrdb-master-file (expand-file-name "~/.xemacs/xdefaults.el"))
    (find-file-read-only xrdb-master-file)
    (xrdb-database-merge-buffer-or-region (point-min) (point-max))
    (kill-buffer (current-buffer))))
  (safe-init-func 'my-xrdb-mode-init-hook)

;;}}}
(blink-cursor-mode )
(display-time)
;;{{{ frames settings

;; These are my settings for 17 inch monitor running 1024 x 768
;; resolution. If you change font sizes, you'll probably have to tweak
;; this.
(set-frame-height (selected-frame) 35)
(set-frame-width (selected-frame) 80);120)

;;}}}

;;; Automatically turn on auto-fill-mode when editing text files
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;(setq display-time-day-and-date t)


(global-set-key [f12] 'bury-buffer)

(put 'overwrite-mode 'disabled t)
