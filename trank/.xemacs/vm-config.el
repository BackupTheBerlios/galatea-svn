;; -*- Mode: Emacs-Lisp -*-

;  Each sublist specifies three entities, a folder, a
;  spool file and a crash box. 
(setq vm-spool-files
      '(;;("~/mail/INBOX.t2"      "imap:t2.technion.ac.il:143:inbox:login:spavlov:*"      "~/mail/INBOX.t2.CRASH")
	("~/mail/INBOX.comnet"  "imap:comnet.technion.ac.il:143:inbox:login:billy:*"    "~/mail/INBOX.comnet.CRASH")
	("~/mail/INBOX.local"   "/var/spool/mail/michael"    "~/mail/INBOX.local.CRASH")))
(setq vm-primary-inbox "~/mail/INBOX.local")
  ;;; check mail every 60 seconds
(setq vm-mail-check-interval 60)
(setq vm-imap-auto-expunge-alist
      '(;; leave message on the server
	("imap:comnet.technion.ac.il:143:inbox:login:billy:*" . nil)
	;; expunge immediately
	;;("imap:hilo.harkie.org:143:inbox:login:kyle:*" . t)
	))

(setq vm-preview-lines 5)
(setq vm-auto-displayed-mime-content-types '("text" "image/jpeg"))
(setq vm-auto-displayed-mime-content-type-exceptions '("text/html"))
(setq vm-toolbar-orientation 'top)
(setq vm-summary-highlight-face 'bold-italic)

;* mail-citation-hook:           Hooks.                  255.
;* mail-yank-hooks:              Hooks.                  239.
;* vm-arrived-message-hook:      Hooks.                  35.
;* vm-arrived-messages-hook:     Hooks.                  50.
;* vm-auto-center-summary:       Summaries.              5.
;* vm-auto-decode-mime-messages: Reading MIME Messages.  42.
;* vm-auto-displayed-mime-content-type-exceptions: Reading MIME Messages.  97.
;* vm-auto-displayed-mime-content-types: Reading MIME Messages.  51.
;* vm-auto-displayed-mime-content-types: Reading MIME Messages.  83.
;* vm-auto-folder-alist:         Saving Messages.        32.
;* vm-auto-folder-case-fold-search: Saving Messages.     60.
;* vm-auto-get-new-mail:         Getting New Mail.       10.
;* vm-auto-get-new-mail:         Starting Up.            13.
;* vm-auto-next-message:         Paging.                 5.
;* vm-berkeley-mail-compatibility: Saving Messages.      113.
;* vm-circular-folders:          Selecting Messages.     22.
;* vm-confirm-new-folders:       Saving Messages.        22.
;* vm-confirm-quit:              Introduction.           83.
;* vm-crash-box:                 Starting Up.            20.
;* vm-crash-box-suffix:          Spool Files.            69.
;* vm-delete-after-archiving:    Saving Messages.        82.
;* vm-delete-after-saving:       Saving Messages.        82.
;* vm-delete-empty-folders:      Introduction.           72.
;* vm-digest-center-preamble:    Forwarding Messages.    28.
;* vm-digest-preamble-format:    Forwarding Messages.    28.
;* vm-digest-send-type:          Forwarding Messages.    17.
;* vm-display-buffer-hook:       Hooks.                  187.
;* vm-display-using-mime:        Reading MIME Messages.  5.
;* vm-edit-message-hook:         Hooks.                  128.
;* vm-fill-paragraphs-containing-long-lines: Paging.     25.
;* vm-flush-interval:            Message Attributes.     64.
;* vm-folder-directory:          Saving Messages.        26.
;* vm-folder-file-precious-flag: Introduction.           64.
;* vm-follow-summary-cursor:     Selecting Messages.     39.
;* vm-follow-summary-cursor:     Summaries.              137.
;* vm-forwarding-digest-type:    Forwarding Messages.    8.
;* vm-forwarding-subject-format: Forwarding Messages.    8.
;* vm-forward-message-hook:      Hooks.                  67.
;* vm-frame-parameter-alist:     Frame Configuration.    50.
;* vm-frame-per-completion:      Frame Configuration.    32.
;* vm-frame-per-composition:     Frame Configuration.    18.
;* vm-frame-per-edit:            Frame Configuration.    25.
;* vm-frame-per-folder:          Frame Configuration.    8.
;* vm-frame-per-help:            Frame Configuration.    29.
;* vm-frame-per-summary:         Frame Configuration.    12.
;* vm-gargle-uucp:               Summaries.              144.
;* vm-highlighted-header-face:   Faces.                  23.
;* vm-highlighted-header-face:   Previewing.             48.
;* vm-highlighted-header-regexp: Faces.                  23.
;* vm-highlighted-header-regexp: Previewing.             48.
;* vm-highlight-url-face:        Faces.                  31.
;* vm-iconify-frame-hook:        Hooks.                  210.
;* vm-imap-auto-expunge-alist:   IMAP Spool Files.       93.
;* vm-imap-bytes-per-session:    IMAP Spool Files.       76.
;* vm-imap-expunge-after-retrieving: IMAP Spool Files.   85.
;* vm-imap-max-message-size:     IMAP Spool Files.       66.
;* vm-imap-messages-per-session: IMAP Spool Files.       76.
;* vm-included-text-attribution-format: Replying.        24.
;* vm-included-text-prefix:      Replying.               17.
;* vm-infer-mime-types:          Reading MIME Messages.  399.
;* vm-init-file:                 Starting Up.            5.
;* vm-in-reply-to-format:        Replying.               32.
;* vm-invisible-header-regexp:   Previewing.             29.
;* vm-jump-to-new-messages:      Selecting Messages.     50.
;* vm-jump-to-unread-messages:   Selecting Messages.     50.
;* vm-mail-check-interval:       Getting New Mail.       16.
;* vm-mail-hook:                 Hooks.                  95.
;* vm-mail-mode-hook:            Hooks.                  134.
;* vm-mail-send-hook:            Hooks.                  232.
;* vm-make-crash-box-name:       Spool Files.            93.
;* vm-make-spool-file-name:      Spool Files.            93.
;* vm-menu-setup-hook:           Hooks.                  215.
;* vm-mime-7bit-composition-charset: MIME Composition.   60.
;* vm-mime-8bit-composition-charset: MIME Composition.   68.
;* vm-mime-8bit-text-transfer-encoding: MIME Composition.  83.
;* vm-mime-alternative-select-method: Reading MIME Messages.  389.
;* vm-mime-attachment-auto-suffix-alist: Reading MIME Messages.  213.
;* vm-mime-base64-decoder-program: Reading MIME Messages.  15.
;* vm-mime-base64-decoder-switches: Reading MIME Messages.  15.
;* vm-mime-base64-encoder-program: Reading MIME Messages.  15.
;* vm-mime-base64-encoder-switches: Reading MIME Messages.  15.
;* vm-mime-button-face:          Faces.                  40.
;* vm-mime-charset-converter-alist: Reading MIME Messages.  305.
;* vm-mime-charset-font-alist:   Reading MIME Messages.  344.
;* vm-mime-confirm-delete:       Reading MIME Messages.  69.
;* vm-mime-decode-for-preview:   Reading MIME Messages.  42.
;* vm-mime-default-face-charsets: Reading MIME Messages.  288.
;* vm-mime-display-function:     Hooks.                  220.
;* vm-mime-external-content-type-exceptions: Reading MIME Messages.  202.
;* vm-mime-external-content-types-alist: Reading MIME Messages.  134.
;* vm-mime-internal-content-type-exceptions: Reading MIME Messages.  130.
;* vm-mime-internal-content-types: Reading MIME Messages.  112.
;* vm-mime-qp-decoder-program:   Reading MIME Messages.  15.
;* vm-mime-qp-decoder-switches:  Reading MIME Messages.  15.
;* vm-mime-qp-encoder-program:   Reading MIME Messages.  15.
;* vm-mime-qp-encoder-switches:  Reading MIME Messages.  15.
;* vm-mime-type-converter-alist: Reading MIME Messages.  247.
;* vm-mime-uuencode-decoder-program: Reading MIME Messages.  15.
;* vm-mime-uuencode-decoder-switches: Reading MIME Messages.  15.
;* vm-mode-hook:                 Hooks.                  140.
;* vm-mode-hooks:                Hooks.                  146.
;* vm-mouse-track-summary:       Faces.                  19.
;* vm-move-after-deleting:       Deleting Messages.      26.
;* vm-move-after-killing:        Deleting Messages.      26.
;* vm-move-after-undeleting:     Deleting Messages.      26.
;* vm-movemail-program:          Spool Files.            14.
;* vm-movemail-program-switches: Spool Files.            14.
;* vm-move-messages-physically:  Sorting Messages.       5.
;* vm-mutable-frames:            Frames and Windows.     12.
;* vm-mutable-windows:           Frames and Windows.     17.
;* vm-paragraph-fill-column:     Paging.                 25.
;* vm-pop-auto-expunge-alist:    POP Spool Files.        77.
;* vm-pop-bytes-per-session:     POP Spool Files.        61.
;* vm-pop-expunge-after-retrieving: POP Spool Files.     69.
;* vm-pop-folder-alist:          POP Folders.            23.
;* vm-pop-max-message-size:      POP Spool Files.        51.
;* vm-pop-md5-program:           POP Spool Files.        39.
;* vm-pop-messages-per-session:  POP Spool Files.        61.
;* vm-popup-menu-on-mouse-3:     Menus.                  5.
;* vm-presentation-mode-hook:    Hooks.                  168.
;* vm-preview-lines:             Previewing.             16.
;* vm-preview-read-messages:     Previewing.             56.
;* vm-primary-inbox:             Starting Up.            13.
;* vm-quit-hook:                 Hooks.                  176.
;* vm-reply-hook:                Hooks.                  60.
;* vm-reply-ignored-addresses:   Replying.               67.
;* vm-reply-subject-prefix:      Replying.               5.
;* vm-resend-bounced-message-hook: Hooks.                74.
;* vm-resend-message-hook:       Hooks.                  81.
;* vm-retrieved-spooled-mail-hook: Hooks.                120.
;* vm-search-other-frames:       Frame Configuration.    42.
;* vm-search-using-regexps:      Selecting Messages.     79.
;* vm-select-message-hook:       Hooks.                  28.
;* vm-select-new-message-hook:   Hooks.                  10.
;* vm-select-unread-message-hook: Hooks.                 20.
;* vm-send-digest-hook:          Hooks.                  88.
;* vm-send-using-mime:           MIME Composition.       5.
;* vm-send-using-mime:           Sending Messages.       31.
;* vm-skip-deleted-messages:     Selecting Messages.     5.
;* vm-skip-read-messages:        Selecting Messages.     5.
;* vm-spooled-mail-waiting-hook: Hooks.                  44.
;* vm-spool-files:               Spool Files.            4.
;* vm-spool-file-suffixes:       Spool Files.            69.
;* vm-startup-with-summary:      Starting Up.            54.
;* vm-strip-reply-headers:       Replying.               39.
;* vm-subject-ignored-prefix:    Sorting Messages.       25.
;* vm-subject-ignored-suffix:    Sorting Messages.       25.
;* vm-subject-significant-chars: Sorting Messages.       36.
;* vm-summary-arrow:             Summaries.              5.
;* vm-summary-format:            Summaries.              19.
;* vm-summary-mode-hook:         Hooks.                  151.
;* vm-summary-mode-hooks:        Hooks.                  157.
;* vm-summary-pointer-update-hook: Hooks.                181.
;* vm-summary-redo-hook:         Hooks.                  108.
;* vm-summary-thread-indent-level: Threading.            9.
;* vm-summary-uninteresting-senders: Summaries.          118.
;* vm-summary-uninteresting-senders-arrow: Summaries.    118.
;* vm-summary-update-hook:       Hooks.                  103.
;* vm-thread-using-subject:      Threading.              18.
;* vm-toolbar-orientation:       Toolbar.                106.
;* vm-toolbar-pixmap-directory:  Toolbar.                115.
;* vm-undisplay-buffer-hook:     Hooks.                  198.
;* vm-url-search-limit:          Faces.                  31.
;* vm-use-menus:                 Menus.                  11.
;* vm-use-toolbar:               Toolbar.                10.
;* vm-virtual-mode-hook:         Hooks.                  162.
;* vm-visible-headers:           Previewing.             24.
;* vm-visit-folder-hook:         Hooks.                  113.
;* vm-visit-when-saving:         Saving Messages.        67.
;* vm-window-configuration-file: Window Configuration.   51.




;(setq vm-mime-external-content-types-alist
;      '(("text/html" 	"netscape")
;	("image/gif" 	"xv")
;	("image/jpeg" 	"xv")
;	("video/mpeg" 	"mpeg_play")
;	("video" 	"xanim")))
;(setq vm-mime-attachment-auto-suffix-alist
;      '(
;	("image/jpeg"		.	".jpg")
;	("image/gif"		.	".gif")
;	("image/png"		.	".png")
;	("text"			.	".txt")
;	))
