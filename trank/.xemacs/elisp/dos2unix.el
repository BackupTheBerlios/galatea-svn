;;; A interactive function for replacing all dos 
;;; carriage returns (^M) with Unix 
;;; line feeds in a selected buffer. 
(defun dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive "b buffer to convert" )
  (goto-char (point-min))
  (while (search-forward (string ?\C-m) nil t)
    (replace-match (string ?\C-j) nil t)))

;;; Usually that is not what you want, though, 
;;; because a DOS line end is \r\n, and 
;;; you just want \n for Unix, so you need to replace \r (^M) with nothing. 
;;; Manually: M-% C-q C-m RET RET. This also helps against really 
;;; corrupt text files 
;;; that have \r\r\n line ends. This sometimes happens with text files 
;;; uploaded and 
;;; downloaded from the web; 
;;; one such example are the source files in the ElispArea. 

;;; Note also that if the file is a well-formed DOS file, 
;;; then you can use C-x C-m f 
;;; (also C-x RET f). This runs set-buffer-file-coding-system; use "unix" or 
;;; "undecided-unix" as the new coding system. When you have a corrupt file as 
;;; described above, that does not help, however. 

