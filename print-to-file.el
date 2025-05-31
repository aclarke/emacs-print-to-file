;;; print-to-file.el --- Print buffer to PDF or PostScript (1-up or 2-up)  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2025 Aaron Clarke

;; Author: Aaron Clarke
;; Description: Print Emacs buffers to PDF or PostScript with optional 2-up layout.
;; Version: 0.1

;;; Commentary:

;; This package provides interactive commands for printing the current
;; buffer to either PDF or PostScript files using `ps-print`, `psnup`,
;; and `ps2pdf`.
;;
;; PDF output commands:
;;
;; - `pdf-print-buffer-to-file-with-faces`           ; 1-up PDF with faces
;; - `pdf-print-buffer-to-file`                      ; 1-up PDF without faces
;; - `pdf-print-buffer-to-file-2up-with-faces`       ; 2-up PDF with faces
;; - `pdf-print-buffer-to-file-2up`                  ; 2-up PDF without faces
;;
;; Each PDF command:
;; - Converts the buffer to a `.ps` file via `ps-print`
;; - Optionally formats the `.ps` as 2-up via `psnup`
;; - Converts the result to `.pdf` via `ps2pdf`
;; - Prompts before overwriting any existing files
;; - Deletes intermediate `.ps` files after successful PDF creation
;;
;; PostScript-only versions are also available:
;;
;; - `ps-print-buffer-to-file-with-faces`              ; 1-up PS with faces
;; - `ps-print-buffer-to-file`                         ; 1-up PS without faces
;; - `ps-print-buffer-to-file-2up-with-faces`          ; 2-up PS with faces
;; - `ps-print-buffer-to-file-2up`                     ; 2-up PS without faces
;;
;; These generate `.ps` or `.2up.ps` files and do not delete them.
;;
;; Honors Emacs variables like `ps-print-color-p` and `ps-font-size`,
;; so you can control color output and font size using your Emacs config.
;; For 2-up layouts, consider setting a smaller font:
;;
;;   (setq ps-font-size 6.0)
;;
;; Output files use the buffer's filename as a base. For example, if
;; editing `hello_world.c`, the following files may be created:
;;
;; - `hello_world.c.ps`        ; for 1-up output
;; - `hello_world.c.2up.ps`    ; for 2-up output
;; - `hello_world.c.pdf`       ; for final 1-up PDF
;; - `hello_world.c.2up.pdf`   ; for final 2-up PDF
;;
;; Lexical binding enabled for performance; not required by current code.
;;
;; Note: You can generate PostScript output without this package by using a prefix argument:
;;   C-u M-x ps-print-buffer-with-faces
;; This prompts for a file and writes PostScript directly.

;;; Code:

(defmacro printfile--validate-and-names (&optional suffix)
  `(let* ((base (file-name-nondirectory buffer-file-name))
          (psfile (concat base ".ps"))
          (ps2upfile (concat base ".2up.ps"))
          (pdffile (concat base ,(if suffix (concat "." suffix ".pdf") ".pdf"))))
     (printfile--validate-buffer)
     (list base psfile ps2upfile pdffile)))

(defun printfile--confirm-no-overwrite (file)
  "Raise an error unless FILE does not exist or the user agrees to overwrite."
  (when (file-exists-p file)
    (unless (yes-or-no-p (format "File %s exists. Overwrite? " file))
      (user-error "Aborted by user"))))

(defun printfile--write-postscript (use-faces psfile)
  "Spool current buffer to PSFILE. USE-FACES determines color vs. plain."
  (when (get-buffer "*PostScript*")
    (kill-buffer "*PostScript*"))
  (if use-faces
      (ps-spool-buffer-with-faces)
    (ps-spool-buffer))
  (with-current-buffer "*PostScript*"
    (write-region (point-min) (point-max) psfile)
    (kill-buffer "*PostScript*")))

(defun printfile--convert-to-pdf (infile outfile)
  "Convert INFILE to OUTFILE using ps2pdf."
  (let ((cmd (format "ps2pdf %s %s"
                     (shell-quote-argument infile)
                     (shell-quote-argument outfile))))
    (message "Running: %s" cmd)
    (if (= (shell-command cmd) 0)
        (progn
          (delete-file infile)
          (message "Created PDF: %s" outfile))
      (message "PDF creation failed. See shell output."))))

(defun printfile--convert-to-2up (infile outfile)
  "Convert INFILE to 2-up OUTFILE using psnup."
  (let ((cmd (format "psnup -2 %s %s"
                     (shell-quote-argument infile)
                     (shell-quote-argument outfile))))
    (message "Running: %s" cmd)
    (if (= (shell-command cmd) 0)
        (progn
          (delete-file infile)
          t)
      (message "2-up conversion failed. See shell output.")
      nil)))

(defun printfile--validate-buffer ()
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file."))
  (when (string= (buffer-name) "*PostScript*")
    (user-error "Don't run this command from the *PostScript* buffer.")))

;;; PDF output commands

;;;###autoload
(defun pdf-print-buffer-to-file-with-faces ()
  "Generate a 1-up PDF with faces."
  (interactive)
  (cl-destructuring-bind (_ psfile _ pdffile) (printfile--validate-and-names nil)
    (dolist (f (list psfile pdffile)) (printfile--confirm-no-overwrite f))
    (printfile--write-postscript t psfile)
    (printfile--convert-to-pdf psfile pdffile)))

;;;###autoload
(defun pdf-print-buffer-to-file ()
  "Generate a 1-up PDF without faces."
  (interactive)
  (cl-destructuring-bind (_ psfile _ pdffile) (printfile--validate-and-names nil)
    (dolist (f (list psfile pdffile)) (printfile--confirm-no-overwrite f))
    (printfile--write-postscript nil psfile)
    (printfile--convert-to-pdf psfile pdffile)))

;;;###autoload
(defun pdf-print-buffer-to-file-2up-with-faces ()
  "Generate a 2-up PDF with faces."
  (interactive)
  (cl-destructuring-bind (_ psfile ps2upfile pdffile) (printfile--validate-and-names "2up")
    (dolist (f (list psfile ps2upfile pdffile)) (printfile--confirm-no-overwrite f))
    (printfile--write-postscript t psfile)
    (when (printfile--convert-to-2up psfile ps2upfile)
      (printfile--convert-to-pdf ps2upfile pdffile))))

;;;###autoload
(defun pdf-print-buffer-to-file-2up ()
  "Generate a 2-up PDF without faces."
  (interactive)
  (cl-destructuring-bind (_ psfile ps2upfile pdffile) (printfile--validate-and-names "2up")
    (dolist (f (list psfile ps2upfile pdffile)) (printfile--confirm-no-overwrite f))
    (printfile--write-postscript nil psfile)
    (when (printfile--convert-to-2up psfile ps2upfile)
      (printfile--convert-to-pdf ps2upfile pdffile))))

;;; PostScript output commands

;;;###autoload
(defun ps-print-buffer-to-file-with-faces ()
  "Generate a 1-up PostScript file with faces."
  (interactive)
  (printfile--validate-buffer)
  (let ((psfile (concat (file-name-nondirectory buffer-file-name) ".ps")))
    (printfile--confirm-no-overwrite psfile)
    (printfile--write-postscript t psfile)
    (message "Created PostScript file: %s" psfile)))

;;;###autoload
(defun ps-print-buffer-to-file ()
  "Generate a 1-up PostScript file without faces."
  (interactive)
  (printfile--validate-buffer)
  (let ((psfile (concat (file-name-nondirectory buffer-file-name) ".ps")))
    (printfile--confirm-no-overwrite psfile)
    (printfile--write-postscript nil psfile)
    (message "Created PostScript file: %s" psfile)))

;;;###autoload
(defun ps-print-buffer-to-file-2up-with-faces ()
  "Generate a 2-up PostScript file with faces."
  (interactive)
  (cl-destructuring-bind (_ psfile ps2upfile _) (printfile--validate-and-names "2up")
    (dolist (f (list psfile ps2upfile)) (printfile--confirm-no-overwrite f))
    (printfile--write-postscript t psfile)
    (when (printfile--convert-to-2up psfile ps2upfile)
      (message "Created 2-up PostScript file: %s" ps2upfile))))

;;;###autoload
(defun ps-print-buffer-to-file-2up ()
  "Generate a 2-up PostScript file without faces."
  (interactive)
  (cl-destructuring-bind (_ psfile ps2upfile _) (printfile--validate-and-names "2up")
    (dolist (f (list psfile ps2upfile)) (printfile--confirm-no-overwrite f))
    (printfile--write-postscript nil psfile)
    (when (printfile--convert-to-2up psfile ps2upfile)
      (message "Created 2-up PostScript file: %s" ps2upfile))))

(provide 'print-to-file)
;;; print-to-file.el ends here
