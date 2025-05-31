;;; ps-print-pdf.el --- Print buffer to PDF via PostScript (1-up or 2-up)  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2025 Aaron Clarke

;; Author: Aaron Clarke
;; Description: Print Emacs buffers to PDF with optional 2-up layout, using psnup and ps2pdf.
;; Version: 0.1

;;; Commentary:

;; Provides four interactive functions to print the current buffer to a PDF:
;;
;; - `ps-print-buffer-to-pdf`
;; - `ps-print-buffer-to-pdf-no-faces`
;; - `ps-print-buffer-to-pdf-2up`
;; - `ps-print-buffer-to-pdf-2up-no-faces`
;;
;; All functions prompt before overwriting files and clean up intermediate
;; PostScript files after successful PDF creation.
;;
;; Lexical binding enabled for performance; not required by current code.

;;; Code:

(defun pspdf--confirm-no-overwrite (file)
  "Raise an error unless FILE does not exist or the user agrees to overwrite."
  (when (file-exists-p file)
    (unless (yes-or-no-p (format "File %s exists. Overwrite? " file))
      (user-error "Aborted by user"))))

(defun pspdf--write-postscript (use-faces psfile)
  "Spool current buffer to PSFILE. USE-FACES determines color vs. plain."
  (when (get-buffer "*PostScript*")
    (kill-buffer "*PostScript*"))
  (if use-faces
      (ps-spool-buffer-with-faces)
    (ps-spool-buffer))
  (with-current-buffer "*PostScript*"
    (write-region (point-min) (point-max) psfile)
    (kill-buffer "*PostScript*")))

;;;###autoload
(defun ps-print-buffer-to-pdf ()
  "Print current buffer to a 1-up PDF with faces (color syntax highlighting)."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file."))
  (when (string= (buffer-name) "*PostScript*")
    (user-error "Don't run this command from the *PostScript* buffer."))

  (let* ((base (file-name-nondirectory buffer-file-name))
         (psfile (expand-file-name (concat base ".ps")))
         (pdffile (expand-file-name (concat base ".pdf"))))
    (dolist (f (list psfile pdffile)) (pspdf--confirm-no-overwrite f))
    (pspdf--write-postscript t psfile)
    (let ((cmd (format "ps2pdf %s %s"
                       (shell-quote-argument psfile)
                       (shell-quote-argument pdffile))))
      (message "Running: %s" cmd)
      (if (= (shell-command cmd) 0)
          (progn
            (delete-file psfile)
            (message "Created PDF: %s" pdffile))
        (message "PDF creation failed. See shell output.")))))

;;;###autoload
(defun ps-print-buffer-to-pdf-no-faces ()
  "Print current buffer to a 1-up PDF without faces (plain black and white)."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file."))
  (when (string= (buffer-name) "*PostScript*")
    (user-error "Don't run this command from the *PostScript* buffer."))

  (let* ((base (file-name-nondirectory buffer-file-name))
         (psfile (expand-file-name (concat base ".ps")))
         (pdffile (expand-file-name (concat base ".pdf"))))
    (dolist (f (list psfile pdffile)) (pspdf--confirm-no-overwrite f))
    (pspdf--write-postscript nil psfile)
    (let ((cmd (format "ps2pdf %s %s"
                       (shell-quote-argument psfile)
                       (shell-quote-argument pdffile))))
      (message "Running: %s" cmd)
      (if (= (shell-command cmd) 0)
          (progn
            (delete-file psfile)
            (message "Created PDF: %s" pdffile))
        (message "PDF creation failed. See shell output.")))))

;;;###autoload
(defun ps-print-buffer-to-pdf-2up ()
  "Print current buffer to a 2-up PDF with faces (color syntax highlighting)."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file."))
  (when (string= (buffer-name) "*PostScript*")
    (user-error "Don't run this command from the *PostScript* buffer."))

  (let* ((base (file-name-nondirectory buffer-file-name))
         (psfile (expand-file-name (concat base ".ps")))
         (ps2upfile (expand-file-name (concat base ".2up.ps")))
         (pdffile (expand-file-name (concat base ".2up.pdf"))))
    (dolist (f (list psfile ps2upfile pdffile)) (pspdf--confirm-no-overwrite f))
    (pspdf--write-postscript t psfile)
    (let ((cmd (format "psnup -2 %s %s && ps2pdf %s %s"
                       (shell-quote-argument psfile)
                       (shell-quote-argument ps2upfile)
                       (shell-quote-argument ps2upfile)
                       (shell-quote-argument pdffile))))
      (message "Running: %s" cmd)
      (if (= (shell-command cmd) 0)
          (progn
            (delete-file psfile)
            (delete-file ps2upfile)
            (message "Created 2-up PDF: %s" pdffile))
        (message "Failed to create 2-up PDF. See shell output.")))))

;;;###autoload
(defun ps-print-buffer-to-pdf-2up-no-faces ()
  "Print current buffer to a 2-up PDF without faces (plain black and white)."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file."))
  (when (string= (buffer-name) "*PostScript*")
    (user-error "Don't run this command from the *PostScript* buffer."))

  (let* ((base (file-name-nondirectory buffer-file-name))
         (psfile (expand-file-name (concat base ".ps")))
         (ps2upfile (expand-file-name (concat base ".2up.ps")))
         (pdffile (expand-file-name (concat base ".2up.pdf"))))
    (dolist (f (list psfile ps2upfile pdffile)) (pspdf--confirm-no-overwrite f))
    (pspdf--write-postscript nil psfile)
    (let ((cmd (format "psnup -2 %s %s && ps2pdf %s %s"
                       (shell-quote-argument psfile)
                       (shell-quote-argument ps2upfile)
                       (shell-quote-argument ps2upfile)
                       (shell-quote-argument pdffile))))
      (message "Running: %s" cmd)
      (if (= (shell-command cmd) 0)
          (progn
            (delete-file psfile)
            (delete-file ps2upfile)
            (message "Created 2-up PDF: %s" pdffile))
        (message "Failed to create 2-up PDF. See shell output.")))))

(provide 'ps-print-pdf)
;;; ps-print-pdf.el ends here
