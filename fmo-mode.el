;;; fmo-mode.el --- Format only changed lines

;; Author: Xichen Zhou <sichem.zh@gmail.com>
;; Copyright (C) 2023, Xichen Zhou, all rights reversed.
;; URL: https://github.com/xeechou/fmo-mode.el
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1") (difflib "0.3.7") (format-all "0.5.0"))
;; Keywords: languages util

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; format-all-mode format the entire buffer, which introduce lines that we do
;; not touch, it's often desirable to apply format-all-region to local changes.

;;; Code:


(require 'difflib)
(require 'format-all)

(defgroup fmo nil
  "Apply format-all-region at only at modified lines."
  :group 'fmo)

(defconst fmo-hunk-header
  "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@")

(defvar fmo-hook nil "Hook for fmo minor mode.")

(defcustom fmo-ensure-formatters t
  "When not-nil, adding format-all-ensure-formatters to the hook."
  :type 'boolean
  :group 'fmo)

(defun fmo--hunk-get-change (header)
  "Return a cons (start-line . line-length) from a hunk HEADER.

Like @@ -38,6 +38,8 @@ if match the header, otherwise return nil."
  (save-match-data			; is usually a good idea
    (if (string-match fmo-hunk-header header)
	(cons (string-to-number (match-string 3 header))
	      (string-to-number (match-string 4 header)))
	   nil)))

(defun fmo--hunk-to-beg-end (hunk)
  "Convenience function to return beg-line end-line from beg-line + offset HUNK."
  (let ((beg-line (car hunk))
	(line-len (cdr hunk)))
    (cons beg-line (+ beg-line line-len))))
;; (fmo--hunk-get-change " @@ -102 +101,0 @@ ")


(defun fmo-print-lines (beg end)
  "Printing line information giving (BEG END) position."
  (message (format "changed lines (%d %d)"
		   (line-number-at-pos beg)
		   (line-number-at-pos end))))

(defun fmo--buffer-content-simple ()
  "Get current buffer content as a string."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun fmo--string-starts-with (re string)
  "Return t if STRING start with RE regular expression."
  (eq (string-match-p re string) 0))

(defun fmo--buffer-total-lines ()
  "Get total number of lines in buffer."
  (count-lines (point-min) (point-max)))

(defun fmo--buffer-last-line ()
  "Get last line number of the buffer."
  (+ 1 (fmo--buffer-total-lines)))

(defun fmo--goto-line (ln)
  "Goto LN lines forward."
  (goto-char (point-min))
  (forward-line (1- ln)))

(defun fmo--pos-bol-at (ln)
  "Goto line at LN and return \"pos-bol\"."
  (save-excursion
    (fmo--goto-line ln)
    (pos-bol)))

(defun fmo--pos-eol-at (ln)
  "Goto line at LN and return \"pos-eol\"."
  (save-excursion
    (fmo--goto-line ln)
    (pos-eol)))

(defun fmo--pos-to-line (pos)
  "Return a line number given POS."
  (line-number-at-pos pos))

(defun fmo--lines-get-pos-region (beg-line end-line)
  "Get positions regions from BEG-LINE, END-LINE region in current buffer.

  Returns a pair of position."
  (cons (if (<= beg-line 1)
	    (point-min)
	  (fmo--pos-bol-at beg-line))
	(if (>= end-line (fmo--buffer-last-line))
	    (point-max)
	  (fmo--pos-eol-at end-line))))
;; (fmo--lines-get-pos 1 10) (point-max)

(defun fmo--get-offsetted-lines (beg-end offset)
  "Return the offset-ted (BEG-END) line set using OFFSET in position."
  (let* ((beg-line (car beg-end))
	 (end-line (cdr beg-end))
	 (pos-region (fmo--lines-get-pos-region beg-line end-line))
	 (pos-beg (car pos-region))
	 (pos-end (cdr pos-region)))
    (cons (fmo--pos-to-line (+ offset pos-beg))
	  (fmo--pos-to-line (+ offset pos-end)))))

;;TODO rely on diff instead of difflib
(defun fmo--buffer-diff-file ()
  "Return the diff of current buffer against the file on the system.

Use n 0 to get precise diff."
  (let ((modified-buffer (fmo--buffer-content-simple))
	(fname (buffer-file-name)))
    (with-temp-buffer
      (insert-file-contents fname)
      (difflib-unified-diff
       (s-split "\n" (fmo--buffer-content-simple)) ;;the file on disk
       (s-split "\n" modified-buffer)
       :fromfile "orignal"
       :tofile   "modified"
       ;;setting to 0 cause some errors
       :n 1))))

(defun fmo--buffer-get-diff-list-reversed ()
  "Return the diff hunk header as a list, in the reserved order.

   So when we apply the formatter, we start from the bottom, there
   is no need to track the change anymore"
  (let ((diff-res (fmo--buffer-diff-file))
	(diffset (list)))
    (progn
      (dolist (line diff-res)
	(when (and line (fmo--string-starts-with fmo-hunk-header line))
	  (add-to-list 'diffset line)))
      diffset)))

(defun fmo-format-lines (beg-end)
  "Run formatter given lines of (BEG-END)."
  (let* ((beg-line (car beg-end))
	 (end-line (cdr beg-end))
	 (region (fmo--lines-get-pos-region beg-line end-line))
	 (pos-beg (car region))
	 (pos-end (cdr region)))
    (format-all-region pos-beg pos-end)))

;;;###autoload
(defun fmo-format-changed-lines ()
  "Run format-all-region on modified lines in current buffer."
  (interactive)
  (let ((lines-changed (fmo--buffer-get-diff-list-reversed))
	(offset 0)) ;;initial offset is 0

    (dolist (hunk lines-changed)
      (let* ((line-beg-len (fmo--hunk-get-change hunk))
	     (line-beg-end (fmo--hunk-to-beg-end line-beg-len)))
	(fmo-format-lines line-beg-end)))))

(defun fmo-debug-format-changed ()
  "Run fmo-format-changed-lines and print lines we need to format.

Writes to *fmo-debug*"
  (interactive)
  (let ((lines-changed (fmo--buffer-get-diff-list-reversed))
	(offset 0)) ;;initial offset is 0

    (dolist (hunk lines-changed)
      (let* ((line-beg-len (fmo--hunk-get-change hunk))
	     (line-beg-end (fmo--hunk-to-beg-end line-beg-len)))
	(progn
	  (with-current-buffer "*fmo-debug*"
	    (insert (format "formatting lines (%d, %d)\n"
			    (car line-beg-end)
			    (cdr line-beg-end))))
	  (fmo-format-lines line-beg-end))))))

(defun fmo-debug-format-region ()
  "Debugging function to print formatting region."
  (interactive)
  (message (format "active region:(%d, %d)"
		   (region-beginning)
		   (region-end)))
  (format-all-region (region-beginning) (region-end)))


;;;###autoload
(define-minor-mode fmo-mode
  "Format modified only mode."
  :lighter " fmo"
  :group 'fmo
  (if fmo-mode
      (progn
	(when fmo-ensure-formatters (format-all-ensure-formatter))
	(add-hook 'before-save-hook 'fmo-format-changed-lines t t))
    (progn
      (remove-hook 'before-save-hook 'fmo-format-changed-lines t))))

(provide 'fmo-mode)

;;; fmo-mode.el ends here
