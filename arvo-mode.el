;;; arvo.el --- Major mode for Arvo

;; Copyright (C) 2011 Pavel Panchekha <me@pavpanchekha.com>
;; Copyright (C) 2015 James Wilcox <jrw12@cs.washington.edu>

;; Author: Pavel Panchekha <me@pavpanchekha.com>
;; Author: James Wilcox <jrw12@cs.washington.edu>
;; Version: 0.1.0
;; Keywords: arvo

;; Based on the blazeit emacs mode by Pavel Panchekha <me@pavpanchekha.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

(defgroup arvo nil
  "The Arvo theorem prover"
  :prefix "arvo-"
  :group 'languages)

(defcustom arvo-program "/usr/bin/arvo"
  "The path to the arvo executable"
  :group 'arvo
  :type 'string)


(defvar arvo-mode-hook
  '(prettify-symbols-mode))


(defvar arvo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'arvo-send-command-under-point)
    (define-key map (kbd "C-c RET") 'arvo-send-buffer-up-to-point)
    (define-key map (kbd "C-c C-t") 'arvo-type-of-term)
    (define-key map (kbd "C-c C-n") 'arvo-normalize-term)
    (define-key map (kbd "C-c C-p") 'arvo-print-term)
    map)
  "Keymap for Arvo major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.arvo\\'" . arvo-mode))

(defun position-of-string (s)
  (save-excursion (search-forward s (line-end-position) 't)))

(defconst arvo-font-lock-keywords
  '(("\\<\\(def\\|axiom\\|import\\|print\\|check\\|simpl\\|data\\)\\>" . font-lock-keyword-face)
    ("\\<Type\\>" . font-lock-type-face)
    ("\\<def\\>" "\\<\\(\\w+\\)\\>" (position-of-string ":") nil (1 font-lock-function-name-face))
    ("\\\\" "\\<\\w+\\>" (let ((pd (position-of-string "."))
                               (c (position-of-string ":")))
                           (if pd
                               (if c (min pd c)
                                 pd)
                             c))
     nil (0 font-lock-variable-name-face))
    ("([^:()]+:" "[^:]\\(\\<\\w+\\>\\)"
     (let ((p (point)))
       (search-backward "(")
       (+ 1 p))
     nil (1 font-lock-variable-name-face))))

(defconst arvo-pretty-symbols
  '(("->" . ?→) ("\\" . "λ") (":=" . "≜")))

(defvar arvo-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?' "w" st)
    st)
  "Syntax table for Arvo major mode")

(define-derived-mode arvo-mode prog-mode "Arvo"
  "Major mode for editing Arvo proof files"
  :syntax-table arvo-mode-syntax-table
  (set (make-local-variable 'prettify-symbols-alist) arvo-pretty-symbols)
  (set (make-local-variable 'font-lock-defaults) '(arvo-font-lock-keywords))
  (font-lock-fontify-buffer))

(defvar arvo-process nil)
(defvar arvo-process-buffer nil)
(defvar arvo-window nil)

(defun arvo-insertion-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      ;; Insert the text, advancing the process marker.
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point))
      (set-window-point arvo-window (point)))))

(defun arvo-start-process (buffer)
  (if (and (equal buffer arvo-process-buffer)
           (eq (process-status arvo-process) 'run))
      arvo-process
    (let* ((pbuffer (get-buffer-create "*arvo*"))
           (process (start-file-process "arvo" pbuffer arvo-program))
           (window (progn (delete-other-windows) (split-window-right))))
      (setf arvo-process process
            arvo-process-buffer buffer
            arvo-window window)
      (set-process-filter process 'arvo-insertion-filter)
      (set-window-buffer window pbuffer)
      process)))

; (defun arvo-parse-output (output) output)

(defun arvo-send-string (s)
  (let ((process (arvo-start-process (current-buffer))))
    (process-send-string process (concat s "\n"))
    (accept-process-output process 0.01)))

(defun arvo-send-string-and-capture-output (s)
  (let ((process (arvo-start-process (current-buffer))) out)
    (set-process-filter process (lambda (process output)
                                  (setf out output)
                                  (arvo-insertion-filter process output)))
    (process-send-string process (concat s "\n"))
    (accept-process-output process 1)
    (set-process-filter process 'arvo-insertion-filter)
    out))

(defun arvo-send-command-and-message (command arg)
  (message (arvo-send-string-and-capture-output (concat command " " s "."))))

(require 'thingatpt)

(defun arvo-type-of-term (s)
  (interactive
   (let ((w (thing-at-point 'word t)))
     (list (read-string (format "Term (default %s): " w) nil nil w))))
  (arvo-send-command-and-message "check" s))

(defun arvo-normalize-term (s)
  (interactive
   (let ((w (thing-at-point 'word t)))
     (list (read-string (format "Term (default %s): " w) nil nil w))))
  (arvo-send-command-and-message "simpl" s))

(defun arvo-print-term (s)
  (interactive
   (let ((w (thing-at-point 'word t)))
     (list (read-string (format "Term (default %s): " w) nil nil w))))
  (arvo-send-command-and-message "print" s))


(defun arvo-send-range (beginning end)
  (arvo-send-string (buffer-substring-no-properties beginning end)))

(defun arvo-send-region ()
  (interactive)
  (arvo-send-range (region-beginning) (region-end)))

(defun arvo-send-buffer ()
  (interactive)
  (arvo-send-range (point-min) (point-max)))

(defun arvo-send-buffer-up-to-point ()
  (interactive)
  (arvo-send-range (point-min) (point)))

(defun re-search-forward-backward (re)
  (when (re-search-forward re (point-max) 'no-error)
    (goto-char (match-beginning 0)))
  (re-search-backward re))

(defun arvo-find-command-under-point ()
  (let ((start (re-search-forward-backward "\\<\\(?:def\\|print\\|check\\|simpl\\|data\\|axiom\\|import\\)\\>"))
        (end (cl-do ((i (point) (+ i 1))
                     (opens 0 (cond ((equal (char-after i) ?.) (- opens 1))
                                    ((equal (char-after i) ?\\) (+ opens 1))
                                    (t opens))))
                 ((and (equal (char-after i) ?.)
                       (equal opens 0))
                  (+ i 1)))))
    (list start end)))

(defun arvo-parse-hole-type (string)
  (cl-loop for line in (split-string string "\n" t)
           until (string-match "Hole has type \\(.*\\)$" line)
           finally return (match-string 1 line)))

(defun arvo-get-type-of-first-hole-in-thing-under-point ()
  (interactive)
  (save-excursion
    (let* ((range (arvo-find-command-under-point))
           (out (arvo-send-string-and-capture-output
                 (buffer-substring (car range) (cadr range))))
           (ans (arvo-parse-hole-type out)))
      (kill-new ans)
      (message ans))))

(defun arvo-send-command-under-point ()
  (interactive)
  (save-excursion
    (let ((range (arvo-find-command-under-point)))
      (arvo-send-range (car range) (cadr range)))))



(provide 'arvo-mode)
