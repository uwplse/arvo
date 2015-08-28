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
    ("\\\\" "\\<\\w+\\>" (let ((pd (position-of-string ".")))
                           (if pd
                               (min pd (or (position-of-string ":") pd))
                             nil))
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
  (let ((process (arvo-start-process (current-buffer))) out)
    ;(set-process-filter process (lambda (process output) (setf out (arvo-parse-output output))))
    (process-send-string process (concat s "\n"))
    (accept-process-output process 0.01)
    out))

(defun arvo-send-range (beginning end)
  (arvo-send-string (buffer-substring-no-properties beginning end)))

(defun arvo-send-region ()
  (interactive)
  (arvo-send-range (region-beginning) (region-end)))


(defun arvo-send-command-under-point ()
  (interactive)
  (save-excursion
    (let ((start (search-backward "def"))
          (end (cl-do ((i (point) (+ i 1))
                       (opens 0 (cond ((equal (char-after i) ?.) (- opens 1))
                                      ((equal (char-after i) ?\\) (+ opens 1))
                                      (t opens))))
                   ((and (equal (char-after i) ?.)
                         (equal opens 0))
                    (+ i 1)))))
      (arvo-send-range start end))))



(provide 'arvo-mode)
