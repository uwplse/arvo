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

(defvar arvo-mode-hook
  '(prettify-symbols-mode))

(defvar arvo-mode-map
  (let ((map (make-keymap)))
    ;;(define-key map "\C-j" 'newline-and-typecheck)
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
    ("\\\\" "\\<\\w+\\>" (if (position-of-string ".")
                             (min (position-of-string ".")
                                  (or (position-of-string ":")
                                      (position-of-string ".")))
                           nil)
 nil (0 font-lock-variable-name-face))
    ("([^:()]+:" "[^:]\\(\\<\\w+\\>\\)" (progn (search-backward "(") (+ 1 (position-of-string ":"))) nil (1 font-lock-variable-name-face))
))

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

(defun arvo-get-type-of-hole ()
  (interactive)
  (call-process (concat (getenv "ARVO_HOME") "/get-type-of-hole.sh")
                nil
                t
                nil
                (buffer-file-name)))

(defun arvo-insert-admit-for-hole-at-point ()
  (interactive)
  (if (not (eq (char-after (point)) ??))
      (error "Point is not on hole.")
    (progn
      (delete-char 1)
      (insert "(admit (")
      (arvo-get-type-of-hole)
      (insert "))"))))


(provide 'arvo-mode)
