;;; org-roam-capture-safari.el --- Org capture helper functions -*- lexical-binding: t -*-

;; Copyright (C) 2024  pxel8063

;; Author: pxel8063 <pxel8063@gmail.com>
;; Version:    0.0.1
;; Keywords:   lisp
;; Package-Requires: ((emacs "27.1")(org-mac-link "1.9")(org-roam "2.2.2"))

;; URL: https://github.com/pxel8063/org-roam-capture-safari

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; invoke this by /usr/local/bin/emacsclient --eval "(org-roam-capture-safari-ref)"

;;; Code:
(require 'org-mac-link)
(require 'org-roam)
(require 'org-roam-protocol)

(defun org-roam-capture-safari-remove-leading-brackets (url)
  (string-trim-left url "^.*]\\["))

;;;
(defun org-roam-capture-safari-ref ()
  "Call `org-roam-protocol-open-ref' based the front most Safari window.
Uses `org-mac-link-safari-get-frontmost-url' to capture url from Safari."
  (interactive)
  (let* ((url (org-mac-link-safari-get-frontmost-url)) ;; url looks like "[[ref][title]]"
	 (orglink (substring url 1 -1))  ;; orglink should be "[ref][title]"
	 (title  (let ((s (string-trim-left url "^.*]\\[")))
		   (string-trim-right s "\\]\\]"))))
    (when (string-match "\\[\\(.*\\)\\]\\[\\(.*\\)\\]" orglink)
      (let ((testinfo (list ':ref (match-string 1 orglink) ':template "r" ':title title ':body "")))
	(org-roam-protocol-open-ref testinfo)
	(ignore-errors)))))

(provide 'org-roam-capture-safari)
;;; org-roam-capture-safari.el ends here
