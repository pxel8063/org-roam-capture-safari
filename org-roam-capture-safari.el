;;; org-roam-capture-safari.el --- Org capture helper functions -*- lexical-binding: t -*-

;; Copyright (C) 2025  pxel8063

;; Author: pxel8063 <pxel8063@gmail.com>
;; Version:    0.0.3
;; Keywords:   lisp
;; Package-Requires: ((emacs "29.1")(org-mac-link "1.9")(org-roam "20250111.252"))

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
;;  M-x org-roam-capture-safari-ref initiates org-roam-capture-open-ref process
;;  based on the Safari window.
;;
;;  The url and title will be set in org-roam-capture-ref-template.
;;  Ref template can be specified by the org-roam-capture-safari-ref-template
;;  variable, which can be set through the customize variable interface of
;;  Emacs.

;;; Code:
(require 'org-mac-link)
(require 'org-roam)
(require 'org-roam-protocol)

(defgroup org-roam-capture-safari nil
  "Custom variables of org-roam-capture-safari."
  :group 'org-roam)

(defcustom org-roam-capture-safari-ref-template "r"
  "String to specity `org-roam-capture-ref-templates'."
  :group 'org-roam-capture-safari
  :type 'string)

(defun org-roam-capture-safari-construct-info (url)
  "Construct info plist from URL."
  (string-match org-link-bracket-re url)
  (let ((ref (match-string 1 url))
	(title (match-string 2 url)))
    (list :ref ref
	  :title title
	  :template org-roam-capture-safari-ref-template
	  :body "")))

;;;###autoload
(defun org-roam-capture-safari-ref ()
  "Initiate `org-roam-capture-open-ref' process based on the Safari window.
This process uses `org-roam-capture-ref-templates'.
See `org-roam-capture-safari-ref-template' for customization."
  (interactive)
  (let* ((url (org-mac-link-safari-get-frontmost-url)) ;; url looks like "[[ref][title]]"
	 (info (org-roam-capture-safari-construct-info url)))
    (org-roam-protocol-open-ref info)
    (ignore-errors)))

;;;###autoload
(defun org-roam-capture-safari-orglink ()
  "Initiate `org-roam-capture-open-ref' process at the point.
The line at the point must have org link format.
This process uses `org-roam-capture-ref-templates'.
See `org-roam-capture-safari-ref-template' for customization."
  (interactive)
  (when (org-in-regexp org-link-bracket-re 1)
    (let* ((url (match-string 0))
	   (info (org-roam-capture-safari-construct-info url)))
	(org-roam-protocol-open-ref info))))

(defun org-roam-capture-safari-orglink-headline ()
  "Insert a link in Org Synatx into headline."
  (interactive)
  (let ((end-of-bracket (cdr (org-in-regexp org-link-bracket-re 1))))
    (when end-of-bracket
      (let* ((url (match-string 0))
	     (info (org-roam-capture-safari-construct-info url)))
	(goto-char end-of-bracket)
	(org-insert-heading)
	(insert (plist-get info :title))
	(newline)
	(insert (plist-get info :ref))))))

(provide 'org-roam-capture-safari)
;;; org-roam-capture-safari.el ends here
