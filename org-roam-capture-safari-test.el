;;; org-roam-capture-safari-test.el --- test functions -*- lexical-binding: t -*-

;; Copyright (C) 2024  pxel8063

;; Author: pxel8063 <pxel8063@gmail.com>

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

;;; Code:
(require 'org-roam-capture-safari)

(ert-deftest remove-leading-brackets-test ()
  (let ((url "[foo][bar]]"))
    (should (string= (org-roam-capture-safari-remove-leading-brackets url)  "bar]]"))))

(ert-deftest extract-title-test ()
  (let ((url "[[foo][bar]]"))
    (should (string= (org-roam-capture-safari-extract-title url) "bar"))))

(ert-deftest construct-info-plist ()
  (let* ((ref "http://localhost/")
	 (title "title A")
	 (target (org-roam-capture-safari-construct-info ref title)))
    (should (string= (plist-get target ':ref) "http://localhost/"))
    (should (string= (plist-get target ':title) "title A"))))

(provide 'org-roam-capture-safari-test)
;;; org-roam-capture-safari-test.el ends here.
