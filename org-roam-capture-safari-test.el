;;; org-roam-capture-safari-test.el --- test functions -*- lexical-binding: t -*-
(require 'org-roam-capture-safari)

(ert-deftest remove-leading-brackets-test ()
  (let ((url "[foo][bar]]"))
    (should (string= (org-roam-capture-safari-remove-leading-brackets url)  "bar]]"))))

(ert-deftest extract-title-test ()
  (let ((url "[[foo][bar]]"))
    (should (string= (org-roam-capture-safari-extract-title url) "bar"))))
