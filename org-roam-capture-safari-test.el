(require 'org-roam-capture-safari)

(ert-deftest remove-trailing-test ()
  (let ((url "[foo][bar]]"))
    (should (string= (org-roam-capture-safari-remove-trailings url)  "bar]]"))))
