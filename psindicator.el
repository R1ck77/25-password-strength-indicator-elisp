(require 'cl)
(require 'psindicator-dsl)
(require 'psindicator-format)
(require 'psindicator-checks)

(defun psindicator--test-password-with-default-rules (password)
  (psindicator--test-password password (psindicator--get-evaluators psindicator--tests)))

(defun psindicator (password)
  (interactive "MEnter a password: ")
  (psindicator--string-result password
                              'psindicator--test-password-with-default-rules))

(provide 'psindicator)
