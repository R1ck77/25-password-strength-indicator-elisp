

(defconst psindicator--strength-to-assessment ["very weak" "weak" "stong" "very strong"])

(defun psindicator-password-validator (password)
  )

(defun psindicator--string-result (password evaluation-function)
  (message "The password '%s' is %s"
           password
           (aref psindicator--strength-to-assessment (funcall evaluation-function password))))


(provide 'psindicator)
