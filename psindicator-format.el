(defconst psindicator--strength-to-assessment ["neither weak or strong" "very weak" "weak" "strong" "very strong"])

(defun psindicator--string-result (password evaluation-function)
  (message "The password '%s' is %s"
           password
           (aref psindicator--strength-to-assessment (funcall evaluation-function password))))

(provide 'psindicator-format)
