

(defconst psindicator--strength-to-assessment ["very weak" "weak" "stong" "very strong"])

(defun psindicator--very-weak-password? (password)
  (and (< (length password) 8)
       (string-match "^[0-9]*$" password)))

(defun psindicator--weak-password? (password)
  (and (< (length password) 8)
       (string-match "^[a-zA-Z]*$" password)))

(defun psindicator--strong-password? (password)
  (and (>= (length password) 8)
       (string-match "[a-zA-Z]." password)
       (string-match "[0-9]." password)))

(defun psindicator--very-strong-password? (password)
  (and (>= (length password) 8)
       (string-match "[a-zA-Z]." password)
       (string-match "[0-9]." password)
       (string-match "[^0-9a-zA-Z]." password)))

(defun psindicator-password-validator (password)
  )

(defun psindicator--string-result (password evaluation-function)
  (message "The password '%s' is %s"
           password
           (aref psindicator--strength-to-assessment (funcall evaluation-function password))))


(provide 'psindicator)
