
(defconst psindicator--strength-to-assessment ["neither weak or strong" "very weak" "weak" "stong" "very strong"])

(defun psindicator--very-weak-password? (password)
  (and (< (length password) 8)
       (string-match "^[0-9]*$" password)
       1))

(defun psindicator--weak-password? (password)
  (and (< (length password) 8)
       (string-match "^[a-zA-Z]*$" password)
       2))

(defun psindicator--strong-password? (password)
  (and (>= (length password) 8)
       (string-match "[a-zA-Z]" password)
       (string-match "[0-9]" password)
       3))

(defun psindicator--very-strong-password? (password)
  (and (>= (length password) 8)
       (string-match "[a-zA-Z]" password)
       (string-match "[0-9]" password)
       (string-match "[^0-9a-zA-Z]" password)
       4))

(defun psindicator-password-validator (password)
  (or (psindicator--very-strong-password? password)
      (psindicator--strong-password? password)
      (psindicator--weak-password? password)
      (psindicator--very-weak-password? password)
      0))

(defun psindicator--string-result (password evaluation-function)
  (message "The password '%s' is %s"
           password
           (aref psindicator--strength-to-assessment (funcall evaluation-function password))))

(provide 'psindicator)
