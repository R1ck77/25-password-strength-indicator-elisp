
(defconst psindicator--strength-to-assessment ["neither weak or strong" "very weak" "weak" "stong" "very strong"])

(defun psindicator--has-digits? (string)
  (string-match "[0-9]" string))

(defun psindicator--has-letters? (string)
  (string-match "[a-zA-Z]" string))

(defun psindicator--has-special? (string)
  (string-match "[^0-9a-zA-Z]" string))

(defun psindicator--is-long? (string)
  (>= (length string) 8))

(defun psindicator--very-weak-password? (password)
  (and (not (psindicator--is-long? password))
       (not (psindicator--has-letters? password))
       (not (psindicator--has-special? password))
       1))

(defun psindicator--weak-password? (password)
  (and (not (psindicator--is-long? password))
       (not (psindicator--has-digits? password))
       (not (psindicator--has-special? password))
       2))

(defun psindicator--strong-password? (password)
  (and (psindicator--is-long? password)
       (psindicator--has-digits? password)
       (psindicator--has-letters? password)
       (not (psindicator--has-special? password))
       3))

(defun psindicator--very-strong-password? (password)
    (and (psindicator--is-long? password)
       (psindicator--has-digits? password)
       (psindicator--has-letters? password)
       (psindicator--has-special? password)
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

(defun psindicator (password)
  (interactive "MEnter a password: ")
  (psindicator--string-result password
                              'psindicator-password-validator))

(provide 'psindicator)
