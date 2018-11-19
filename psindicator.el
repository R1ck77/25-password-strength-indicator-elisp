
(defconst psindicator--strength-to-assessment ["neither weak or strong" "very weak" "weak" "strong" "very strong"])

(defconst psindicator--tests '(psindicator--very-strong-password?
                               psindicator--strong-password?
                               psindicator--weak-password?
                               psindicator--very-weak-password?
                               psindicator--undecided-password?))

(defun psindicator--has-digits? (string)
  (string-match "[0-9]" string))

(defun psindicator--has-letters? (string)
  (string-match "[a-zA-Z]" string))

(defun psindicator--has-special? (string)
  (string-match "[^0-9a-zA-Z]" string))

(defun psindicator--is-long? (string)
  (>= (length string) 8))

(defconst psindicator--very-weak-password? '(short (no letters) (no special)))
(defconst psindicator--weak-password? '(short (no digits) (no special)))
(defconst psindicator--strong-password? '(long digits letters (no special)))
(defconst psindicator--very-strong-password? '(long digits letters specials))

(defconst psindicator--rule-function (list
                                      (cons 'short (lambda (x) (not (psindicator--is-long? x))))
                                      (cons 'long  'psindicator--is-long?)
                                      (cons 'no 'not)
                                      (cons 'digits 'psindicator--has-digits?)
                                      (cons 'letters 'psindicator--has-letters?)
                                      (cons 'special 'psindicator--has-special?)))

(defun psindicator--condition-symbols-to-functions (condition-or-conditions)
  "A single condition or a list of conditions enter, a list of functions in application order exits"
  (let ((rules (if (listp condition-or-conditions)
                   condition-or-conditions
                 (list condition-or-conditions))))
    (reverse
     (seq-map (lambda (rule-symbol)
                (alist-get rule-symbol psindicator--rule-function))
              rules))))

(defun psindicator--interpret-conditions (condition-or-conditions)
  "Receives a list of condition symbols, return a single function"
  (lexical-let ((conditions (psindicator--condition-symbols-to-functions condition-or-conditions)))
    (lambda (password)
      (let ((result password))
        (dolist (f conditions result)
          (setq result (funcall f result)))))))

(defun psindicator--interpret-rule (rule)
  "Receives a list of conditions (or lists of conditions) and returns a validator function"
  (lexical-let ((condition-functions (seq-map 'psindicator--interpret-conditions
                                              rule)))
    (lambda (password)
      (let ((result t))
        (dolist (f condition-functions result)
          (setq result (and result (funcall f password))))))))

(defun psindicator--test-password (password rules-list)
  ;;; TODO/FIXME read the rules in order, check the password, go with the flow
  )

(defun psindicator--create-validator (rules)
  (lambda (x)
    (let ((result t))
     (dolist (rule )))
    
))

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

(defun psindicator--undecided-password? (password)
  0)

(defun psindicator-password-validator (password)
  (let ((tests psindicator--tests)
        (result nil))
    (while (null result)
      (setq result (funcall (car tests) password))
      (setq tests (cdr tests)))
    result))

(defun psindicator--string-result (password evaluation-function)
  (message "The password '%s' is %s"
           password
           (aref psindicator--strength-to-assessment (funcall evaluation-function password))))

(defun psindicator (password)
  (interactive "MEnter a password: ")
  (psindicator--string-result password
                              'psindicator-password-validator))

(provide 'psindicator)
