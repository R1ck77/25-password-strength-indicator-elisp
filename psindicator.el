
(defconst psindicator--strength-to-assessment ["neither weak or strong" "very weak" "weak" "strong" "very strong"])

(defun psindicator--has-digits? (string)
  (string-match "[0-9]" string))

(defun psindicator--has-letters? (string)
  (string-match "[a-zA-Z]" string))

(defun psindicator--has-special? (string)
  (string-match "[^0-9a-zA-Z]" string))

(defun psindicator--is-long? (string)
  (>= (length string) 8))

(defconst psindicator--very-weak-password? '(short (no letters) (no special) digits))
(defconst psindicator--weak-password? '(short (no digits) (no special) letters))
(defconst psindicator--strong-password? '(long digits letters (no special)))
(defconst psindicator--very-strong-password? '(long digits letters specials))

(defconst psindicator--tests (list nil
                                   psindicator--very-weak-password?
                                   psindicator--weak-password?
                                   psindicator--strong-password?
                                   psindicator--very-strong-password?))

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

(defun psindicator--enumerate (list)
  (let ((counter -1))
    (seq-map (lambda (rule)
               (setq counter (+ counter 1))
               (cons rule counter))
             list)))

(defun psindicator--get-evaluators (rules)
  (reverse
   (psindicator--enumerate
    (seq-map 'psindicator--interpret-rule rules))))

(defun psindicator--test-password (password ordered-rules-list)
  ;;; TODO/FIXME read the rules in order, check the password, go with the flow
  (let ((result)
        (tests ordered-rules-list))
    (while (not result)
      (let ((rule (car tests)))
        (setq tests (car rule))
        (if (funcall (psindicator--interpret-rule (cdr rule)) password)
            (setq result (car rule)))))
    result))

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
