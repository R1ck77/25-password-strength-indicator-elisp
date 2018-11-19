(require 'cl)

(defconst psindicator--very-weak-password? '(short (no letters) (no specials) digits))
(defconst psindicator--weak-password? '(short (no digits) (no specials) letters))
(defconst psindicator--strong-password? '(long digits letters (no specials)))
(defconst psindicator--very-strong-password? '(long digits letters specials))

(defconst psindicator--tests (list nil
                                   psindicator--very-weak-password?
                                   psindicator--weak-password?
                                   psindicator--strong-password?
                                   psindicator--very-strong-password?))


(defconst psindicator--strength-to-assessment ["neither weak or strong" "very weak" "weak" "strong" "very strong"])

(defun psindicator--has-digits? (string)
  (string-match "[0-9]" string))

(defun psindicator--has-letters? (string)
  (string-match "[a-zA-Z]" string))

(defun psindicator--has-specials? (string)
  (string-match "[^0-9a-zA-Z]" string))

(defun psindicator--is-long? (string)
  (>= (length string) 8))

(defconst psindicator--rule-function (list
                                      (cons 'short (lambda (x) (not (psindicator--is-long? x))))
                                      (cons 'long  'psindicator--is-long?)
                                      (cons 'no 'not)
                                      (cons 'digits 'psindicator--has-digits?)
                                      (cons 'letters 'psindicator--has-letters?)
                                      (cons 'specials 'psindicator--has-specials?)))

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
  "Take a list and return a new one with each item numbered as ((x0 . 0) (x1 . 1)) etc"
  (let ((counter -1))
    (seq-map (lambda (rule)
               (setq counter (+ counter 1))
               (cons rule counter))
             list)))

(defun psindicator--get-evaluators (rules)
  "Take a list of rules, and return a numbered list of evaluation functions"
  (reverse
   (psindicator--enumerate
    (seq-map 'psindicator--interpret-rule rules))))

(defun psindicator--check-indexed-rule (password rule-index)
  "Return the index of the rule if the indexed rule matches the password"
  (when (funcall (car rule-index) password)
    (cdr rule-index)))

(defun psindicator--get-first-matching-index (password rule-index-pairs)
  "Return the index of the first rule passing the test

If no rule passes the test, issue an error"
  (let ((result nil)
        (pairs rule-index-pairs))
    (while (and (not result) pairs)
      (setq result (psindicator--check-indexed-rule password (car pairs)))
      (setq pairs (cdr pairs)))
    (when (not result)
      (error "Rules are missing a final \"catch all\" function!"))
    result))

(defun psindicator--test-password (password evaluator-score-pairs)
  (psindicator--get-first-matching-index password evaluator-score-pairs))

(defun psindicator--test-password-with-default-rules (password)
  (psindicator--test-password password (psindicator--get-evaluators psindicator--tests)))

(defun psindicator--string-result (password evaluation-function)
  (message "The password '%s' is %s"
           password
           (aref psindicator--strength-to-assessment (funcall evaluation-function password))))

(defun psindicator (password)
  (interactive "MEnter a password: ")
  (psindicator--string-result password
                              'psindicator--test-password-with-default-rules))

(provide 'psindicator)
