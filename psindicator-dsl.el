
(defconst psindicator--very-weak-password? '(short (no letters) (no specials) digits))
(defconst psindicator--weak-password? '(short (no digits) (no specials) letters))
(defconst psindicator--strong-password? '(long digits letters (no specials)))
(defconst psindicator--very-strong-password? '(long digits letters specials))

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

(provide 'psindicator-dsl)
