(defconst psindicator--tests (list nil
                                   psindicator--very-weak-password?
                                   psindicator--weak-password?
                                   psindicator--strong-password?
                                   psindicator--very-strong-password?))

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

(provide 'psindicator-checks)
