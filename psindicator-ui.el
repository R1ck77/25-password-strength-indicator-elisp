(require 'widget)
(require 'psindicator)

(defun psindicator--password-changed (widget &optional source event)
  (psindicator (widget-value widget)))

(defun psindicator--insert-widgets ()
  (widget-create 'editable-field
                 :size 13
                 :secret ?*
                 :notify 'psindicator--password-changed
                 :format "Password: %v\n")
  (widget-setup))

(defun psindicator-ui ()
  (interactive)
  (switch-to-buffer "*Password Strength Indicator*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (psindicator--insert-widgets))

(provide 'psindicator-ui)
