(defun choose-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "urban.yoga.journeys@gmail.com" from) "urban")
               ((string-match "momo.reina@gmail.com" from) "momo")
               ((string-match "mo.opensource@gmail.com" from) "opensource")
               ((string-match "mozart@mozartreina.com" from) "mozart")
               ((string-match "info@yoga-momo.com" from) "yogamomo")))
             (account-vars (cdr (assoc account mo-mu4e-account-alist))))
          (mapc #'(lambda (var)
                    (set (car var) (cadr var)))
                account-vars)
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(defun mo-mu4e-set-account () 
  "Set the account for composing a message." 
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-msg-field mu4e-compose-parent-message :maildir))) 
                (string-match "/\\(.*?\\)/" maildir) 
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) " 
                                     (mapconcat #'(lambda (var) (car var)) mo-mu4e-account-alist "/")) 
                             (mapcar #'(lambda (var) (car var)) mo-mu4e-account-alist) 
                             nil t nil nil (caar mo-mu4e-account-alist))))
         (account-vars (cdr (assoc account mo-mu4e-account-alist))))
    (if account-vars 
        (mapc #'(lambda (var) 
                  (set (car var) (cadr var))) 
              account-vars))))
