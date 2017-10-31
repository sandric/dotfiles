(defun sandric/company-backend-with-yas (backend)
  "Add yasnippet backend to company."
  (if (or (not company-mode/enable-yas)
          (and (listp backend)
               (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun sandric/company-space ()
  "Disable expanding for space in company."
  (interactive)
  (company-abort)
  (insert " "))


(use-package yasnippet
  :ensure t
  :config (progn
            (require 'yasnippet)
            (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
            (yas-global-mode 1)))

(use-package company
  :ensure t
  :config (progn
            (setq company-minimum-prefix-length 1)
            (setq company-tooltip-limit 20)

            (custom-set-variables
             '(company-idle-delay 0.1))

            (define-key company-active-map (kbd "<SPC>") 'sandric/company-space)
            (define-key company-active-map (kbd "<tab>") nil)

            (global-company-mode 1)))
