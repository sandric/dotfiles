(defun sandric/company-space ()
  "Disable expanding for space in company."
  (interactive)
  (company-abort)
  (insert " "))

(use-package yasnippet
  :after (company)
  :ensure t
  :config (progn
            (require 'yasnippet)
            (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
            (yas-global-mode 1)))

(use-package company
  :ensure t
  :config (progn
            (setq company-minimum-prefix-length 3)
            (setq company-tooltip-limit 20)

            (custom-set-variables
             '(company-idle-delay 0.1))

            (define-key company-active-map (kbd "<SPC>") 'sandric/company-space)
            (define-key company-active-map (kbd "<tab>") nil)
            
            (add-to-list 'company-backends 'company-yasnippet 'company-robe)

            (global-company-mode 1)))

(use-package company-web
  :ensure t
  :config (progn
            (add-to-list 'company-backends 'company-web-html)
            (add-to-list 'company-backends 'company-web-slim)))

(use-package company-tern
  :ensure t
  :config (progn
            (add-to-list 'company-backends 'company-tern)

            (add-hook 'js-mode-hook (lambda ()
                                      (tern-mode)
                                      (company-mode)))))
