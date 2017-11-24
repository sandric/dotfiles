(use-package ivy
  :ensure t
  :defer t
  :config (progn
            (setq ivy-switch-buffer-faces-alist
                  '((dired-mode . ivy-subdir)))

            (use-package smex
              :ensure t
              :config (progn
                        (require 'smex)

                        (setq smex-save-file
                              (expand-file-name "/home/sandric/.smex-history"))

                        (smex-initialize)))

            (use-package counsel
              :ensure t
              :config (progn
                        (setq ivy-initial-inputs-alist '()))))

  :bind (("<C-f4> c" . ivy-switch-buffer)
         ("<C-f4> q" . counsel-M-x)
         :map ivy-minibuffer-map
         ("<C-f4> w" . sandric/ivy-replace)
         :map wgrep-mode-map
         ("<C-f4> r" . wgrep-finish-edit)))

(use-package swiper
  :ensure t
  :defer t
  :config (progn
            (use-package wgrep
              :ensure t))
  :bind (("C-f" . sandric/swiper-or-region)))

(use-package counsel-dash
  :ensure t
  :config (progn
            (setq counsel-dash-common-docsets '("Emacs Lisp"))

            (setq-default helm-dash-browser-func 'w3m)

            (add-hook 'emacs-lisp-mode-hook
                      (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
            (add-hook 'ruby-mode-hook
                      (lambda () (setq-local counsel-dash-docsets '("Ruby"
                                                                    "Ruby on Rails"))))
            (add-hook 'js-mode-hook
                      (lambda () (setq-local counsel-dash-docsets '("JavaScript"
                                                                    "NodeJS"
                                                                    "Express"
                                                                    "React"))))
            (add-hook 'coffee-mode-hook
                      (lambda () (setq-local counsel-dash-docsets '("JavaScript"
                                                                    "NodeJS"
                                                                    "Express"
                                                                    "React"))))
            (add-hook 'css-mode-hook
                      (lambda () (setq-local counsel-dash-docsets '("CSS"))))))
