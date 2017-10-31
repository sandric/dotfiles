(defun sandric/eval-last-or-region-ruby (arg)
  "Evaluates last sexp or region if active in ruby mode."
  (interactive "P")
  (if (use-region-p)
      (ruby-send-region (region-beginning) (region-end))
    (ruby-send-last-sexp arg)))

(defun sandric/eval-last-or-region-js (beg end)
  "Evaluates last sexp or region if active js mode."
  (interactive "r")
  (if (use-region-p)
      (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer)
    (skewer-eval-last-expression beg)))

(use-package evil-nerd-commenter
  :ensure t
  :bind ("S-C-s-o" . evilnc-comment-or-uncomment-lines))

(use-package flycheck
  :ensure t
  :defer t
  :commands (flycheck-mode)
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))
  :config (progn
            (setq-default flycheck-disabled-checkers
                          (append flycheck-disabled-checkers
                                  '(javascript-jshint)))

            (flycheck-add-mode 'javascript-eslint 'web-mode)

            (setq-default flycheck-temp-prefix ".flycheck")

            (setq-default flycheck-disabled-checkers
                          (append flycheck-disabled-checkers
                                  '(json-jsonlist)))))

(use-package restclient
  :ensure t
  :defer t
  :config (progn
            (define-key restclient-mode-map (kbd "C-x C-e")
              'restclient-http-send-current)))

(use-package rubocop
  :ensure t)

(use-package inf-ruby
  :ensure t
  :defer t
  :commands (inf-ruby-minor-mode)
  :init (progn
          (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)))

(use-package robe
  :ensure t
  ;; :defer t
  ;; :mode "\\.rb\\'"
  ;; :commands (inf-ruby-minor-mode)
  :config (progn
            (define-key inf-ruby-minor-mode-map (kbd "M-n") 'windmove-left)
            (add-hook 'ruby-mode-hook 'robe-mode)))

(use-package web-mode
  :ensure t
  :defer t
  :config (progn
            (require 'web-mode)

            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-attr-indent-offset 2)

            (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))))

(use-package emmet-mode
  :ensure t
  :defer t
  :config (progn (require 'emmet-mode)
                 (add-hook 'sgml-mode-hook 'emmet-mode)
                 (add-hook 'html-mode-hook 'emmet-mode)
                 (add-hook 'css-mode-hook  'emmet-mode)

                 (define-key emmet-mode-keymap (kbd "TAB") 'emmet-expand-line)))

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :init (progn
          (setq exec-path (cons
                           (expand-file-name
                            "/Users/sandric/.rbenv/shims/sass")
                           exec-path))))

(use-package yaml-mode
  :ensure t)

(use-package tern
  :ensure t
  :defer t
  :init (progn (add-hook 'js-mode-hook 'tern-mode))
  :commands (tern-mode))

(use-package nodejs-repl
  :ensure t
  :defer t)
