(defun sandric/hydra-interact-description (arg)
  "Hydra interact description."
  (interactive "P")
  (cond ((string= major-mode "emacs-lisp-mode")
         (describe-symbol (or (symbol-at-point) (error "No symbol-at-point"))))
        ((string= major-mode "ruby-mode")
         (call-interactively 'robe-doc arg))
        ((string= major-mode "js-mode")
         (call-interactively 'sandric/counsel-dash-at-point arg))))

(defun sandric/hydra-interact-definition (arg)
  "Hydra interact definition."
  (interactive "P")
  (cond ((string= major-mode "emacs-lisp-mode")
         (xref-find-definitions (thing-at-point 'symbol)))
        ((string= major-mode "ruby-mode")
         (call-interactively 'robe-jump arg))
        ((string= major-mode "js-mode")
         (call-interactively 'tern-find-definition arg))))

(defun sandric/hydra-interact-scratch (arg)
  "Hydra interact scratch."
  (interactive "P")
  (cond ((string= major-mode "emacs-lisp-mode")
         (find-file "/home/sandric/scratch/scratch.el"))
        ((string= major-mode "ruby-mode")
         (find-file "/home/sandric/scratch/scratch.rb"))
        ((string= major-mode "js-mode")
         (find-file "/home/sandric/scratch/scratch.js"))
        ((string= major-mode "html-mode")
         (find-file "/home/sandric/scratch/scratch.html"))))

(defun sandric/hydra-interact-repl (arg)
  "Hydra interact repl."
  (interactive "P")
  (cond ((string= major-mode "emacs-lisp-mode")
         (ielm))
        ((string= major-mode "ruby-mode")
         (ruby-switch-to-inf arg))
        ((string= major-mode "js-mode")
         (find-file "/home/sandric/scratch/scratch.js"))))


(use-package hydra
  :ensure t
  :config (progn
            (defhydra hydra-interact (:exit t)
              "Interact"
              ("<C-f4> n" sandric/hydra-interact-definition "definition")
              ("<C-f4> e" sandric/hydra-interact-description "docs")
              ("<C-f4> r" sandric/hydra-interact-scratch "scratch")
              ("<C-f4> t" sandric/hydra-interact-repl "repl")
              ("<C-f4> s" flycheck-list-errors "lint"))

            (defhydra hydra-rg (:exit t)
              "PT"
              ("<C-f4> f" sandric/rg-or-region "current")
              ("<C-f4> a" sandric/rg-home "home")
              ("<C-f4> r" sandric/rg-emacs "emacs")
              ("<C-f4> s" sandric/rg-projects "projects")
              ("<C-f4> n" sandric/rg-host "host"))

            (defhydra hydra-fzf (:exit t)
              "FZF"
              ("<C-f4> t" sandric/fzf "current")
              ("<C-f4> a" sandric/fzf-home "home")
              ("<C-f4> r" sandric/fzf-emacs "emacs")
              ("<C-f4> s" sandric/fzf-projects "projects")
              ("<C-f4> n" sandric/fzf-host "host"))

            (defhydra hydra-dired (:exit t)
              "Dired"
              ("<C-f4> v" dired-jump "current")
              ("<C-f4> a" sandric/dired-jump-home "home")
              ("<C-f4> r" sandric/dired-jump-emacs "emacs")
              ("<C-f4> s" sandric/dired-jump-projects "projects")
              ("<C-f4> n" sandric/dired-jump-host "host"))


            (defhydra hydra-git (:exit t)
              "Git"
              ("<C-f4> w" magit-status "status")
              ("<C-f4> t" git-timemachine "timemachine")
              ("<C-f4> f" magit-log-buffer-file "log-file")
              ("<C-f4> p" magit-log-all "log")
              ("<C-f4> a" magit-blame "annotate"))


            (define-key sandric-right-ctrl-shift-map (kbd "n") 'hydra-interact/body)
            (define-key sandric-right-ctrl-shift-map (kbd "f") 'hydra-rg/body)
            (define-key sandric-right-ctrl-shift-map (kbd "t") 'hydra-fzf/body)
            (define-key sandric-right-ctrl-shift-map (kbd "v") 'hydra-dired/body)
            (define-key sandric-right-ctrl-shift-map (kbd "w") 'hydra-git/body)
            ))
