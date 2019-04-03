(use-package org-bullets
  :ensure t)

(defun sandric/org-mode-setup ()
  "Org mode hook."

  (setq org-support-shift-select 'always)

  (key-chord-define org-mode-map "UU" 'outline-move-subtree-up)
  (key-chord-define org-mode-map "EE" 'outline-move-subtree-down)
  (key-chord-define org-mode-map "NN" 'org-promote-subtree)
  (key-chord-define org-mode-map "II" 'org-demote-subtree)

  (key-chord-define org-mode-map "TT" 'org-meta-return)
  (key-chord-define org-mode-map "LL" 'org-todo)
  (key-chord-define org-mode-map "YY" 'org-todo)

  (define-key org-mode-map (kbd "M-e") nil)

  (org-bullets-mode t))

(add-hook 'org-mode-hook 'sandric/org-mode-setup)
