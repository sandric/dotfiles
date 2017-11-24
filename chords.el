(use-package key-chord
  :ensure t
  :config (progn
            (key-chord-define-global "NN" 'nlinum-mode)
            (key-chord-define-global "TT" 'sandric/open-current-buffer-in-other-frame)
            (key-chord-mode t)))
