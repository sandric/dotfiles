(use-package key-chord
  :ensure t
  :config (progn
            (key-chord-mode t)
            (key-chord-define-global "WW" 'undo-tree-visualize)

            (key-chord-define-global "TT" 'sandric/open-buffer-other-pane)
            (key-chord-define-global "NN" 'select-window-1)
            (key-chord-define-global "II" 'select-window-2)))
