(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

(define-key key-translation-map (kbd "M-q") (kbd "C-g"))

(global-set-key (kbd "M-y") 'wsl-copy)
(global-set-key (kbd "M-k") 'wsl-cut)

(global-set-key (kbd "<home>") 'sandric/smart-beginning-of-line)
(global-set-key (kbd "<select>") 'end-of-line)

(global-set-key (kbd "<prior>") 'sandric/scroll-up)
(global-set-key (kbd "<next>") 'sandric/scroll-down)

(global-set-key (kbd "M-v") 'sandric/eval-last-or-region)
(global-set-key (kbd "<f1> t") 'sandric/select-symbol-under-cursor)

(global-set-key (kbd "<f2> a") 'kill-this-buffer)
(global-set-key (kbd "<f2> s") 'save-buffer)

(global-set-key (kbd "C-h") 'left-word)
(global-set-key (kbd "C-o") 'right-word)

(global-set-key (kbd "C-S-<left>") 'sandric/select-word-left)
(global-set-key (kbd "C-S-<right>") 'sandric/select-word-right)
