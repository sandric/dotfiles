(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

(define-key key-translation-map (kbd "M-q") (kbd "C-g"))

(global-set-key (kbd "C-r") 'cua-copy-region)
(global-set-key (kbd "C-s") 'cua-cut-region)
(global-set-key (kbd "C-t") 'cua-paste)

(global-set-key (kbd "<home>") 'sandric/smart-beginning-of-line)
(global-set-key (kbd "<select>") 'end-of-line)

(global-set-key (kbd "<prior>") 'sandric/scroll-up)
(global-set-key (kbd "<next>") 'sandric/scroll-down)

(global-set-key (kbd "M-r") 'sandric/eval-last-or-region)

(global-set-key (kbd "C-S-t") 'sandric/select-symbol-under-cursor)

(global-set-key (kbd "M-A") 'kill-this-buffer)
(global-set-key (kbd "M-R") 'save-buffer)
(global-set-key (kbd "M-S") 'counsel-find-file)

(global-set-key (kbd "C-h") 'left-word)
(global-set-key (kbd "C-o") 'right-word)

(global-set-key (kbd "C-S-h") 'sandric/select-word-left)
(global-set-key (kbd "C-S-o") 'sandric/select-word-right)


(define-prefix-command 'sandric-left-ctrl-map)
(define-prefix-command 'sandric-right-ctrl-map)
(define-prefix-command 'sandric-left-ctrl-shift-map)
(define-prefix-command 'sandric-right-ctrl-shift-map)
(define-prefix-command 'sandric-left-alt-shift-map)

(global-set-key (kbd "C-<f1>") 'sandric-left-ctrl-map)
(global-set-key (kbd "C-<f2>") 'sandric-right-ctrl-map)
(global-set-key (kbd "C-<f3>") 'sandric-left-ctrl-shift-map)
(global-set-key (kbd "C-<f4>") 'sandric-right-ctrl-shift-map)
(global-set-key (kbd "C-<f5>") 'sandric-left-alt-shift-map)

(define-key sandric-right-ctrl-map (kbd "r") 'sandric/eval-last-or-region)
(define-key sandric-right-ctrl-map (kbd "w") 'sandric/open-buffer-other-pane)

(define-key sandric-left-ctrl-shift-map (kbd "t") 'sandric/select-symbol-under-cursor)

(define-key sandric-right-ctrl-shift-map (kbd "a") 'kill-this-buffer)
(define-key sandric-right-ctrl-shift-map (kbd "r") 'save-buffer)
(define-key sandric-right-ctrl-shift-map (kbd "s") 'counsel-find-file)

(define-key sandric-left-alt-shift-map (kbd "u") 'sandric/switch-to-previous-buffer)
(define-key sandric-left-alt-shift-map (kbd "e") 'sandric/switch-to-previous-buffer)
