(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])


(global-set-key (kbd "<home>") 'sandric/smart-beginning-of-line)
(global-set-key (kbd "<select>") 'end-of-line)

(global-set-key (kbd "<prior>") 'sandric/scroll-up)
(global-set-key (kbd "<next>") 'sandric/scroll-down)


(define-prefix-command 'sandric-left-ctrl-map)
(define-prefix-command 'sandric-right-ctrl-map)
(define-prefix-command 'sandric-left-ctrl-shift-map)
(define-prefix-command 'sandric-right-ctrl-shift-map)

(global-set-key (kbd "C-<f1>") 'sandric-left-ctrl-map)
(global-set-key (kbd "C-<f2>") 'sandric-right-ctrl-map)
(global-set-key (kbd "C-<f3>") 'sandric-left-ctrl-shift-map)
(global-set-key (kbd "C-<f4>") 'sandric-right-ctrl-shift-map)

(define-key sandric-right-ctrl-map (kbd "r") 'sandric/eval-last-or-region)
(define-key sandric-right-ctrl-map (kbd "w") 'sandric/open-current-buffer-in-other-frame)

(define-key sandric-left-ctrl-shift-map (kbd "t") 'sandric/select-symbol-under-cursor)

(define-key sandric-right-ctrl-shift-map (kbd "a") 'sandric/kill-buffer-and-delete-splitted-window)
(define-key sandric-right-ctrl-shift-map (kbd "r") 'save-buffer)
(define-key sandric-right-ctrl-shift-map (kbd "s") 'counsel-find-file)
