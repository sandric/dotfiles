(require 'cl)

(setq load-prefer-newer t)


(menu-bar-mode -1)


(setq x-select-enable-primary t)
(setq select-enable-primary t)
(setq mouse-drag-copy-region t)

(electric-indent-mode +1)
(show-paren-mode t)
(blink-cursor-mode t)
(winner-mode t)
(column-number-mode t)
(setq shift-select-mode t)
(setq-default indent-tabs-mode nil)
(global-auto-revert-mode t)

(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(cua-mode t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(cursor-color . "#F74874"))

(setq use-dialog-box nil)

(setq sgml-basic-offset 2)
(setq js-indent-level 2)

(setq debug-on-error t)
(setq whitespace-line-column 800)
(setq ns-function-modifier 'hyper)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(setq show-paren-style 'parenthesis)

(setq enable-recursive-minibuffers t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq initial-major-mode (quote text-mode))
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix
      temporary-file-directory)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default tab-width 2)

(setq split-height-threshold 5000)
(setq split-width-threshold 5000)


(add-to-list 'display-buffer-alist
             '("." display-buffer-same-window))

(custom-set-variables
 '(display-buffer-base-action 
   '(display-buffer-reuse-window (reusable-frames . t))))






(setq cyrillic-language "Ukrainian")

(quail-define-package
 "cyrillic-translit" "Cyrillic Translit" "UA" nil
 "Better ukrainian translit."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("a" ?а)
 ("b" ?б)
 ("v" ?в)
 ("w" ?в)
 ("g" ?г)
 ("d" ?д)
 ("e" ?е)
 ("jh" ?ж)
 ("z" ?з)
 ("i" ?i)
 ("y" ?и)
 ("j" ?й)
 ("k" ?к)
 ("l" ?л)
 ("m" ?м)
 ("n" ?н)
 ("o" ?о)
 ("p" ?п)
 ("r" ?р)
 ("s" ?с)
 ("t" ?т)
 ("u" ?у)
 ("f" ?ф)
 ("h" ?х)
 ("c" ?ц)
 ("ch" ?ч)
 ("sh" ?ш)
 ("shh" ?щ)
 ("q" ?ь)
 ("yu" ?ю)
 ("ya" ?я)
 ("ye" ?є)
 ("yi" ?ї)
 
 ("A" ?А)
 ("B" ?Б)
 ("V" ?В)
 ("W" ?В)
 ("G" ?Г)
 ("D" ?Д)
 ("E" ?Е)
 ("Jh" ?Ж)  ("JH" ?Ж)
 ("Z" ?З)
 ("I" ?I)
 ("Y" ?И)
 ("J" ?Й)
 ("K" ?К)
 ("L" ?Л)
 ("M" ?М)
 ("N" ?Н)
 ("O" ?О)
 ("P" ?П)
 ("R" ?Р)
 ("S" ?С)
 ("T" ?Т)
 ("U" ?У)
 ("F" ?Ф)
 ("H" ?Х)
 ("C" ?Ц)
 ("Ch" ?Ч) ("CH" ?Ч)
 ("Sh" ?Ш) ("SH" ?Ш)
 ("Shh" ?Щ) ("SHH" ?Щ)
 ("Q" ?Ь)
 ("Yu" ?Ю) ("YU" ?Ю)
 ("Ya" ?Я) ("YA" ?Я)
 ("Ye" ?Є) ("YE" ?Є)
 ("Yi" ?Ї) ("YI" ?Ї))


(defun cua-copy-region (arg)
  "Copy region to tmp clip."
  (interactive "P")
  (if (use-region-p)
      (write-region (region-beginning) (region-end) "~/host/tmp/clipboard" t)))

(defun cua-cut-region (arg)
  "Cut region to tmp clip."
  (interactive "P")
  (if (use-region-p)
      (progn
        (write-region (region-beginning) (region-end) "~/host/tmp/clipboard" t)
        (delete-region (region-beginning) (region-end)))))

(define-minor-mode xterm-clipboard-mode
  (global-set-key (kbd "<xterm-paste>") 'sandric/clipboard-paste))

(add-hook 'prog-mode-hook 'xterm-clipboard-mode)


;; (defun sandric/on-emacs-get-focus ()
;;   "On emacs frame getting focus."
;;   (shell-command-to-string "tmux_set_name \"lala\"")
;;   )

;; (add-hook 'focus-in-hook 'xterm-clipboard-mode)

