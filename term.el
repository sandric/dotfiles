(defun sandric/term-move-to-current-prompt ()
  "Move cursor to current prompt position."
  (interactive)
  (re-search-backward term-prompt-regexp)
  (forward-char 3))

(defun sandric/term-get-current-command ()
  "Get current term command in term line mode."
  (interactive)
  (end-of-buffer)
  (backward-char)
  (setq sandric/term-command-end (point))
  (sandric/term-move-to-current-prompt)
  (setq sandric/term-command-start (point))
  (setq sandric/term-command-start-column (current-column))
  (buffer-substring-no-properties sandric/term-command-start sandric/term-command-end))

(defun sandric/term-delete-current-command ()
  "Delete current command in term char mode."
  (interactive)
  (term-send-end)
  (term-send-raw-string "\C-u"))

(defun sandric/term-switch-char-mode ()
  "Switch to term char mode replacing command in exist."
  (interactive)
  (when (term-in-line-mode)
    (setq sandric/term-command-current-column (current-column))
    (setq sandric/term-command (sandric/term-get-current-command))
    (setq sandric/term-times-to-move-right (-
                                            sandric/term-command-current-column
                                            sandric/term-command-start-column))
    
    (term-char-mode)
    (sandric/term-delete-current-command)
    (term-send-raw-string sandric/term-command)
    (when sandric/term-eol
      (term-send-end)
      (term-send-backspace)
      (setq sandric/term-eol nil))
    (term-send-home)
    (dotimes (i sandric/term-times-to-move-right)
      (term-send-right))
    (set (make-local-variable 'cua-mode) nil)
    (set (make-local-variable 'transient-mark-mode) nil)
    (sandric/title-update)))

(defun sandric/term-switch-line-mode ()
  "Switch to term line mode."
  (interactive)
  (when (term-in-char-mode)
    (if (= (point) (point-at-eol))
        (progn
          (term-send-raw-string " ")
          (term-send-left)
          (setq sandric/term-eol 1))
      (setq sandric/term-eol nil))
    (term-line-mode)
    (set (make-local-variable 'cua-mode) t)
    (set (make-local-variable 'transient-mark-mode) t)
    (sandric/title-update)))

(defun sandric/term-switch-line-mode-left ()
  "Switch to term line mode and select left character."
  (interactive)
  (sandric/term-switch-line-mode)
  (when (not (region-active-p))
    (push-mark (point) t t))
  (backward-char)
  (setq transient-mark-mode  (cons 'only transient-mark-mode)))

(defun sandric/term-switch-line-mode-right ()
  "Switch to term line mode and select right character."
  (interactive)
  (sandric/term-switch-line-mode)
  (when (not (region-active-p))
    (push-mark (point) t t))
  (forward-char)
  (setq transient-mark-mode  (cons 'only transient-mark-mode)))

(defun sandric/term-switch-line-mode-up ()
  "Switch to term line mode and select up character."
  (interactive)
  (sandric/term-switch-line-mode)
  (when (not (region-active-p))
    (push-mark (point) t t))
  (previous-line)
  (setq transient-mark-mode  (cons 'only transient-mark-mode)))

(defun sandric/term-switch-line-mode-down ()
  "Switch to term line mode and select down character."
  (interactive)
  (sandric/term-switch-line-mode)
  (when (not (region-active-p))
    (push-mark (point) t t))
  (next-line)
  (setq transient-mark-mode  (cons 'only transient-mark-mode)))

(defun sandric/term-switch-line-mode-word-left ()
  "Switch to term line mode and select left word."
  (interactive)
  (sandric/term-switch-line-mode)
  (sandric/select-word-left))

(defun sandric/term-switch-line-mode-word-right ()
  "Switch to term line mode and select right word."
  (interactive)
  (sandric/term-switch-line-mode)
  (sandric/select-word-right))

(defun sandric/term-switch-line-mode-home ()
  "Switch to term line mode and select to home."
  (interactive)
  (sandric/term-switch-line-mode)
  (when (not (region-active-p))
    (push-mark (point) t t))
  (sandric/term-move-to-current-prompt)
  (setq transient-mark-mode  (cons 'only transient-mark-mode)))

(defun sandric/term-switch-line-mode-end ()
  "Switch to term line mode and select to end."
  (interactive)
  (sandric/term-switch-line-mode)
  (when (not (region-active-p))
    (push-mark (point) t t))
  (move-end-of-line nil)
  (setq transient-mark-mode  (cons 'only transient-mark-mode)))

(defun sandric/term-switch-line-mode-symbol ()
  "Switch to term line mode and select symbol."
  (interactive)
  (sandric/term-switch-line-mode)
  (sandric/select-symbol-under-cursor))


(defun sandric/term-interrupt ()
  "Interrupt command with previously switch to char mode."
  (interactive)
  (sandric/term-switch-char-mode)
  (sandric/term-move-end)
  (call-interactively (term-interrupt-subjob)))

(defun sandric/term-send-input ()
  "Interrupt command with previously switch to char mode."
  (interactive)
  (sandric/term-switch-char-mode)
  (term-send-raw-string "\^M"))

(defun sandric/term-move-word-left ()
  "Move word left in term."
  (interactive)
  (term-send-raw-string "\eb"))

(defun sandric/term-move-word-right ()
  "Move word right in term."
  (interactive)
  (term-send-raw-string "\ef"))

(defun sandric/term-move-home ()
  "Move home in term."
  (interactive)
  (term-send-home))

(defun sandric/term-move-end ()
  "Move end in term."
  (interactive)
  (term-send-end))


(defun sandric/term-man ()
  "Get MAN input for thing under cursor."
  (interactive)
  (man (thing-at-point 'symbol)))

(defun sandric/term-which ()
  "Open dired for thing under cursor."
  (interactive)
  (dired-jump nil (s-trim-right (shell-command-to-string (concat "which " (thing-at-point 'symbol))))))

(defun sandric/term-find-file ()
  "Find file for thing under cursor."
  (interactive)
  (find-file (thing-at-point 'symbol)))

(defun sandric/term-find-file-from-selection ()
  "Find file for thing under cursor."
  (interactive)
  (when (region-active-p)
    (find-file (buffer-substring (region-beginning) (region-end)))))



(defun sandric/term-insert-hook ()
  "Term hook to switch into char mode on insert."
  (sandric/term-switch-char-mode))

(defun sandric/term-format-name (buffer-name)
  "."
  (interactive)
  (s-upcase (s-replace-all '(("*" . "")
                             ("<" . "")
                             (">" . ""))
                           buffer-name)))

(defun sandric/term-getenv (env)
  "Get environment variable for current term."
  (interactive)
  (s-trim-right (shell-command-to-string
                 (s-concat
                  "echo $"
                  (sandric/term-format-name (buffer-name))
                  "_"
                  env))))

(defun sandric/term-dired ()
  "Open dired buffer from current term."
  (interactive)
  (dired (sandric/term-getenv "PWD")))

(defun sandric/term-rg (beg end)
  "Open rg buffer from current term."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (sandric/rg-or-region beg end (sandric/term-getenv "PWD")))



(defun term-exec-1 (name buffer command switches)
  ;; We need to do an extra (fork-less) exec to run stty.
  ;; (This would not be needed if we had suitable Emacs primitives.)
  ;; The 'if ...; then shift; fi' hack is because Bourne shell
  ;; loses one arg when called with -c, and newer shells (bash,  ksh) don't.
  ;; Thus we add an extra dummy argument "..", and then remove it.
  (let ((process-environment
         (nconc
          (list
           (format "TERM=%s" term-term-name)
           (format "TERMINFO=%s" data-directory)
           (format term-termcap-format "TERMCAP="
                   term-term-name term-height term-width)

           ;; This is for backwards compatibility with Bash 4.3 and earlier.
           ;; Remove this hack once Bash 4.4-or-later is common, because
           ;; it breaks './configure' of some packages that expect it to
           ;; say where to find EMACS.
           (format "EMACS=%s (term:%s)" emacs-version term-protocol-version)

           (format "INSIDE_EMACS=%s,term:%s" emacs-version term-protocol-version)
           (format "LINES=%d" term-height)
           (format "COLUMNS=%d" term-width)
           (format "EMACS_TERM_NAME=%s" (sandric/term-format-name name)))
          process-environment))
        (process-connection-type t)
        ;; We should suppress conversion of end-of-line format.
        (inhibit-eol-conversion t)
        ;; The process's output contains not just chars but also binary
        ;; escape codes, so we need to see the raw output.  We will have to
        ;; do the decoding by hand on the parts that are made of chars.
        (coding-system-for-read 'binary))
    (apply 'start-process name buffer
           "/bin/sh" "-c"
           (format "stty -nl echo rows %d columns %d sane 2>/dev/null;\
if [ $1 = .. ]; then shift; fi; exec \"$@\""
                   term-height term-width)
           ".."
           command switches)))

(defun sandric/term-setup ()
  "Term setup on term-mode-hook."
  (highlight-symbol-mode)

  (modify-syntax-entry ?. "_")
  
  (setq term-prompt-regexp " ^ ")
  (setq term-unbind-key-list '("C-x"
                               "C-h"
                               "M-x"
                               "C-z"))
  (term-set-escape-char ?\C-x)

  (setq term-bind-key-alist
        '(
          ("C-q" . term-interrupt-subjob)
          ("C-p" . previous-line)
          ("C-n" . next-line)
          ("C-s" . isearch-forward)
          ("C-r" . isearch-backward)
          ("C-m" . term-send-raw)
          ("M-f" . term-send-forward-word)
          ("M-b" . term-send-backward-word)
          ("M-o" . term-send-backspace)
          ("M-p" . term-send-up)
          ("M-n" . term-send-down)
          ("M-M" . term-send-forward-kill-word)
          ("M-N" . term-send-backward-kill-word)
          ("M-r" . term-send-reverse-search-history)
          ("M-," . term-send-input)
          ("M-." . comint-dynamic-complete)))

  (add-hook 'post-self-insert-hook 'sandric/term-insert-hook nil t))

(add-hook 'term-mode-hook 'sandric/term-setup)

(eval-after-load "term" '(progn
                           (define-key term-mode-map (kbd "C-g")
                             'sandric/term-interrupt)
                           (define-key term-mode-map (kbd "RET")
                             'sandric/term-send-input)

                           (define-key term-raw-map (kbd "C-g")
                             'sandric/term-interrupt)
                           

                           (define-key term-raw-map (kbd "<C-left>")
                             'sandric/term-move-word-left)
                           (define-key term-raw-map (kbd "<C-right>")
                             'sandric/term-move-word-right)

                           (define-key term-raw-map [S-left]
                             #'sandric/term-switch-line-mode-left)
                           (define-key term-raw-map [S-right]
                             #'sandric/term-switch-line-mode-right)
                           (define-key term-raw-map [S-up]
                             #'sandric/term-switch-line-mode-up)
                           (define-key term-raw-map [S-down]
                             #'sandric/term-switch-line-mode-down)

                           (define-key term-raw-map [S-C-left]
                             'sandric/term-switch-line-mode-word-left)
                           (define-key term-raw-map [S-C-right]
                             'sandric/term-switch-line-mode-word-right)

                           (define-key term-raw-map [S-home]
                             'sandric/term-switch-line-mode-home)
                           (define-key term-raw-map [S-end]
                             'sandric/term-switch-line-mode-end)

                           (define-key term-raw-map (kbd "<C-f3> t")
                             'sandric/term-switch-line-mode-symbol)

                           (define-key sandric-right-alt-shift-map (kbd "l") 'multi-term-prev)
                           (define-key sandric-right-alt-shift-map (kbd "y") 'multi-term-next)

                           ))


(setq multi-term-buffer-name "tty")
