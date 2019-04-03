(require 'ansi-color)

(defun sandric/tramp-guest ()
  (interactive)
  (find-file "/ssh:guest:"))

(defun sandric/display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun sandric/tmux-show-history ()
  "Show tmux history with colors."

  (find-file "/home/sandric/.tmux_history")
  (sandric/display-ansi-colors)
  (end-of-buffer)
  (skip-chars-backward "\n"))

(defun sandric/switch-to-previous-buffer ()
  "Switching to previously used buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun sandric/select-left-pane ()
  "Select right pane window."
  (interactive)
  (select-window-1))

(defun sandric/select-right-pane ()
  "Select right pane window."
  (interactive)
  (select-window-2))

(defun sandric/open-buffer-other-pane ()
  "Open current buffer in other pane window."
  (interactive)
  (let ((buffer (current-buffer)))
    (other-window 1)
    (switch-to-buffer buffer)))

(defun sandric/swiper-or-region (beg end)
  "Swiper region or 'empty string' if none highlighted."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and beg end)
      (progn
        (deactivate-mark)
        (swiper (buffer-substring-no-properties beg end)))
    (swiper)))

(defun sandric/ivy-replace ()
  "Ivy replace with mc selction."
  (interactive)
  (run-at-time nil nil (lambda ()
                         (ivy-wgrep-change-to-wgrep-mode)))
  (ivy-occur))

(defun sandric/counsel-dash-at-point ()
  "Find documentation at point."
  (interactive)
  (counsel-dash (thing-at-point 'symbol)))


(defun sandric/w3m-force-quit ()
  "Force quit from w3m buffer."
  (interactive)
  (w3m-quit 1))

(defun sandric/clipboard-paste (arg)
  (interactive "P")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end)))
  (xterm-paste))

(defun sandric/describe-symbol-at-point ()
  "Describe default symbol under cursor."
  (interactive)
  (describe-symbol (or (symbol-at-point) (error "No symbol-at-point"))))


(defun sandric/magit-status ()
  "Open magit status dependent on current frame."
  (interactive)
  (if (sandric/is-left-frame)
      (sandric/select-right-frame))
  (magit-status))


(defmacro sandric/minibuffer-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (minibuffer-keyboard-quit)))

(defun sandric/remove-read-only-on-all-buffers ()
  "Kill open buffers by major mode name."
  (interactive)
  (mapc (lambda (buffer)
          (with-current-buffer buffer (wgrep-set-readonly-area nil)))
        (buffer-list)))

(defun sandric/get-major-mode ()
  "Print major mode name."
  (interactive)
  (print major-mode))

(defun sandric/kill-buffers-by-major-mode (mode-name)
  "Kill open buffers by major mode name."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq mode-name (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun sandric/kill-buffers-by-major-mode-regexp (mode-regexp)
  "Kill open buffers by major mode regexp."
  (interactive)
  (mapc (lambda (buffer)
          (when (string-match mode-regexp (symbol-name (buffer-local-value 'major-mode buffer)))
            (kill-buffer buffer)))
        (buffer-list)))

(defun sandric/switch-buffers-by-major-mode (mode-name)
  "Switch to open buffers by major mode name."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq mode-name (buffer-local-value 'major-mode buffer))
            (switch-to-buffer buffer)))
        (buffer-list)))

(defun sandric/switch-buffers-by-name-regexp (mode-regexp)
  "Switch to open buffers by buffer name regexp."
  (interactive)
  (catch 'break
    (mapc (lambda (buffer)
            (when (string-match mode-regexp (buffer-name buffer))
              (switch-to-buffer buffer)
              (throw 'break buffer)))
          (buffer-list))))

(defun sandric/open-scratch-elisp ()
  "Open coffee scratch buffer with repl"
  (interactive)
  (find-file "~/scratch/scratch.el"))

(defun sandric/open-scratch-ruby ()
  "Open ruby scratch buffer with repl"
  (interactive)
  (find-file "~/scratch/test.rb")
  (robe-start 1)
  (ruby-switch-to-inf nil))

(defun sandric/open-scratch-js ()
  "Open JS scratch buffer with repl"
  (interactive)
  (find-file "~/scratch/test.js")
  (nodejs-repl))

(defun sandric/open-scratch-coffee ()
  "Open coffee scratch buffer with repl"
  (interactive)
  (find-file "~/scratch/test.coffee")
  (coffee-repl))

(defun sandric/open-scratch-html ()
  "Open html scratch buffer with repl"
  (interactive)
  (find-file "~/scratch/test.html")
  (browse-url "file:///Users/sandric/scratch/test.html")
  (sandric/hammerspoon-split-chrome))

(defun sandric/select-minibuffer (&rest ...)
  "Select right minibuffer."
  (interactive)
  (select-window-0))

(defun sandric/new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((ξbuf (generate-new-buffer "untitled")))
    (switch-to-buffer ξbuf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(defun sandric/eval-last-or-region (arg)
  "Evaluates last sexp or region if active."
  (interactive "P")
  (if (use-region-p)
      (eval-region (region-beginning) (region-end))
    (eval-last-sexp arg)))

(defun sandric/scroll-up ()
  "My scroll up."
  (interactive)
  (forward-line -5))

(defun sandric/scroll-down ()
  "My scroll down."
  (interactive)
  (forward-line 5))

(defun sandric/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun sandric/select-word-left ()
  "Select word left."
  (interactive)
  (when (not (region-active-p))
    (push-mark (point) t t))
  (backward-word)
  (setq transient-mark-mode  (cons 'only transient-mark-mode)))

(defun sandric/select-word-right ()
  "Select word right."
  (interactive)
  (when (not (region-active-p))
    (push-mark (point) t t))
  (forward-word)
  (setq transient-mark-mode  (cons 'only transient-mark-mode)))

(defun sandric/select-symbol-under-cursor ()
  "Select symbol under cursor."
  (interactive)
  (unless (region-active-p)
    (let* ((bounds     (bounds-of-thing-at-point 'symbol))
           (beginning  (car bounds))
           (ending     (cdr bounds)))
      (goto-char beginning)
      (push-mark beginning t t)
      (goto-char ending)
      (setq transient-mark-mode  (cons 'only transient-mark-mode)))))

(defun sandric/string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun sandric/bytes-point ()
  "Print point position in bytes"
  (interactive)
  (print (position-bytes (point))))

(defun sandric/rbenv-gems-directory ()
  "Rbenv gems directory."
  (format "%s/gems"
          (s-trim-right (shell-command-to-string "gem environment gemdir"))))

(defun sandric/mc-next-like-or-region (beg end)
  "MC edit lines or like this if none highlighted."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and beg end)
      (mc/edit-lines)
    (mc/mark-next-like-this)))

(defun sandric/translit ()
  "Print point position in bytes"
  (interactive)
  (sandric/new-empty-buffer)
  (set-input-method 'cyrillic-translit))

(defun sandric/chrome-console (http)
  "Connect to chrome tab console window."
  (indium-chrome--get-tabs-data "10.0.2.2" "9222" #'indium-chrome--connect-to-tab))
