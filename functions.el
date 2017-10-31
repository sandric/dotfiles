(defun cua-copy-region (arg)
  "Copy region to tmp clip."
  (interactive "P")
  (if (use-region-p)
      (write-region (region-beginning) (region-end) "~/host/tmp/clip" t)))

(defun cua-cut-region (arg)
  "Cut region to tmp clip."
  (interactive "P")
  (if (use-region-p)
      (progn
        (write-region (region-beginning) (region-end) "~/host/tmp/clip" t)
        (delete-region (region-beginning) (region-end)))))

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

(defun sandric/switch-buffers-by-name-right-pane-regexp (mode-regexp)
  "Switch to open buffers by buffer name regexp in regexp."
  (interactive)
  (catch 'break
    (mapc (lambda (buffer)
            (when (string-match mode-regexp (buffer-name buffer))
              (sandric/select-right-pane)
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


(defun sandric/select-left-pane (&rest ...)
  "Select left pane."
  (interactive)
  (select-window-1))

(defun sandric/select-right-pane (&rest ...)
  "Select right pane."
  (interactive)
  (select-window-2))

(defun sandric/select-minibuffer (&rest ...)
  "Select right pane."
  (interactive)
  (select-window-0))

(defun sandric/select-previous-window ()
  "Select previous window."
  (interactive)
  (select-window (previous-window)))

(defun sandric/is-window-in-left-pane ()
  "Check if window is in left pane."
  (interactive)
  (eq (window-numbering-get-number) 1))

(defun sandric/is-window-in-right-pane ()
  "Check if window is in right pane."
  (interactive)
  (eq (window-numbering-get-number) 2))

(defun sandric/open-buffer-in-left-pane (buffer)
  "Open buffer in left pane."
  (sandric/select-left-pane)
  (switch-to-buffer buffer))

(defun sandric/open-buffer-in-right-pane (buffer)
  "Open buffer in right pane."
  (sandric/select-right-pane)
  (switch-to-buffer buffer))

(defun sandric/eval-in-right-pane (callee)
  "Eval in right pane."
  (interactive)
  (sandric/select-right-pane)
  (funcall-interactively callee))

(defun sandric/shackle-in-left-pane (buffer alist plist)
  "Shackle rule to open buffer in left pane."
  (sandric/select-left-pane)
  (shackle--display-buffer-same buffer alist))

(defun sandric/shackle-in-right-pane (buffer alist plist)
  "Shackle rule to open buffer in right pane."
  (sandric/select-right-pane)
  (shackle--display-buffer-same buffer alist))

(defun sandric/toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun sandric/transpose-windows ()
  "Transpose two windows.  If more or less than two windows are visible, error."
  (interactive)
  (unless (= 2 (count-windows))
    (error "There are not 2 windows."))
  (let* ((windows (window-list))
         (w1 (car windows))
         (w2 (nth 1 windows))
         (w1b (window-buffer w1))
         (w2b (window-buffer w2)))
    (set-window-buffer w1 w2b)
    (set-window-buffer w2 w1b)))

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
  (forward-line 5))

(defun sandric/scroll-down ()
  "My scroll down."
  (interactive)
  (forward-line -5))

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
  (backward-word))

(defun sandric/select-word-right ()
  "Select word right."
  (interactive)
  (when (not (region-active-p))
    (push-mark (point) t t))
  (forward-word))

(defun sandric/select-symbol-under-cursor ()
  "Select symbol under cursor."
  (interactive)
  (when (not (region-active-p))
    (push-mark (point) t t))
  (print (thing-at-point 'symbol)))

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
