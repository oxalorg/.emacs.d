;; ~/.emacs or ~/.emacs.d/init.el

;; Store custom vars in a different file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install straight.el

(defvar bootstrap-version)
(let ((install-url "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el")
      (bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously install-url 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install and enable use-package

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup before Corgi

(setq evil-want-C-u-scroll t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install Corgi

(use-package corgi-packages
  :straight (corgi-packages
             :type git
             :host github
             :repo "lambdaisland/corgi-packages"))

(add-to-list #'straight-recipe-repositories 'corgi-packages)

(let ((straight-current-profile 'corgi))
  (use-package corgi-defaults)
  (use-package corgi-editor)
  (use-package corgi-emacs-lisp)
  (use-package corgi-commands)
  (use-package corgi-clojure)
  (use-package corgi-stateline))

(defun ox/corkey-reload ()
  (interactive)
  (let ((straight-current-profile 'corgi))
    (use-package corkey
      :config
      (corkey-mode 1)
      (corkey/install-bindings '(custom-keys) '(corgi-signals custom-signals)))))

(ox/corkey-reload)

(defun ox/open-custom-keys ()
  (interactive)
  (find-file (expand-file-name "custom-keys.el" user-emacs-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Your own stuff goes here, we recommend these extra packages

(setq user-full-name "Mitesh Shah"
      user-mail-address "mitesh@miteshshah.com"
      ring-bell-function 'ignore
      display-time-world-list '(("America/Chicago" "Wisconsin")
                                ("America/Recife" "Recife")
                                ("Europe/Berlin" "Berlin")
                                ("Asia/Jerusalem" "Israel")
                                ("Asia/Kolkata" "Mumbai"))
      display-time-world-time-format "%a, %l:%M %p"
      org-directory "~/org/"
      org-journal-file-type 'monthly
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org"
      dired-recursive-copies (quote always) ; “always” means no asking
      dired-recursive-deletes (quote top) ; “top” means ask once
      dired-dwim-target t ; Copy from one dired dir to the next dired dir shown in a split window
      split-height-threshold nil
      split-width-threshold 0
      )

(defun custom-dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'custom-dired-mode-setup)

(with-eval-after-load 'dired
  (put 'dired-find-alternate-file 'disabled nil) ; disables warning
  (evil-define-key '(normal) dired-mode-map
    (kbd "RET") 'dired-find-alternate-file ; was dired-advertised-find-file
    (kbd "^") (lambda () (interactive) (find-alternate-file "..")))) ; was dired-up-directory

(setq ivy-initial-inputs-alist nil)

;; (global-superword-mode t)
;; (and window-system (server-start))
;; (desktop-save-mode 0)
(show-paren-mode 1)
(set-frame-font "Iosevka 20")
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package dired-subtree)
(use-package markdown-mode)
(use-package yaml-mode)
(use-package org
  :config
  (setq org-startup-indented t))
(use-package magit)
(use-package counsel-projectile
  :init
  (setq projectile-indexing-method 'hybrid)
  ;; (setq projectile-sort-order 'recently-active)
  (setq projectile-sort-order 'recentf))
(use-package cherry-blossom-theme
  :config
  (load-theme 'cherry-blossom t))
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-nord t)
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))
(use-package vundo
  :straight (vundo :type git :host github :repo "casouri/vundo"))
(use-package evil-cleverparens
  :commands evil-cleverparens-mode
  :init
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (setq evil-cleverparens-complete-parens-in-yanked-region t)
  :config
  (setq evil-cleverparens-use-s-and-S nil)
  (evil-define-key '(normal visual) evil-cleverparens-mode-map
    "s" nil
    "S" nil
    "{" nil
    "}" nil
    "[" nil
    "]" nil
    (kbd "<tab>") 'evil-jump-item))
(use-package zprint-mode)
(use-package evil-escape
  :config
  (setq-default evil-escape-key-sequence "qp")
  (evil-escape-mode))
(use-package evil-cleverparens)
(use-package evil-commentary
  :config
  (evil-commentary-mode))
(use-package html-to-hiccup
  :load-path "~/projects/html-to-hiccup")
(use-package git-link
  :config
  (setq git-link-open-in-browser t
        git-link-use-commit t))
(use-package magit-delta
  :after (magit)
  :config
  (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1))))
(use-package diff-hl
  :config
  (evil-set-initial-state 'diff-hl-show-hunk-posframe--transient-mode 'motion)
  (global-diff-hl-mode))
(use-package helpful
  ;; a better *help* buffer
  :commands helpful--read-symbol
  :init
  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))

(with-eval-after-load 'evil
  (fset 'evil-visual-update-x-selection 'ignore)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-insert-state-cursor '(bar "green"))
  (setq-default evil-symbol-word-search t))

(with-eval-after-load 'diff-hl
  '(progn
     ;; let diff-hl inline popup keymaps take priority over evil
     (evil-make-overriding-map diff-hl-show-hunk--inline-popup-map 'normal)
     ;; force update evil keymaps after diff-hl-mode loaded
     (add-hook 'diff-hl-mode-hook #'evil-normalize-keymaps)))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
  (define-key evil-motion-state-map (kbd ";") 'evil-ex))

;; web-mode
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 4))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (add-hook 'web-mode-hook  'web-mode-init-hook))

(defun html-to-hiccup-buffer ()
  (interactive)
  ;; (evil-visual-select (point-min) (point-max))
  (html-to-hiccup-convert-region (point-min) (point-max))
  (zprint))

;; (map! :localleader
;;       :map (clojure-mode-map clojurescript-mode-map)
;;       "=" #'zprint)

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

(use-package paren
  ;; highlight matching delimiters
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))

(use-package org-superstar ; "prettier" bullets
  :hook (org-mode . org-superstar-mode)
  :config
  ;; Make leading stars truly invisible, by rendering them as spaces!
  (setq org-superstar-leading-bullet ?\s
        org-superstar-leading-fallback ?\s
        org-hide-leading-stars nil
        org-superstar-todo-bullet-alist
        '(("TODO" . 9744)
          ("[ ]"  . 9744)
          ("DONE" . 9745)
          ("[X]"  . 9745))))

(defun ox/cider-eval-defun-at-point-and-run-test ()
  (interactive)
  (cider-eval-defun-at-point)
  (cider-test-run-test))

(defun ox/cider-switch-to-repl-buffer-same-window-force ()
  (interactive)
  (let ((repl (cider-current-repl nil nil)))
    (if repl
        (switch-to-buffer repl)
      (switch-to-buffer (cider-current-repl 'any 'ensure)))))

(defun ox/toggle-parens--replace (pair start end)
  "Replace parens with a new PAIR at START and END in current buffer.
   A helper function for `toggle-parens'."
  (goto-char start)
  (delete-char 1)
  (insert (substring pair 0 1))
  (goto-char end)
  (delete-char 1)
  (insert (substring pair 1 2))
  (goto-char start))

(defun ox/toggle-parens ()
  "Toggle parens () <> [] at cursor.

Turn on `show-paren-mode' to see matching pairs of parentheses
and other characters in buffers. This function then uses the same
function `show-paren-data-function' to find and replace them with
the other pair of brackets.

This function can be easily modified and expanded to replace
other brackets. Currently, mismatch information is ignored and
mismatched parens are changed based on the left one."
  (interactive)
  (let* ((parens (funcall show-paren-data-function))
         (start (if (< (nth 0 parens) (nth 2 parens))
                    (nth 0 parens) (nth 2 parens)))
         (end (if (< (nth 0 parens) (nth 2 parens))
                  (nth 2 parens) (nth 0 parens)))
         (startchar (buffer-substring-no-properties start (1+ start)))
         (mismatch (nth 4 parens)))
    (when parens
      (pcase startchar
        ("(" (ox/toggle-parens--replace "[]" start end))
        ("[" (ox/toggle-parens--replace "{}" start end))
        ("{" (ox/toggle-parens--replace "()" start end))))))

(defun ox/refresh-projects-dir ()
  (interactive)
  (projectile-discover-projects-in-directory "~/projects"))

(defun ox/open-round-insert ()
  (interactive)
  (paredit-open-round)
  (evil-insert 0))

(define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "<backtab>") 'ivy-previous-line)

(setq save-interprogram-paste-before-kill t)

(defun counsel-helpful-keymap-describe ()
  "select keymap with ivy, display help with helpful"
  (interactive)
  (ivy-read "describe keymap: " (let (cands)
				  (mapatoms
				   (lambda (x)
				     (and (boundp x) (keymapp (symbol-value x))
					  (push (symbol-name x) cands))))
				  cands)
	    :require-match t
	    :history 'counsel-describe-keymap-history
	    :sort t
	    :preselect (ivy-thing-at-point)
	    :keymap counsel-describe-map
	    :caller 'counsel-helpful-keymap-describe
	    :action (lambda (map-name)
		      (helpful-variable (intern map-name))) ))

(defun ox/cider-switch-to-repl-buffer-same-window-force ()
  (interactive)
  (let ((repl (cider-current-repl nil nil)))
    (if repl
        (switch-to-buffer repl)
      (switch-to-buffer (cider-current-repl 'any 'ensure)))))

(evil-define-key '(normal visual) cider-repl-mode-map
  (kbd "SPC,") 'evil-switch-to-windows-last-buffer)

;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable)
;;   :config
;;   (setq elpy-rpc-python-command "python3")
;;   (setq python-shell-interpreter "python3")
;;   ;;python-shell-interpreter-args "-i")
;;   )

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (add-hook 'org-mode-hook 'evil-org-mode)
  )

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  ;; If using org-roam-protocol
  ;;(require 'org-roam-protocol)
  )

(use-package forge
  :after magit)
