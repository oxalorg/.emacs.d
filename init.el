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
  (use-package corgi-stateline)
  (use-package corkey
    :config
    (corkey-mode 1)
    (corkey/install-bindings '(corgi-keys custom-keys) '(corgi-signals custom-signals))))

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
      org-journal-file-format "%Y-%m-%d.org")

(setq ivy-initial-inputs-alist nil)

(set-frame-font "Iosevka 16")
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package markdown-mode)
(use-package yaml-mode)
(use-package org)
(use-package magit)
(use-package counsel-projectile)
(use-package cherry-blossom-theme
  :config
  (load-theme 'cherry-blossom t))
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
    "]" nil))
(use-package zprint-mode)
(use-package evil-cleverparens)
(use-package html-to-hiccup
  :load-path "~/projects/html-to-hiccup")
(use-package git-link
  :config
  (setq git-link-open-in-browser t))
(use-package magit-delta
  :after (magit)
  :config
  (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1))))
(use-package diff-hl
  :config
  (evil-set-initial-state 'diff-hl-show-hunk-posframe--transient-mode 'motion)
  (global-diff-hl-mode))

(with-eval-after-load 'evil
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

