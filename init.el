(setq package-enable-at-startup nil)
(defvar elpaca-installer-version 0.9)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(defun +elpaca/build-if-new (e)
  (setf (elpaca<-build-steps e)
        (if-let ((default-directory (elpaca<-build-dir e))
                 (main (ignore-errors (elpaca--main-file e)))
                 (compiled (expand-file-name (concat (file-name-base main) ".elc")))
                 ((file-newer-than-file-p main compiled)))
            (progn (elpaca--signal e "Rebuilding due to source changes")
                   (cl-set-difference elpaca-build-steps
                                      '(elpaca--clone elpaca--configure-remotes elpaca--checkout-ref)))
          (elpaca--build-steps nil (file-exists-p (elpaca<-build-dir e))
                               (file-exists-p (elpaca<-repo-dir e)))))
  (elpaca--continue-build e))

;; (setq use-package-always-ensure t)
(elpaca-wait)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; (use-package corgi-packages
;;              :ensure (corgi-packages
;;                        :host github
;;                        :repo "corgi-emacs/corgi-packages"
;;                        :branch "ox/separate-completion-ui"))

(use-package transient)
(use-package json :ensure nil)
(use-package request)

(message "ox's emacs initializing...")

;; Allow Ctrl-u to scroll up a page like vim
(setq evil-want-C-u-scroll t)
(setq evil-want-keybinding nil)

;; set relative file numbers
(setq display-line-numbers-type 'relative)

;; (setq warning-minimum-level :error)
;; (setq warning-minimum-log-level :error)

(message "[ox] Loading corgi")

;; Loading corgi deps
(use-package clj-ns-name
  :ensure (clj-ns-name
	   :type git
	   :host github
	   :files ("clj-ns-name.el")
	   :repo "corgi-emacs/clj-ns-name"))

(use-package walkclj
  :ensure
  (walkclj
   :type git
   :host github
   :files ("walkclj.el")
   :repo "corgi-emacs/walkclj"))

(use-package pprint-to-buffer
  :ensure
  (pprint-to-buffer
   :type git
   :host github
   :files ("pprint-to-buffer/pprint-to-buffer.el")
   :repo "plexus/plexmacs"))

(defmacro corgi-use-package (package)
  `(use-package ,package
     :ensure (,package
              :repo "~/projects/corgi-packages"
              :local-repo ,(symbol-name package)
              :files (,(concat (symbol-name package) "/" (symbol-name package) ".el"))
	      :build (+elpaca/build-if-new)
              :branch "main")))

(corgi-use-package corgi-editor)
(corgi-use-package corgi-commands)
(corgi-use-package corgi-clojure)
(corgi-use-package corgi-emacs-lisp)
(corgi-use-package corgi-stateline)
(use-package corgi-bindings
  :ensure
  (corgi-bindings
   :type git
   :host github
   :branch "main"
   :files ("corgi-bindings/corgi-bindings.el"
           "corgi-bindings/corgi-keys.el"
           "corgi-bindings/corgi-signals.el"
           "corgi-bindings/user-keys-template.el"
           "corgi-bindings/user-signals-template.el")
   :repo "corgi-emacs/corgi-packages"))

(use-package corkey
  :ensure (corkey
           :type git
           :host github
           :repo "corgi-emacs/corkey")
  :config
  (corkey-mode 1)
  (corkey/reload))

(message "[ox] Corgi loaded.")

(setq backup-directory-alist
      `(("." . "~/.emacs-saves")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

(use-package magit)
(use-package org
  :ensure nil
  :init
  (setq org-directory "~/projects/org")
  :config
  (require 'org-tempo))
(use-package org-modern
  :hook '(org-mode-hook . org-modern-mode))
(use-package org-journal
  :after (org)
  :config
  (setq org-journal-dir (expand-file-name "journal" org-directory)
        org-journal-file-type 'monthly
        org-journal-date-format "%Y-%m-%d, %a"
        org-journal-file-format "%Y-%m.org"
        org-journal-time-format ""))
(use-package markdown-mode)
(use-package yaml-mode)
(use-package hcl-mode)
(use-package typescript-mode)
(use-package dockerfile-mode)
(use-package groovy-mode)
(use-package buttercup)
(use-package rainbow-mode)
(use-package pkg-info)
(use-package goto-last-change)
(use-package dumb-jump)
(use-package expand-region)

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun magnars/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'magnars/create-non-existent-directory)

;; unless existing server does not exist
(when (and (fboundp 'server-running-p)
           (not (server-running-p server-name)))
  (server-start))
(global-display-line-numbers-mode 1)

;; use with ,,<letter>, e.g. `,,g' runs (user/go)
(set-register ?k "#_clj (do (require 'kaocha.repl) (kaocha.repl/run))")
(set-register ?K "#_clj (do (require 'kaocha.repl) (kaocha.repl/run-all))")
(set-register ?r "#_clj (do (require 'user :reload) (user/reset))")
(set-register ?t "#_clj (do (require 'clojure.test) (clojure.test/run-tests {{(cider-current-ns)}}))")
(set-register ?t "#_clj (do (require 'clojure.test) (clojure.test/run-tests")
(set-register ?T "#_clj (do (require 'clojure.test) (clojure.test/run-all-tests))")
(set-register ?g "#_clj (user/go)")
(set-register ?b "#_clj (user/browse)")
(set-register ?c "#_clj ((requiring-resolve 'nextjournal.clerk/serve!) {})")
(set-register ?, "#_clj (nextjournal.clerk/show! \"{{buffer-file-name}}\")")
(set-register ?p "#_clj (user/portal)")
(set-register ?P "#_cljs (user/portal)")
(set-register ?z "#_clj (do (user/pathom-reload-env) nil)")

;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (load-theme 'sanityinc-tomorrow-night t))

(use-package cherry-blossom-theme
  :ensure t
  :config
  (load-theme 'cherry-blossom t))

(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

;;(require 'corgi-clojure-cider-extras)
;;(require 'corgi-cider-connection-indicator)

(setq cider-connection-message-fn
      nil )

(setq recentf-max-saved-items 100)

;; (when (executable-find "bb")
;;   (corgi/cider-jack-in-babashka))
;; (run-at-time nil (* 5 60) 'recentf-save-list)
;; (corgi/enable-cider-connection-indicator)

;; cider-connected-hook

;; clojure-mode

(use-package inf-clojure)
(use-package clj-refactor
  :after (cider)
  :diminish clj-refactor-mode
  :config
  (setq cljr-cljc-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-cljs-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-eagerly-build-asts-on-startup nil
        cljr-warn-on-eval nil)
  :hook ((clojurex-mode-hook
          clojurescript-mode-hook
          clojurec-mode-hook
          clojure-mode-hook)
         . clj-refactor-mode))

(use-package visual-fill-column)

(use-package flycheck-clj-kondo
  :ensure t)

(use-package zprint-mode)

(with-eval-after-load 'clojure-mode
  (with-current-buffer (get-buffer-create "*scratch-clj*")
    (clojure-mode))

  (with-current-buffer (get-buffer-create "*scratch*")
    (lisp-interaction-mode))

  (require 'flycheck-clj-kondo)

  (put-clojure-indent 'lambdaisland.morf/deform 1)
  (put-clojure-indent 'reflect/extend-signatures '(1 :form (1)))
  (put-clojure-indent 'sc.api/letsc '(1)))

(defun ox/cider-eval-defun-at-point-and-run-test ()
  (interactive)
  (cider-eval-defun-at-point)
  (cider-test-run-test))

;; (eval-after-load 'projectile
;;   (setq projectile-project-root-files-bottom-up
;;         (cons "deps.edn"
;;               projectile-project-root-files-bottom-up)))

;; Allow Ctrl-u to scroll up a page like vim
(setq evil-want-C-u-scroll t)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)
;; Option or Alt is naturally 'Meta'
(setq mac-option-modifier 'meta)
;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
(setq mac-right-option-modifier 'nil)

(global-auto-revert-mode t)

(defun ox/open-init-el ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;; You will most likely need to adjust this font size for your system!
(defvar ox/default-font-size 160)
(defvar ox/default-variable-font-size 160)
(set-face-attribute 'default nil :font "Iosevka" :height ox/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height ox/default-font-size)

;; Unfortunately emacs launched from `.app` launcher does not get the full exec path which our shell has. Let's fix that
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (fset 'evil-visual-update-x-selection 'ignore)
  (setq evil-kill-on-visual-paste nil)
  (setq-default evil-symbol-word-search t)
  :config
  (evil-mode t)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (require 'evil-maps)
  (define-key evil-motion-state-map "L" nil)
  (define-key evil-motion-state-map "M" nil)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Use another key to go into normal / escape mode. I have it configured as `qp`
(use-package evil-escape
  :config
  (setq-default evil-escape-key-sequence "qp")
  (evil-escape-mode))

(use-package evil-cleverparens
  :after (evil)
  :commands evil-cleverparens-mode
  :hook ((clojure-mode . evil-cleverparens-mode)
	 (emacs-lisp-mode . evil-cleverparens-mode))
  :init
  (setq evil-cleverparens-use-s-and-s nil)
  (setq evil-cleverparens-complete-parens-in-yanked-region t)
  :config
  (evil-define-key '(normal visual) evil-cleverparens-mode-map
    "s" nil
    "s" nil
    "{" nil
    "}" nil
    "[" nil
    "]" nil
    (kbd "m-[") nil
    (kbd "<tab>") 'evil-jump-item))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package projectile
  :init
  (setq projectile-create-missing-test-files t)
  (setq projectile-project-search-path '("~/projects/"
					 "~/playground/"
					 "~/projects/lambdaisland"
					 "~/projects/gaiwanteam"))
  (defun ox/refresh-projects-dir ()
    (interactive)
    ;; (projectile-discover-projects-in-directory "~/projects")
    (projectile-discover-projects-in-search-path))
  :config
  (projectile-global-mode))

;; command-log-mode is useful for displaying a panel showing each key binding
;; you use in a panel on the right side of the frame. Great for live streams and
;; screencasts!
(use-package command-log-mode)

(use-package forge
  :after magit
  :config
  (transient-append-suffix 'forge-dispatch '(0)
    ["Forge browse"
     ("@" "browse" forge-browse)])
  (transient-append-suffix 'forge-dispatch '(0)
    ["PR"
     ("p c" "pullreq checkout" forge-checkout-pullreq)]
    )
  (transient-append-suffix 'forge-dispatch '(0)
    ["Edit"
     ("e p" "post" forge-edit-post)
     ("e a" "assignees" forge-topic-set-assignees)
     ("e r" "review requests" forge-topic-set-review-requests)
     ]))

(use-package git-link
  :config
  (setq git-link-open-in-browser t
        git-link-use-commit t))

;; Configure common Emoji fonts, making it more likely that Emoji will work out of the box
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

(use-package emojify)
;; (use-package gitmoji
;;   :ensure nil
;;   :load-path "~/projects/emacs-gitmoji")

(use-package default-text-scale
  :config
  (setq default-text-scale-amount 20))

(use-package html-to-hiccup
  :ensure (:host github :repo "plexus/html-to-hiccup"))

(message "loading secrets...")
(load-file (expand-file-name (concat user-emacs-directory "/secrets.el")))
(message "secrets loaded")

(defun send-discord-message-with-webhook (webhook-url message)
  "Send a message to a Discord channel using a webhook URL."
  (request webhook-url
    :type "POST"
    :data (json-encode `(("content" . ,message)))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :sync t
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (message "Done: %s" (request-response-status-code response))))))

(defun send-slack-message-with-webhook (webhook-url message)
  "Send a message to a Slack channel using a webhook URL."
  (request webhook-url
    :type "POST"
    :data (json-encode `(("message" . ,message)))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :sync t
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (message "Done: %s" (request-response-status-code response))))))

(defun ox/journal-discord-gaiwan ()
  "Interactively send a message to a Discord channel using a webhook URL."
  (interactive)
  (let* ((message (read-string "Enter message: ")))
    (send-discord-message-with-webhook discord-ox-journal-webhook-url message)
    ))

(defun ox/journal-slack-gaiwan ()
  "Interactively send a message to a Discord channel using a webhook URL."
  (interactive)
  (let* ((message (read-string "Enter message: ")))
    (send-slack-message-with-webhook discord-slack-journal-webhook-url message)))

;; (use-package clockify
;;   :load-path "~/projects/emacs-clockify")

;; Usage example: M-x send-discord-message

(setq create-lockfiles nil)

(defun css-region-to-garden (start end)
  (interactive "r")
  (replace-regexp "\\([a-z-]+\\): \\(.*\\);" ":\\1 \"\\2\"" nil start end))

(defun projectile-kill-all-repl-buffers ()
  "Kill all repls in the current project"
  (interactive)
  (dolist (buffer (buffer-list))
    (when (or (string-match-p (concat "cider-repl projects/" (projectile-project-name) ":localhost") (buffer-name buffer)))
      (kill-buffer buffer))))

(use-package piglet-emacs
  :ensure (piglet-emacs :repo "~/projects/piglet-emacs"))
(use-package adoc-mode)

;; erlang
(defvar erlang-root-dir "/opt/homebrew/lib/erlang")
(defvar erlang-lib-dir (expand-file-name "lib/tools-4.1.1/emacs" erlang-root-dir))
(setq load-path (cons erlang-lib-dir load-path))
(require 'erlang-start)
(setq exec-path (cons (expand-file-name "bin" erlang-root-dir) exec-path))
(setq erlang-man-root-dir (expand-file-name "man" erlang-root-dir))

(use-package just-mode)

(use-package web-mode
  ;;   :mode (("\\.js\\'" . web-mode)
  ;; 	 ("\\.jsx\\'" .  web-mode)
  ;; 	 ("\\.ts\\'" . web-mode)
  ;; 	 ("\\.tsx\\'" . web-mode)
  ;; 	 ("\\.html\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode)))

(use-package dotenv-mode)
(use-package git-timemachine)
(use-package coverlay)
(use-package origami)

(use-package css-in-js-mode
  :ensure '(css-in-js-mode :type git :host github :repo "orzechowskid/tree-sitter-css-in-js"))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?_)          ;; Orderless field separator
  (corfu-auto-prefix 2)           ;; Minimum length of prefix for completion
  (corfu-auto-delay 0)            ;; No delay for completion
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current 'insert)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-popupinfo-delay '(0.2 . 0.2))
  :bind (:map corfu-map
              ("S-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . corfu-insert))
  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (corfu-echo-mode)
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))
            nil
            t)
  )

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package wgrep)

(use-package tsx-mode
  :ensure '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (setq consult-narrow-key "<"))

(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode 1))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
;; (use-package savehist
;;   :init
;;   (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; ;; Define a function to paste text
;; (defun my-paste ()
;;   "Paste text."
;;   (interactive)
;;   (yank))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  )

;; ;; (define-key input-decode-map (kbd "M-v") 'my-paste-from-clipboard)
;; ;; (defun my-paste-from-clipboard ()
;; ;;   "Paste from clipboard in Evil normal and insert modes."
;; ;;   (interactive)
;; ;;   (if (not (evil-insert-state-p))
;; ;;       (progn
;; ;;         (evil-normal-state)
;; ;;         (evil-paste-after))
;; ;;     (yank)))

(use-package lsp-mode
  :ensure t
  :hook (
	 ;; (clojure-mode . lsp)
         ;; (clojurec-mode . lsp)
         ;; (clojurescript-mode . lsp)
	 ;; (typescript-ts-mode . lsp)
	 (lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
	   web-mode
	   sh-mode
	   bash-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred)
	 )
  :custom
  (lsp-completion-provider :none)       ;; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter
  :init
  ;; (setq lsp-use-plists t)
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                  "/usr/local/bin" path-separator
                  (getenv "PATH")))
  (setq lsp-enable-indentation nil)
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq bash-allowed-shells '(sh bash zsh))
  (setq lsp-copilot-applicable-fn
	(lambda (buf-name buf-mode)
          (pcase buf-mode
	    ('erlang-mode t)
	    ('bash-ts-mode t)
	    ('sh-mode t)
            ('c-mode t)
            ('go-mode t)
            ('emacs-lisp-mode t)
            (t lsp-copilot-enabled))))
  )

;; (add-hook 'typescript-ts-mode 'lsp-mode-hook)

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-doc-show
	     lsp-ui-doc-glance
	     lsp-ui-mode)
  :config
  ;; (ivy-rich-mode 1)
  ;; (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  ;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-enable t
        evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
        lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
        lsp-ui-doc-include-signature t       ; Show signature
        lsp-ui-doc-position 'at-point)
  )

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package lsp-treemacs
  :after (treemacs evil lsp-mode)
  :ensure t)


;; ;; (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))                                                                                    (add-hook 'evil-normal-state-entry-hook (lambda () (send-string-to-terminal "\033[0 q")))

;; ;; `evil-terminal-cursor-changer' utilizes custom terminal escape sequences
;; ;; (which work in most, but not all, terminals) to adjust the appearance of the
;; ;; Emacs cursor based on which Vim mode is currently active. Note that this
;; ;; package is only required when running in a terminal (hence the `unless').
;; (use-package evil-terminal-cursor-changer
;;   :config
;;   (unless (display-graphic-p)
;;     (require 'evil-terminal-cursor-changer)
;;     (evil-terminal-cursor-changer-activate)
;;     (setq evil-insert-state-cursor 'bar)
;;     ))

(setq-default c-default-style "linux")
(setq-default c-basic-offset 2)

;; ;; (use-package company
;; ;;   :disabled t)

(use-package cider
  :after '(evil)
  :ensure t
  :init
  (setq cider-dynamic-indentation nil
	cider-font-lock-dynamically nil
	cider-font-lock-reader-conditionals nil)
  (setq cider-clojure-cli-global-options "")
  :config
  (evil-define-key '(normal visual) cider-repl-mode-map
    (kbd "SPC,") 'evil-switch-to-windows-last-buffer))

;; (defun ox/counsel-rg-change-dir (arg)
;;   (let ((current-prefix-arg '(4)))
;;     (counsel-rg ivy-text nil "")))

;; (with-eval-after-load 'counsel
;;   (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s || true"))

;; (with-eval-after-load 'ivy
;;   (with-eval-after-load 'counsel
;;     (ivy-add-actions
;;      'counsel-rg
;;      '(("r" ox/counsel-rg-change-dir "change root directory")))))

(use-package avy
  :ensure t
  :config
  (setq avy-timeout-seconds 0.5)
  (setq avy-ignored-modes
        '(image-mode magit-mode doc-view-mode pdf-view-mode exwm-mode))
  (evil-define-key nil evil-normal-state-map
    "s" 'avy-goto-char-2))

(use-package devdocs)

(with-eval-after-load 'require
  (add-to-list 'load-path (expand-file-name "~/projects/clojuredocs.el"))
  (require 'clojuredocs))

(use-package difftastic
  :demand t
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))


(use-package harpoon)

(use-package haskell-mode)

(use-package terraform-mode)

(use-package rust-mode)

(use-package go-ts-mode
  :ensure nil
  :hook
  (go-ts-mode . lsp-deferred)
  ;; (go-ts-mode . go-format-on-save-mode)
  :init
  (add-to-list 'treesit-language-source-alist '(go "https://github.com/tree-sitter/tree-sitter-go"))
  (add-to-list 'treesit-language-source-alist '(gomod "https://github.com/camdencheek/tree-sitter-go-mod"))
  ;; (dolist (lang '(go gomod)) (treesit-install-language-grammar lang))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
  :config
  (reformatter-define go-format
		      :program "goimports"
		      :args '("/dev/stdin"))
  )

(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
	 ("\\.py\\'"  . python-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  ;; (dolist (mapping
  ;;          '((python-mode . python-ts-mode)
  ;;            (css-mode . css-ts-mode)
  ;;            (typescript-mode . typescript-ts-mode)
  ;;            (js-mode . typescript-ts-mode)
  ;;            (js2-mode . typescript-ts-mode)
  ;;            (c-mode . c-ts-mode)
  ;;            (c++-mode . c++-ts-mode)
  ;;            (c-or-c++-mode . c-or-c++-ts-mode)
  ;;            (bash-mode . bash-ts-mode)
  ;;            (css-mode . css-ts-mode)
  ;;            (json-mode . json-ts-mode)
  ;;            (js-json-mode . json-ts-mode)
  ;;            (sh-mode . bash-ts-mode)
  ;;            (sh-base-mode . bash-ts-mode)))
  ;;   (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-ts-mode))


(defun ox/lsp-get-signature ()
  (interactive)
  (let ((contents (-some->> (lsp--text-document-position-params)
                    (lsp--make-request "textDocument/hover")
                    (lsp--send-request)
                    (lsp:hover-contents))))
    (let ((contents (and contents
			 (lsp--render-on-hover-content
			  contents
			  t))))
      (message contents))))

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

(defun ox/open-round-insert ()
  (interactive)
  (paredit-open-round)
  (evil-insert 0))

(defun ox/left-paren-call ()
  (interactive)
  (let ((last-command-event ?\())
    (call-interactively (key-binding "("))))

(defun ox/right-paren-call ()
  (interactive)
  (let ((last-command-event ?\())
    (call-interactively (key-binding ")"))))

;; (use-package evil-swap-keys
;;   :after (evil)
;;   :config
;;   (evil-swap-keys-add-pair "9" "(")
;;   (evil-swap-keys-add-pair "0" ")"))

;; I use parens so much that it makes sense to make them easier to type
;; (general-define-key
;;  :states 'insert
;;  "9" 'ox/left-paren-call
;;  "0" 'ox/right-paren-call
;;  "(" (lambda () (interactive) (insert "9"))
;;  ")" (lambda () (interactive) (insert "0")))

;; (evil-define-key 'insert 'global
;; (kbd "9") 'ox/left-paren-call
;; (kbd "0") 'ox/right-paren-call
;; (kbd "(") (lambda () (interactive) (insert "9"))
;; (kbd ")") (lambda () (interactive) (insert "0"))
;; )


;; (use-package magit-delta)

;; (defun aankh/toggle-magit-delta ()
;;   (interactive)
;;   (magit-delta-mode
;;    (if magit-delta-mode
;;        -1
;;      1))
;;   (magit-refresh))

;; ;; For some reason, this was being called twice without the guard.
;; (with-eval-after-load 'magit-diff
;;   (unless (boundp 'aankh/added-magit-diff-suffixes)
;;     (transient-append-suffix 'magit-diff '(-1 -1)
;;       [("l" "Toggle magit-delta" aankh/toggle-magit-delta)
;;        ;; ("D" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
;;        ;; ("S" "Difftastic Show" th/magit-show-with-difftastic)
;;        ]))
;;   (setf aankh/added-magit-diff-suffixes t))


(message "[ox] loading tailwind cheatsheet...")
(load-file (expand-file-name (concat user-emacs-directory "/tailwind_cheatsheet.el")))
(message "[ox] tailwind cheatsheet loaded")

(message "[ox] init.el finished loading.")

(provide 'init)
;;; local.ox.el ends here
