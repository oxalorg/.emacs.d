;; ~/.emacs or ~/.emacs.d/init.el

;; Store custom vars in a different file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  (package-install 'quelpa))

(unless (package-installed-p 'quelpa)
  (package-install 'quelpa))

(require 'use-package)

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(setq use-package-ensure-function 'quelpa)
(setq use-package-always-ensure t)

;; Install straight.el

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 6))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))
;;
;; (straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)
;;
(setq evil-want-C-u-scroll t)

(require 'seq)

(defun corgi/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun corgi/switch-to-last-elisp-buffer ()
  (interactive)
  (when-let ((the-buf (seq-find (lambda (b)
                                  (with-current-buffer b
                                    (derived-mode-p 'emacs-lisp-mode)))
                                (buffer-list))))
    (pop-to-buffer the-buf)))

(defun corgi/double-columns ()
  "Simplified version of spacemacs/window-split-double-column"
  (interactive)
  (delete-other-windows)
  (let* ((previous-files (seq-filter #'buffer-file-name
                                     (delq (current-buffer) (buffer-list)))))
    (set-window-buffer (split-window-right)
                       (or (car previous-files) "*scratch*"))
    (balance-windows)))

(defun ox/open-init-el ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defaults
;; Various things that really should have been configured this way
;; out of the box. This is mostly copied from Corgi defaults which
;; in turn has been copied from Magnar Sveen's config, but stripped down.

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-message t)

;; Allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8)   ; pretty
(set-terminal-coding-system 'utf-8)  ; pretty
(set-keyboard-coding-system 'utf-8)  ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8)        ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Split threshold
(setq split-width-threshold 200
      split-height-threshold nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)
(set-default 'fill-column 80)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me, please
;; (setq-default truncate-lines t)

;; Scroll off
(setq scroll-margin 10
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 2000000)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; No electric indent
(setq electric-indent-mode nil)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Don't make backup~ files
;; (setq make-backup-files nil)

;; Put backups and auto-save files in subdirectories, so the
;; user-emacs-directory doesn't clutter
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

;; Stop asking about following symlinks to version controlled files
(setq vc-follow-symlinks t)

;; Configure common Emoji fonts, making it more likely that Emoji will work out of the box
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

;;;;;;;;;;;;;;; Corgi Editor

(use-package diminish
  :diminish
  elisp-slime-nav-mode
  eldoc-mode
  subword-mode)

;; Make counsel-M-x show most recently used commands first
(use-package smex)

(use-package avy)

(use-package undo-fu)

(use-package general
  :config
  (general-create-definer ox/leader-define-key
    :states 'normal
    :keymaps 'override
    :prefix "SPC"))

(use-package evil
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode t)
  (evil-set-undo-system 'undo-fu)
  (setq evil-move-cursor-back nil
        evil-move-beyond-eol t
        evil-want-fine-undo t
        evil-mode-line-format 'before
        evil-normal-state-cursor '(box "orange")
        evil-insert-state-cursor '(box "green")
        evil-visual-state-cursor '(box "#F86155")
        evil-emacs-state-cursor  '(box "purple"))

  ;; (with-eval-after-load 'evil-maps
  ;;   (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
  ;;   (define-key evil-motion-state-map (kbd ";") 'evil-ex))
  (general-define-key
   :states 'motion
   ";" 'evil-ex
   ":" 'evil-repeat-find-char)

  ;; Prevent evil-motion-state from shadowing previous/next sexp
  (require 'evil-maps)
  (define-key evil-motion-state-map "L" nil)
  (define-key evil-motion-state-map "M" nil))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init)
  ;; Stop changing how last-sexp works. Even though we have evil-move-beyond-eol
  ;; set, this is still being added, and I can't figure out why. Resorting to
  ;; this hack.
  (cl-loop
   for fun
   in '(elisp--preceding-sexp cider-last-sexp pp-last-sexp)
   do (advice-mapc (lambda (advice _props) (advice-remove fun advice)) fun)))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package winum
  :config (winum-mode 1))

(use-package smartparens
  :init (require 'smartparens-config)
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode))

;; We don't actually enable cleverparens, because most of their bindings we
;; don't want, we install our own bindings for specific sexp movements
(use-package evil-cleverparens
  :after (evil smartparens)
  :config
  
  )

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook ((clojurex-mode
          clojurescript-mode
          clojurec-mode
          clojure-mode
          emacs-lisp-mode
          lisp-data-mode
          typescript-ts-mode
          tsx-ts-mode
          js-ts-mode
          piglet-mode)
         . aggressive-indent-mode))

(use-package rainbow-delimiters
  :hook ((cider-repl-mode
          clojurex-mode
          clojurescript-mode
          clojurec-mode
          clojure-mode
          emacs-lisp-mode
          lisp-data-mode
          inferior-emacs-lisp-mode)
         . rainbow-delimiters-mode))

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-create-missing-test-files t)
  (setq projectile-project-search-path '("~/projects/" "~/playground/"))
  (defun ox/refresh-projects-dir ()
    (interactive)
    ;; (projectile-discover-projects-in-directory "~/projects")
    (projectile-discover-projects-in-search-path)))

;; (use-package dumb-jump)

;; (use-package goto-last-change)

;; (use-package expand-region)

(use-package string-edit)

;; (use-package xclip
;;   :config
;;   (when (executable-find xclip-program)
;;     (xclip-mode t)))

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun magnars/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'magnars/create-non-existent-directory)

(defun corgi-editor/-on-buffer-change (win)
  (unless (and (minibufferp) (not evil-want-minibuffer))
    (evil-normal-state)))

(add-to-list 'window-buffer-change-functions #'corgi-editor/-on-buffer-change)

;;;;;;;;;;;;;; Corgi emacs lisp

;; Show emacs-lisp eval results in an overlay, CIDER style.
;; https://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
;; We rely on CIDER to do the heavy lifting, can't seem to find a general library
;; for doing this style of overlays.
(defun corgi/eval-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (corgi/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (corgi/eval-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (corgi/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thread-first / thread-last, ported from clojure-mode

(defun corgi/thread ()
  "Thread by one more level an existing threading macro."
  (interactive)
  (ignore-errors
    (when (looking-at "(")
      (forward-char 1)
      (forward-sexp 1)))
  (search-backward-regexp "(thread-")
  (down-list)
  (when (clojure--threadable-p)
    (prog1 (cond
            ((looking-at "thread-first")  (clojure--thread-first))
            ((looking-at "thread-last") (clojure--thread-last)))
      (clojure--fix-sexp-whitespace 'move-out))))

(defun corgi/elisp--thread-all (first-or-last-thread but-last)
  (save-excursion
    (insert-parentheses 1)
    (insert first-or-last-thread))
  (while (save-excursion (corgi/thread)))
  (when (or but-last clojure-thread-all-but-last)
    (clojure-unwind)))

(defun corgi/elisp-thread-first-all (but-last)
  (interactive "P")
  (corgi/elisp--thread-all "thread-first " but-last))

(defun corgi/elisp-thread-last-all (but-last)
  (interactive "P")
  (corgi/elisp--thread-all "thread-last " but-last))

(use-package elisp-slime-nav
  :config
  :hook ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode))

;; (use-package pprint-to-buffer)

;;; corgi-clojure.el --- Clojure configuration for Corgi
(use-package clojure-mode
  :magic ("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode)
  :config
  (setq clojure-toplevel-inside-comment-form t
        ;; Because of CIDER's insistence to send forms to all linked REPLs, we
        ;; *have* to be able to switch cljc buffer to clj/cljs mode without
        ;; cider complaining.
        clojure-verify-major-mode nil)

  ;; TODO: get this upstream. #_ is not a logical sexp
  (defun corgi/clojure--looking-at-non-logical-sexp (command)
    "Return non-nil if text after point is \"non-logical\" sexp.
\"Non-logical\" sexp are ^metadata and #reader.macros."
    (comment-normalize-vars)
    (comment-forward (point-max))
    (looking-at-p "\\(?:#?\\^\\)\\|#:?:?[[:alpha:]]\\|#_"))

  (advice-add #'clojure--looking-at-non-logical-sexp :around #'corgi/clojure--looking-at-non-logical-sexp))

(use-package cider
  :diminish cider-mode
  :config
  (setq cider-preferred-build-tool 'clojure-cli
        ;; ~make sure we can always debug nrepl issues~
        ;; Turning this off again, seems it may really blow up memory usage
        ;; nrepl-log-messages nil
        )

  ;; TODO: clean this up, submit to upstream where possible
  ;; More CIDER/clojure-mode stuff
  ;; - logical-sexp doesn't treat #_ correctly

  ;; New function, should go upstream. Kill all associated REPLs
  (defun corgi/cider-quit-all ()
    "Quit all current CIDER REPLs."
    (interactive)
    (let ((repls (seq-remove (lambda (r)
                               (equal r (get-buffer "*babashka-repl*")))
                             (seq-mapcat #'cdr (sesman-current-sessions 'CIDER)))))
      (seq-do #'cider--close-connection repls))
    ;; if there are no more sessions we can kill all ancillary buffers
    (cider-close-ancillary-buffers)
    ;; need this to refresh sesman browser
    (run-hooks 'sesman-post-command-hook))

  ;; When asking for a "matching" REPL (clj/cljs), and no matching REPL is found,
  ;; return any REPL that is there. This is so that cider-quit can be called
  ;; repeatedly to close all REPLs in a process. It also means that , s s will go
  ;; to any REPL if there is one open.
  (defun corgi/around-cider-current-repl (command &optional type ensure)
    (let ((repl (or
                 (if (not type)
                     (or (funcall command nil)
                         (funcall command 'any))
                   (funcall command type))
                 (get-buffer "*babashka-repl*"))))
      (if (and ensure (null repl))
          (cider--no-repls-user-error type)
        repl)))

  (advice-add #'cider-current-repl :around #'corgi/around-cider-current-repl)

  ;; This essentially redefines cider-repls. The main thing it does is return all
  ;; REPLs by using sesman-current-sessions (plural) instead of
  ;; sesman-current-session. It also falls back to the babashka repl if no repls
  ;; are connected/linked, so we can always eval.
  (defun corgi/around-cider-repls (command &optional type ensure)
    (let ((type (cond
                 ((listp type)
                  (mapcar #'cider-maybe-intern type))
                 ((cider-maybe-intern type))))
          (repls (delete-dups (seq-mapcat #'cdr (or (sesman-current-sessions 'CIDER)
                                                    (when ensure
                                                      (user-error "No linked %s sessions" system)))))))
      (or (seq-filter (lambda (b)
                        (and (cider--match-repl-type type b)
                             (not (equal b (get-buffer "*babashka-repl*")))))
                      repls)
          (list (get-buffer "*babashka-repl*")))))

  (advice-add #'cider-repls :around #'corgi/around-cider-repls)

  (defun corgi/cider-eval-last-sexp-and-replace ()
    "Alternative to cider-eval-last-sexp-and-replace, but kills
clojure logical sexp instead of ELisp sexp, and pprints the
result."
    (interactive)
    (let ((last-sexp (cider-last-sexp)))
      ;; we have to be sure the evaluation won't result in an error
      (cider-nrepl-sync-request:eval last-sexp)
      ;; seems like the sexp is valid, so we can safely kill it
      (let ((opoint (point)))
        (clojure-backward-logical-sexp)
        (kill-region (point) opoint))
      (cider-interactive-eval last-sexp
                              (cider-eval-pprint-with-multiline-comment-handler
                               (current-buffer)
                               (set-marker (make-marker) (point))
                               ""
                               " "
                               "")
                              nil
                              (cider--nrepl-print-request-map fill-column))))

  (defun corgi/cider-pprint-eval-last-sexp-insert ()
    (interactive)
    (let ((cider-comment-prefix "")
          (cider-comment-continued-prefix " ")
          (cider-comment-postfix ""))
      (cider-pprint-eval-last-sexp-to-comment)))

  (defadvice cider-find-var (before add-evil-jump activate)
    (evil-set-jump)))

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

;; (use-package clj-ns-name
;;   :config
;;   (clj-ns-name-install))

;; TODO: submit upstream (?)
(defun corgi/cider-pprint-register (register)
  "Evaluate a Clojure snippet stored in a register.

Will ask for the register when used interactively. Put `#_clj' or
`#_cljs' at the start of the snippet to force evaluation to go to
a specific REPL type, no matter the mode (clojure-mode or
clojurescript-mode) of the current buffer."
  (interactive (list (register-read-with-preview "Eval register: ")))
  (let ((reg (get-register register)))
    (cond
     ((string-match-p "^#_cljs" reg)
      (with-current-buffer (car (cider-repls 'cljs))
        (cider--pprint-eval-form reg)))
     ((string-match-p "^#_clj" reg)
      (with-current-buffer (car (cider-repls 'clj))
        (cider--pprint-eval-form reg)))
     (t
      (cider--pprint-eval-form reg)))))


;;; corgi-stateline.el --- Change the background color of the modeline based on the evil state
;;
;; Filename: corgi-stateline.el
;; Package-Requires: ()
;;
;;; Code:

(defcustom corgi-stateline-normal-fg "black"
  "Foreground color of the modeline in evil normal state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-normal-bg "gray"
  "Background color of the modeline in evil normal state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-motion-fg "black"
  "Foreground color of the modeline in evil motion state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-motion-bg "orangered"
  "Background color of the modeline in evil motion state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-insert-fg "black"
  "Foreground color of the modeline in evil insert state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-insert-bg "green"
  "Background color of the modeline in evil insert state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-visual-fg "white"
  "Foreground color of the modeline in evil visual state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-visual-bg "royalblue1"
  "Background color of the modeline in evil visual state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-emacs-fg "white"
  "Foreground color of the modeline in evil emacs state"
  :type 'color
  :group 'corgi)

(defcustom corgi-stateline-emacs-bg "slateblue3"
  "Background color of the modeline in evil emacs state"
  :type 'color
  :group 'corgi)

(defun corgi-stateline/enter-normal-state ()
  (face-remap-add-relative 'mode-line :foreground corgi-stateline-normal-fg)
  (face-remap-add-relative 'mode-line :background corgi-stateline-normal-bg))

(defun corgi-stateline/enter-motion-state ()
  (face-remap-add-relative 'mode-line :foreground corgi-stateline-motion-fg)
  (face-remap-add-relative 'mode-line :background corgi-stateline-motion-bg))

(defun corgi-stateline/enter-insert-state ()
  (face-remap-add-relative 'mode-line :foreground corgi-stateline-insert-fg)
  (face-remap-add-relative 'mode-line :background corgi-stateline-insert-bg))

(defun corgi-stateline/enter-visual-state ()
  (face-remap-add-relative 'mode-line :foreground corgi-stateline-visual-fg)
  (face-remap-add-relative 'mode-line :background corgi-stateline-visual-bg))

(defun corgi-stateline/enter-emacs-state ()
  (face-remap-add-relative 'mode-line :foreground corgi-stateline-emacs-fg)
  (face-remap-add-relative 'mode-line :background corgi-stateline-emacs-bg))

(add-hook 'evil-normal-state-entry-hook #'corgi-stateline/enter-normal-state)
(add-hook 'evil-motion-state-entry-hook #'corgi-stateline/enter-motion-state)
(add-hook 'evil-insert-state-entry-hook #'corgi-stateline/enter-insert-state)
(add-hook 'evil-visual-state-entry-hook #'corgi-stateline/enter-visual-state)
(add-hook 'evil-emacs-state-entry-hook #'corgi-stateline/enter-emacs-state)


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
      dired-recursive-copies (quote always) ; “always” means no asking
      dired-recursive-deletes (quote top) ; “top” means ask once
      dired-dwim-target t ; Copy from one dired dir to the next dired dir shown in a split window
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
(and window-system (server-start))
;; (desktop-save-mode 0)
(show-paren-mode 1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (set-frame-font "Iosevka 20")
;; You will most likely need to adjust this font size for your system!
(defvar ox/default-font-size 200)
(defvar ox/default-variable-font-size 200)
;; Make frame transparency overridable
(defvar ox/frame-transparency '(90 . 90))

(set-face-attribute 'default nil :font "Iosevka" :height ox/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height ox/default-font-size)
;; Set the variable pitch face
;; (set-face-attribute 'variable-pitch nil :font "Iosveka" :height ox/default-variable-font-size :weight 'regular)

(use-package dired-subtree)
(use-package markdown-mode)
(use-package yaml-mode)

(use-package org
  :config
  (setq org-startup-indented t
        org-extend-today-until 4))

(use-package org-journal
  :after (org)
  :config
  (setq org-journal-dir (expand-file-name "journal" org-directory)
        org-journal-file-type 'monthly
        org-journal-date-format "%Y-%m-%d, %a"
        org-journal-file-format "%Y-%m.org"
        org-journal-time-format ""))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

;; (use-package cherry-blossom-theme
;;   :config
;;   (load-theme 'cherry-blossom t))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package vundo
;;   :straight (vundo :type git :host github :repo "casouri/vundo"))

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

;; web-mode
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 4))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  ;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
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

(defun ox/open-round-insert ()
  (interactive)
  (paredit-open-round)
  (evil-insert 0))

;; (define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-next-line)
;; (define-key ivy-minibuffer-map (kbd "<backtab>") 'ivy-previous-line)

(setq save-interprogram-paste-before-kill t)

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

(defun ox/left-paren-call ()
  (interactive)
  (let ((last-command-event ?\())
    (call-interactively (key-binding "("))))

(defun ox/right-paren-call ()
  (interactive)
  (let ((last-command-event ?\())
    (call-interactively (key-binding ")"))))

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

(use-package evil-swap-keys
  :after (evil)
  :config
  (evil-swap-keys-add-pair "9" "(")
  (evil-swap-keys-add-pair "0" ")"))

(use-package vertico
  :init
  (vertico-mode)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.1 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  
  )

(use-package consult-projectile)

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-popupinfo-delay '(0.1 . 0.1))
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  (eldoc-add-command #'corfu-insert)
  :config
  (general)
  )

(use-package cbor)
(use-package websocket
  :after (cbor))

(use-package pdp
  :ensure nil
  :load-path "~/projects/piglet-emacs")

(use-package piglet-mode
  :ensure nil
  :after (pdp)
  :load-path "~/projects/piglet-emacs")

(use-package treesit
  :ensure nil
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-font-lock-level 4)
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	(treesit-install-language-grammar lang)
	(message "`%s' parser was installed." lang)
	(sit-for 0.75))))
  (setq treesit-max-buffer-size (* 2048 1024 1024))
  :init
  (setq c-ts-mode-indent-offset 4)
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(lua-mode . lua-ts-mode))
  (add-to-list 'major-mode-remap-alist '(sql-mode . sql-ts-mode))
  (add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))
  (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
  (add-to-list 'major-mode-remap-alist '(bash-mode . bash-ts-mode))
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode)))


(require 'pdp)

(use-package eglot
  :hook
  ((js-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure))
  ;; :custom
  ;; (eglot-stay-out-of indent-tabs-mode) ;; (This didn't work)
  )

(defun kap/eglot-current-server ()
  (interactive)
  (message "%s" (process-command (jsonrpc--process (eglot-current-server)))))

(defun corgi/cider-jack-in-babashka ()
  "Start a utility CIDER REPL backed by Babashka, not related to a
specific project."
  (interactive)
  (let ((project-dir user-emacs-directory))
    (nrepl-start-server-process
     project-dir
     "bb --nrepl-server 0"
     (lambda (server-buf)
       (set-process-query-on-exit-flag
        (get-buffer-process server-buf) nil)
       (cider-nrepl-connect
        (list :repl-buffer server-buf
              :repl-type 'clj
              :host (plist-get nrepl-endpoint :host)
              :port (plist-get nrepl-endpoint :port)
              :project-dir project-dir
              :session-name "babashka"
              :repl-init-function (lambda ()
                                    (setq-local cljr-suppress-no-project-warning t
                                                cljr-suppress-middleware-warnings t
                                                process-query-on-exit-flag nil)
                                    (set-process-query-on-exit-flag
                                     (get-buffer-process (current-buffer)) nil)
                                    (rename-buffer "*babashka-repl*"))))))))

;; Create a *scratch-clj* buffer for evaluating ad-hoc Clojure expressions. If
;; you make sure there's always a babashka REPL connection then this is a cheap
;; way to always have a place to type in some quick Clojure expression evals.
(with-current-buffer (get-buffer-create "*scratch-clj*")
  (clojure-mode))

;; Connect to Babashka if we can find it. This is a nice way to always have a
;; valid REPL to fall back to. You'll notice that with this all Clojure buffers
;; get a green "bb" indicator, unless there's a more specific clj/cljs REPL
;; available.
;; (when (executable-find "bb")
;;   (corgi/cider-jack-in-babashka))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.d.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))

(use-package expand-region
  :config
  (load-file (expand-file-name "treesit-er-expansions.el" user-emacs-directory)))

(use-package treemacs
  :ensure t)

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package json)
(use-package request)
(use-package clockify
  :ensure nil
  :load-path "~/projects/emacs-clockify"
  :init
  (setq clockify-api-key "<>")
  ;; (setq clockify-user-id "<user-id>")
  (setq clockify-workspace "630c782eb59c366b0e2038be")
  )

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line)))

(use-package emojify)
(use-package gitmoji
  :ensure nil
  :load-path "~/projects/emacs-gitmoji")

;; Possibly look in the future
;; https://github.com/fourier/ztree
;; https://github.com/Boruch-Baum/emacs-diredc

;; Keybindings
(ox/leader-define-key
  "." 'find-file
  "/" 'consult-ripgrep
  "RET" 'vertico-repeat-select
  "a" 'evil-cp-insert-at-end-of-form
  "bb" 'consult-buffer
  "bB" 'consult-buffer-other-window
  "bd" 'evil-delete-buffer
  "bd" 'kill-current-buffer
  "bm" 'consult-bookmark
  "fe" 'ox/open-init-el
  "ff" 'projectile-find-file
  "fi" 'ox/open-init-el
  "fr" 'consult-recent-file
  "ft" 'treemacs
  "gg" 'magit-status
  "ge" 'gitmoji-insert-emoji
  "h" 'help-command
  "jj" 'org-journal-new-entry
  "pa" 'projectile-add-known-project
  "pg" 'projectile-grep
  "pp" 'projectile-switch-project
  "pr" 'ox/refresh-projects-dir
  "rr" 'consult-register
  "rr" 'eglot-rename
  "rs" 'consult-register-store
  "sl" 'consult-line
  "w1" 'delete-other-windows
  "w2" 'corgi/double-columns
  "wd" 'delete-window
  "wo" 'other-window
  "ww" 'evil-window-next
  "/" 'execute-extended-command
  )

(ox/leader-define-key
  :keymaps 'emacs-lisp-mode-map
  "ee" 'eval-last-sexp
  )

(ox/leader-define-key
  :keymaps 'cider-mode-map
  "ee" 'cider-eval-last-sexp
  "eb" 'cider-eval-buffer
  "ed" 'cider-eval-defun-at-point
  "eD" 'cider-
  )

(general-define-key
 :states 'normal
 "RET" 'er/expand-region
 "DEL" 'evil-switch-to-windows-last-buffer)

(general-define-key
 :states 'normal
 :keymaps 'eglot-mode-map
 "gd" 'xref-find-definitions
 "gD" 'xref-find-definitions-other-window
 "g5" 'xref-find-definitions-other-frame
 "C-t" 'xref-pop-marker-stack
 "gr" 'xref-find-references
 ;; "K" 'eldoc-doc-buffer
 )
