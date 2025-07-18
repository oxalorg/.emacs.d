;;; -*- no-byte-compile: t -*-

;; This is your user keys file, here you can configure key bindings that will
;; get added to Corgi. You can also override Corgi's default bindings this way.
;;
;; Bindings here are nested, e.g. `("SPC" ("b" ("k" kill-buffer)))' means that
;; "space" followed by "b" and then "k" will invoke `M-x kill-buffer'.
;;
;; You can add a descriptions before the command, this will show up in a pop-up
;; when you press the prefix key and wait a bit. (This uses which-key)
;;
;; `("SPC" ("b" ("k" "Choose a buffer to kill" kill-buffer)))'
;;
;; Instead of a prefix key you can use a symbol like `normal' or `insert', which
;; designates the Evil state (what vim calls the mode). `global' means any
;; state, `normal|visual' means either normal or visual.
;;
;; Instead of a command like `kill-buffer' you can put a keyword like
;; `:eval/buffer'. This is called a "signal". In the `corgi-signals' (or
;; `user-signals') file these are bound to specific commands based on the major
;; mode. E.g. in Emacs Lisp `:eval/buffer' means `eval-buffer', whereas in
;; Clojure it means `cider-eval-buffer'.

(bindings
 ;; "global" bindings are always active regardless of Evil's "state" (= vim mode)
 ;; If you don't provide this the default is `normal'.
 (global
  ;; ("M-x" "meta-x" counsel-M-x)
  )

 (motion
  (":" "Swap" evil-repeat-find-char)
  (";" "Swap" evil-ex))

 ;; Bindings for commands are usually only active in normal and visual state.
 (normal|visual
  ;; ("I" "Start insert on forms" evil-cp-insert-at-beginning-of-form)
  ("\\" "emacs mode" evil-execute-in-emacs-state)
  ("*" "search word swap" evil-search-unbounded-word-forward)
  ("g" "goto"
   ("*" "search word swap" evil-search-word-forward)
   ("d" "Find definition" lsp-find-definition)
   ("r" "Find ref" lsp-find-references)
   ("D" "Find UI definition" lsp-ui-peek-find-definitions)
   ("R" "Find ref" lsp-ui-peek-find-references)
   )

  ("[" "unimpared behind"
   ("c" "Prev hunk of git change" git-gutter:previous-hunk)
   ("m" "Prev merge conflict" smerge-prev)
   )

  ("]" "unimpared ahead"
   ("c" "Next hunk of git change" git-gutter:next-hunk)
   ("m" "Next merge conflict" smerge-next)
   )

  ("<backspace>" "Switch to previous buffer" corgi/switch-to-previous-buffer)

  ;; ("s" "Jump" avy-goto-char-2)


  ("z" "zzzzz"
   ;; ("z" "Fold toggle" evil-toggle-fold)
   )

  ("SPC" "leader"
   ("." "Find file from point" find-file)
   ("a" "End append on forms" evil-cp-insert-at-end-of-form)
   ("0" "Select Treemacs" treemacs-select-window)
   ("1" "Harpoon 1" harpoon-go-to-1)
   ("2" "Harpoon 2" harpoon-go-to-2)
   ("3" "Harpoon 3" harpoon-go-to-3)
   ("4" "Harpoon 4" harpoon-go-to-4)
   ("5" "Harpoon 5" harpoon-go-to-5)
   ("6" "Harpoon 6" harpoon-go-to-6)
   ("7" "Harpoon 7" harpoon-go-to-7)
   ("8" "Harpoon 8" harpoon-go-to-8)
   ("=" "Zoom in" default-text-scale-increase)
   ("-" "Zoom out" default-text-scale-decrease)
   ("[" "Prev error" flycheck-previous-error)
   ("]" "Next error" flycheck-next-error)
   ;; ("`" "Switch to from cider" :switch-to-from-cider-repl)

   ("b" "Buffer commands"
    ("b" "Switch buffer" consult-buffer))

   ("i" "inspect"
    ("r" "last result" cider-inspect-last-result))

   ("c" "clojure"
    ("a" "add arity" clojure-add-arity)
    ("p" "run python" ox/run-python-on-current-file)
    ("c" "Recompile" recompile))

   ("d" "docs for now"
    ("c" "clojuredocs" clojuredocs-lookup))

   ("f"
    ("f" "Projectile file" projectile-find-file)
    ("t" "Turn Treemacs on/off" treemacs)
    ("T" "Focus current file in file tree" treemacs-find-file)
    ("e" "edit"
     ("o" "init.el" ox/open-init-el)))

   ("g" "Git"
    ("s" "Magit Status" magit-status)
    ("." "Magit file dispatch" magit-file-dispatch)
    ("r" "Git repo home" git-link-homepage)
    ("l" "Git repo link" git-link)
    ("g" "Git status" magit-status)
    ("l" "Keep lower merge conflict" smerge-keep-lower)
    ("u" "Keep upper merge conflict" smerge-keep-upper)
    ("w" "Emojify" emojify-insert-emoji)
    ("e" "Gitemoji" gitmoji-insert-emoji)
    )

   ("j" "Journal / jump"
    ("j" "journal" org-journal-new-entry)
    ("o" "journal open" ox/journal-open-dir)
    ("d" "Discord log" ox/journal-discord-gaiwan)
    ("s" "slack log" ox/journal-slack-gaiwan)
    ("f" "Harpoon toogle file" harpoon-toggle-file)
    ("a" "Harpoon add file" harpoon-add-file)
    ("c" "Harpoon clear" harpoon-clear)
    )

   ("l" "lsp"
    ("l" "lsp inline completion" lsp-inline-completion-display)
    )

   ("p" "projectile"
    ("r" "refresh projects" ox/refresh-projects-dir))

   ("r" "(w)rap"
    ("r" "round insert" ox/open-round-insert)
    ("w" "wrap" paredit-wrap-round)
    ("(" "wrap" paredit-wrap-round)
    ("[" "wrap square" paredit-wrap-square)
    ("{" "wrap curly" paredit-wrap-curly))

   ("t" "Toggle"
    ("t" "parens toggle" ox/toggle-parens))

   ("y" "Yank"
    ("p" "Yank pop list" consult-yank-pop))

   ("o" "Go to other window" other-window)

   ("O" "Open"
    ("u" "Open URL at point" browse-url-at-point)
    ("s" "Edit string at point" string-edit-at-point))

   ("w" "Windows"
    ("w" "Go to other window" other-window)
    ("r" "Rotate / swap windows" window-swap-states)
    ("s" "Rotate / swap windows" window-swap-states))

   ("e" "Evaluate expressions"
    ("d" "Eval defun at point" :eval/outer-sexp)
    ("D" "Eval defun at point to a comment" :eval/outer-sexp-comment)
    ("t" "Eval defun at point and run test" ox/cider-eval-defun-at-point-and-run-test)
    ("b" "Eval buffer" :eval/buffer)
    ("e" "Eval form before cursor" :eval/last-sexp)
    ("p" "Eval and pretty print" :eval/last-sexp-pprint)
    ;; TODO: make this consistent, in clojure buffers it prints to a comment, in elisp it inserts directly
    ("P" "Eval to comment" :eval/last-sexp-pprint-comment)
    ("n" "Eval ns form" :eval/ns-form)
    ("r" "Eval region" :eval/region)
    ("i" "Interrupt eval" :eval/interrupt)
    ("-" "Eval up to point" :eval/up-to-point))

   ("SPC" "Switch buffer" consult-buffer)
   ("/" "Search in project" consult-ripgrep)
   ("9" "Clever slurp backwd" evil-cp-<)
   ("0" "Clever barf backwd" evil-cp->)
   ("RET" "Resume last sess" ivy-resume)
   ("." "Find file" :file/open)
   ("DEL" "Last buffer" corgi/switch-to-previous-buffer)

   )))
