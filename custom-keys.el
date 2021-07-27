(normal|visual
 ("g" "g commands"
  ("c" "comment region" evil-commentary))
 ("SPC" "Global leader"
  ("o" "other window" other-window)
  ("RET" "resume last sess" ivy-resume)
  ("SPC" "Switch buffer" ivy-switch-buffer)
  ("." "Find file" :file/open)
  ("r" "(w)rap"
   ("r" "open" ox/open-round-insert)
   ("w" "wrap" paredit-wrap-round))
  ("0" "Clever slurp fwd" evil-cp->)
  ("9" "Clever slurp backwd" evil-cp-<)
  ("a" "Insert at end of form" evil-cp-insert-at-end-of-form)
  ("/" "Search in project" counsel-projectile-rg)
  ("y" "Yank"
   ("p" "pop" counsel-yank-pop))
  ("t" "toggles"
   ("t" "parens" ox/toggle-parens))
  ("e" "Evaluate expressions"
   ("e" "Eval form before cursor" :eval/last-sexp)
   ("t" "Eval and run test" ox/cider-eval-defun-at-point-and-run-test)
   ("p" "Eval and pretty print" :eval/last-sexp-pprint)
   ("P" "Eval to comment" :eval/last-sexp-pprint-comment)
   ("n" "Eval ns form" :eval/ns-form)
   ("i" "Interrupt eval" :eval/interrupt)
   ("d" "Eval defun at point" :eval/outer-sexp)
   ("D" "Eval defun at point to comment" :eval/outer-sexp-comment)
   ("f" "Eval list at point" :eval/list-at-point))
  ("g" "Git commands"
   ("r" "Git repo" git-link-homepage)
   ("l" "Git repo" git-link)
   ("g" "Git status" magit-status))
  ("`" "Switch to repl in same window" ox/cider-switch-to-repl-buffer-same-window-force)
  ("f" "File commands"
   ("f" "Find file" projectile-find-file)))

 ("s" "Avy jump sneak" evil-avy-goto-char-timer)

 ("," "Local leader"
  ("," "Switch to repl in same window" ox/cider-switch-to-repl-buffer-same-window-force))
 (normal|visual|insert
  ("M-x" "meta-x" counsel-M-x))

 )
