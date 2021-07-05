(normal|visual
 ("SPC" "Global leader"
  ("SPC" "Switch buffer" ivy-switch-buffer)
  ("." "Find file" :file/open)
  ("0" "Clever slurp fwd" evil-cp->)
  ("9" "Clever slurp backwd" evil-cp-<)
  ("a" "Insert at end of form" evil-cp-insert-at-end-of-form)
  ("/" "Search in project" counsel-projectile-rg)
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
  ("f" "File commands"
   ("f" "Find file" projectile-find-file)))

 ("s" "Avy jump sneak" evil-avy-goto-char-timer)

 (normal|visual|insert
  ("M-x" "meta-x" counsel-M-x))
 )

