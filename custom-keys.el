(normal|visual
 ("SPC" "Global leader"
  ("SPC" "Switch buffer" ivy-switch-buffer)
  ("." "Find file" :file/open)
  ("0" "Clever slurp fwd" evil-cp->)
  ("9" "Clever slurp backwd" evil-cp-<)
  ("a" "Insert at end of form" evil-cp-insert-at-end-of-form)
  ("e" "Evaluate expressions"
   ("d" "Eval defun at point" :eval/outer-sexp))
  ("f" "File commands"
   ("f" "Find file" projectile-find-file))
  ("g" "Git commands"
   ("g" "Git status" magit-status)))
 )