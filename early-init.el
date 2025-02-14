(message "Early init loading...")

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-to-list 'default-frame-alist '(undecorated-round . t))

(setq package-enable-at-startup nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 33554432 ; 32mb
                  gc-cons-percentage 0.1)
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(message "Early init finished loading...")

(provide 'early-init)
