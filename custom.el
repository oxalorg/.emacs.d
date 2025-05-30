(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2aa073a18b2ba860d24d2cd857bcce34d7107b6967099be646d9c95f53ef3643"
     "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6"
     "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf"
     "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4"
     "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae"
     default))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((corgi-bindings :url
		     "https://github.com/corgi-emacs/corgi-packages.git"
		     :lisp-dir "corgi-bindings/")
     (pprint-to-buffer :url "https://github.com/plexus/plexmacs.git"
		       :lisp-dir "pprint-to-buffer/")
     (tsx-mode :url "https://github.com/orzechowskid/tsx-mode.el.git")
     (css-in-js-mode :url
		     "https://github.com/orzechowskid/tree-sitter-css-in-js.git")
     (html-to-hiccup :url
		     "https://github.com/plexus/html-to-hiccup.git")
     (corkey :url "https://github.com/corgi-emacs/corkey.git")
     (clj-ns-name :url
		  "https://github.com/corgi-emacs/clj-ns-name.git")
     (walkclj :url "https://github.com/corgi-emacs/walkclj.git")))
 '(safe-local-variable-values
   '((cider-save-file-on-load)
     (elisp-lint-indent-specs (if-let* . 2) (when-let* . 1)
			      (let* . defun)
			      (nrepl-dbind-response . 2)
			      (cider-save-marker . 1)
			      (cider-propertize-region . 1)
			      (cider-map-repls . 1)
			      (cider--jack-in . 1)
			      (cider--make-result-overlay . 1)
			      (insert-label . defun)
			      (insert-align-label . defun)
			      (insert-rect . defun) (cl-defun . 2)
			      (with-parsed-tramp-file-name . 2)
			      (thread-first . 0) (thread-last . 0)
			      (transient-define-prefix . defmacro)
			      (transient-define-suffix . defmacro))
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (projectile-test-suffix . "_test")
     (eval with-eval-after-load 'clojure-mode
	   (define-clojure-indent (assoc 0) (ex-info 0)))
     (eval progn
	   (setenv "OTEL_EXPORTER_OTLP_HEADERS"
		   "x-honeycomb-team=0h2EYmxmhbiGe4tfm6W6wH,x-honeycomb-dataset=grid-metrics")
	   (make-variable-buffer-local
	    'cider-jack-in-nrepl-middlewares)
	   (add-to-list 'cider-jack-in-nrepl-middlewares
			"shadow.cljs.devtools.server.nrepl/middleware"))
     (eval define-clojure-indent (reg-cofx :defn) (reg-event-db :defn)
	   (reg-event-fx :defn) (reg-fx :defn) (reg-sub :defn)
	   (reg-event-domain :defn) (reg-block-event-fx :defn)
	   (reg-event-domain-fx :defn) (this-as 0))
     (eval define-clojure-indent (for! 1) (assoc 0) (ex-info 0))
     (eval define-clojure-indent (for! 0) (assoc 0) (ex-info 0))
     (eval define-clojure-indent (for! 2) (assoc 0) (ex-info 0))
     (cider-auto-track-ns-form-changes)
     (eval progn (make-local-variable 'process-environment)
	   (setenv "XTDB_ENABLE_BYTEUTILS_SHA1" "true"))
     (eval define-clojure-indent (assoc 0) (ex-info 0))
     (eval progn
	   (make-variable-buffer-local
	    'cider-jack-in-nrepl-middlewares)
	   (add-to-list 'cider-jack-in-nrepl-middlewares
			"shadow.cljs.devtools.server.nrepl/middleware"))
     (cider-repl-display-help-banner))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
