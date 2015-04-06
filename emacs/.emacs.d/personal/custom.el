;;; package --- Summary

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-electric-left-right-brace nil)
 '(TeX-electric-escape nil)
 '(TeX-electric-math nil)
 '(TeX-insert-braces nil)
 '(ac-use-fuzzy t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" default)))
 '(electric-indent-mode nil)
 '(electric-pair-mode nil)
 '(exec-path
   (quote
    ("/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/usr/lib/emacs/24.4/x86_64-unknown-linux-gnu" "/home/anthony/.cabal/bin")))
 '(fci-rule-color "#383838")
 '(font-latex-fontify-script nil)
 '(font-latex-math-environments
   (quote
    ("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align" "alignat" "xalignat" "xxalignat" "flalign" "IEEEeqnarray")))
 '(haskell-doc-prettify-types t)
 '(haskell-mode-hook
   (quote
    (imenu-add-menubar-index turn-on-haskell-decl-scan turn-on-haskell-indentation
                             #[nil "\300\301!\207"
                                   [run-hooks prelude-haskell-mode-hook]
                                   2])))
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save nil)
 '(indent-tabs-mode nil)
 '(js2-basic-offset 2)
 '(js3-auto-indent-p nil)
 '(js3-compact nil)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-continued-expr-mult 0)
 '(js3-enter-indents-newline t)
 '(js3-global-externs (quote ("require" "process" "module" "console")))
 '(js3-indent-dots t)
 '(js3-lazy-dots nil)
 '(menu-bar-mode t)
 '(nxml-child-indent 4)
 '(preview-scale-function 1.3)
 '(preview-transparent-color (quote (highlight :background)))
 '(sp-cancel-autoskip-on-backward-movement nil)
 '(tab-always-indent (quote complete))
 '(tab-stop-list nil)
 '(tab-width 4)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-closing t)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-auto-opening t)
 '(web-mode-enable-auto-quoting t)
 '(web-mode-enable-engine-detection t)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-sql-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Hooks for every programming language
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'conf-mode-hook 'rainbow-mode)

;; My Haskell hooks
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-hi2)
;;(add-hook 'haskell-mode-hook #'hindent-mode)

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))


;; Mutt hooks
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

(set-frame-font "inconsolatazi4")

(provide 'custom)

;;; custom.el ends here
