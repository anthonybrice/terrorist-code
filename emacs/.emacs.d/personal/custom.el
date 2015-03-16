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
 '(exec-path
   (quote
    ("/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/usr/lib/emacs/24.4/x86_64-unknown-linux-gnu" "/home/anthony/.cabal/bin")))
 '(font-latex-fontify-script nil)
 '(font-latex-math-environments
   (quote
    ("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align" "alignat" "xalignat" "xxalignat" "flalign" "IEEEeqnarray")))
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-doc
     #[nil "\300\301!\207"
           [run-hooks prelude-haskell-mode-hook]
           2]
     interactive-haskell-mode)))
 '(preview-scale-function 1.3)
 '(preview-transparent-color (quote (highlight :background)))
 '(sp-cancel-autoskip-on-backward-movement nil)
 '(tab-stop-list nil)
 '(tab-width 4)
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

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
