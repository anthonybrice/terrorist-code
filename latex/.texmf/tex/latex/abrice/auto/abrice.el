(TeX-add-style-hook
 "abrice"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("enumitem" "inline") ("fontenc" "T1") ("ccfonts" "boldsans") ("eulervm" "euler-digits" "euler-hat-accent") ("minted" "newfloat")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-environments-local "VerbatimOut")
   (add-to-list 'LaTeX-verbatim-environments-local "SaveVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "Verb")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb")
   (TeX-run-style-hooks
    "tufte-handout"
    "tufte-handout10"
    "graphicx"
    "amsmath"
    "amsthm"
    "amssymb"
    "amsfonts"
    "IEEEtrantools"
    "booktabs"
    "units"
    "multicol"
    "multirow"
    "lipsum"
    "fancyvrb"
    "enumitem"
    "wasysym"
    "fontenc"
    "ccfonts"
    "eulervm"
    "cabin"
    "caption"
    "newfloat"
    "minted"
    "inconsolata"
    "hyperref"
    "natbib"
    "perpage"
    "mathtools"
    "tikz")
   (TeX-add-symbols
    '("filler" ["argument"] 0)
    '("e" 1)
    '("docclsopt" 1)
    '("doccls" 1)
    '("docpkg" 1)
    '("docenv" 1)
    '("docarg" 1)
    '("docopt" 1)
    '("doccmd" 1)
    "Log"
    "Arg"
    "sech"
    "csch"
    "arcsec"
    "arccot"
    "arccsc"
    "arccosh"
    "arcsinh"
    "arctanh"
    "arcsech"
    "arccsch"
    "arccoth"
    "Im"
    "Re"
    "mathnote")
   (LaTeX-add-environments
    "docspec"))
 :latex)

