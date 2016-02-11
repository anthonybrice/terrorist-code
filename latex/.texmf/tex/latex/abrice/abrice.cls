\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{abrice}[2015/02/21 My custom Tufte variant]

\LoadClass{tufte-handout}

\renewcommand{\maketitle}{%
  \newpage
  \global\@topnum\z@% prevent floats from being placed at the top of the page
  \begingroup
  \setlength{\parindent}{0pt}
  \setlength{\parskip}{4pt}
  \ifthenelse{\boolean{@tufte@sfsidenotes}}
    {\begingroup
      % FIXME fails with \thanks
      \sffamily
      \par{\LARGE\allcaps{\@title}}
      \ifthenelse{\equal{\@author}{}}{}{\par{\Large\allcaps{\@author}}}
      \ifthenelse{\equal{\@date}{}}{}{\par{\Large\allcaps{\@date}}}
    \endgroup}
    {\begingroup
      \sffamily
      \par{\LARGE{\@title}}
      \ifthenelse{\equal{\@author}{}}{}{\par{\Large{\@author}}}
      \ifthenelse{\equal{\@date}{}}{}{\par{\Large{\@date}}}
    \endgroup}
  \par
  \endgroup
  \thispagestyle{plain}% suppress the running head
}

\titleformat{\section}%
  [hang]% shape
  {\normalfont\Large\sffamily}% format applied to label+text
  {\thesection}% label
  {1em}% horizontal separation between label and title body
  {}% before the title body
  []% after the title body

\titleformat{\subsection}%
  [hang]% shape
  {\normalfont\large\sffamily}% format applied to label+text
  {\thesubsection}% label
  {1em}% horizontal separation between label and title body
  {}% before the title body
  []% after the title body

\titleformat{\paragraph}%
  [runin]% shape
  {\normalfont\itshape}% format applied to label+text
  {\theparagraph}% label
  {1em}% horizontal separation between label and title body
  {}% before the title body
  []% after the title body

\RequirePackage{graphicx} % allow embedded images
\setkeys{Gin}{width=\linewidth,totalheight=\textheight,keepaspectratio}
% \graphicspath{{graphics/}} % set of paths to search for images
\RequirePackage{amsmath, amsthm, amssymb, amsfonts}  % extended mathematics
\RequirePackage{IEEEtrantools} % for fancy equations
\RequirePackage{booktabs} % book-quality tables
\RequirePackage{units}    % non-stacked fractions and better unit spacing
\RequirePackage{multicol} % multiple column layout facilities
\RequirePackage{multirow} % ???
\RequirePackage{lipsum}   % filler text
\RequirePackage{fancyvrb} % extended verbatim environments
  \fvset{fontsize=\normalsize}% default font size for fancy-verbatim
                              % environments
\RequirePackage[inline]{enumitem} % for fancy lists

\RequirePackage{wasysym} % for that creepy blank-expression smiley

\RequirePackage[T1]{fontenc}
\RequirePackage[boldsans]{ccfonts}
\RequirePackage[euler-digits,euler-hat-accent]{eulervm}
\RequirePackage{cabin}
\RequirePackage[ activate={true,nocompatibility},final,tracking=true,%
  kerning=true,spacing=]{microtype}
%\renewcommand{\textbf}{\textsf}
\RequirePackage{caption}
\RequirePackage{newfloat}
\RequirePackage[newfloat]{minted}
%\captionsetup[listing]{position=below}
\usemintedstyle{bw}
\geometry{textwidth=.533\paperwidth}
\setminted{bgcolor=LightGray,fontsize=\footnotesize,frame=leftline,linenos}
\renewcommand{\theFancyVerbLine}{\ttfamily {\scriptsize \arabic{FancyVerbLine}}}
\RequirePackage{inconsolata}

% Standardize command font styles and environments
\newcommand{\doccmd}[1]{\texttt{\textbackslash#1}}% command name -- adds backslash automatically
\newcommand{\docopt}[1]{\ensuremath{\langle}\textrm{\textit{#1}}\ensuremath{\rangle}}% optional command argument
\newcommand{\docarg}[1]{\textrm{\textit{#1}}}% (required) command argument
\newcommand{\docenv}[1]{\textsf{#1}}% environment name
\newcommand{\docpkg}[1]{\texttt{#1}}% package name
\newcommand{\doccls}[1]{\texttt{#1}}% document class name
\newcommand{\docclsopt}[1]{\texttt{#1}}% document class option name
\newenvironment{docspec}{\begin{quote}\noindent}{\end{quote}}
% command specification environment

\newcommand{\e}[1]{\ensuremath{\times 10^{#1}}} % Macro for scientific
                                                % notation

\DeclareMathOperator{\Log}{Log}
\DeclareMathOperator{\Arg}{Arg}
\let\Im\relax
\DeclareMathOperator{\Im}{Im}
\let\Re\relax
\DeclareMathOperator{\Re}{Re}
\DeclareMathOperator{\sech}{sech}
\DeclareMathOperator{\csch}{csch}
\DeclareMathOperator{\arcsec}{arcsec}
\DeclareMathOperator{\arccot}{arccot}
\DeclareMathOperator{\arccsc}{arccsc}
\DeclareMathOperator{\arccosh}{arccosh}
\DeclareMathOperator{\arcsinh}{arcsinh}
\DeclareMathOperator{\arctanh}{arctanh}
\DeclareMathOperator{\arcsech}{arcsech}
\DeclareMathOperator{\arccsch}{arccsch}
\DeclareMathOperator{\arccoth}{arccoth}



% Use fancy symbols for footnotes
\RequirePackage{hyperref}
\RequirePackage{natbib}
\renewcommand{\thefootnote}{\fnsymbol{footnote}}%
\RequirePackage{perpage}
\MakePerPage{footnote}
%\usepackage[style=nature,natbib=true,backend=bibtex]{biblatex}

%\RequirePackage{manfnt} % ???

\RequirePackage{mathtools} % for \DeclarePairedDelimiter

\DeclarePairedDelimiter\abs{\lvert}{\rvert}%
\DeclarePairedDelimiter\norm{\lVert}{\rVert}% for nice absolute value
                                            % bars
\DeclarePairedDelimiter\ceil{\lceil}{\rceil}
\DeclarePairedDelimiter\floor{\lfloor}{\rfloor}

% For marginnotes in math floats
\newcommand{\filler}[1][10]%
{   \foreach \x in {1,...,#1}
    {   test
    }
}

\def\mathnote#1{%
  \tag*{\rlap{\hspace\marginparsep\smash{\parbox[t]{\marginparwidth}{%
  \footnotesize#1}}}}
}

%\renewcommand{\descriptionlabel}[1]{\hspace{\labelsep}\textsf{#1}}%

\RequirePackage{tikz}
\usetikzlibrary{shapes.multipart, shapes.geometric, arrows, positioning, fit}

\tikzstyle{startstop} = [rectangle, rounded corners, minimum width=3cm, minimum
height=1cm, text centered, draw=black]
\tikzstyle{io} = [trapezium, trapezium left angle=70, trapezium right angle=110,
minimum width=3cm, minimum height=1cm, text centered, draw=black]
\tikzstyle{process} = [rectangle, minimum width=3cm, minimum height=1cm, text
centered, draw=black]
\tikzstyle{decision} = [diamond, minimum width=3cm, minimum height=1cm, text
centered, draw=black]
\tikzstyle{preprocess} = [rectangle split, rectangle split horizontal, rectangle
split parts=3, minimum height=1cm, draw=black]
\tikzstyle{arrow} = [thick,->,>=stealth]
