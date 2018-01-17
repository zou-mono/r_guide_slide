\NeedsTeXFormat{LaTeX2e}[1994/12/01]
\ProvidesPackage{beamerthemeMono}[2017/12/20]
\LoadClass[xcolor=x11names,notheorems,10pt,table,hyperref={pdfpagelabels=false}]{beamer}

\usetheme[
%%% options passed to the outer theme
%    hidetitle,           % hide the (short) title in the 
    hideauthor,          % hide the (short) author in the sidebar
    hideinstitute,       % hide the (short) institute in the bottom of the sidebar
%    shownavsym,          % show the navigation symbols
    width=1.5cm,           % width of the sidebar (default is 2 cm)
    hideothersubsections,% hide all subsections but the subsections in the current section
%    hideallsubsections,  % hide all subsections
   left                % right of left position of sidebar (default is right)
]{Aalborg}

\usecolortheme{orchid} 

\RequirePackage{subfig} %%图形或表格并排排列
\RequirePackage{algorithm}
\RequirePackage{calc}
%\RequirePackage[linesnumbered,boxruled]{algorithm2e}
\RequirePackage{csquotes}
\RequirePackage{colortbl,dcolumn}     %% 彩色表格
\RequirePackage{longtable,booktabs,multirow,tabularx}
\RequirePackage{bbding} %手势
\RequirePackage[timeinterval=10]{tdclock}  %时钟宏包

\RequirePackage{xeCJK}
%\RequirePackage[usenames,dvipsnames,svgnames,x11names]{xcolor}
\RequirePackage{graphicx}
\RequirePackage{fancybox}
\RequirePackage[utf8]{inputenc}
\RequirePackage[english]{babel}
\RequirePackage[T1]{fontenc}
% ... or whatever. Note that the encoding and the font should match.
% If T1 does not look nice, try deleting the line with the fontenc.
\RequirePackage{lmodern} %optional
\RequirePackage{listings}
\RequirePackage{wasysym}
\RequirePackage{amsmath,amssymb,bm}
\RequirePackage[style=numeric-comp,citetracker=true,sorting=none]{biblatex}

\beamertemplatetransparentcovereddynamic
\beamertemplatetransparentcovereddynamicmedium
\beamertemplateballitem
\beamertemplatenumberedballsectiontoc
\beamertemplateboldpartpage
\setbeamertemplate{section}{square}
\setbeamertemplate{subsection}{square}
\setbeamercolor{myalertcolor}{fg=red,bg=pink}
\setbeamercolor{block title alerted}{fg=white,bg=red!85}
\setbeamercolor{block body alerted}{bg=red!20,fg=black}

%%%%%%%% User Specified Commands %%%%%%%%
\setbeamercolor{alerted text}{fg=red!2!green!35!blue}
\newenvironment{boxalertenv}{\begin{altenv}%
  {\usebeamertemplate{alerted text begin}\usebeamercolor[fg]
    {alerted text}\usebeamerfont{alerted text}\colorbox{bg}}
  {\usebeamertemplate{alerted text end}}{\color{.}}{}}{\end{altenv}}
\newcommand<>{\boxalert}[1]{{%
  \begin{boxalertenv}#2{#1}\end{boxalertenv}%
}}

\newenvironment{variableblock}[3]{%
  \setbeamercolor{block body}{#2}
  \setbeamercolor{block title}{#3}
  \begin{block}{#1}}{\end{block}}

\renewcommand{\raggedright}{\leftskip=0pt \rightskip=0pt plus 0cm}
\raggedright %% 中文对齐

%% 自定义命令, 源自 beamer_guide. item 逐步显示时, 使将要出现的item、正在显示的item、 已经出现的item呈现不同颜色.
\def\hilite<#1>{\temporal<#1>{\color{gray!30}}{\color{magenta}}{\color{black}}}

%\newcolumntype{H}{>{\columncolor{blue!20}}c!{\vrule}}
\newcolumntype{H}{>{\columncolor{blue!20}}c}  %% 表格设置


%%让单元格垂直居中
\newcommand{\PreserveBackslash}[1]{\let\temp=\\#1\let\\=\temp}
\newcolumntype{C}[1]{>{\PreserveBackslash\centering}p{#1}}

%公式断行
\allowdisplaybreaks

% 设置脚注
% Code for placing the footnote above the navigiation symbols
\addtobeamertemplate{footnote}{\vspace{-6pt}\advance\hsize-.5cm}{\vspace{6pt}}
% Alternative A: footnote rule
\renewcommand*{\footnoterule}{\kern -3pt \hrule \@width 2in \kern 8.6pt}
\setbeamerfont{footnote}{size=\scriptsize}

%% 定义表格单元格内换行
\newcommand{\tabincell}[2]{\begin{tabular}{@{}#1@{}}#2\end{tabular}}
%%单元格垂直居中
\newcommand{\mgape}[1]{\gape{$\vcenter{\hbox{#1}}$}}

%解决columns里面用itemize顶部无法对齐
\define@key{beamerframe}{t}[true]{% top
  \beamer@frametopskip=0cm plus .1\paperheight\relax%
  \beamer@framebottomskip=0pt plus 1fill\relax%
  \beamer@frametopskipautobreak=\beamer@frametopskip\relax%
  \beamer@framebottomskipautobreak=\beamer@framebottomskip\relax%
  \def\beamer@initfirstlineunskip{}%
}

% colored hyperlinks
\newcommand*{\chref}[2]{%
  \href{#1}{{\usebeamercolor[bg]{Aalborg}#2}}
}

%%%%%%%% 定义颜色 %%%%%%%%
\definecolor{LightCyan}{rgb}{0.88,1,1} 
\definecolor{listinggray}{gray}{0.9}
\definecolor{lbcolor}{rgb}{0.9,0.9,0.9}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 设置中文字体
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\setCJKfamilyfont{songti}{SimSun}  
\setCJKfamilyfont{heiti}{SimHei}  
\setCJKfamilyfont{kaishu}{KaiTi}  
\setCJKfamilyfont{fangsong}{FangSong}
\setCJKfamilyfont{wenquanyi}{WenQuanYi Micro Hei}

\newcommand*{\songti}{\CJKfamily{songti}} % 宋体  
\newcommand*{\heiti}{\CJKfamily{heiti}}   % 黑体  
\newcommand*{\kaishu}{\CJKfamily{kaishu}}  % 楷书  
\newcommand*{\fangsong}{\CJKfamily{fangsong}} % 仿宋  
\newcommand*{\wenquanyi}{\CJKfamily{wenquanyi}}    % 文泉驿   
\newcommand*{\emphText}[1]{\textcolor{red}{\kaishu{#1}}}
\newcommand*{\emphTextStep}[2]{\textcolor<#1>{red}{\kaishu{#2}}}

\setCJKmainfont[BoldFont={SimHei}]{KaiTi}  
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 设置字号
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand{\chuhao}{\fontsize{42pt}{\baselineskip}\selectfont}
\newcommand{\xiaochuhao}{\fontsize{36pt}{\baselineskip}\selectfont}
\newcommand{\yihao}{\fontsize{28pt}{\baselineskip}\selectfont}
\newcommand{\erhao}{\fontsize{21pt}{\baselineskip}\selectfont}
\newcommand{\xiaoerhao}{\fontsize{18pt}{\baselineskip}\selectfont}
\newcommand{\sanhao}{\fontsize{15.75pt}{\baselineskip}\selectfont}
\newcommand{\sihao}{\fontsize{14pt}{\baselineskip}\selectfont}
\newcommand{\xiaosihao}{\fontsize{12pt}{\baselineskip}\selectfont}
\newcommand{\wuhao}{\fontsize{10.5pt}{\baselineskip}\selectfont}
\newcommand{\xiaowuhao}{\fontsize{9pt}{\baselineskip}\selectfont}
\newcommand{\liuhao}{\fontsize{7.875pt}{\baselineskip}\selectfont}
\newcommand{\xiaoliuhao}{\fontsize{6.05pt}{\baselineskip}\selectfont}
\newcommand{\qihao}{\fontsize{5.25pt}{\baselineskip}\selectfont}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 设置标题和图表
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\RequirePackage{caption}
\DeclareCaptionFont{subtitle}{\tiny}
\captionsetup[figure]{font=rm,belowskip=-5pt, textfont=small, position=bottom, name=图}
\captionsetup[table]{font=rm, belowskip=0pt, textfont=small, position=bottom, singlelinecheck=false, justification=centering, name=表}
\captionsetup[subfigure]{labelfont=small, textfont=small, position=bottom,justification=centering}
\captionsetup[subtable]{labelfont=small, textfont=small,position=bottom, farskip=2pt,captionskip=0pt,topadjust=-5pt, nearskip=2pt,margin=0pt}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 设置自定义环境
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newtheoremstyle{definition}% name
  {1pt}%      Space above
  {1pt}%      Space below
  {\footnotesize}%         Body font
  {}%         Indent amount (empty = no indent, \parindent = para indent)
  {\footnotesize}% Thm head font
  {:}%        Punctuation after thm head
  {.5em}%     Space after thm head: " " = normal interword space;
        %       \newline = linebreak
  {}%         Thm head spec (can be left empty, meaning `normal')

\theoremstyle{definition}
\newtheorem{mynote}{Note}
\newtheorem{definition}{定义}
\newtheorem{theorem}{定理}
\newtheorem{lemma}{引理}
\newtheorem{corollary}{推论}
\newtheorem{proposition}{性质}
\newtheorem{example}{例}
\newtheorem{remark}{注}

%%%%%%%%%%% 设置表格单元格距上下线的距离，确保\rowcolor不出问题
\setlength{\aboverulesep}{0pt} 
\setlength{\belowrulesep}{0pt}
\setlength{\extrarowheight}{.75ex}

%%%%%%%%%%% 装饰框
\RequirePackage[object=vectorian]{pgfornament}
\RequirePackage[framemethod=tikz]{mdframed}
\RequirePackage{tcolorbox}
\usetikzlibrary{shapes.geometric,calc}
\newlength{\picwidth}
\setlength{\picwidth}{0.5cm}
\newmdenv[%
  hidealllines=true,
  innertopmargin=\picwidth,
  innerbottommargin=\picwidth,
  singleextra={%
    \node[anchor=north west] 
      at (O|-P) {\pgfornament[width=\picwidth]{41}};
    \node[anchor=north east] 
      at (P)  {\pgfornament[width=\picwidth]{42}};
    \node[anchor=south west] 
      at (O) {\pgfornament[width=\picwidth,symmetry=h]{41}};
    \node[anchor=south east] 
      at (O-|P)  {\pgfornament[width=\picwidth,symmetry=h]{42}};
  }
]{ornamentblock}

%%%%%%%%%% 代码高亮
\definecolor{codegreen}{rgb}{0,0.6,0}
\definecolor{codegray}{rgb}{0.5,0.5,0.5}
\definecolor{codepurple}{rgb}{0.58,0,0.82}
\definecolor{backcolour}{rgb}{0.95,0.95,0.92}

\lstdefinestyle{mystyle}{
    %literate={-}{{{\color{blue}-}}}1,
    backgroundcolor=\color{backcolour},   
    commentstyle=\color{codegreen},
    keywordstyle=\color{magenta},
    keywordstyle=[2]{\color{blue}},
    numberstyle=\tiny\color{codegray},
    stringstyle=\color{codepurple},
    basicstyle=\ttfamily\scriptsize,
    otherkeywords={!,!=,~,\$,*,\%,\&,<-,<<-,_,/},%
    morekeywords=[2]{!,!=,~,\$,*,\%,\&,<-,<<-,_,/},
    breakatwhitespace=false,         
    breaklines=true,                 
    captionpos=b,                    
    keepspaces=true,                 
    numbers=left,                    
    numbersep=5pt,                  
    showspaces=false,                
    showstringspaces=false,
    showtabs=false,                  
    tabsize=4
}
\lstset{style=mystyle}
\lstnewenvironment{rc}[1][]{\lstset{language=R,style=mystyle}}{}

\newtcolorbox{goodbox}[1]{
    colback=green!20,
    colframe=green!75!black,
    coltitle=white,
    coltext=black,
    %fonttitle=\bfseries
    title=#1
}
\newtcolorbox{badbox}[1]{
    colback=red!20,
    colframe=red!75!black,
    coltitle=white,
    coltext=black,
    title=#1
}

% \lstset{% 
%     basicstyle=\small\ttfamily\bfseries,
%     columns=flexible,
%     language=SAS,
%     keywordstyle=\color{blue}\bfseries,
%     commentstyle=\color{green},
%     morecomment=[f]{*},
%     morecomment=[s]{/*}{*/},
%     morecomment=[n]{/*}{*/},  
%     escapechar=|,
%     otherkeywords={!,!=,~,$,\&,_,<,>=,=<,>},
%     stringstyle=\color[rgb]{0.639,0.082,0.082}\ttfamily,
%     showstringspaces=false,
%     keepspaces=true,
%     sensitive=false,
% }