# Automatic Document Generation

One important application of File I/O is to create documents automatically.
Once a piece of analysis is complete, it is useful to hand it off to another
analyst to run periodically to produce an automated dashboard of measurements.
Furthermore, in a research project, it is often very convenient to
generate documents automatically so that they can be regenerated when the data and
the analytical procedures are adjusted.
Statistical analysis is often an iterative process; it is rare for even the best 
analysts to achieve the perfect answer on the first run. 

## Generating Documents with LaTeX

LaTeX is a document markup language for the TeX typesetting program. 
The minimal document can be created with the following commands in a file that could be called ```Paper.tex```.
For this demonstration, we'll call it ```Paper_0.tex```.

```
\documentclass[11pt]{article}
\begin{document}
This is my document.
\end{document}
```

This program sets the class of the document to ```paper``` but it could also be set to 
```article```, ```book```, ```report``` or ```letter```.
The ```document``` ```begin```s on line 2 and ```end```s on line 4. 
Line 3 is the single sentence that is printed in the document. 

Documents are typically divided into ```sections``` and ```subsections```. 
Elements of a document, such as sections, tables, figures and equations are ```ref```erenced by ```label```s. 
Using programmatic references to components of the document eliminates the need to change reference numbers when the components are put in a different order or
when components are added or removed. 


Now consider the following script, called ```Paper_1.tex```.

```
\documentclass[11pt]{article}
\begin{document}

This is my document.
The introduction is in the beginning in Section \ref{sec:intro}. 
Next is the middle, in Section \ref{sec:middle}. 
We conclude at the end in Section \ref{sec:conc}. 

\section{Introduction} \label{sec:intro}

This is the introduction. 

\section{Middle} \label{sec:middle}

This is the introduction. 

\section{Conclusion} \label{sec:conc}

This is the conclusion. 


\end{document}
```


### Figures

The notation for including figures uses the... ```figure``` environment. 
To display figures, you need to declare the ```graphicx``` package in the preamble 
of the markup script (before the ```\begin{document}``` command). 
Depending on the platform, you might also have to specify how to
translate images in ```eps``` format into ```pdf``` format. 
Using ```eps``` figures is worthwhile because they contain text code for making quick adjustments and the images are rendered more clearly, since
no translation of the images is required. 
To handle cases on different platforms, you can use ```if``` statements, 
much like you can in other languages. 


We add a figure to a script called ```Paper_2.tex```.

```
\documentclass[11pt]{article}

\ifx\pdftexversion\undefined
    \usepackage[dvips]{graphicx}
\else
    \usepackage[pdftex]{graphicx}
    \usepackage{epstopdf}
    \epstopdfsetup{suffix=}
\fi

\begin{document}
This is my document.

In Figure \ref{fig:example}, there is an image. 

\begin{figure}
\centering
\includegraphics[width=\textwidth]{../Figures/name_of_figure.eps}
\caption{Caption Goes Here}
\label{fig:example}
\end{figure}

\end{document}
```


### Tables

A table is added with the ```table``` environment. 
The notation takes some getting used to but the following script was
generated automatically using the ```xtable``` package in ```R```. 
Life is too short to type this sort of thing manually. 


We add a table to a script called ```Paper_3.tex```.

```
\documentclass[11pt]{article}

\begin{document}
This is my document.

In Table \ref{tab:summary}, there are some numbers. 

\begin{table}[ht]
\centering
\begin{tabular}{rlrr}
  \hline
 & Statistic & Variable 1 & Variable 2 \\ 
  \hline
  1 & Min. & 0.19 & 0.08 \\ 
  2 & Mean & 0.70 & 0.10 \\ 
  3 & S.D. & 0.18 & 0.01 \\ 
  4 & Max. & 1.09 & 0.13 \\ 
   \hline
\end{tabular}
\caption{Summary of Numeric Variables} 
\label{tab:summary}
\end{table}


\end{document}
```



A good way of organizing your work when generating documents automatically
is to enter the code for the table in a separate script and
include it in the document using the ```input``` command. 


We import some text and the table in a script called ```Paper_4.tex```.

```
\documentclass[11pt]{article}

\begin{document}

\input{../Text/my_text.tex}

\input{../Tables/my_table.tex}


\end{document}
```
The content within the ```table``` environment is in the file called ```my_table.tex```. 
The ```input``` command can be used to ```input``` all kinds of scripts and is useful for dividing your document into smaller parts.
For example, you could use an ```input``` statement for each section of the document
and write the sections separately. 


### Equations

Where LaTeX shines is in the display of equations. 
My personal conspiracy theory for the apalling lack of quality of
equation renerding in the Windows suite
is that Bill Gates deliberately sabotages any capability for Windows
products to display equations adequately. 
The people at Microsoft could certainly do it but they just don't. 
It's the only possible explanation to prevent them from rendering equations in a manner that is even remotely adequate for mathematical communication. 
Maybe it has to do with dropping out of a math program in college. 
I think about it far too often.

In any case, the simple equation environment is indicated by the dollar sign ```$```.
This is typically used for inline equations, such as 
```$y_i = \beta_0 + \beta_1 x_i + \epsilon_i$```. 

For larger expressions, the ```equation``` environment is preferred:

```
\begin{equation}
y_i = \beta_0 + \beta_1 x_i + \epsilon_i
\end{equation}
```

Entire books have been written about interacting with the ```equation``` environment. Most writers use the Internet. 


### Code Snippets

You can also display the commands you used to generate some output, 
which might be useful to help explain your analysis. 
First, you have to declare a few more packages 
in the preamble before the ```\begin{document}``` command. 

```
% Packages for displaying code:
\usepackage{listings}
\usepackage{textcomp}
\usepackage{color}

% Color settings used in the code below:
\definecolor{dkgreen}{rgb}{0,0.6,0}
\definecolor{gray}{rgb}{0.5,0.5,0.5}
\definecolor{mauve}{rgb}{0.58,0,0.82}

% Settings for the formatting of the code on display:
\lstset{frame=tb,
  language=R,
  aboveskip=3mm,
  belowskip=3mm,
  showstringspaces=false,
  columns=flexible,
  basicstyle={\small\ttfamily},
  numbers=none,
  numberstyle=\tiny\color{gray},
  keywordstyle=\color{blue},
  commentstyle=\color{dkgreen},
  stringstyle=\color{mauve},
  breaklines=true,
  breakatwhitespace=true,
  tabsize=3
}
```
The first is the set of packages to display code, 
the main one being ```listings```. 
The rest are for customized settings, such as the color
and format of the syntax within the code.

A block of R code is displayed with the following commands.

```
\begin{lstlisting}[language=R]
R> # Generate a random variable.
    epsilon <- rnorm(1000)
\end{lstlisting}
```


This is all collected with an example of a figure
in the script ```Paper_5.tex```. 


### File Organization

Some analysts follow the "kitchen-sink" approach to organization of files within a single folder. 
But since we're generating the documents automatically, why not be more organized? 
After all, it takes a little bit of the OCD personality to write documents this way in the first place. 
A sensible approach is to divide files into the following headings: 

- Code
- Data
- Figures
- Paper
- Tables
- Text

This system of file organization is used in the following examples. 

### Examples

The script ```Paper.tex``` contains all of these components.
To build the ```pdf``` file from these scripts, 
you will have to run a program such as ```pdflatex```. 

```
pdflatex Paper.tex
pdflatex Paper.tex
```

This will produce a ```pdf``` document called ```Paper.pdf```, along with some other intermediate files
that are sometimes useful for troubleshooting
(although Google is often more useful for that). 

The command is shown twice because there are references to the tables,
figures and equations in the script. 
The first time it will produce a document with question marks 
in place of all the reference numbers. 
After this accounting exercise, the numbers are filled in
in the second pass. 



In the folders above, there are two examples of end-to-end data analysis that
generates figures and tables for documents in LaTeX. 
One is in 
[LaTeX_from_R](https://github.com/LeeMorinUCF/QMB6911S21/tree/master/LaTeX_from_R)
and the other is in 
[LaTeX_from_python](https://github.com/LeeMorinUCF/QMB6911S21/tree/master/LaTeX_from_python)
and the names are self-explanatory. 
Instructions are in the ```README.md``` files in each folder. 


