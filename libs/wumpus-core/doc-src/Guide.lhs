\documentclass{article}

\usepackage{comment}
\usepackage{amssymb}
\usepackage{alltt}
\usepackage[dvips]{graphicx}

\newcommand{\wumpuscore}{\texttt{wumpus-core} }

\begin{document}

\title{\wumpuscore Guide}
\author{Stephen Tetley}
\maketitle

%-----------------------------------------------------------------




%-----------------------------------------------------------------
\section{About \wumpuscore}
%-----------------------------------------------------------------

\wumpuscore is a Haskell library for generating 2D vector 
pictures. It was written with portability as a priority, so it has 
no dependencies on foreign (i.e. C) libraries. It supports output 
to PostScript and SVG (Scalable Vector Graphics). 

\wumpuscore is rather primitive, the basic drawing objects are 
paths and text labels. A secondary library \texttt{wumpus-extra}
has been prototyped containing some higher level objects 
(arrowheads, etc.), but hasn't been officially released - the code 
needs more thought before being put into the wild. Previews are 
available from the \texttt{copperbox} project repository 
hosted by Googlecode.

Although \wumpuscore is heavily inspired by PostScript it avoids 
PostSript's notion of an (implicit) current point and the 
movements \texttt{lineto}, \texttt{moveto} etc., instead 
\wumpuscore aims for a more \emph{coordinate free} style.


%-----------------------------------------------------------------
\section{Drawing model}
%-----------------------------------------------------------------

\includegraphics{WorldFrame.eps}

The most primitive objects are \emph{points} and \emph{vectors} 
to displace them. Points are used to describe \emph{paths} which 
are made from stright line segments or cubic Bezier curves. Paths 
can be \emph{stroked} (drawing the outline), \emph{filled} 
(colouring in the shape bounded by the path) or \emph{clipped} 
which turns the path into a mask to crop pictures with. 
\emph{Labels} contain printed text. Labels and stoked, filled 
or clipped paths are considered to be \emph{primitives}, which 
are aggregated to make \emph{pictures}. Pictures themselves can 
be aggregated to making composite pictures (Wumpus-core represents 
pictures as a tree, branches can contain one or more pictures, 
leaves contain primitives).
 


%-----------------------------------------------------------------
\section{PostScript SVG}
%-----------------------------------------------------------------

\begin{tabular}{l l}
[ $e_{0}X$ $e_{0}Y$ $e_{1}X$ $e_{1}Y$ $oX$ $oY$ ] concat & 
matrix($e_{0}X$, $e_{0}Y$, $e_{1}X$, $e_{1}Y$, $oX$, $oY$) \\
$x$ $y$ moveto & M $x$ $y$ \\
$x$ $y$ lineto & L $x$ $y$ \\
$x_{1}$ $y_{1}$ $x_{2}$ $y_{2}$ $x_{3}$ $y_{3}$ curveto  & 
C $x_{1}$ $y_{1}$ $x_{2}$ $y_{2}$ $x_{3}$ $y_{3}$ \\
\end{tabular}




%-----------------------------------------------------------------
\section{References}
%-----------------------------------------------------------------

PostScript is a registered trademark of Adobe Systems Inc.

\end{document}
