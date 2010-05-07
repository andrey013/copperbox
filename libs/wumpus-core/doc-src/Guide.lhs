\documentclass{article}

%include polycode.fmt
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
PostScript's notion of an (implicit) current point and the 
movements \texttt{lineto}, \texttt{moveto} etc., instead 
\wumpuscore aims for a more \emph{coordinate free} style.


%-----------------------------------------------------------------
\section{Exposed modules}
%-----------------------------------------------------------------

\wumpuscore exports the following modules:

\begin{description}
\item[\texttt{Wumpus.Core.}]
Top-level import module, re-exports the exposed modules. Exports 
as opaque some of the internal data types, where the export is 
necessary for writing type signatures to user functions but access
to the objects themselves is hidden by \emph{smart} constructors.

\item[\texttt{Wumpus.Core.AffineTrans.}]
The standard affine transformations (scaling, rotation, 
translation) implemented as type classes, with a of derived 
operations - reflections about the X or Y axes, rotations through
common angles. 

\item[\texttt{Wumpus.Core.BoundingBox.}]
Data type representing bounding boxes and operations on them. 
This module is potentially important for defining higher-level
graphics objects (arrowheads and the like).

\item[\texttt{Wumpus.Core.Colour.}]
Colour types (RGB, grayscale and HSB) and conversion between 
them. Some named colours, which should be hidden or import
qualified if a more extensive package of colours (e.g. the named
SVG colours) is used. RGB is the default format, where black is 
\texttt{(0.0, 0.0, 0.0)}, and white is \texttt{(1.0, 1.0, 1.0)}.

\item[\texttt{Wumpus.Core.FontSize.}]
Various calculations for font size metrics. Generally not useful 
to a user but exposed so that variations of the standard Label 
type are possible.

\item[\texttt{Wumpus.Core.Geometry.}]
Usual types an operations from affine geometry - points, vectors 
and frames. The \texttt{Pointwise} type class which is essential
for defining transformable drawable types. 

\item[\texttt{Wumpus.Core.GraphicsState.}]
Data types modelling the attributes of PostScript's graphics 
state (stroke style, dash pattern, etc.). Note \wumpuscore 
annotates primitives - paths, text labels - with their rendering 
style. PostScript has a mutable graphics state, changing via 
inheritance how the curent object is drawn.

\item[\texttt{Wumpus.Core.OutputPostScript.}]
Functions to write PostScript or encapsulated PostScript files.

\item[\texttt{Wumpus.Core.OutputSVG.}]
Functions to write SVG files.

\item[\texttt{Wumpus.Core.Picture.}]
Operations to build \emph{pictures} - paths and labels within
an affine frame. Type classes overloading convenience 
constructors for building paths, labels, ellipses... The 
constructors are convenient in that attributes - colour, line
width, etc. - may be specified or not. The technique is due to 
Iavor S. Diatchki's XML-Light.

\item[\texttt{Wumpus.Core.PictureLanguage.}]
Composition operators for pictures. The operators are somewhat 
analogue to the usual operators or pretty-printing libraries, 
but work in 2D rather than largely horizontally with some 
vertical concatenation.

\item[\texttt{Wumpus.Core.TextEncoder.}]
Types for handling non-ASCII character codes. This module is
perhaps under-cooked thou it appears adequate for Latin 1...

\item[\texttt{Wumpus.Core.TextLatin1.}]
A instance of the TextEncoder type for mapping Latin 1 characters
to the PostScript and SVG escape characters.
\end{description}

%-----------------------------------------------------------------
\section{Drawing model}
%-----------------------------------------------------------------

\wumpuscore has two main drawable primitives \emph{paths}
and text \emph{labels}, ellipses are also a primitive although 
this is a concession to efficiency when drawing dots (which would 
otherwise require 4 to 8 Bezier arcs to describe). Paths are made 
from straight sections or Bezier curves, they can be open and 
\emph{stroked} to produce a line; or closed and \emph{stroked}, 
\emph{filled} or \emph{clipped}. Labels represent a single 
horizontal line of text - multiple lines must be composed from 
multiple labels.

Primitives are attributed with drawing styles - font name and 
size for labels; line width, colour, etc. for paths - and 
place within a picture. The function \texttt{frame} lifts a 
primitive to a picture within the standard affine frame (the 
standard frame has origin at (0,0) and unit bases for the X and
Y axes). The function \texttt{frameMulti} places one or more 
primitives in a frame - this will produce more efficient 
PostScript and should be preferred for creating scatter-plots 
and the like.

\begin{figure}
\centering
\includegraphics{WorldFrame.eps}
\caption{The world frame, with origin at the bottom left.}
\end{figure}

\wumpuscore uses the same picture frame as PostScript with 
the origin at the bottom left, see Figure 1. This contrasts to SVG 
where the origin at the top-left. When \wumpuscore generates SVG, 
the whole picture is produced within a matrix transformation 
[ 1.0, 0.0, 0.0, -1.0, 0.0, 0.0 ] that changes the picture to use 
PostScript coordinates. This has the side-effect that text is 
otherwise drawn upside down, so \wumpuscore adds a rectifying 
transform to each text element.

Once labels and paths are assembled as a \emph{Picture} they are
transformable with the usual affine transformations (scaling, 
rotation, translation) and multiple pictures can be composed with
the operations provided by the \texttt{PictureLanguage} module.
The operations should be largely familiar from pretty-printing 
libraries although here they are extended to 2 dimensions.

Once assembled into pictures graphics properties (e.g. colour) 
are opaque - it is not possible to write a transformation function
that turns a picture blue. In some ways this is a limitation - 
for instance, the \texttt{Diagrams} library appears to support 
some notion of attribute overriding; however it is conceptually 
simple. If one wanted to make blue arrows or red arrows with 
\wumpuscore one would make colour a parameter of the arrow 
creating function.

%-----------------------------------------------------------------
\section{Affine transformations}
%-----------------------------------------------------------------

For affine transformations Wumpus uses the \texttt{Matrix3} data 
type to represent 3x3 matrices in row-major form. The constructor
 \texttt{(M3'3 a b c  d e f  g h i)} builds this matrix:

\begin{displaymath}
\begin{array}{ccc}
a & b & c\\
d & e & f\\
g & h & i
\end{array}
\end{displaymath}

Note, in practice the elements \emph{g} and \emph{h} are 
superflous. They are included in the data type to make it match 
the typical representation from geometry texts. Also, typically 
matrices will implicitly created with functions from the 
\texttt{Core.Geometry} and \texttt{Core.AffineTrans} modules.

For example a translation matrix moving 10 units in the X-axis and
20 in the Y-axis will be encoded as 
 \texttt{(M3'3 1.0 0.0 10.0   0.0 1.0 20.0  0.0  0.0 1.0)}

\begin{displaymath}
\begin{array}{ccc}
1.0 & 0.0 & 10.0\\
0.0 & 1.0 & 20.0\\
0.0 & 0.0 & 1.0
\end{array}
\end{displaymath}

Affine transformations are communicated to PostScript as 
\texttt{concat} commands. Effectively \wumpuscore performs no
transformations itself, delegating all the work to PostScript or
SVG. This means transformations can generally be located in the 
output if a picture needs to be debugged, though as this might 
not be very helpful in practice. Internally \wumpuscore only 
performs the transformation on the pictures bounding box - it 
needs to do this so transformed pictures can still be composed 
with the picture language operations.

PostScript uses column-major form and uses a six element matrix
rather than a nine element one. The translation matrix above 
would produce this concat command:

\begin{verbatim}
[1.0 0.0 0.0 1.0 10.0 20.0] concat
\end{verbatim}

Similarly, it would be communicated to SVG via a 
\texttt{<g ...> </g>} element:

\begin{verbatim}
<g transform="matrix(1.0, 0.0, 0.0, 1.0, 10.0, 20.0)"> ... </g>
\end{verbatim}




%-----------------------------------------------------------------
\section{Font handling}
%-----------------------------------------------------------------

Font handling is quite primitive in \wumpuscore. The bounding box 
of text label is only estimated - based on the length of the 
label's string rather than the metrics of the individual letters 
encoded in the font. Accessing the glyph metrics in a font would 
require a font loader to read TrueType font files. This would be 
a significant effort, probably larger than the effort put into 
\wumpuscore itself; for \wumpuscore's intended use - producing 
diagrams and pictures rather than high quality text - its 
primitive font handling is not such a draw back.


In both PostScript and SVG mis-named fonts can cause somewhat
inscrutable printing anomalies - usually falling back to a default 
font but not always. Paricularly note, that PostScript fonts may
only support glyphs in a limited set of sizes 
(10, 12, 18, 24, 26), for labels at other sizes the text should
be drawn at a regular size then scaled once it has been lifted 
with the \texttt{frame} function to the Picture type.

The following table lists PostScript fonts and their SVG 
equivalents. As of revision 0.15.0 \wumpuscore includes a module 
\texttt{Wumpus.Extra.SafeFonts} encoding this list to avoid 
typographical slips...



\begin{tabular}{ l l }
PostScript name   & SVG name      \\
\hline
Times-Roman       & Times New Roman \\
Times-Italic      & Times New Roman - style="italic" \\
Times-Bold        & Times New Roman - font-weight="bold" \\
Times-BoldItalic  & Times New Roman - style="italic", font-weight="bold" \\
Helvetica         & Helvetica \\
Helvetica-Oblique & Helvetica - style="italic" \\
Helvetica-Bold    & Helvetica - font-weight="bold" \\
Helvetica-Bold-Oblique & Helvetica - style="italic", font-weight="bold" \\
Courier           & Courier New \\
Courier-Oblique   & Courier New - style="italic" \\
Courier-Bold      & Courier New - font-weight="bold" \\
Courier-Bold-Oblique & Courier New - style="italic", font-weight="bold" \\
Symbol & Symbol \\
\hline
\end{tabular}




%-----------------------------------------------------------------
\section{Acknowledgments}
%-----------------------------------------------------------------

PostScript is a registered trademark of Adobe Systems Inc.

\end{document}
