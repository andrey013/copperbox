\documentclass{article}

%include polycode.fmt
\usepackage{alltt}

\begin{document}

\title{HNotate Guide}
\author{Stephen Tetley}
\maketitle

%---------------------------------------------------------------------------
\section{Introduction}
%---------------------------------------------------------------------------
HNotate is a Haskell preprocessor for producing typeset sheet 
music. It accommodates two popular free typesetting systems - 
Abc and LilyPond, although support for LilyPond is more extensive. 
It follows the common \emph{note list} model for representing 
music.

Composition systems such as \emph{Haskore} usually work with 
music represented as \emph{note lists} (sometimes called 
\emph{event lists}) - serial lines of notes and rests - 
or \emph{trees} that add some parallelism allowing them to 
model simultaneous musical events like chords. For the 
rest of this manual the term \emph{note lists} will be used 
both for note lists and note trees. 

Note lists easily be translated to MIDI giving users an 
immediate means to hear their composition. Unfortunately note 
lists cannot produce high quality typeset music quite so readily. 
The reason is very simple - typeset music requires much more 
information than is typically present in note lists\footnote{
As an illustrative example consider the pitch 
F$\sharp$ above middle C - Haskore will always render this in 
MIDI as its note value 66. However a typesetting system such 
as LilyPond has a notion of the current key, if the current 
key is C major it will print a sharp with the F as F$\sharp$ 
is an exceptional pitch. But if it is in G major it will just 
print F as the key signature has already told the instrumentalist 
to interpret F as F$\sharp$.}. Rather than try to synthesize 
this extra information HNotate delegates the work to the user 
- they must supply a score file which contains all the 
information they care about (title, key signature, time signature 
etc.) plus special meta-comments that indicate where to render 
the note lists inside the score file. 

%---------------------------------------------------------------------------
\subsection{Compatibility}
%---------------------------------------------------------------------------

HNotate is still in progress - it is written in the with latest
stable GHC release (currently 6.8.3) using some GHC 
specific extensions.

HNotate outputs both LilyPond and Abc scores. The LilyPond 
output has been developed using LilyPond version 2.10.33. 
The output does not make advanced use of LilyPond so HNotate 
should have some forwards and backwards compatibility with 
other versions.

Unlike LilyPond, Abc does not have a canonical implementation.
Different implementations accept different input formats and 
the Abc standard itself seems to still be in flux. HNotate 
generates output specifically for \texttt{abcm2ps}, this is a 
advanced implementation which supports features from the draft 
Abc 2.0 specification. In particular, HNotate uses voice 
overlays for polyphonic music - these are not supported in 
the Abc reference implementation \texttt{abc2ps}. HNotate
has been developed with version 4.12.30 (May 28, 2007) of
\texttt{abcm2ps}. 

%---------------------------------------------------------------------------
\subsection{License}
%---------------------------------------------------------------------------

HNotate is distributed with an unrestricted BSD style license 
as per the Haskell Hierarchical Libraries.

\emph{Copyright 2008, Stephen Peter Tetley. All rights reserved.}  

%---------------------------------------------------------------------------
\section{Using HNotate}
%---------------------------------------------------------------------------

HNotate renders a score file from a template file and
a \texttt{System}. A template file is an ordinary input file for 
either LilyPond or Abc marked up with special 
\emph{meta-comments.} A \texttt{System} is a Haskell datatype
exported by the HNotate API, it is a collection note lists 
indexed by name. When HNotate renders a score file it looks 
for \texttt{output} meta-comments in the template file, finds the 
corresponding note list in the system and replaces the 
meta-comment with the rendered notes - this is commonly 
called \emph{plugging}.
 
Meta-comments are transparent to the original application - 
to LilyPond a meta-comment is treated just like a comment and
ignored. The same is true for abcm2ps. A score file with 
meta-comments must be always readable by the respective system 
whether or not it has been processed with HNotate, it cannot 
contain syntax errors\footnote{By this virtue, if a score file 
is acceptable to LilyPond or abcm2ps before it has been 
processed by HNotate, but then generates a syntax error 
afterwards then it points to HNotate having a bug - an error
report would be greatly appreciated.}. 

LilyPond delimits comments with \verb+%{+ and \verb+%}+,  
HNotate adds a hash character to these delimiters to 
recognize a comment as a meta-comment \verb+%{#+ 
and \verb+#%}+ - as the outer two characters remain the 
same LilyPond will see meta-comments only as comments.
A typical meta-comment might be:
\begin{verbatim}
%{# output: \relative bulgarian6 #%}
\end{verbatim}

This tells HNotate to output the note list \texttt{bulgarian6}
using the \texttt{relative}\footnote{Relative rendering is 
shorthand used by LilyPond where the pitch of one note is 
pitch of the previous note, this saves the user typing
octave designations for every note.} rendering scheme.  

Comments in Abc start with a \verb+%+ symbol and continue to 
the end of the line. Meta-comments for HNotate \textbf{must} 
start at the beginning of a line with the symbol \verb+%#+, like 
ordinary Abc comments they continue to the end of the line.
This example directs HNotate to used the \texttt{default} 
rendering scheme on the note list \texttt{example1}:
\begin{verbatim}
%# output: example1
\end{verbatim}




\begin{table}
\input{LyCommands}%
\caption{LilyPond commands recognized by HNotate}\label{lycommands}
\end{table}




\begin{table}
\input{AbcFields}%
\caption{Abc fields recognized by HNotate}\label{abcfields}
\end{table}

     
%---------------------------------------------------------------------------
\section{The HNotate API}
%---------------------------------------------------------------------------
HNotate is not tied to a specify composition system, though it 
is only useful for Haskell. 



% Pitch and Duration are predefined types in HNotate, it maybe 
% that the composition system being used already has types 
% called Pitch and Duration to represent pitch and duration 
% (they are obvious names after all). 


\end{document} 
