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
Abc and LilyPond, though support for LilyPond is more extensive. 
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
specific extensions. It has a dependency on Daan Leijen's 
\verb|PPrint| package which is available from Hackage.

HNotate outputs both LilyPond and Abc scores. The LilyPond 
output has been developed using LilyPond version 2.10.33. 
The output does not make advanced use of LilyPond so HNotate 
should have some forwards and backwards compatibility with 
other versions.

Unlike LilyPond, Abc does not have a canonical implementation.
Different implementations accept different input formats and 
the Abc standard itself seems to still be in flux. HNotate 
generates output specifically for \verb|abcm2ps|, this is a 
advanced implementation which supports features from the draft 
Abc 2.0 specification. In particular, HNotate uses voice 
overlays for polyphonic music - these are not supported in 
the Abc \emph{reference implementation} \verb|abc2ps|. HNotate
has been developed with version 4.12.30 (May 28, 2007) of
\verb|abcm2ps|. 

%---------------------------------------------------------------------------
\subsection{License}
%---------------------------------------------------------------------------

HNotate is distributed with an unrestricted BSD style license 
as per the Haskell Hierarchical Libraries.

\emph{Copyright 2008, Stephen Peter Tetley. All rights reserved.}  

%---------------------------------------------------------------------------
\section{Using HNotate}
%---------------------------------------------------------------------------

HNotate renders a score file from a \emph{template file} and
a \emph{system}. A template file is an ordinary input file for 
either LilyPond or Abc marked up with special 
\emph{meta-comments.} A system is a collection note lists 
indexed by name. When HNotate renders a score file it looks 
for \verb|output| meta-comments in the template file, finds the 
corresponding note list in the system and replaces the 
meta-comment with the rendered notes - this is commonly 
called \emph{plugging}.
 


A score file with meta-comments must be readable by the 
respective system before it has been processed with HNotate, 
it cannot contain syntax errors.



LilyPond delimits comments with \verb+%{+ and \verb+%}+.  
HNotate generally ignores comments in LilyPond files but
it recognizes \emph{meta-comments} that are delimited with 
\verb+%{#+ and \verb+#%}+. 
\begin{verbatim}
%{# bulgarian6:relative #%}
\end{verbatim}
HNotate also recognizes a limited number of LilyPond commands
these are shown in table~\ref{lycommands}. 

\begin{table}
\input{LyCommands}%
\caption{LilyPond commands recognized by HNotate}\label{lycommands}
\end{table}


Comments in Abc start with a \verb+%+ symbol and continue to 
the end of the line, a meta comment for HNotate starts with 
\verb+%#+, for example:
\begin{verbatim}
%# example1:default
\end{verbatim}

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
