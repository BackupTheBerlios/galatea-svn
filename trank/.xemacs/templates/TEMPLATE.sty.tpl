%%% TEMPLATE.sty.tpl --- (>>>COMMENT<<<)

%% Author: (>>>AUTHOR<<<)
%% Version: $Id: TEMPLATE.sty.tpl,v 1.1.1.1 2003/03/10 20:23:17 michael Exp $

%%% Commentary:

%% (>>>1<<<)

%% Put this file into your TEXINPUTS and the following into your preamble:
%%   \usepackage[OPTIONS]{(>>>FILE_SANS<<<)}

%% OPTIONS defaults to (>>>3<<<)

%%% Code:

\NeedsTeXFormat{LaTeX2e}(>>>POINT<<<)[1995/12/01]
\def\@rcs@ $#1Date: #2 #3$$#4Revision: #5$ {
   \ProvidesPackage{(>>>FILE_SANS<<<)}[#2 v#5(>>>COMMENT<<<)]}
\@rcs@ $Date: 2003/03/10 20:23:17 $$Revision: 1.1.1.1 $

\DeclareOption{(>>>4<<<)}{%%%
  }

%%\DeclareOption*{\PassOptionsToPackage{\CurrentOption}{(>>>9<<<)}}
\ExecuteOptions{(>>>5<<<)}
\ProcessOptions



%%%%##########################################################################
%%%%  Main code
%%%%##########################################################################

(>>>6<<<)


\endinput

%%% Local Variables:
%%% TeX-auto-save: nil
%%% TeX-auto-parse-length: 99999
%%% End:
