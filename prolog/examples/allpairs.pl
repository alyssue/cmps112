% $Id: allpairs.pl,v 1.2 2011-05-19 19:53:59-07 - - $ */

%
% Query pair will return all pairs.
%

positive(red).
positive(green).
positive(blue).
negative(cyan).
negative(magenta).
negative(yellow).

pair(Pos,Neg) :-
  positive(Pos),
  negative(Neg).

allpairs :-
  pair(Pos,Neg),
  print( pair(Pos,Neg) ), nl, fail.

:- initialization(allpairs).
