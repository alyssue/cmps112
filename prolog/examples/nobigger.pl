% $Id: nobigger.pl,v 1.3 2011-05-19 19:53:59-07 - - $ */

%
% Sample of the use of tracing calls.
% Find the biggest number in a list.
%

mynumber( 3 ).
mynumber( 6 ).
mynumber( 9 ).

biggest( Number ) :-
  mynumber( Number ),
  nobigger( Number ).

nobigger( Number ) :-
  mynumber( Other ),
  Other > Number, !, fail.

nobigger( _ ).

traceon :-
   trace( mynumber ),
   trace( biggest ),
   trace( nobigger ).

% TEST: biggest(N)..
