% $Id: towers.pl,v 1.3 2011-05-19 19:53:59-07 - - $ */

%
% Towers problem.
%

towers( N ) :-
   move( N, source, spare, target ).

% if N = 0
move( 0, _, _, _ ) :-
   !.

% else n != 0
move( N, Source, Spare, Target ) :-
   M is N - 1, %decrease N
   move( M, Source, Target, Spare ),
   report( Source, Target ),
   move( M, Spare, Source, Target ).

report( Source, Target ) :-
   write( 'Move a disk from the ' ),
   write( Source ),
   write( ' peg to the ' ),
   write( Target ),
   write( ' peg.'),
   nl.

% TEST: towers(4).
