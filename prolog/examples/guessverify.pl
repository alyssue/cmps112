% $Id: guessverify.pl,v 1.4 2012-03-15 13:31:34-07 - - $

%
% Illustrate guess and verify style.
% We have a set of numbers and we want to find all pairs
% where the first number is greater than the second.
%

a_number( 1.41421356237309504880).
a_number( 2.5).
a_number( 2.7182818284590452354).
a_number( 3.14159265358979323846).
a_number( 6.02e23).
a_number( 8).

guess( X, Y) :- a_number( X), a_number( Y).

verify( X, Y) :- X > Y.

getpair( X, Y) :- guess( X, Y), verify( X, Y).

% TEST: getpair( X, Y).
% TEST: ;
% TEST: ;
% TEST: ;
% TEST: ;
% TEST: ;
% TEST: ;
% TEST: ;
% TEST: ;
% TEST: ;
% TEST: ;
% TEST: ;
