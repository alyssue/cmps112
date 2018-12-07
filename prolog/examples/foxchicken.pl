% $Id: foxchicken.pl,v 1.3 2011-05-19 19:53:59-07 - - $ */

%
% A farmer has with him a fox, a chicken, and a sack of grain.
% He comes to a river and sees a small boat.  He needs to bring
% all three of his things across the river, but the boat is so
% small that only one thing will fit in it with him.  He can not
% leave the fox and the chicken together or the fox will eat the
% chicken.  He can not leave the chicken and the grain together
% or the chicken will eat the grain.  The fox, however, does not
% eat grain.  How should the farmer proceed?
%

not( X ) :- X, !, fail.
not( _ ).

%
% FACTS AND CULLINARY HABITS:
%

eats( fox, chicken ).
eats( chicken, grain ).
property( [ fox, chicken, grain ] ).
goal( other ).
start( first ).

chow_time( List ) :-
   member_of( Diner, List ),
   member_of( Dinner, List ),
   eats( Diner, Dinner ).

%
% SET RELATIONS:
%

member_of( H, [ H | _ ] ).
member_of( H, [ _ | T ] ) :- member_of( H, T ).

matches( [], [] ).
matches( [ H | T1 ], [ H | T2 ] ) :- matches( T1, T2 ).

removex( H, [ H | T ], T ).
removex( X, [ H | T ], [ H | U ] ) :- removex( X, T, U ).

insert( H, T, [ H | T ] ).

%
% TRAVEL PLANS AND FREQUENT PADDLER MILES.
%

travel :-
   start( From ),
   goal( To ),
   property( Property ),
   print_start( From, To, Property, [] ),
   move( From, To, Property, [], nothing ),
   print_done.

move( From, _, _, [], _ ) :-
   goal( From ).

move( From, To, This, That, _ ) :-
   start( To ),
   goal( From ),
   not( chow_time( This )),
   print_alone( From, To, This, That ),
   move( To, From, That, This, nothing ).

move( From, To, This, That, Just_took ) :-
   removex( What, This, This_later ),
   not( Just_took = What ),
   not( chow_time( This_later )),
   insert( What, That, That_later ),
   print_takes( What, From, To, This_later, That_later ),
   move( To, From, That_later, This_later, What ).

%
% TRAVEL INSTRUCTIONS.
%

print_start( From, To, This, That ) :-
   nl,
   write( 'The farmer is by a river and ' ),
   write( 'wants to take his cargo across.' ),
   nl,
   print_status( From, This ),
   print_status( To, That ).

print_done :-
   nl,
   write( 'Finally, the farmer is done!' ),
   nl.

print_takes( What, From, To, This, That ) :-
   nl,
   write( 'The farmer takes the ' ),
   write( What ),
   write( ' from the ' ),
   write( From ),
   write( ' side to the ' ),
   write( To ),
   write( ' side.' ),
   nl,
   print_status( From, This ),
   print_status( To, That ).

print_alone( From, To, This, That ) :-
   nl,
   write( 'The farmer travels alone from the ' ),
   write( From ),
   write( ' side to the ' ),
   write( To ),
   write( ' side.' ),
   nl,
   print_status( From, This ),
   print_status( To, That ).

print_status( Where, What ) :-
   tab( 10 ),
   write( 'On the ' ),
   write( Where ),
   write( ' side is ' ),
   print_list( nothing, '', What ).

print_list( Nothing, _, [] ) :-
   write( Nothing ),
   write( '.' ),
   nl.

print_list( _, Comma, [ H | T ] ) :-
   write( Comma ),
   write( 'the ' ),
   write( H ),
   print_list( '', ', ', T ).

% TEST: travel.

