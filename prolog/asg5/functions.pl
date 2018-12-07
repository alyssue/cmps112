%
% Alyssa Melton
% Asg5: Prolog Flights
% functions.pl
%
% -----------------------------------------------------------------------------
% convert degrees and minutes to radians.
 in_radians( degmin( Deg, Min ), Radians ) :-
     Radians is (Deg + Min / 60) * pi / 180.

   not( X ) :- X, !, fail.
   not( _ ).

% -----------------------------------------------------------------------------
% calculate distance from one airport to another.
haversine_radians( XLat1, XLon1, XLat2, XLon2, Distance ) :-
  in_radians( XLat1, Lat1 ),
  in_radians( XLon1, Lon1 ),
  in_radians( XLat2, Lat2 ),
  in_radians( XLon2, Lon2 ),
  Dlon is Lon2 - Lon1,
  Dlat is Lat2 - Lat1,
  A is sin( Dlat / 2 ) ** 2
    + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
  Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
  Distance is Dist * 3961. %3961 is earth radius in miles

% -----------------------------------------------------------------------------
% arrival time can be computed from the departure time.
% plane travels at 500 miles per hour
arrival_time( flight(Airport1, Airport2, time(Hours, Minutes)), ArrivalTime ) :-
  airport( Airport1, _, Lat1, Lon1 ),
  airport( Airport2, _, Lat2, Lon2 ),
  haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance),
  FlightTime is Distance / 500,
  DepartureTime is Hours + Minutes / 60,
  ArrivalTime is DepartureTime + FlightTime.

% -----------------------------------------------------------------------------
% compare the times of an arrival time in Decimal
% and a departing time in Hours:Minutes.
time_okay( ArrivalTime, time(Hours, Minutes) ) :-
  DepartureTime is Hours + Minutes/60,
  DepartureTime > ArrivalTime + 29. % departure must be 30 minutes or more after arrival of previous flight.

% -----------------------------------------------------------------------------
get_HourMin( ArrivalTime, time(Hours, Mins) ) :-
  TimeInMins is ArrivalTime * 60,
  Hours is TimeInMins/60,
  Mins is mod(TimeInMins, 60).

to_Upper( Lower, Upper ) :-
   atom_chars( Lower, Lowerlist),
   maplist( lower_upper, Lowerlist, Upperlist),
   atom_chars( Upper, Upperlist).
% -----------------------------------------------------------------------------

writeRoute( flight(Previous, Node, time(PrevHour, PrevMinute)), [] ) :-
  airport( Node, City, _, _),
  arrival_time( flight(Previous, Node, time(PrevHour, PrevMinute)), ArrivalTime ),
  get_HourMin(ArrivalTime, time(AHour, AMin)),
  to_Upper( Node, NNode),
  write( 'arrive   '), write( NNode ), write('   '), write( City ), write(AHour), write(':'), write(AMin),
  nl.

writeRoute( [flight(Node, Next, time(Hour, Minute))|Tail] ) :-
  airport(Node, City, _, _),
  to_Upper( Node, NNode),
  write( 'depart   '), write( NNode ), write('   '), write( City ), write(Hour), write(':'), write(Minute),
  nl,
  writeRoute(flight(Node, Next, time(Hour, Minute)), Tail ).


writeRoute(flight(Previous, Node, time(PrevHour, PrevMinute)), [flight(Node, Next, time(Hour, Minute))|Tail] ) :-
  airport( Node, City, _, _),
  arrival_time( flight(Previous, Node, time(PrevHour, PrevMinute)), ArrivalTime ),
  get_HourMin(ArrivalTime, time(AHour, AMin)),
  to_Upper( Node, NNode),
  write( 'arrive   '), write( NNode ), write('   '), write( City ), write(AHour), write(':'), write(AMin),
  nl,
  write( 'depart   '), write( NNode ), write('   '), write( City ), write(Hour), write(':'), write(Minute),
  nl,
  writeRoute(flight(Node, Next, time(Hour, Minute)), Tail ).


% -----------------------------------------------------------------------------
%first call
getRoute( Node, End, Route ) :-
   getRoute( Node, 0, End, [Node], Route ).
   % current, destination, Tried[], Route[]

getRoute( Node, _, Node, _, [] ).
% found the destination! return that Node.

getRoute( Node, ArrivalTime, End, Tried, [flight(Node, Next, time(Hours,Minutes))|Route] ) :-
   flight(Node, Next, time(Hours, Minutes)),
   not( member( Next, Tried )), %if haven't tried this neighbor yet
   time_okay(ArrivalTime, time(Hours, Minutes)),
   arrival_time( flight(Node, Next, time(Hours, Minutes)), NextArrivalTime ),
   getRoute( Next, NextArrivalTime, End, [Next|Tried], Route ). %recur

% -----------------------------------------------------------------------------
fly( Start, Destination ) :-
  write('first call of fly '), write(Start), write(Destination), nl,
  getRoute( Start, Destination, Route),
  nl,
  writeRoute(Route), !.

fly( Start, Start ) :-
  write(' No need to fly yourself to your current location, silly! '),
  !, fail.
