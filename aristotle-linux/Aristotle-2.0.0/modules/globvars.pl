% Define global variables

:- module(globvars,[ var/2 ] ).

var( Owner:Identifier, @Value ) :-
   nonvar( Owner ),
   nonvar( Identifier ),
   nonvar( Value ),
   atomic_list_concat( [ Owner, '_', Identifier ], Flag),
   flag( Flag, _, Value ), !.

var( Owner:Identifier, @Value ) :-
   nonvar( Owner ),
   nonvar( Identifier ),
   var( Value ),
   atomic_list_concat( [ Owner, '_', Identifier ], Flag),
   flag( Flag, Value, Value ),
   Value \= 0, !.

