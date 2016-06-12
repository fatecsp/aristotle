/*----------------------------------------------------------------+
|* Authors: Silvio P. Lago, Luiz F. Zarco & Lucio N. Lira.        |
|* Institution: São Paulo Technological College (FATEC-SP).       |
|* Copyright (c) 2013-2016					  |
|*								  |
|* This file is part of aristotle software source code.           |
|* The software source code is distributed under MIT License.     |
|* Read LICENSE.txt for further information.			  |
+----------------------------------------------------------------*/

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

