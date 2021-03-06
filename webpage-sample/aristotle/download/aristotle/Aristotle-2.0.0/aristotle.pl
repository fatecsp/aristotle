/*----------------------------------------------------------------+
|	     Aristotle - Computational Argumentation		  |
+----------------------------------------------------------------*/

% Modules Import
:- use_module( modules/globvars ).
:- use_module( modules/socrates ).
:- use_module( modules/plato ).
:- use_module( modules/help ).
:- use_module( modules/about ).

% Operators Declartion
:- op(100,fy,not ).
:- op(101,xfy,and ).
:- op(102,xfx,then ).
:- op(103,xfx,: ).
:- op(104,xfx,believes ).
:- op(105,xfx,precedes ).

/*----------------------------------------------------------------+
| Graphical User Interface					  |
+----------------------------------------------------------------*/
aristotle :-
   catch(win_window_pos([show(false)]),_,true),
   new( Frame, frame( 'Aristotle 2 - Computational Argumentation' ) ),
   send( Frame,  append, new( Dialog, dialog ) ),
   var( aristotle:dialog, Dialog),
   send( Dialog, append, bitmap( image('./resources/images/aristotle.jpg' ) ) ),
   send( Dialog, append, button( socrates, message( @prolog, socrates ) ) ),
   send( Dialog, append, button( plato,    message( @prolog, plato ) ) ),
   send( Dialog, append, button( help,     message( @prolog, help ) ) ),
   send( Dialog, append ,button( about,    message( @prolog, about) ) ),
   send( Dialog, append, button( quit,     message( @prolog, close_all ) ) ),
   send( Dialog, gap, size(0, 30)),
   send( Dialog, gap, size( 3, 3 ) ),
   send( Frame,  open, point( 0, 0 ) ).

aristotle :-
   new( Frame, frame( 'Aristotle 2 - Computational Argumentation' ) ),
   send( Frame,  append, new( Dialog, dialog ) ),
   var( aristotle:dialog, Dialog),
   send( Dialog, append, bitmap( image('./resources/images/aristotle.jpg' ) ) ),
   send( Dialog, append, button( socrates, message( @prolog, socrates ) ) ),
   send( Dialog, append, button( plato,    message( @prolog, plato ) ) ),
   send( Dialog, append, button( help,     message( @prolog, help ) ) ),
   send( Dialog, append ,button( about,    message( @prolog, about) ) ),
   send( Dialog, append, button( quit,     message( @prolog, close_all ) ) ),
   send( Dialog, gap, size(0, 30)),
   send( Dialog, gap, size( 3, 3 ) ),
   send( Frame,  open, point( 0, 0 ) ).

close_all :- halt.

:- aristotle.







