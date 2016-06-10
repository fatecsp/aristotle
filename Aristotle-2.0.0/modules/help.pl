% help - about aristotle system

:- module(help,[help/0]).

help :-
   not( exists_frame ),
   new( Frame,  frame( 'Aristotle help (version 1.0)' ) ),
   send( Frame, append, new( Dialog, dialog ) ),
   send( Dialog, append, new( Editor, editor( new( text_buffer ), 136,31) ) ),
   send( Dialog, append, button( quit, message( @prolog, close ) ) ),
   send_list( Editor, append,
	 ['Aristotle - A system for Computational Argumentation and defeasible reasoning\n\n',
	  'Rule syntax:\n',
	     '\tRule => Presumption | Label Conditions then Conclusion\n',
	     '\tPresumption => true then Literal | Literal\n',
	     '\tLiteral => Atom | not Atom\n',
	     '\tLabel => [] | Identifier :\n',
	     '\tIdentifier => [a-z]+[0-9]*\n',
	     '\tConditions => Literal | Literal and Conditions\n',
	     '\tConclusion => Literal\n\n',

	  'Clause syntax for monologues:\n\n',
	     '\tClause => [Rule | Identifier precedes Identifier].\n\n',

	  'Monologue example (file name extension MKB):\n\n',
	     '\ttrue then chicken(tina).		  % Presumption\n',
	     '\tscared(tina).		                  % Presumption\n',
	     '\tchicken(X) then bird(X).                  % Rule without label\n',
	     '\tr1: bird(X) then fly(X).                  % Rule with label\n',
	     '\tr2: chicken(X) then not fly(X).           % Rule with label\n',
	     '\tr3: chicken(X) and scared(x) then fly(X). % Rule with label\n',
	     '\tr2 precedes r1.                           % Precedence rule\n\n',

	  'Clause syntax for dialogues:\n\n',
	     '\tClause => [Agent believes Rule | Identifier precedes Identifier].\n',
	     '\tAgent => SHe | paul | olga\n\n',

	  'Dialogue example  (file name extension DKB):\n\n',
	     '\tSHe believes chicken(tina).                   % They believe in a presumption\n',
             '\tSHe believes chicken(X) then bird(X).	      % They believe in a rule\n',
	     '\tpaul believes p1: bird(X) then fly(X).        % Paul believes in rule p1\n',
             '\tolga believes o1: chicken(X) then not fly(X). % Olga believes in rule o1\n',
             '\to2 precedes p1.                               % Precedence rule\n'] ),

   var( help:frame, Frame ),
   send( Editor, font, font( screen, normal, 12 ) ),
   send( Editor, caret, 0),
   send( Editor, editable, @off),
   send( Frame,  open, point( 450, 0 ) ).

exists_frame :-
   var( help:frame, Frame ),
   catch( send( Frame, expose ), Error, true ),
   var( Error ).

close :-
   var( help:frame, Frame ),
   send( Frame, destroy ).










