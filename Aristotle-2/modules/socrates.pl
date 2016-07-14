/*----------------------------------------------------------------+
|* Authors: Luiz F. Zarco, Silvio do Lago.                        |
|* Institution: São Paulo Technological College (FATEC-SP).       |
|* Copyright (c) 2013-2016					  |
|*								  |
|* This file is part of aristotle software source code.           |
|* The software source code is distributed under MIT License.     |
|* Read LICENSE.txt for further information.			  |
|-----------------------------------------------------------------|
| Socrates - a system for monological argumentation		  |
|								  |
| Socrates (c. 469 BC - 399 BC) was a classical Greek philosopher |
| and is one of the founders of Western philosophy. He is known	  |
| chiefly through the writings of his students Plato and Xenophon,|
| and the plays of his contemporary Aristophanes. He influenced	  |
| subsequent philosophers by his approach to logic (the Socratic  |
| Method) and his infamous trial and subsequent execution,	  |
| as described by Plato. [http://thyselfknow.com/socrates]        |
+----------------------------------------------------------------*/

/*----------------------------------------------------------------+
| Modules Import						  |
+----------------------------------------------------------------*/

:- module( socrates,[ socrates/0 ] ).

:- use_module( fbrowser ).
:- use_module( updateKB ).
:- use_module( globvars ).

/*----------------------------------------------------------------+
| Operators Declaration						  |
+----------------------------------------------------------------*/

:- op(100,  fy, not).                   % Negation operator
:- op(101, xfy, and).                   % Logical conjuntion operator
:- op(102, xfx, then).                  % Logical implication operator
:- op(103, xfx, :).                     % Label rules operator
:- op(105, xfx, precedes).              % Precedence relation operator
:- discontiguous precedence_relation/2. % Indicate different file for predicate
:- dynamic tmp/1.                       % Temporary clause
:- dynamic next/2.		        % Next allowed speech act
:- dynamic ptype/1.                     % Precedence Type (explicit, implicit and mixed)
:- dynamic verbose/1.		        % Activates the output (speech acts)
:- dynamic line/1.                      % Asserts each line of the dialogue on memory
:- flag(first_time,_,1).                % Creates a flag for dialogue start

/*----------------------------------------------------------------+
| Socrates' Graphical User Interface				  |
+----------------------------------------------------------------*/

socrates :-
   nb_setval(precedences,[]),	% Declare variable precedences
   not( exists_frame ),
   new( Frame,                      frame( 'Socrates - Monological Argumentation (Version 2.1.94)' ) ),
   new( MenuBar,                    menu_bar ),
   new( FileMenu,                   popup( file )),
   new( ToolsMenu,		    popup( tools ) ),
   new( KnowledgebaseTab,           tab( knowledgebase ) ),
   new( DisplayTab,	            tab( display ) ),
   new( ArgumentsTab,               tab( arguments ) ),
   new( DialogueTab,                tab( dialogue ) ),
   new( TabStack,                   tab_stack ),
   new( KnowledgebaseEditor,        editor ),
   new( DisplayEditor,		    editor ),
   new( ArgumentsEditor,            editor ),
   new( DialogueEditor,             editor ),
   new( RulePrecedenceMenu,         menu( precedence_relations, toggle ) ),
   new( Picture,                    picture),
   new( RightDialog,                dialog),
   new( LeftDialog,                 dialog),
   new( Column_Left,                dialog),
   new( LeftButtons,                dialog),
   new( RightButtons,		    dialog),
   new( ArgumentsButton,            button( arguments,           message( @prolog, arguments_click ) ) ),
   new( ConflictsButton,            button( conflicts,           message( @prolog, consist_prec ) ) ),
   new( DialogueButton,             button( dialogue,            message( @prolog, dialogue ) ) ),
   new( DialecticalTreeButton,      button( dialectical_tree,	 message( @prolog, draw_tree ) ) ),
   new( PopUpButton,                button( popup,               message(@prolog, popup_tree ))),
   new( OpenMenuItem,               menu_item( open,             message( @prolog, loadfile, socrates, mkb ) ) ),
   new( NewMenuItem,                menu_item( new,              message( @prolog, newfile, socrates, mkb ) ) ),
   new( SaveMenuItem,               menu_item( save,             message( @prolog, savefile, socrates ) ) ),
   new( SaveAsMenuItem,             menu_item( save_as,		 message( @prolog, saveasfile, socrates, mkb ), end_group := true ) ),
   new( QuitMenuItem,               menu_item( quit,             message( @prolog, close ) ) ),
   new( VerboseMenuItem,            menu_item( exhibit_dialogue, message( @prolog, verbose ) ) ),
   new( AutoLabelMenuItem,          menu_item( 'Number Lines',       message( @prolog, number_lines ) ) ),
   new( ShowPrecMenuItem,           menu_item( show_precedences, message( @prolog, show_precedences ) ) ),
   var( socrates:frame, Frame ),
   var( socrates:knowledgebase_tab,	     KnowledgebaseTab ),
   var( socrates:display_tab,                DisplayTab ),
   var( socrates:arguments_tab,              ArgumentsTab ),
   var( socrates:dialogue_tab,               DialogueTab ),
   var( socrates:knowledgebase_editor,       KnowledgebaseEditor ),
   var( socrates:display_editor,	     DisplayEditor ),
   var( socrates:arguments_editor,           ArgumentsEditor ),
   var( socrates:dialogue_editor,            DialogueEditor ),
   var( socrates:tab_stack,                  TabStack ),
   var( socrates:rule_precedence_menu,       RulePrecedenceMenu ),
   var( socrates:picture,                    Picture),
   var( socrates:current_path,		     @('')),
   send_list( Frame,                append, [LeftDialog, RightDialog ] ),
   send_list( MenuBar,              append, [FileMenu, ToolsMenu ] ),
   send_list( FileMenu,             append, [OpenMenuItem, NewMenuItem, SaveMenuItem, SaveAsMenuItem, QuitMenuItem] ),
   send_list( ToolsMenu,	    append, [AutoLabelMenuItem,ShowPrecMenuItem,VerboseMenuItem]),
   send_list( TabStack,             append, [KnowledgebaseTab, DisplayTab, ArgumentsTab, DialogueTab ] ),
   send_list( LeftDialog,	    append, [MenuBar, TabStack, ArgumentsButton,
					     ConflictsButton, DialogueButton, DialecticalTreeButton]),
   send_list( RightDialog,          append, [RulePrecedenceMenu] ),
   send_list( RulePrecedenceMenu,   append, [menu_item( explicit, message( @prolog, consist_prec ) ),
			                     menu_item( implicit, message( @prolog, consist_prec ) )]),
   forall( member( Tab/Editor, [ KnowledgebaseTab/KnowledgebaseEditor, DisplayTab/DisplayEditor,
				 ArgumentsTab/ArgumentsEditor, DialogueTab/DialogueEditor ] ),
	   ( send(Tab, gap, size( 2, 2 ) ),
	     send(Tab, append, Editor ) ) ),
   forall( member( Editor/Background, [ KnowledgebaseEditor/aliceblue, DisplayEditor/lavenderblush,
                                        ArgumentsEditor/aliceblue, DialogueEditor/ivory ] ),
	   ( send( Editor, size, size( 70, 26 ) ),
	     send( Editor, font, font( screen, normal, 12 ) ),
	     send( Editor, colour, darkblue ),
	     send( Editor, background, Background ) ) ),
   send( LeftButtons, gap, size(5,1)),
   send( LeftDialog, left, RightDialog ),
   send( RightDialog, gap, size( 15, 3 ) ),
   send( RightDialog, append, PopUpButton),
   send( PopUpButton, right, RulePrecedenceMenu),
   send( RightDialog, append, Picture ),
   send( Picture, size, size( 480, 450 ) ),
   send( Column_Left,  pen, 0),
   send( Column_Left,  gap,    size(0,0)),
   send( Column_Left,  left,   LeftDialog),
   send( Column_Left,  append, bitmap(image('resources/images/column.gif'))),
   send( Frame, open, point( 50, 70 )).

/*----------------------------------------------------------------+
| Window Management                                               |
+----------------------------------------------------------------*/

% Verify whether a frame already exists (being displayed).

exists_frame :-
   var(socrates:frame, Frame),
   catch(send(Frame, expose), Error, true),
   var(Error).

% Verify whether dialog already exists (being displayed).

exists_dialog :-
   var(socrates:dialog, Dialog),
   catch(send(Dialog,expose), Error, true),
   var(Error).

% Close the Socrates module window.

close :-
   var(socrates:frame, Frame),
   send(Frame, destroy).

/*----------------------------------------------------------------+
| Argument Generation						  |
+----------------------------------------------------------------*/

% Function: Generate all arguments.
% Output: Confirmation message of the operation success.
% Options: "OK" Button closes dialog.

% Check whether there are errors before generating arguments.

arguments_click :-
   var( socrates:display_editor, DisplayEditor ),
   updateKB( socrates ),
   ( get( DisplayEditor, selected, _ )
   ->  var( socrates:display_tab, DisplayTab ),
       var( socrates:tab_stack, TabStack ),
       send( TabStack, on_top, DisplayTab )
   ;   update_args,!, arguments_success ).

% Generate arguments again, when conflicts resolution is altered

arguments :-
   var( socrates:display_editor, DisplayEditor ),
   updateKB( socrates ),
   ( get( DisplayEditor, selected, _ )
   ->  var( socrates:display_tab, DisplayTab ),
       var( socrates:tab_stack, TabStack ),
       send( TabStack, on_top, DisplayTab )
   ;   update_args ).

% Display success dialog message, if successfully generated

arguments_success :-
   not(exists_dialog),
   new(Dialog,dialog('Argumentational System')),
   send(Dialog, append, text('All arguments were successfully generated.')),
   send(Dialog, append, button(ok,message(Dialog,destroy))),
   var(socrates:dialog,Dialog),
   send(Dialog,open, point(880,280)).

% Update the arguments visualization

update_args :-
   var( socrates:arguments_editor, ArgumentsEditor ),
   var( socrates:arguments_tab, ArgumentsTab ),
   var( socrates:tab_stack, TabStack ),
   send( ArgumentsEditor, clear ),
   all_arguments( Args ),
   forall( member( Arg, Args ),
	   ( formatted( Arg, Text ),
	     send( ArgumentsEditor, append, Text ) ) ),
   send( TabStack, on_top, ArgumentsTab ),
   send( ArgumentsEditor, caret, 0 ).

% Format the argument as text

formatted( Number:Support, Text ) :-
   term_to_atom( Number, N ),
   atom_length( N, Length ),
   Tab is Length + 5,
   swritef( NewLine, ',\n%r', [' ', Tab] ),
   join( NewLine, Support, Atoms ),
   atomic_list_concat( ['A',N, ' : [' | Atoms], Text ).

% Join the clauses in a list with a new line

join( _, [N:Last], [Atom,']\n\n'] ) :- !, term_to_atom( N:Last, Atom ).

join( NewLine, [N:First|Rest], [Atom,NewLine|Atoms] ) :-
   term_to_atom( N:First, Atom ),
   join( NewLine, Rest, Atoms ).

% Generate all possible arguments
% and build Attacks Graph

all_arguments( Arguments ) :-
  retractall( vertex(_) ),
  retractall( edge(_,_) ),
  flag( arg_number, _, 1 ),
  claims( Claims ),
  findall( T/Support,
	   ( member( Fact, Claims ),
	     ( argument( Fact, Support )
	     ; argument( not( Fact ), Support ) ),
	     length( Support, T ) ),
	   Supports),
  msort( Supports, SortedSupports ),
  findall( N:Support,
	   ( member( _/Support, SortedSupports ),
	     flag( arg_number, N, N+1),
	     assertz( vertex( N ) ) ),
	   Arguments ),
  forall( ( member( N1:S1, Arguments),
	    member( N2:S2, Arguments),
	    N1 \= N2, attacks( S1, S2 ) ),
	  assertz( edge(N1,N2) ) ).

% Generate all possible claims

claims(Claims) :-
   predicates(Predicates),
   objects(Objects),
   findall(Claim, (member(Predicate/Arity, Predicates),
		     length(Arguments, Arity),
		     tuple(Arguments, Objects),
		     Claim =.. [Predicate|Arguments]),
	    Claims).

% Localize all relevant objects in the knowledgebase

objects(Objects) :-
   findall(Object, (then(_,_,Conclusion),
		      (Conclusion = not(Fact) -> Fact =.. [_|Arguments]
		      ; Conclusion =.. [_|Arguments]),
		      member(Object, Arguments),
		      atomic(Object)),
	    ObjectList),
  sort(ObjectList, Objects).

% Localize all relevant predicates in the knowledgebase

predicates(Predicates) :-
  findall(Predicate/Arity, (then(_,_,Conclusion),
			      (Conclusion = not(Fact) -> Fact =.. [Predicate|Arguments]
			      ; Conclusion =.. [Predicate|Arguments]),
			      length(Arguments, Arity)),
	   PredicatetList),
  sort(PredicatetList, Predicates).

% Generate a tuple with the joint objects

tuple([], _).
tuple([Object|Tuple], Set) :-
   member(Object, Set),
   tuple(Tuple, Set).

% Generate an argument for a fact (i.e., a support)

argument(Fact, Support) :-
   argument(Fact and true, [], Support).

argument(true, Support, Support).
argument(Fact and Facts, Partial, Support) :-
   then(RuleNumber, Conditions, Fact),
   add(Conditions, Facts, NewFacts),
   union([RuleNumber : Conditions then Fact], Partial, NewPartial),
   argument(NewFacts, NewPartial, Support).

% Attach two conjunctive formulas

add(true, B, B) :- !.
add(A and B, C, A and D) :- add(B, C, D).
add(A, B, A and B).

% Verify whether S1 support attacks S2 support.

attacks( Support1, Support2 ) :-
   last( Support1, Rule1:then(_,Fact1) ),
   member( Rule2:then(_C,Fact2), Support2 ),
   neg( Fact1, Fact2 ),
   not(prec( Rule2, Rule1)), !.

% Verify what kind of precedence relation between rules will be used

prec(Rule1,Rule2) :- nb_getval(precedences,Precedences), memberchk(Rule1<Rule2,Precedences).

/*----------------------------------------------------------------+
| Draw Conflicts Graph						  |
+----------------------------------------------------------------*/

conflicts :-
   arguments,
   var(socrates:picture, Picture),
   send(Picture, clear),
   draw_vertices,
   draw_links.

% Draw vertices

draw_vertices :-
   initiate_position_variables,
   retractall( vertex(_,_) ),
   forall( vertex(Label),
	  ( draw_vertex( Label, ObjectId ),
	    assertz( vertex(Label,ObjectId) ) ) ),
   colour_vertices.

% Initialization of variables global position

initiate_position_variables :-
   findall( Vertex, vertex(Vertex), Vertices ),
   length( Vertices, Length ),
   Height is 75 + ( ceil( sqrt(Length)-1) ) * 75,
   flag( current_x, _, 50),
   flag( current_y, _, 50),
   flag( picture_h, _, Height).

% Draw a single vertice and return its id

draw_vertex( Label, ObjectId ) :-
   var( socrates:picture, Picture ),
   get_position( X, Y ),
   send( Picture, display, new( ObjectId, circle(25) ), point(X,Y) ),
   send( Picture, display, new(Text, text(Label) ), point(X,Y) ),
   send( Text, font, font( screen ,bold, 11 ) ),
   new( _, constraint( ObjectId, Text, identity(center) ) ),
   send( ObjectId, handle, handle(w/2,0,up) ),
   send( ObjectId, handle, handle(w/2,h,dn) ),
   send( ObjectId, handle, handle(0,h/2,lt) ),
   send( ObjectId, handle, handle(w,h/2,rt) ),
   send( ObjectId, recogniser, new( move_gesture(left) ) ).

% Returns the position where the next vertex will be drawn

get_position( X, Y ) :-
   flag( picture_h, H, H ),
   flag( current_x, X, X ),
   flag( current_y, Y, Y ),
   ( Y+75 < H -> flag( current_y, _, Y+75 )
   ; flag( current_x, _, X+75 ),
     flag( current_y, _, 50 ) ).

% Draw the edges between vertices

draw_links :-
   forall( edge(Label1,Label2),
	  ( vertex(Label1,Vertex1),
	    vertex(Label2,Vertex2),
	    handles( Vertex1, Vertex2, Out, In ),
	    new( Link, link( Out, In, line(arrows := second) ) ),
	    send( Vertex1, connect, Vertex2, Link ) ) ).

% Decides which conector to use in a link between vertices

handles( Vertex1, Vertex2, Out, In) :-
   get( Vertex1, position, point(X1,Y1) ),
   get( Vertex2, position, point(X2,Y2) ),
   DeltaX is X2 - X1,
   DeltaY is Y2 - Y1,
   ( DeltaX = 0 -> ( DeltaY > 0 -> Out = dn, In = up ; Out = up, In = dn)
   ; DeltaY = 0 -> ( DeltaX > 0 -> Out = rt, In = lt ; Out = lt, In = rt)
   ; DeltaX > 0 -> ( DeltaY > 0 -> Out = dn, In = up ; Out = up, In = dn)
   ; DeltaY > 0 -> ( DeltaX > 0 -> Out = up, In = dn ; Out = dn, In = up)
   ; DeltaX < 0 -> ( DeltaY > 0 -> Out = dn, In = up ; Out = up, In = dn)
   ; Out=dn, In=up ).

% Colour vertices as coherent (green), incoherent (red) or undefined (yellow).

colour_vertices :-
   findall(V/Vo, vertex(V,Vo), Vs),
   colour_vertices(Vs).

colour_vertices([]) :- !.

colour_vertices(Vs) :-
   select(W/Wo, Vs, Vss),
   (edge(V,W), vertex(V,Vo), get(Vo, fill_pattern, colour(palegreen)), !, C = salmon
   ; not((edge(V,W), vertex(V,Vo), get(Vo, fill_pattern, @nil))), !, C = palegreen),
   send(Wo, fill_pattern, colour(C)), !,
   colour_vertices(Vss).

colour_vertices(Vs) :-
   forall(member(_/Wo, Vs),
	   send(Wo, fill_pattern, colour(yellow))).

consist_prec :- ( consist_prec(socrates) -> set_type, conflicts ; true).

/*----------------------------------------------------------------+
| Set Precedence Type (explicit, implicit or mixed)               |
+----------------------------------------------------------------*/

:- retractall(ptype(_)), assert(ptype(none)).

set_type :-
   var( socrates:rule_precedence_menu, RulePrecedenceMenu ),
   get( RulePrecedenceMenu, selected, explicit, Explicit ),
   get( RulePrecedenceMenu, selected, implicit, Implicit ),
   ( Explicit = @on,  Implicit = @off, !, explicit, nb_getval(explicit,V)
   ; Explicit = @off, Implicit = @on,  !, implicit, nb_getval(implicit,V)
   ; Explicit = @on,  Implicit = @on,  !, mixed,    nb_getval(mixed,V)
   ; Explicit = @off, Implicit = @off, !, none, V=[]),
   nb_setval(precedences,V).

explicit :- retractall(ptype(_)), assert(ptype(explicit)).
implicit :- retractall(ptype(_)), assert(ptype(implicit)).
mixed    :- retractall(ptype(_)), assert(ptype(mixed)).
none     :- retractall(ptype(_)), assert(ptype(none)).

/*----------------------------------------------------------------+
| Display Precedence Relation	                                  |
+----------------------------------------------------------------*/

show_precedences :-
   updateKB(socrates),
   consist_prec(socrates),
   var( socrates:display_editor, DisplayEditor ),
   var( socrates:display_tab, DisplayTab ),
   var( socrates:tab_stack, TabStack ),
   send( TabStack, on_top, DisplayTab ),
   nb_getval(explicit,Explicit), term_to_atom(Explicit,E),
   nb_getval(implicit,Implicit), term_to_atom(Implicit,I),
   nb_getval(mixed,Mixed),	 term_to_atom(Mixed,M),
   Separator='\n--------------------------------------------------------------------\n',
   send(DisplayEditor,clear),
   atomic_list_concat(['Precedence Relations',Separator],Title),
   send(DisplayEditor,append, Title),
   atom_concat('Explicit: ',  E,EText), send(DisplayEditor,append, EText),
   atom_concat('\nImplicit: ',I,IText), send(DisplayEditor,append, IText),
   atom_concat('\nMixed:    ',M,MText), send(DisplayEditor,append, MText),
   send(DisplayEditor,append, Separator),
   ptype(SelectedType),
   atomic_list_concat(['Currently using: ',SelectedType,Separator], CText),
   send(DisplayEditor,append, CText).

/*----------------------------------------------------------------+
| Automatic Number Lines                                          |
+----------------------------------------------------------------*/

number_lines :-
   var( socrates:knowledgebase_editor, KnowledgebaseEditor ),
   send( KnowledgebaseEditor, mark_whole_buffer ),
   get( KnowledgebaseEditor, selected, Selection ),
   get( Selection, value, Text ),
   send( KnowledgebaseEditor, caret, 0 ),
   send( KnowledgebaseEditor, editable, @on ),
   split_string(Text, "\n", "", List),
   delete(List,"",Rules),
   send(KnowledgebaseEditor, clear),
   forall(member(Rule,Rules),
	  ( flag(counter,N,N+1),
		(   Line is N+1,
		    split_string(Rule, "%"," %",[String|_]),
		    atom_length(String,L),
		    Tab is 45-L,
		    swritef( Spaces, '%r', [' ', Tab] ),
		    atomics_to_string([String,Spaces,'% Line ',Line,'\n'],Numbered),
	            send(KnowledgebaseEditor,append,Numbered) )
	  )),flag(counter,N,0).

/*----------------------------------------------------------------+
|	          Automatic Persuasion Dialogue                   |
+----------------------------------------------------------------*/

% Make dialogue start claim dialog

dialogue :-
	not(exists_dialog),
	new(D,dialog('Persuasion Dialogue')),
	new(T,text_item('Insert Claim')),
	send(D,append,T),
	new(Step,button(start,message(@prolog,step,T?selection))),
	new(Show,button(show,message(@prolog,show,T?selection))),
	send(D,append,Step),
	send(D,append,Show),
	send(D,append,button(clear,message(T,clear))),
	var(socrates:step_button, Step),
	var(socrates:dialog,D),
	send(D,open,point(880,280)).

/*----------------------------------------------------------------+
| Set Verbose Mode					          |
+----------------------------------------------------------------*/

:- retractall(verbose(_)), assert(verbose(active)).

verbose :- retract(verbose(active)), assert(verbose(inactive)), !.
verbose :- retract(verbose(inactive)), assert(verbose(active)), !.

/*----------------------------------------------------------------+
| Show Dialogue (step by step mode)                               |
+----------------------------------------------------------------*/

% Verify whether narrative started

step(C) :-
   get(C, value, Text),
   read_from_chars(Text,Literal),
   flag(first_time,V,0),
   (V=1 -> start_dialogue(Literal)),
   fail.

% Show narrative, by each step

step(_) :-
   var( socrates:dialogue_editor, DialogueEditor ),
   var( socrates:step_button, Step),
   flag(index,H,H+1),
   J is H+1,
   atomic_list_concat(['Step <',J,'>'],S),
   send(Step,label,S),
   retract(line(Text)),
   send(DialogueEditor,append,Text).

% Enable start when not started

step(_) :-
   var( socrates:dialogue_editor, DialogueEditor ),
   var( socrates:step_button, Step),
   flag(index,_,0),
   flag(first_time,_,1),
   send(Step,label,'Start'),
   send(DialogueEditor,clear).

/*----------------------------------------------------------------+
| Show Dialogue (at once mode)                                    |
+----------------------------------------------------------------*/

% Verify whether narrative started

show(C) :-
	get(C, value, Text),
        read_from_chars(Text,Literal),
	flag(first_time,V,0),
	(V=1 -> start_dialogue(Literal)),
	fail.

% Show entire narrative, at once

show(_) :-
	var( socrates:dialogue_editor, DialogueEditor ),
        var( socrates:step_button, Step),
        flag(index,_,0),
        send(Step,label,'Start'),
        forall( line(Text),
	    (  send(DialogueEditor,append,Text),
	       retract(line(Text)) )      ),
	flag(first_time,_,1).

% Update editor for new exhibition

show(_) :-
	var( socrates:dialogue_editor, DialogueEditor ),
	send(DialogueEditor,clear).

/*----------------------------------------------------------------+
| Start Persuasion Dialogue					  |
+----------------------------------------------------------------*/

% Start Dialogue: simulates a persuasion dialogue whose goal is
% to prove that Literal is justified by the Domain's knowledge.
% The Domain is the name of a file with the Domain knowledebase.

:- dynamic no/5.

start_dialogue(Literal) :-

   retractall(no(_,_,_,_,_)),
   flag(node,_,1),

   var( socrates:dialogue_editor, DialogueEditor ),
   send( DialogueEditor,clear),
   updateKB(socrates),
   flag(arg,_,1),
   retractall(next(start,_)),
   asserta(next(start,pro:claim(Literal))),


   nb_setval(narrative,[]),
   ignore(dialogue(0,start)),

   outcome(Literal,Outcome,C),
   term_to_atom(Literal,L),
   atomic_list_concat(['\n',Outcome,': ',L],Result),
   assertz(line(Result)),
   send( DialogueEditor?image, colour, C),!.

% claim(+Rule,-Claim)

claim(_:_ then C,C).

/*----------------------------------------------------------------+
| Simulate Dialogue						  |
+----------------------------------------------------------------*/

% dialogue(+Act): simulates a dialogue between 'pro' and 'con'
% (where both roles are played by the same agent), where pro tries
% to defend a claim. To start the simulation, Act must be 'start'.

dialogue(P,Act) :-
   next(Act,NextAct),

   flag(node,N,N+1),

   swritef(Atom,'<%w> %w',[N,NextAct]),

%   term_to_atom(NextAct,Atom),
   assertz(no(N,P,Atom,0,0)),

   new(NextAct),
   say(NextAct),
   upd(NextAct),

   not(final(NextAct)),
   not(dialogue(N,NextAct)).

% new(+Agent:+Locution): is true if Locution is new in the
% narrative (only agent 'con' can repeat since locutions).

new(Agent:Locution) :-
   ( Agent = con, Locution = since(_)
   ; nb_getval(narrative,Narrative),
     not(memberchk(_:Locution,Narrative)) ).

% upd(+Agent:Locution): updates the current narrative with the
% speech act Agent:Locution.

upd(Agent:Locution) :-
   nb_getval(narrative,Narrative),
   nb_setval(narrative,[Agent:Locution|Narrative]).

% say(+Agent:+Locution): write the speech act Agent:Locution to the
% current output (if verbose mode is active).

say(Agent:Locution) :-
    var( socrates:dialogue_editor, DialogueEditor ),
    var( socrates:dialogue_tab, DialogueTab ),
    var( socrates:tab_stack, TabStack ),
   (  verbose(active)
   -> flag(arg,N,N+1),
      term_to_atom(Locution,AtomicLocution),
      atomic_list_concat(['<',N,'> ',Agent,' : ',AtomicLocution,'\n'],Text),
      assertz(line(Text))
   ;  true ),
   send( TabStack, on_top, DialogueTab ),
   send( DialogueEditor, caret, 0 ).

/*----------------------------------------------------------------+
| Protocol Rules                                                  |
+----------------------------------------------------------------*/

% Transition rules (legal moves)
% - A and B are adversarial agents in the dialogue
% - F is a presumed fact and C is its complement
% - R and S are defeasible rules

next(A:claim(F),B:why(F)    ) :- adv(A,B).
next(A:claim(F),B:agree(F)  ) :- adv(A,B).
next(A:why(F),  B:since(R)  ) :- adv(A,B), adduces(R,F).
next(A:why(F),  B:retract(F)) :- adv(A,B).
next(A:since(R),B:why(P)    ) :- adv(A,B), premise(P,R).
next(A:since(R),B:since(S)  ) :- adv(A,B), attacks(B,S,R).
next(A:since(R),B:agree(F)  ) :- adv(A,B), claim(R,F).

% Termination rules

final(_:agree(_)).
final(_:retract(_)).

% adv(?Agent,?Adversary): is true if Agent and Adversary have
% opposite roles in a soliloquy.

adv(pro,con).
adv(con,pro).

% adduces(+R,+F): is true if rule R adduces fact F and is coherent with
% the current state of the narrative.

adduces(R: A then C,C) :-
   rule(R: A then C),
   coherent(A then C).

% premise(-L,+R): is true if L is a premise in rule R

premise(Literal,_:Antecedent then _) :-
   in(Literal,Antecedent),
   neg(Literal,Complement),
   coherent(Complement then nil).

% attacks(+A,+R1,+R2): is true if agent A can use rule R1 to attack
% rule R2.

attacks(B,S: C then N,R:_ then F) :-
   neg(F,N),
   rule(S: C then N),
   coherent(C then N),
   defeats(B,S,R).

% rule(?Rule): is true if Rule is in the knowledge base.

rule(Rule) :- findall(R:A then C,updateKB:then(R,A,C),Clauses), member(Rule,Clauses).

% coherent(+C): is true if the conjunction C is coherent with the
% commitements of the agent in current state of the narrative.

coherent(A then C) :-
   nb_getval(narrative,Narrative),
   forall(in(L,C and A),not(member(_:retract(L),Narrative))),
   forall(in(L,C and A),not((neg(L,N),member(_:agree(N),Narrative)))),
   forall(in(L,A),not((member(_:why(L),Narrative),not(member(_:agree(L),Narrative))))),
   forall(in(L,A),not((neg(L,N),member(_:why(N),Narrative),not(member(_:retract(N),Narrative))))).

% defeating rules

defeats(pro,S,R) :- prec(S,R), !.
defeats(con,S,R) :- prec(S,R), !.
defeats(con,S,R) :- incomparable(S,R).

incomparable(S,R) :- not(prec(S,R)), not(prec(R,S)).

/*----------------------------------------------------------------+
| Outcome Rules				                          |
+----------------------------------------------------------------*/
% - Literal is the claimed fact
% - Outcome is the result of the dialogue

outcome(Literal,Outcome,Color) :-
  nb_getval(narrative,Narrative),
  Narrative \= [],
  (  member(con:agree(Literal),Narrative)
  -> (Outcome,Color) = ('Accepted',blue)
  ;  (Outcome,Color) = ('Rejected',red) ).

/*----------------------------------------------------------------+
| Auxiliary Predicates		                                  |
+----------------------------------------------------------------*/

% neg(?Literal,?Complement): is true if Literal and Complement are
% complementary literals.

neg(not(Proposition),Proposition) :- !.
neg(Proposition,not(Proposition)).

% in(?L,+C): is true if L is a literal in conjunction C.

in(L,L) :- L \= true, L \= and(_,_).
in(L,and(L,_)).
in(L,and(_,C)) :- in(L,C).

/*----------------------------------------------------------------+
| Automatic Dialectical Tree Prototype				  |
+----------------------------------------------------------------*/

:- dynamic vertice/2.

draw_tree :-
   var( socrates:picture, Picture ),
   adjust_coordinates,
   send(Picture,clear),
   retractall(vertice(_,_)),
   draw(Picture,1),
   draw_edges.

popup_tree :-
   var( socrates:picture, Picture ),
   send(Picture,clear),
   new(Dialog,dialog('Dialectical Proof Tree')),
   send(Dialog,append,new(PopUpPicture,picture)),
   send(Dialog,gap,size(0,0)),
   send(Dialog,size,size(700,350)),
   new(_,constraint(Dialog,PopUpPicture,spatial(xref=x,yref=y,xref=x,yref=y,w2=w-25,h2=h-25))),
   send(Dialog,open),
   adjust_coordinates,
   send(PopUpPicture,clear),
   retractall(vertice(_,_)),
   draw(PopUpPicture,1),
   draw_edges.

% Draw tree vertices

draw(Picture,V) :-
   forall( no(N,V,_,_,_), draw(Picture,N)),
   no(V,_,_,Xv,Yv),
   draw_vertices(Picture,V,Xv,Yv,O),
   assertz(vertice(V,O)).

% Draw a single vertex

draw_vertices(Picture,N,Xn,Yn,O) :-
   X is Xn + 5, % *34
   Y is Yn*55 + 5,
   no(N,_,TT,_,_),
   new(T,text(TT)),
   get(T,width,W),
   send(Picture,display,new(O,box(W+10,18)),point(X,Y)),
   send(Picture,display,T,point(X,Y)),
   ( not(no(_,N,_,_,_))  -> Colour = lightpink
   ; (no(V,N,_,_,_),
      vertice(V,O1),
      get(O1,colour,colour(C)),
      C=darkseagreen1) -> Colour=lightpink
   ; Colour =  darkseagreen1),
   send(O,fill_pattern,colour(Colour)),
   send(O,colour,colour(Colour)),
   send(T,font,font(curier,bold,11)),
   new(_,constraint(O,T,identity(center))),
   send(O,handle,handle(w/2,0,up)),
   send(O,handle,handle(w/2,h,dn)),
   send(O,recogniser,new(move_gesture(left))).

% Draw tree edges

draw_edges :-
   forall( ( no(F,P,_,_,_), F>1 ),
	   ( vertice(P,Op),
	     vertice(F,Of),
	     new(Link,link(dn,up,new(L,line(arrows:=second)))),
	     send(L,colour,gray29),
	     send(Op,connect,Of,Link))).

% Adjust coordinates of the tree nodes

adjust_coordinates :-
   forall((current_flag(Y),integer(Y)),flag(Y,_,0)),
   positions_node(1,0).

% Positions the P node in Yp line

positions_node(P,Yp) :-
   Yf is Yp+1,
   forall(no(F,P,_,_,_),positions_node(F,Yf)),
   flag(Yp,X,X),
   average_x(P,Xm),
   Xp is max(X,Xm),
   define(P,Xp,Yp),
   Dx is X-Xm,
   forall((Xp>Xm,no(F,P,_,_,_)),move_child(F,Dx)).

% Determines the average position Xm between P node children

average_x(P,Xm) :-
   findall(X,no(_,P,_,X,_),Xs),
   (  Xs = []
   -> Xm is 0
   ;  min_list(Xs,Xmin),
      max_list(Xs,Xmax),
      no(_,P,T,Xmax,_),
      new(TT,text(T)),
      get(TT,width,W),
      free(TT),
      no(P,_,Tp,_,_),

            new(TTp,text(Tp)),
      get(TTp,width,Wp),
      free(TTp),


      %get(TTp,width,Wp),
      Xm is (Xmin+Xmax+W)/2 - Wp/2 ).

% Defines P node position as (X,Y) coordinates

define(P,X,Y) :-
   retract(no(P,A,T,_,_)),
   assertz(no(P,A,T,X,Y)),
         new(TT,text(T)),
      get(TT,width,W),
      free(TT),

%   get(T,width,W),
   flag(Y,_,X+W+20),!. % <==== horizontal division

% Moves P node to the right, adding Dx to its X

move_child(F,Dx) :-
   (  no(F,_,_,X,Y)
   -> define(F,X+Dx,Y),
      forall(no(N,F,_,_,_),move_child(N,Dx))
   ;  true ).













