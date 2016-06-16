/*----------------------------------------------------------------+
|* Authors: Silvio do Lago, Luiz F. Zarco & Lucio N. Lira.        |
|* Institution: São Paulo Technological College (FATEC-SP).       |
|* Copyright (c) 2013-2016					  |
|*								  |
|* This file is part of aristotle software source code.           |
|* The software source code is distributed under MIT License.     |
|* Read LICENSE.txt for further information.			  |
+----------------------------------------------------------------*/

/*----------------------------------------------------------------+
| Knowledgebase Core Operations	                                  |
+----------------------------------------------------------------*/

:- module(updateKB, [then/3, believes/2, precedes/2, vert/2, vertex/1, vertex/2, edge/2, updateKB/1,consist_prec/1,acyclic/1,transitive_closure/2,filtered/2]).
:- use_module(globvars).
:- op(100, fy, not).
:- op(101, xfy, and).
:- op(102, xfx, then).
:- op(103, xfx, :).
:- op(104, xfx, believes).
:- op(105, xfx, precedes).
:- dynamic then/3, believes/2, precedes/2, vert/2, vertex/1, vertex/2, edge/2.
:- discontiguous precedence_relation/2.

updateKB(Caller) :-
   var(Caller:knowledgebase_editor,KnowledgebaseEditor),
   get(KnowledgebaseEditor,modified,Modified),
   Modified = @on, !,
   retractall(then(_, _, _)),
   retractall(believes(_, _)),
   retractall(precedes(_, _)),
   retractall(vert(_, _)),
   retractall(vertex(_)),
   retractall(vertex(_, _)),
   retractall(edge(_, _)),
   var(Caller:knowledgebase_editor, KnowledgebaseEditor),
   var(Caller:arguments_editor,     ArgumentsEditor),
   var(Caller:display_editor,       DisplayEditor),
   var(Caller:dialogue_editor,      DialogueEditor),
   var(Caller:picture,              Picture),
   send(ArgumentsEditor,            clear),
   send(DisplayEditor,              clear),
   send(DialogueEditor,             clear),
   send(Picture,                    clear),
   send(KnowledgebaseEditor,        mark_whole_buffer),
   get(KnowledgebaseEditor,         selected, Selection),
   get(Selection,                   value, Text),
   flag(rule_number, _, 1),
   clauses(Caller, Text, Clauses),
   forall(member(Clause, Clauses),
	   catch(insertKB(Clause),
	   Error,
	   show_error(Caller, Error))),
   findall(R: A then C, then(R, A ,C), SetOfClauses),
   unflawed(SetOfClauses),
   not(gratuitous(SetOfClauses)),
   consist_prec(Caller),
   precedence_relation(explicit, Explicit),
   precedence_relation(implicit, Implicit),
   precedence_relation(mixed,    Mixed),
   nb_setval(explicit, Explicit),
   nb_setval(implicit, Implicit),
   nb_setval(mixed,    Mixed),
   send(KnowledgebaseEditor, caret, 0),
   send(KnowledgebaseEditor, editable, @on),
   send(KnowledgebaseEditor, modified, @off).

updateKB(_).

clauses( Caller, Text, Clauses ) :-
   atom_chars( Text, CharList ),
   split( Caller, CharList, SetOfClauses ),
   subtract( SetOfClauses, [syntax_error], Clauses ).

split( Caller, CharList, [Clause|Clauses] ) :-
   append( Prefix, ['.'|Rest], CharList ), !,
   atom_chars( Text, Prefix ),
   get_clause( Caller, Text, Clause ),
   split( Caller, Rest, Clauses ).

split( Caller, Text, [Clause] ) :-
   get_clause( Caller, Text, Clause ).

get_clause( Caller, Text, Clause ) :-
   catch( read_from_chars( Text, Clause ),
	  Error,
	  ( show_error( Caller, Error ), Clause = syntax_error ) ).

/*----------------------------------------------------------------+
| Consistency Check                                               |
+----------------------------------------------------------------*/

% Function: Verify consistence between explict precedence rules
% Output: Message indicating the existence of cycles (that conflicts
% cannot be generated due to inconsistency)

consist_prec(Caller) :-
   var( Caller:display_editor, DisplayEditor ),
   var( Caller:display_tab, DisplayTab ),
   var( Caller:tab_stack, TabStack ),
   vertices(Caller,Vs),
   findall(V->R,(member(V,Vs),acessa(Caller,V,ciclo(R))),L),
   findall(A,(member(T,L), term_to_atom(T,A)),P),
   P \= [],
   Separator='\n--------------------------------------------------------------------\n',
   atomics_to_string(['Error: cyclic precedence rules.',Separator],S),
   atomic_list_concat([S|P],Ciclos),
   send( TabStack, on_top, DisplayTab ),
   send(DisplayEditor,clear),
   send(DisplayEditor,append, Ciclos), !, fail.

consist_prec(_).

% Draw vertices which represent the arguments

vertices(Caller,Vertices) :-
       findall([V,W],Caller:precedes(V,W),L),
       flatten(L,F),
       sort(F,Vertices).

acessa(Caller,V,L) :-
	acessa(Caller,[V],nil,[],L).

acessa(_,[],_,A,acessa(L)) :-
	reverse(A,L), !.

acessa(_,_,A,A,ciclo(L)) :-
	reverse(A,L), !.

acessa(Caller,[V|Vs],_,A,L) :-
       findall(W,Caller:precedes(V,W),F),
       union(Vs,F,N),
       union([V],A,M),
       acessa(Caller,N,A,M,L).

/*----------------------------------------------------------------+
| Check if a set of clauses is consistent                         |
+----------------------------------------------------------------*/

% unflawed(SetOfClauses): is true if all defeasible rules in
% SetOfClauses are consistent rules.

unflawed([]) :- !.
unflawed([precedes(_,_)|SetOfClauses]) :- !,
   unflawed(SetOfClauses).
unflawed([_:then(A,C)|SetOfClauses]) :-
   consistent(then(A,C)), !,
   unflawed(SetOfClauses).
unflawed([L:then(A,C)|_]) :-
	show_error( socrates, error('Semantic Error -> inconsistent_rule',L:then(A,C))),!,fail.

% consistent(+then(Antecedent,Consequent)): is true if
% 'Antecedent and Consequent' is consistent.

consistent(then(Antecedent,Consequent)) :-
   findall(Premise,in(Premise,Antecedent),Premises),
   not( ( member(Proposition1,[Consequent|Premises]),
          member(Proposition2,[Consequent|Premises]),
          neg(Proposition1,Proposition2) ) ), !.

/*----------------------------------------------------------------+
| Check if a set of clauses is gratuitous                         |
+----------------------------------------------------------------*/

gratuitous(Clauses) :-
   findall(R,( member(R,Clauses),gratuitous(R,Clauses)),G), G\=[],
   show_error( socrates, error('Semantic Error -> gratuitous_consequent',G)).

gratuitous(_:then(A,C),_) :- in(C,A), !.
gratuitous(_:then(A1,C1),Clauses) :-
   member(_:then(A2,C2),Clauses),
   in(L1,A1), der(L1,C2),
   in(L2,A2), der(L2,C1), !.

/*----------------------------------------------------------------+
| Insert a clause into the knowledgebase                          |
+----------------------------------------------------------------*/

insertKB(believes(A, then(P, Q))   ) :- !, flag(rule_number, R, R + 1), assertz(believes(A, then(R, P, Q))).
insertKB(believes(A, R: then(P, Q))) :- !, assertz(believes(A, then(R, P, Q))).
insertKB(believes(A, R: P)         ) :- !, assertz(believes(A, then(R, true, P))).
insertKB(believes(A, P)            ) :- !, flag(rule_number, R, R + 1), assertz(believes(A, then(R, true, P))).
insertKB(then(P, Q)                ) :- !, flag(rule_number, R, R + 1), assertz(then(R, P, Q)).
insertKB(R: then(P, Q)             ) :-    number(R), throw(error('Syntax Error -> invalid_label', R: then(P, Q))), !.
insertKB(R: then(P, Q)             ) :- !, assertz(then(R, P, Q)).
insertKB(precedes(M, N)		   ) :- !, assertz(precedes(M, N)).
insertKB(R: P                      ) :- !, assertz(then(R, true, P)).
insertKB(end_of_file               ) :- !.
insertKB(P		           ) :- !, flag(rule_number, R, R + 1), assertz(then(R, true, P)).

/*----------------------------------------------------------------+
| Empty Precedence Relation                                       |
+----------------------------------------------------------------*/

precedence_relation(none,[]).

/*----------------------------------------------------------------+
| Explicit Precedence Relation                                    |
+----------------------------------------------------------------*/

% precedence_relation(explicit,-Precedences): is true if Precedences
% is the set of explitit precedence rules synthesized from precedence
% rules declared in the knowledge base.

precedence_relation(explicit,Precedences) :-
   findall(L1<L2,updateKB:precedes(L1,L2),Explicit),
   transitive_closure(Explicit,Closure),
   filtered(Closure,Precedences).

% transitive_closure(+R,-T): is true if the transitive closure of the
% acyclic binary relation R is T

transitive_closure(R,T) :-
   acyclic(R),
   ( setof(L1<L2,transition(L1,L2,R),T), ! ; T=[] ).

% acyclic(+Relation): is true if the Relation is acyclic

acyclic([]) :- !.
acyclic(Relation) :-
   select(L1<_,Relation,NewRelation),
   not(member(_<L1,Relation)),
   acyclic(NewRelation), !.

% transition(-L1,-L2,+R): is true if the transition L1<L2 can be
% inferred from the precedence rules in the relation R.

transition(L1,L2,R) :- member(L1<L2,R).
transition(L1,L2,R) :- member(L1<L,R), transition(L,L2,R).

% Elimiate precedences between rules which have no complementary
% consequents (since they are useless).

filtered(Closure,Precedences) :-
   findall(Label1<Label2,
	   ( member(Label1<Label2,Closure),
	     ( then(Label1,_,Consequent1) ; believes(_,then(Label1,_,Consequent1))),
	     ( then(Label2,_,Consequent2) ; believes(_,then(Label2,_,Consequent2))),
	     neg(Consequent1,Consequent2) ),
	   Precedences).

/*----------------------------------------------------------------+
| Implicit Precedence Relation                                    |
+----------------------------------------------------------------*/

% precedence_relation(implicit,-Precedences): is true if Precedences is
% the set of implitit precedence rules synthesized from defeasible
% rules declared in the knowledge base.

precedence_relation(implicit,Precedences) :-
   findall(Label,(then(Label,_,_) ; believes(_,then(Label,_,_))),Labels),
   findall(L1<L2,
	   ( member(L1,Labels),
             member(L2,Labels),
             L1 \= L2,
	     more_specific(L1,L2) ),
	   Precedences).

% more_specific(L1,L2): is true if: (1) L1 is not a presumption, (2) L1
% and L2 are conflicting rules, (3) the antecedent of L2 can be derived
% from the antecedent of L1, and (4) and the antecedent of L1 cannot be
% derived from the antecedent of L1.

more_specific(L1,L2) :-
   rule(L1: A1 then C1),
   rule(L2: A2 then C2),
   neg(C1,C2),
   der(A2,A1),
   not(der(A1,A2)), !.

rule(L: A then C) :- then(L,A,C).
rule(L: A then C) :- believes(_, then(L,A,C)).

% rule(Rule) :- nb_getval(kb,Clauses), member(Rule,Clauses).

% der(+C1,+C2): is true if conjunction C1 is defeasibly derivable from
% the conjunction C2.

der(C1,C2) :-
   setof(L,in(L,C2),Ls),
   forall(in(L,C1),ddt([L],Ls)).

% Defeasible Derivation Tree

ddt([L|H],_) :- memberchk(L,H), !, fail. % avoid cyclic rules
ddt([L|_],Ls) :- memberchk(L,Ls), !.
ddt([L|H],Ls) :-
   rule(_: A then L), in(X,A),
   ddt([X,L|H],Ls).

in(L,L) :- L \= true, L \= and(_,_).
in(L,and(L,_)).
in(L,and(_,C)) :- in(L,C).

/*----------------------------------------------------------------+
| Mixed Precedence Relation                                       |
+----------------------------------------------------------------*/

% precedence_relation(mixed,-Precedences): is true if
% Precedences is the set of mixed precedence rules synthesized
% from the integration of implicit and explicit precedence
% relations.

precedence_relation(mixed,Precedences) :-
   precedence_relation(explicit,Explicit),
   precedence_relation(implicit,Implicit),
   integrate(Explicit,Implicit,Mixed),
   transitive_closure(Mixed,Closure),
   filtered(Closure,Precedences).

% Integrate explicit and implicit precedence relations
% (prioritizing explicit precedences).

integrate(Explicit,Implicit,Mixed) :-
   union(Explicit,Implicit,Union),
   purge(Union,Explicit,Mixed).

% Eliminate cycles caused by explcit precedences, by
% removing the weakest implicit precedences in the Relation.

purge(Relation,Explicit,Purged) :-
   reject(Relation,Explicit,Links), !,
   subtract(Relation,Links,NewRelation),
   purge(NewRelation,Explicit,Purged).
purge(Relation,_,Relation).

% reject(+Relation,+Explicit,-Links): is true if Relation is cyclic and
% Links are the weakest link in a shortest cycle in this relation.

reject(Relation,Explicit,Links) :-
   setof([L1],L2^member(L1<L2,Explicit),Sources),
   shortest(Sources,Relation,Cycle),
   weakest(Cycle,Explicit,Links), !,
   Links \= [].

% shortest(+ListOfSources,+Relation,-Cycle): is true if Cycle is a
% shortest cycle in the Relation, starting from one of the labels in the
% ListOfSources. If Relation is acyclic, Cycle is an empty path.

shortest([],_,[]) :- !.
shortest([[Label|Labels]|_Paths],_Relation,[Label|Labels]) :-
   member(Label,Labels), !.
shortest([Path|Paths],Relation,Cycle) :-
   Path = [Label|_],
   findall([NextLabel|Path],member(Label<NextLabel,Relation),Successors),
   append(Paths,Successors,NewForest),
   shortest(NewForest,Relation,Cycle).

% weakest(+Cycle,+Explicit,-Links): is true if Links is the set
% of weakest links in the Cycle. The weakest links in the Cycle is
% all implicit links (derived from an implicit precedence) that
% immediately precedes an explict link (derived from an explicit
% precedence).

weakest(Cycle,Explicit,Links) :-
   findall(L1<L2,
	   ( append(_,[L2,L1|_],Cycle),	  % find link that
	     not(member(L1<L2,Explicit)), % is an implicit link and
	     memberchk(L2<_,Explicit) ),  % precedes an explicit link
	   Links).

/*----------------------------------------------------------------+
| Auxiliary Predicate		                                  |
+----------------------------------------------------------------*/

neg(not(Proposition),Proposition) :- !.
neg(Proposition,not(Proposition)).

/*----------------------------------------------------------------+
| Error Messages                                                  |
+----------------------------------------------------------------*/

show_error( Caller, error( syntax_error( ErrorType ), string( String, ErrorPosition ) )) :-
   var( Caller:display_editor, DisplayEditor ),
   atom_chars( String, CharList ),
   length( PrefixWithError, ErrorPosition ),
   append( PrefixWithError, _, CharList ),
   atom_chars( AtomicPrefixWithError, PrefixWithError ),
   atomic_list_concat( [ ErrorType,': ', AtomicPrefixWithError], Message ),
   Separator='\n--------------------------------------------------------------------\n',
   atomic_list_concat( ['Syntax Error -> ', Message, Separator],Text),
   send( DisplayEditor, append, Text ), !.

show_error( Caller, error(Type,Error)) :-
   var( Caller:display_editor, DisplayEditor ),
   term_to_atom( Error, Message ),
   Separator='\n--------------------------------------------------------------------\n',
   atomic_list_concat([Type,': ',Message,Separator],Text),
   send( DisplayEditor, append, Text), !.















