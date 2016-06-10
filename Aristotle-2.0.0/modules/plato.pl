%+-----------------------------------------------------------------+
%� plato - a system for dialogical argumentation                   �
%+-----------------------------------------------------------------+
%� Plato (424/423 BC � 348/347 BC), was a Classical Greek          �
%� philosopher, mathematician, student of plato, writer of         �
%� philosophical dialogues, and founder of the Academy in Athens,  �
%� the first institution of higher learning in the Western world.  �
%� Along with his mentor, plato, and his student, Aristotle, Plato �
%� helped to lay the foundations of Western philosophy and science.�
%� [http://thyselfknow.com/plato/]                                 �
%+-----------------------------------------------------------------+

:- module(plato, [plato/0]).

% ------------------------------------------------------------------
% Importa��o de m�dulos
% ------------------------------------------------------------------

:- use_module(modules/fbrowser).
:- use_module(modules/updateKB).
:- use_module(modules/globvars).

% ------------------------------------------------------------------
% Defini��o de operadores
% ------------------------------------------------------------------

%:- export(':'/2).
:- op(100,  fy, not).      % Operador de nega��o.
:- op(101, xfy, and).      % Operador l�gico de conjun��o.
:- op(102, xfx, then).     % Operador de implica��o.
:- op(103, xfx, :).        % Operador para r�tulo de regras.
:- op(104, xfx, believes). % Operador que indica cren�a.
:- op(105, xfx, precedes). % Operador que estabelece preced�ncia.

% ------------------------------------------------------------------
% Defini��o de predicados din�micos
% ------------------------------------------------------------------

:- dynamic aprende/1. % Define a possibilidade de aprendizado durante o di�logo.
:- dynamic choice/1.  % Guarda a preced�ncia mediada entre regras revog�veis.
:- dynamic line/1.    %
:- dynamic explain/1. % Define a necessidade de explica��o total no di�logo.
:- dynamic next/2.    % Define o pr�ximo ato de fala permitido.
:- dynamic ptype/1.   % Define o tipo de rela��o de preced�ncia ativo.
:- dynamic tmp/1.     % Cl�usula para regras tempor�rias.
:- dynamic verbose/1. % Define se o di�logo ter� sa�das detalhadas.

:- discontiguous precedence_relation/2.

% ------------------------------------------------------------------
% Interface gr�fica
% ------------------------------------------------------------------

plato :-
    not(exists_frame),
    new(Frame,                       frame('Plato - Dialogical Argumentation (version 2.0.0)')),
    new(MenuBar,                     menu_bar),
    new(FileMenu,                    popup(file)),
    new(OptionsMenu,                 popup(options)),
    new(KnowledgebaseTab,            tab(knowledgebase)),
    new(DisplayTab,                  tab(display)),
    new(ArgumentsTab,                tab(arguments)),
    new(DialogueTab,                 tab(dialogue)),
    new(TabStack,                    tab_stack),
    new(KnowledgebaseEditor,         editor),
    new(ArgumentsEditor,             editor),
    new(DisplayEditor,               editor),
    new(DialogueEditor,              editor),
    new(RulePrecedenceMenu,          menu(rule_precedence, toggle)),
    new(Picture,                     picture),
    new(RightDialog,                 dialog),
    new(LeftDialog,                  dialog),
    new(Column_Left,                 dialog),
    var(plato:frame,                 Frame),
    var(plato:knowledgebase_tab,     KnowledgebaseTab),
    var(plato:arguments_tab,         ArgumentsTab),
    var(plato:display_tab,           DisplayTab),
    var(plato:dialogue_tab,          DialogueTab),
    var(plato:knowledgebase_editor,  KnowledgebaseEditor),
    var(plato:arguments_editor,      ArgumentsEditor),
    var(plato:display_editor,        DisplayEditor),
    var(plato:dialogue_editor,       DialogueEditor),
    var(plato:tab_stack,             TabStack),
    var(plato:rule_precedence_menu,  RulePrecedenceMenu),
    var(plato:picture,               Picture),
    var(plato:current_path,          @('')),
    new(ArgumentsButton,             button(arguments,           message(@prolog, arguments_click))),
    new(ConflictsButton,             button(conflicts,           message(@prolog, consist_prec))),
    new(DialogueButton,              button(dialogue,            message(@prolog, dialogue))),
    new(OpenMenuItem,                menu_item(open,             message(@prolog, loadfile, plato, dkb))),
    new(NewMenuItem,                 menu_item(new,              message(@prolog, newfile, plato, dkb))),
    new(SaveMenuItem,                menu_item(save,             message(@prolog, savefile, plato))),
    new(SaveAsMenuItem,              menu_item(save_as,          message(@prolog, saveasfile, plato, dkb), end_group := true)),
    new(QuitMenuItem,                menu_item(quit,             message(@prolog, close))),
    new(LearningMenuItem,            menu_item('Learning mode (off)',    message(@prolog, learning_mode))),
    new(ExplanationMenuItem,         menu_item('Explanation mode (off)', message(@prolog, explanation_mode))),
    new(VerboseMenuItem,             menu_item('Verbose mode (off)',     message(@prolog, verbose_mode),end_group := true)),
    var(plato:learning_menu_item,    LearningMenuItem),
    var(plato:explanation_menu_item, ExplanationMenuItem),
    var(plato:verbose_menu_item,     VerboseMenuItem),
    new(PrecedencesMenuItem,         menu_item(show_precedences, message(@prolog, show_precedences))),
    new(ItemExplicit,                menu_item(explicit,         message(@prolog, consist_prec))),
    new(ItemImplicit,                menu_item(implicit,         message(@prolog, consist_prec))),
    new(ItemMediated,                menu_item(mediated,         message(@prolog, consist_prec))),
    var(plato:item_explicit,         ItemExplicit),
    var(plato:item_implicit,         ItemImplicit),
    var(plato:item_mediated,         ItemMediated),
    send_list(Frame,                 append, [LeftDialog,RightDialog]),
    send_list(MenuBar,               append, [FileMenu,OptionsMenu]),
    send_list(FileMenu,              append, [OpenMenuItem,NewMenuItem,SaveMenuItem,SaveAsMenuItem,QuitMenuItem]),
    send_list(OptionsMenu,           append, [LearningMenuItem,ExplanationMenuItem,VerboseMenuItem,PrecedencesMenuItem]),
    send_list(TabStack,              append, [KnowledgebaseTab,DisplayTab,ArgumentsTab,DialogueTab]),
    send_list(LeftDialog,            append, [MenuBar,TabStack,ArgumentsButton,ConflictsButton,DialogueButton]),
    send_list(RightDialog,           append, [RulePrecedenceMenu]),
    send_list(RulePrecedenceMenu,    append, [ItemExplicit,ItemImplicit,ItemMediated]),
    forall(member(Tab/Editor, [KnowledgebaseTab/KnowledgebaseEditor,DisplayTab/DisplayEditor,
                               ArgumentsTab/ArgumentsEditor,DialogueTab/DialogueEditor]),
          (send(Tab, gap, size(2, 2)),
           send(Tab, append, Editor))),
    forall(member(Editor/Background, [KnowledgebaseEditor/aliceblue,DisplayEditor/lavenderblush,
                                      ArgumentsEditor/lightyellow,DialogueEditor/mintcream]),
          (send(Editor, size, size(70, 28)),
           send(Editor, font, font(screen, normal, 12)),
           send(Editor, colour, darkblue),
           send(Editor, background, Background))),
    send(LeftDialog, left, RightDialog),
    send(RightDialog, gap, size(15, 3)),
    send(RightDialog, append, Picture),
    send(Picture, size, size(480, 480)),
    send(Column_Left, pen, 0),
    send(Column_Left, gap,    size(0,0)),
    send(Column_Left, left,   LeftDialog),
    send(Column_Left, append, bitmap(image('resources/images/column_p.jpg'))),
    send(Frame, open, point(50, 70)).

% Fun��o: Gera todos os argumentos.
% Sa�das: Mensagem confirmando a gera��o de argumentos.
% Op��es: Bot�o "Ok" para fechar a caixa de di�logo.

arguments_click :-
   var( plato:display_editor, DisplayEditor ),
   updateKB( plato ),
   ( get( DisplayEditor, selected, _ )
   ->  var( plato:display_tab, DisplayTab ),
       var( plato:tab_stack, TabStack ),
       send( TabStack, on_top, DisplayTab )
   ;   update_args,!, arguments_success ).

arguments_success :-
    not(exists_dialog),
    new(Dialog, dialog('Argumentational System')),
    send(Dialog, append, text('All arguments were successfully generated.')),
    send(Dialog, append, button(ok, message(Dialog, destroy))),
    var(plato:dialog, Dialog),
    send(Dialog, open, point(880, 280)).

% Verifica se o frame j� existe (est� em exibi��o).
exists_frame :-
    var(plato:frame, Frame),
    catch(send(Frame, expose), Error, true),
    var(Error).

% Verifica se o dialog j� existe (est� em exibi��o).
exists_dialog :-
    var(plato:dialog, Dialog),
    catch(send(Dialog, expose), Error, true),
    var(Error).

% Fecha a janela do m�dulo plato.
close :-
    var(plato:frame, Frame),
    send(Frame, destroy).

% Fun��o: Verifica se as regras de preced�ncia expl�cita est�o corretas.
% Sa�das: (1) Mensagem indicando a exist�ncia de ciclos.
%         (2) Mensagem indicando a verifica��o de consist�ncia.
%         (3) Mensagem indicando que os conflitos n�o podem ser gerados.
consist_prec :-
    vertices(Vs),
    findall(V->R, (member(V, Vs), acessa(V, ciclo(R))), L),
    findall(A, (member(T, L), term_to_atom(T, A)), P),
    P \= [],
    atomics_to_string(['Error: cyclic precedence rules.\n'], S),
    atomic_list_concat([S|P], '\n', Ciclos),
    not(exists_dialog),
    new(D, dialog('plato\' Automatic Consistency Check')),
    new(F, picture('box')),
    send(F, display, text(Ciclos), point(20, 20)),
    send(D, append, text('Conflicts cannot be generated.')),
    send(D, append, F),
    var(plato:dialog, D),
    send(D, open).

consist_prec :- set_type, conflicts.

vertices(Vertices) :-
    findall([V,W], plato:precedes(V, W), L),
    flatten(L, F),
    sort(F, Vertices).

acessa(V,L) :- acessa([V], nil, [], L).

acessa([], _, A, acessa(L)) :- reverse(A, L), !.

acessa(_, A, A, ciclo(L)) :- reverse(A, L), !.

acessa([V|Vs], _, A, L) :-
    findall(W, plato:precedes(V, W), F),
    union(Vs, F, N),
    union([V], A, M),
    acessa(N, A, M, L).

% ------------------------------------------------------------------
% Gera��o de argumentos e atualiza��es
% ------------------------------------------------------------------

% arguments(+File): gera os argumentos a partir das regras.
% +File: define se as regras ser�o coletadas do arquivo (yes) ou,
% caso contr�rio (no), da mem�ria.

arguments(Update) :-
    var(plato:display_editor, DisplayEditor),
    ( Update=yes -> updateKB(plato); true),
    (get(DisplayEditor, selected, _)
    ->  var(plato:display_tab, DisplayTab),
        var(plato:tab_stack, TabStack),
        send(TabStack, on_top, DisplayTab)
    ;   update_args).

% Atualiza a visualiza��o dos argumentos.
update_args :-
    var(plato:arguments_editor, ArgumentsEditor),
    var(plato:arguments_tab, ArgumentsTab),
    var(plato:tab_stack, TabStack),
    send(ArgumentsEditor, clear),
    all_arguments(Args),
    forall(member(Arg, Args),
          (formatted(Arg, Text),
	   send(ArgumentsEditor, append, Text))),
    send(TabStack, on_top, ArgumentsTab),
    send(ArgumentsEditor, caret, 0).

% Formata um argumento como um texto.
formatted(Number:Support, Text) :-
    term_to_atom(Number, N),
    atom_length(N, Length),
    Tab is Length + 5,
    swritef(NewLine, ',\n%r', [' ', Tab]),
    join(NewLine, Support, Atoms),
    vert(Agent, Number),
    (Agent = paul -> A = 'P' ; A = 'O'),
    atomic_list_concat([A, N, ' : [' | Atoms], Text).

% Junta as cla�sulas em uma lista com uma nova linha.
join(_, [N: Last], [Atom,']\n\n']) :- !, term_to_atom(N:Last, Atom).

join(NewLine, [N:First|Rest], [Atom,NewLine|Atoms]) :-
    term_to_atom(N:First, Atom),
    join(NewLine, Rest, Atoms).

% Gera todos os argumentos poss�veis.
all_arguments(Arguments) :-
    retractall(vert(_,_)),
    retractall(edge(_,_)),
    flag(arg_number, _, 1),
    claims(Claims),
    findall(Order/Agent/Length/Support,
	   (member(Order/Agent, [1/paul, 2/olga]),
	    member(Fact, Claims),
	   (argument(Agent, Fact, Support)
	   ;argument(Agent, not(Fact), Support)),
	   length(Support, Length)),
	   Supports),
    msort(Supports, SortedSupports),
    findall(N:Support,
	   (member(Order/Agent/Length/Support, SortedSupports),
	    flag(arg_number, N, N + 1),
	    assertz(vert(Agent, N))),
	    Arguments),
    forall((member(N1:S1, Arguments),
	    member(N2:S2, Arguments),
	    N1 \= N2, attacks(S1, S2)),
	    assertz(edge(N1, N2))).

% Verifica se o suporte de S1 ataca o suporte de S2.
attacks(Support1, Support2) :-
    last(Support1, Rule1:then(_, Fact1)),
    member(Rule2:then(_C, Fact2), Support2),
    neg(Fact1, Fact2),
    not(prec(Rule2, Rule1)), !.

% Gera todas as locu��es poss�veis.
claims(Claims) :-
    predicates(Predicates),
    objects(Objects),
    findall(Claim, (member(Predicate/Arity, Predicates),
		    length(Arguments, Arity),
		    tuple(Arguments, Objects),
		    Claim =.. [Predicate|Arguments]),
	    Claims).

% Localiza todos os objetos relevantes na base de conhecimento.
objects(Objects) :-
    findall(Object, (believes(_,then(_,_,Conclusion)),
		    (Conclusion = not(Fact) -> Fact =.. [_|Arguments]
		    ;Conclusion =.. [_|Arguments]),
		     member(Object, Arguments),
		     atomic(Object)),
	    ObjectList),
    sort(ObjectList, Objects).

% Localiza todos os predicados relevantes na base de conhecimento.
predicates(Predicates) :-
    findall(Predicate/Arity, (believes(_,then(_,_,Conclusion)),
			      (Conclusion = not(Fact) -> Fact =.. [Predicate|Arguments]
			      ; Conclusion =.. [Predicate|Arguments]),
			      length(Arguments, Arity)),
	    PredicatetList),
    sort(PredicatetList, Predicates).

% Gera uma tupla com os objetos de um conjunto.
tuple([], _).

tuple([Object|Tuple], Set) :-
    member(Object, Set),
    tuple(Tuple, Set).

% Gera um argumento para um fato (i.e., um suporte).
argument(Agent, Fact, Support) :- argument(Agent, Fact and true, [], Support).

argument(_Agent, true, Support, Support).

argument(Agent, Fact and Facts, Partial, Support) :-
    believes(Agent, then(RuleNumber, Conditions, Fact)),
    add(Conditions, Facts, NewFacts),
    union([RuleNumber : Conditions then Fact], Partial, NewPartial),
    argument(Agent, NewFacts, NewPartial, Support).

% Anexa duas f�rmulas conjuntivas.
add(true, B, B) :- !.
add(A and B, C, A and D) :- add(B, C, D).
add(A, B, A and B).

%+-----------------------------------------------------------------+
%�                  GRAFO DE DECIS�O DE COER�NCIA                  �
%� amarelo (bloqueado), vermelho(derrotado) e verde(n�o derrotado) �
%+-----------------------------------------------------------------+

% Exibe o grafo de decis�o de coer�ncia dos argumentos da base de
% conhecimentos.
conflicts :-
    update_args,
    var(plato:picture, Picture),
    send(Picture, clear),
    draw_vertices,
    draw_links.

pconflicts :-
    var(plato:picture, Picture),
    send(Picture, clear),
    draw_vertices,
    draw_links.

% Desenha os v�rtices.
draw_vertices :-
    initiate_position_variables,
    retractall(vertex(_, _)),
    forall(vert(_, Label),
	  (draw_vertex(Label, ObjectId),
	   assertz(vertex(Label, ObjectId)))),
    colour_vertices.

% Inicializa��o da posi��o global das vari�veis.
initiate_position_variables :-
    findall(Vertex, vert(_, Vertex), Vertices),
    length(Vertices, Length),
    Height is 75 + (ceil(sqrt(Length) -1)) * 75,
    flag(current_x, _, 50),
    flag(current_y, _, 50),
    flag(picture_h, _, Height).

% Desenha um �nico v�rtice e retorna seu identificador.
draw_vertex(Label, ObjectId) :-
    var(plato:picture, Picture),
    get_position(X, Y),
    (vert(paul,Label) -> V = box(25,25) ; V = circle(25)),
    send(Picture, display, new(ObjectId, V), point(X,Y)),
    send(Picture, display, new(Text, text(Label)), point(X,Y)),
    send(Text, font, font(screen ,bold, 11)),
    new(_, constraint(ObjectId, Text, identity(center))),
    send(ObjectId, handle, handle(w/2,0,up)),
    send(ObjectId, handle, handle(w/2,h,dn)),
    send(ObjectId, handle, handle(0,h/2,lt)),
    send(ObjectId, handle, handle(w,h/2,rt)),
    send(ObjectId, recogniser, new(move_gesture(left))).

% Retorna a posi��o onde o pr�ximo v�rtice ser� desenhado.
get_position(X, Y) :-
    flag(picture_h, H, H),
    flag(current_x, X, X),
    flag(current_y, Y, Y),
    (Y+75 < H -> flag(current_y, _, Y+75)
    ;flag(current_x, _, X+75),
     flag(current_y, _, 50)).

% Desenha as arestas entre os v�rtices.
draw_links :-
    forall(edge(Label1,Label2),
	  (vertex(Label1,Vertex1),
	   vertex(Label2,Vertex2),
	   handles(Vertex1, Vertex2, Out, In),
	   new(Link, link(Out, In, line(arrows := second))),
	   send(Vertex1, connect, Vertex2, Link))).

% Define qual o formato da aresta entre dois v�rtices.
handles(Vertex1, Vertex2, Out, In) :-
    get(Vertex1, position, point(X1,Y1)),
    get(Vertex2, position, point(X2,Y2)),
    DeltaX is X2 - X1,
    DeltaY is Y2 - Y1,
    (DeltaX = 0 -> (DeltaY > 0 -> Out = dn, In = up ; Out = up, In = dn)
    ;DeltaY = 0 -> (DeltaX > 0 -> Out = rt, In = lt ; Out = lt, In = rt)
    ;DeltaX > 0 -> (DeltaY > 0 -> Out = dn, In = up ; Out = up, In = dn)
    ;DeltaY > 0 -> (DeltaX > 0 -> Out = up, In = dn ; Out = dn, In = up)
    ;DeltaX < 0 -> (DeltaY > 0 -> Out = dn, In = up ; Out = up, In = dn)
    ;Out=dn, In=up).

% Colore os v�rtices para indicar a coer�ncia dos argumentos.
colour_vertices :-
    findall(V/Vo, vertex(V,Vo), Vs),
    colour_vertices(Vs).

colour_vertices([]) :-
    forall((vert(olga, V), vertex(V,Vo)),
	   (get(Vo, fill_pattern, colour(honeydew2)) -> send(Vo, fill_pattern, colour(palegreen))
	   ;get(Vo, fill_pattern, colour(lightpink)   ) -> send(Vo, fill_pattern, colour(salmon))
	   ;send(Vo, fill_pattern, colour(khaki)) )), !.

colour_vertices(Vs) :-
    select(W/Wo, Vs, Vss),
    (edge(V,W), vertex(V,Vo), get(Vo, fill_pattern, colour(honeydew2)), !, C = lightpink
    ;not((edge(V,W), vertex(V,Vo), get(Vo, fill_pattern, @nil))), !, C = honeydew2),
    send(Wo, fill_pattern, colour(C)), !,
    colour_vertices(Vss).

colour_vertices(Vs) :-
   forall(member(_/Wo, Vs),
	   send(Wo, fill_pattern, colour(cornsilk))),
   colour_vertices([]).

%+-----------------------------------------------------------------+
%�                     RELA��ES DE PRECED�NCIA                     �
%�	       (expl�cita, impl�cita, mista e mediada)             �
%+-----------------------------------------------------------------+

% ------------------------------------------------------------------
% Rela��o de preced�ncia vazia
% ------------------------------------------------------------------

precedence_relation(none, []).

% ------------------------------------------------------------------
% Rela��o de preced�ncia expl�cita
% ------------------------------------------------------------------

% precedence_relation(+explicit,-Precedences): verdade se
% Precedences � um conjunto de regras de preced�ncia expl�cita
% sintetizado das regras de preced�ncia declaradas na base de
% conhecimentos.
precedence_relation(explicit, Precedences) :-
    findall(L1<L2, precedes(L1, L2), Explicit),
    transitive_closure(Explicit, Closure),
    filtered(Closure, Precedences).

% transitive_closure(+R,-T): verdade se o fecho transitivo da
% rela��o bin�ria ac�clica R � T.
transitive_closure(R, T) :-
    acyclic(R),
    setof(L1<L2, transition(L1, L2, R), T), !; T = [].

% acyclic(+Relation): verdade se Relation � ac�clico.
% +Relation: rela��o de regras de preced�ncia.
acyclic([]) :- !.

acyclic(Relation) :-
    select(L1<_, Relation, NewRelation),
    not(member(_<L1, Relation)),
    acyclic(NewRelation), !.

% transition(+L1,+L2,+R): verdade se a regra de preced�ncia L1<L2
% pode ser inferida da rela��o R, seja diretamente ou por
% transitividade, ou seja, L1<Lx e Lx<L2.
% +L1: R�tulo de uma regra revog�vel
% +L2: R�tulo de uma regra revog�vel
% +R:  Rela��o de regras de preced�ncia
transition(L1, L2, R) :- member(L1<L2, R).
transition(L1, L2, R) :- member(L1<Lx, R), transition(Lx, L2, R).

% filtered(+Closure,-Precedences): remove as regras de preced�ncia entre
% regras revog�veis que n�o t�m consequentes complementares.
% +Closure: fecho transitivo de regras de preced�ncia
% -Precedences: rela��o de regras de preced�ncia filtrada
filtered(Closure, Precedences) :-
    findall(L1<L2,
        (member(L1<L2, Closure),
         believes(_, then(L1, _, Consequent1)),
         believes(_, then(L2, _, Consequent2)),
         neg(Consequent1, Consequent2)),
	Precedences).

% ------------------------------------------------------------------
% Rela��o de preced�ncia impl�cita
% ------------------------------------------------------------------

% precedence_relation(+implicit,-Precedences): � verdadeiro se
% Precedences � o conjunto de regras de preced�ncia impl�cita
% sintetizado de regras revog�veis declaradas na base de
% conhecimentos.
precedence_relation(implicit, Precedences) :-
    findall(Label, believes(_, then(Label,_,_)), Labels),
    findall(L1<L2,
	   (member(L1, Labels),
	    member(L2, Labels),
	    L1 \= L2,
	    more_specific(L1, L2)),
	   Precedences).

% more_specific(+L1,+L2): verdade se:
% (1) L1 e L2 s�o r�tulos de regras revog�veis
% (2) L1 n�o � uma presun��o
% (3) L1 e L2 t�m consequentes complementares
% (4) O antecedente de L2 pode ser derivado do antecedente de L1
% (5) O antecedente de L1 n�o pode ser derivado do antecedente de L2
more_specific(L1, L2) :-
    rule(L1: A1 then C1),
    rule(L2: A2 then C2),
    A1 \= true,
    neg(C1, C2),
    der(A2, A1),
    not(der(A1, A2)), !.

% der(+C1,+C2): verdade se a conjun��o C1 � revogavelmente
% deriv�vel da conjun��o C2.
der(C1, C2) :-
    setof(L, in(L, C2), Ls),
    forall(in(L, C1), ddt([L], Ls)).

% �rvore de deriva��o revog�vel.
ddt([L|H], _)  :- memberchk(L, H), !, fail. % Evita regras c�clicas
ddt([L|_], Ls) :- memberchk(L, Ls), !.
ddt([L|H], Ls) :-
    rule(_: A then L), in(X, A),
    ddt([X,L|H], Ls).

% ------------------------------------------------------------------
% Rela��o de preced�ncia mista
% ------------------------------------------------------------------

% precedence_relation(+mixed,-Precedences): verdade se Precedences
% � o conjunto de regras de preced�ncia mista sintetizado da
% integra��o das rela��es de preced�ncia impl�cita e expl�cita.
precedence_relation(mixed, Precedences) :-
    precedence_relation(explicit, Explicit),
    precedence_relation(implicit, Implicit),
    integrate(Explicit, Implicit, Mixed),
    transitive_closure(Mixed, Closure),
    filtered(Closure, Precedences).

% integrate(+E, +I, -M): Integra E e I em M (priorizando E).
% +E: Lista com a rela��o de preced�ncia expl�cita.
% +I: Lista com a rela��o de preced�ncia impl�cita.
% -M: Lista com a rela��o de preced�ncia mista.
integrate(E, I, M) :-
    union(E, I, U), % U ser� a uni�o de E e I.
    purge(U, E, M). % M ser� U purificado.

% purge(+R, +E, -P): Gera P eliminando os c�clos de R (priorizando E).
% +R: Uni�o das rela��es de preced�ncias expl�citas e impl�citas.
% +E: Lista com as rela��es de preced�ncias expl�citas.
% -P: Uni�o purificada.
purge(Relation, Explicit, Purged) :-
    reject(Relation, Explicit, Links), !,
    subtract(Relation, Links, NewRelation),
    purge(NewRelation, Explicit, Purged).

purge(Relation, _, Relation).

% reject(+Relation,+Explicit,-Link): verdade se Relation �
% c�clico e Link � o elo mais fraco em um c�clo mais curto nesta rela��o
reject(Relation, Explicit, Links) :-
    setof([L1], L2^member(L1<L2, Explicit), Sources),
    shortest(Sources, Relation, Cycle),
    weakest(Cycle, Explicit, Links), !,
    Links \= [].

% shortest(+ListOfSources,+Relation,-Cycle): verdade se Cycle �
% um ciclo mais curto em Relation, come�ando de um dos r�tulos
% ListOfSources. Se Relation � ac�clico, Cycle � um caminho vazio.
shortest([], _, []) :- !.

shortest([[Label|Labels]|_Paths], _Relation, [Label|Labels]) :-
    member(Label, Labels), !.

shortest([Path|Paths], Relation, Cycle) :-
    Path = [Label|_],
    findall([NextLabel|Path], member(Label<NextLabel, Relation), Successors),
    append(Paths, Successors, NewForest),
    shortest(NewForest, Relation, Cycle).

% weakest(+Cycle, +Explicit, -Links): verdade se Links � o
% conjunto de elos mais fracos em Cycle. Os elos mais fracos em Cycle
% s�o todas as liga��es impl�citas (derivadas de preced�ncias impl�citas)
% que precedem imediatamente uma liga��o expl�cita (derivada de uma
% preced�ncia expl�cita).
weakest(Cycle, Explicit, Links) :-
    findall(L1<L2,
        (append(_, [L2,L1|_], Cycle),
	 not(member(L1<L2, Explicit)),
	 memberchk(L2<_, Explicit)),
	Links).

% ------------------------------------------------------------------
% Rela��o de preced�ncia mediada
% ------------------------------------------------------------------

% precedence_relation(+mediated,-Precedences): verdade se
% Precedences � um conjunto de regras de preced�ncia mediadas
% pelo usu�rio partindo dos conflitos da base de conhecimentos.
precedence_relation(mediated, Precedences) :-
    findall(L1<L2,
	    (believes(_, then(L1, _, C1)),
	     believes(_, then(L2, _, C2)),
	     L1 \= L2,
	     neg(C1, C2)),
	    Conflicts1),
    nb_getval(precedences, ActivePrecedences1),
    reverse_pairs(ActivePrecedences1, ActivePrecedences2),
    subtract(Conflicts1, ActivePrecedences1, Conflicts2),
    subtract(Conflicts2, ActivePrecedences2, Conflicts),
    escolhe(Conflicts, [], Escolhidas),
    union(ActivePrecedences1, Escolhidas, Precedences1),
    transitive_closure(Precedences1, Closure),
    filtered(Closure, Precedences).

% reverse_pair(?A,?B): gera B com a invers�o dos pares de A.
% ?A: Lista com rela��es de preced�ncia.
% ?B: Lista com rela��es de preced�ncia.
reverse_pairs([], []) :- !.
reverse_pairs([(X<Y)|Resto1], [(Y<X)|Resto2]) :- reverse_pairs(Resto1, Resto2).

% escolhe(+O, +A, -E): gera E a partir das op��es escolhidas.
% +O: lista de op��es de regras conflitantes pass�veis de media��o.
% +A: acumulador de regras de preced�ncia escolhidas.
% -E: lista final com as regras de preced�ncia mediadas.
escolhe([], Acumulador, Escolhidas) :- !, reverse(Acumulador, Escolhidas).

escolhe([(L1<L2)|Opcoes], Acumulador, Escolhidas) :-
    ((acyclic([L1<L2|Acumulador]), acyclic([L2<L1|Acumulador]))
    -> choose([L1,L2])
    ;  true),
    repeat, sleep(1), retract(choice(L)), !,
    (L = L1 -> NovoAcumulador = [(L1<L2)|Acumulador]
    ;L = L2 -> NovoAcumulador = [(L2<L1)|Acumulador]
    ;NovoAcumulador = Acumulador),
    subtract(Opcoes, [(L1<L2),(L2<L1)], NovaOpcoes),
    escolhe(NovaOpcoes, NovoAcumulador, Escolhidas).

% choose(+[L1,L2]): Abre uma janela para escolha de uma preced�ncia.
% +[L1,L2]: lista com um par de r�tulos de regras conflitantes.
choose([L1,L2]) :-
    believes(_, then(L1, A1, C1)),
    believes(_, then(L2, A2, C2)),
    term_to_atom(L1:then(A1, C1), Atom1),
    term_to_atom(L2:then(A2, C2), Atom2),
    new(D, dialog('Conflict Mediator')),
    send(D, append, new(C, menu(choose, marked))),
    send_list(C, append, [Atom1,Atom2,none]),
    send(C, layout, vertical),
    send(D, append, button(ok, and(message(@prolog, set_choice, C?selection),
                                   message(D, destroy)))),
    send(D, default_button, ok),
    send(D, open, point(880, 280)).

set_choice(Choice) :-
	(Choice = none
	-> Label = none
	;atom_to_term(Choice, Label:_,_)),
	assert(choice(Label)).

% ------------------------------------------------------------------
% Predicados auxiliares
% ------------------------------------------------------------------

% unflawed(+Clauses): verdade se todas as regras revog�veis
% na lista Clauses s�o consistentes.
unflawed([]) :- !.

unflawed([precedes(_,_)|Clauses]) :- !, unflawed(Clauses).

unflawed([_ believes _ : then(A,C)|Clauses]) :-
    consistent(then(A,C)), !,
    unflawed(Clauses).

unflawed([Agent believes L : then(A,C)|_]) :-
    ansi_format([bold,fg(black)],'~nError: ', []),
    format('inconsistent rule (~w)~n~n', [Agent believes L : then(A,C)]),
    !, fail.

% consistent(+then(Antecedent, Consequent)): verdade se n�o existe um
% par de literais complementares no conjunto de literais formado pelos
% literais usados no antecedente mais o literal usado no consequente.
% a(b, c, d)        --> (a, b, c, d) ok
% a(b, c, d, not d) --> (a, b, c, d, not d) erro!
% a(b, c, d, not a) --> (a, b, c, d, not a) erro!
consistent(then(Antecedent, Consequent)) :-
    findall(Premise, in(Premise, Antecedent), Premises),
    not((member(Proposition1, [Consequent|Premises]),
         member(Proposition2, [Consequent|Premises]),
         neg(Proposition1, Proposition2))), !.

% gratuitous(+Clauses): verdade se Clauses tem uma regra com um
% consequente gratuito (e.g., 'a then a', ou 'p then q' e 'q then p').
gratuitous(Clauses) :-
    findall(R, (member(R, Clauses), gratuitous(R, Clauses)), G), G \= [],
    ansi_format([bold,fg(black)],'~nWarning: ', []),
    format('gratuitous consequents (~w)~n~n', [G]).

gratuitous(_ believes _ : then(A, C), _) :- in(C, A), !.

gratuitous(_ believes _ : then(A1, C1), Clauses) :-
    member(_ believes _ : then(A2, C2), Clauses),
    in(L1, A1), der(L1, C2),
    in(L2, A2), der(L2, C1), !.

% rule(+L: A then C): verdade se existe a regra de r�tulo L, com
% antecedente A e consequente C, na base de conhecimentos.
rule(Label: Antecedent then Consequent) :-
    believes(_, then(Label, Antecedent, Consequent)).

% in(?L, +C): verdade se L � um literal na conjun��o C.
in(L, L) :- L \= true, L \= and(_, _).
in(L, and(L, _)).
in(L, and(_, C)) :- in(L, C).

% sset(+A, +B): verdade se cada literal na conjun��o A � tamb�m
% um literal na conjun��o B, ou seja, A � um subconjunto de B.
sset(A, B) :- forall(in(X, A), in(X, B)).

% Define a rela��o de preced�ncia que ser� usada.
set_type :-
    var(plato:rule_precedence_menu, RulePrecedenceMenu),
    var(plato:item_explicit, @E),
    var(plato:item_implicit, @I),
    get(RulePrecedenceMenu, selected, explicit, Explicit),
    get(RulePrecedenceMenu, selected, implicit, Implicit),
    get(RulePrecedenceMenu, selected, mediated, Mediated),
    (Explicit = @on,  Implicit = @off, !, def_prec(explicit), nb_getval(explicit, V)
    ;Explicit = @off, Implicit = @on,  !, def_prec(implicit), nb_getval(implicit, V)
    ;Explicit = @on,  Implicit = @on,  !, def_prec(mixed),    nb_getval(mixed,    V)
    ;Explicit = @off, Implicit = @off, !, def_prec(none),     V = []),
    nb_setval(precedences, V),
    (Mediated = @on
    -> (send(@E, active, @off),
        send(@I, active, @off),
        def_prec(mediated),
        precedence_relation(mediated, W),
        nb_setval(mediated, W),
	nb_setval(precedences, W))
    ;  (send(@E, active, @on),
        send(@I, active, @on),
        nb_setval(mediated, []))).

% Define que n�o ser� usada rela��o de preced�ncia.
:- retractall(ptype(_)), assert(ptype(none)).

% Define que n�o ser� permitido o aprendizado no di�logo.
:- retractall(aprende(_)), assert(aprende(no)).

% Define que toda alega��o do oponente deve ser justificada.
:- retractall(explain(_)), assert(explain(no)).

% Define o modo de di�logo como n�o detalhado.
:- retractall(verbose(_)), assert(verbose(no)).

% Define a rela��o de preced�ncia que ser� usada.
def_prec(Precedence) :- retractall(ptype(_)), assert(ptype(Precedence)).

% Define a possibilidade de aprendizado no di�logo.
learning_mode :- retract(aprende(no)), assert(aprende(yes)),
	modify_name(plato:learning_menu_item, 'Learning mode (on)'), !.

learning_mode :- retract(aprende(yes)), assert(aprende(no)),
	modify_name(plato:learning_menu_item, 'Learning mode (off)'), !.

% Define a possibilidade de aceitar alega��es j� conhecidas sem explica��es.
explanation_mode :- retract(explain(no)), assert(explain(yes)),
	modify_name(plato:explanation_menu_item, 'Explanation mode (on)'), !.

explanation_mode :- retract(explain(yes)), assert(explain(no)),
	modify_name(plato:explanation_menu_item, 'Explanation mode (off)'), !.

% Define o modo de di�logo como detalhado ou n�o.
verbose_mode :- retract(verbose(no)), assert(verbose(yes)),
	modify_name(plato:verbose_menu_item, 'Verbose mode (on)'), !.

verbose_mode :- retract(verbose(yes)), assert(verbose(no)),
	modify_name(plato:verbose_menu_item, 'Verbose mode (off)'), !.

% modify_name(+C, +N): altera o nome de C para N.
% +C: controle
% +N: novo nome
modify_name(Control, Name) :- var(Control, C), send(C, value, Name).

% Exibe todas as poss�veis preced�ncias entre regras revog�veis.
show_precedences :-
   var(plato:display_editor, DisplayEditor ),
   var(plato:display_tab, DisplayTab ),
   var(plato:tab_stack, TabStack ),
   send(TabStack, on_top, DisplayTab ),
   nb_getval(explicit, Explicit), term_to_atom(Explicit, E),
   nb_getval(implicit, Implicit), term_to_atom(Implicit, I),
   nb_getval(mixed,    Mixed),    term_to_atom(Mixed, M),
   nb_getval(mediated, Mediated), term_to_atom(Mediated, Me),
   Separator = '\n--------------------------------------------------------------------\n',
   send(DisplayEditor, clear),
   atomic_list_concat(['Precedence Relations',Separator], Title),
   send(DisplayEditor, append, Title),
   atom_concat('Explicit: ',   E, EText),   send(DisplayEditor, append, EText),
   atom_concat('\nImplicit: ', I, IText),   send(DisplayEditor, append, IText),
   atom_concat('\nMixed:    ', M, MText),   send(DisplayEditor, append, MText),
   atom_concat('\nMediated  ', Me, MeText), send(DisplayEditor, append, MeText),
   send(DisplayEditor, append, Separator),
   ptype(SelectedType),
   atomic_list_concat(['Currently using: ',SelectedType,Separator], CText),
   send(DisplayEditor, append, CText).

%+------------------------------------------------------------------+
%�                        DI�LOGO PERSUASIVO                        �
%�          Dois agentes no di�logo (proponente e oponente)         �
%+------------------------------------------------------------------+

% start_dialogue(+L): inicia o di�logo persuasivo cujo objetivo
% � provar que L � coerente.
% +L: literal que dever� ser defendido
start_dialogue(Literal) :-
    var(plato:dialogue_editor, DialogueEditor),
    send(DialogueEditor, clear),
    flag(arg, _, 1),
    retractall(next(start, _)),
    asserta(next(start, paul:claim(Literal))),
    nb_setval(narrative, []),
    ignore(dialogue(start)),
    outcome(Literal, Outcome, C),
    term_to_atom(Literal, L),
    atomic_list_concat(['\n', Outcome, ': ', L], Result),
    assertz(line(Result)),
    send(DialogueEditor?image, colour, C), !.

% ------------------------------------------------------------------
% Di�logo persuasivo e in�cio da locu��o
% ------------------------------------------------------------------

% Abre uma janela para a inser��o do literal que ser� defendido.
dialogue :-
    not(exists_dialog),
    new(D, dialog('Persuasion Dialog')),
    new(T, text_item('Insert Claim')),
    send(D, append, T),
    send(D, append, button(show, message(@prolog, show, T?selection))),
    send(D, append, button(clear, message(T, clear))),
    var(plato:dialog, D),
    send(D, open, point(880, 280)).

% dialogue(+Act): simula um di�logo persuasivo entre paul e olga
% +Act: para come�ar o di�logo dever� ser 'start'.
dialogue(Act) :-
    next(Act, NextAct),
    new(NextAct),
    say(NextAct),
    upd(NextAct),
    not(final(NextAct)),
    not(dialogue(NextAct)).

claim(C):-
    get(C, value, Text),
    read_from_chars(Text, Proposition),
    start_dialogue(Proposition).

% Exibe todo o di�logo de uma s� vez
show(C) :-
    get(C, value, Text),
    read_from_chars(Text, Literal),
    flag(first_time, V, 0),
    (V = 1 -> start_dialogue(Literal)),
    fail.

show(_) :-
    var(plato:dialogue_editor, DialogueEditor),
    flag(index, _, 0),
    forall(line(Text), (send(DialogueEditor, append, Text), retract(line(Text)))),
    flag(first_time, _, 1).

show(_) :-
    var(plato:dialogue_editor, DialogueEditor),
    send(DialogueEditor, clear).

% new(+Agent:Locution): verdade se a locu��o � in�dita na narrativa.
% Obs.: Somente o agente 'olga' pode repetir locu��es.
new(Agent:Locution) :-
    (Agent = olga, Locution = since(_)
    ;nb_getval(narrative, Narrative),
    not(memberchk(_:Locution, Narrative))).

% say(+Agent:Locution): exibe a locu��o do agente.
say(Agent:Locution) :-
    var(plato:dialogue_editor, DialogueEditor),
    var(plato:dialogue_tab, DialogueTab),
    var(plato:tab_stack, TabStack),
    (  verbose(yes)
    -> flag(arg, N, N + 1),
       term_to_atom(Locution, AtomicLocution),
       atomic_list_concat(['<', N, '> ', Agent, ' : ', AtomicLocution, '\n'], Text),
       assertz(line(Text))
    ;  true),
    send(TabStack, on_top, DialogueTab),
    send(DialogueEditor, caret, 0).

% upd(+Agent:Locution): atualiza a narrativa com a locu��o do agente.
upd(Agent:Locution) :-
    nb_getval(narrative, Narrative),
    nb_setval(narrative, [Agent:Locution|Narrative]).

% ------------------------------------------------------------------
% Regras de protocolo
% ------------------------------------------------------------------

% next(+L1, +L2): verdade se L1 � uma locu��o com L2 como contra
% movimento permitido.
next(A:claim(F), B:why(F))     :- adv(A, B), (explain(no), suspicion(B, F); explain(yes)).
next(A:claim(F), B:agree(F))   :- adv(A, B).
next(A:since(R), B:why(P))     :- adv(A, B), premise(P, R), (explain(no), suspicion(B, P); explain(yes)).
next(A:since(R), B:agree(P))   :- adv(A, B), premise(P, R), (explain(no), not(suspicion(B, P)); explain(yes)).
next(A:since(R), B:since(S))   :- adv(A, B), attacks(B, S, R).
next(A:since(R), B:agree(F))   :- adv(A, B), claim(R, F).
next(A:since(R), B:learn(F))   :- adv(A, B), aprende(yes), claim(R, F), learn(B, F).
next(A:why(F),   B:since(R))   :- adv(A, B), adduces(B, R, F).
next(A:why(F),   B:retract(F)) :- adv(A, B).

% final(+L): verdade se L � uma locu��o de finaliza��o.
% +L: locu��o
final(_:agree(_)).
final(_:learn(_)).
final(_:retract(_)).

% adv(?A,?B): verdade se A e B s�o advers�rios.
% ?A: agente
% ?B: agente
adv(paul, olga).
adv(olga, paul).

% suspicion(+A, +C): verdade se A tem motivos para suspeitar de C.
% +A: agente
% +C: consequente
suspicion(Agent, Consequent) :-
    not(argument(Agent, Consequent, _));
    neg(Consequent, Oposto),
    argument(Agent, Oposto, A),
    forall(argument(Agent, Consequent, B),
              (last(A, L1:_), last(B, L2:_),
               not(prec(L2, L1)))).

% learn(+A, +C): verdade se A n�o tem um argumento para C.
% +A: agente
% +C: consequente
learn(A, C) :-
    not(argument(A, C, _)),
    updateKB:insertKB(believes(A, then(true, C))),
    conflicts.

% outcome(+L, -O, -C): define o resultado do di�logo e a cor
% que ser� exibida.
% +L: literal
% -O: resultado
% -C: cor
outcome(Literal, Outcome, Color) :-
    nb_getval(narrative, Narrative),
    Narrative \= [],
    (member(olga:agree(Literal), Narrative)
    -> (Outcome, Color) = ('Accepted', blue)
    ;  (Outcome, Color) = ('Rejected', red)).

% neg(?L, ?C): verdade se L e C s�o complementares.
% ?L: literal
% ?C: literal
neg(not(Proposition), Proposition) :- !.
neg(Proposition, not(Proposition)).

% coherent(+A then +C): verdade se A alegar C � coerente com a
% narrativa atual.
% +A: agente
% +C: consequente
coherent(A then C) :-
    nb_getval(narrative, Narrative),
    forall(in(L, C and A), not(member(_:retract(L), Narrative))),
    forall(in(L, C and A), not((neg(L, N), member(_:agree(N), Narrative)))),
    forall(in(L, A), not((member(_:why(L), Narrative), not(member(_:agree(L), Narrative))))),
    forall(in(L, A), not((neg(L,N), member(_:why(N), Narrative), not(member(_:retract(N), Narrative))))).

% adduces(+Agent, +R, +C): verdade se Agent pode alegar R com C.
% +Agent: agente
% +R:     regra revog�vel
% +C:	  consequente da regra revog�vel
adduces(Agent, R: A then C, C) :-
    believes(Agent, then(R, A, C)),
    coherent(A then C).

% premise(-L, +R): verdade se L � uma premissa de R.
% -L: literal
% +R: regra revog�vel
premise(Literal, _:Antecedent then _) :-
    in(Literal, Antecedent),
    neg(Literal, Complement),
    coherent(Complement then nil).

% attacks(+A, +R1, +R2): verdade se A pode usar R1 para atacar R2.
% +A:  agente
% +R1: r�tulo de uma regra revog�vel
% +R2: r�tulo de uma regra revog�vel
attacks(Agent, R1: C then N, R2: _ then F) :-
    neg(F, N),
    believes(Agent, then(R1, C, N)),
    coherent(C then N),
    defeats(Agent, R1, R2).

% defeats(+A, +R1, +R2): verdade se R1 derrota R2.
% +A:  agente
% +R1: r�tulo de uma regra revog�vel
% +R2: r�tulo de uma regra revog�vel
defeats(paul, R1, R2) :- prec(R1, R2), !.
defeats(olga, R1, R2) :- prec(R1, R2), !.
defeats(olga, R1, R2) :- not(prec(R1, R2)), not(prec(R2, R1)).

% prec(+R1, +R2): verdade se R1 precede R2.
% +R1: r�tulo de uma regra revog�vel
% +R2: r�tulo de uma regra revog�vel
prec(R1, R2) :-
    nb_getval(precedences, Precedences),
    memberchk(R1<R2, Precedences).

% claim(+R, -C): verdade se h� regra revog�vel com consequente C.
% +R: regra revog�vel
% -C: consequente da regra revog�vel
claim(_ : _ then C, C).


















