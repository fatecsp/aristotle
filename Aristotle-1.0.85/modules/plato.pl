%+-----------------------------------------------------------------+
%¦ plato - a system for dialogical argumentation                   ¦
%+-----------------------------------------------------------------+
%¦ Plato (424/423 BC – 348/347 BC), was a Classical Greek          ¦
%¦ philosopher, mathematician, student of plato, writer of         ¦
%¦ philosophical dialogues, and founder of the Academy in Athens,  ¦
%¦ the first institution of higher learning in the Western world.  ¦
%¦ Along with his mentor, plato, and his student, Aristotle, Plato ¦
%¦ helped to lay the foundations of Western philosophy and science.¦
%¦ [http://thyselfknow.com/plato/]                                 ¦
%+-----------------------------------------------------------------+

:- module(plato, [plato/0]).

% ------------------------------------------------------------------
% Importação de módulos
% ------------------------------------------------------------------

:- use_module(modules/fbrowser).
:- use_module(modules/updateKB).
:- use_module(modules/globvars).

% ------------------------------------------------------------------
% Definição de operadores
% ------------------------------------------------------------------

:- export(':'/2).
:- op(100,  fy, not).      % Operador de negação.
:- op(101, xfy, and).      % Operador lógico de conjunção.
:- op(102, xfx, then).     % Operador de implicação.
:- op(103, xfx, :).        % Operador para rótulo de regras.
:- op(104, xfx, believes). % Operador que indica crença.
:- op(105, xfx, precedes). % Operador que estabelece precedência.

% ------------------------------------------------------------------
% Definição de predicados dinâmicos
% ------------------------------------------------------------------

:- dynamic aprende/1. % Define a possibilidade de aprendizado durante o diálogo.
:- dynamic choice/1.  % Guarda a precedência mediada entre regras revogáveis.
:- dynamic line/1.    %
:- dynamic explain/1. % Define a necessidade de explicação total no diálogo.
:- dynamic next/2.    % Define o próximo ato de fala permitido.
:- dynamic ptype/1.   % Define o tipo de relação de precedência ativo.
:- dynamic tmp/1.     % Cláusula para regras temporárias.
:- dynamic verbose/1. % Define se o diálogo terá saídas detalhadas.

:- discontiguous precedence_relation/2.

% ------------------------------------------------------------------
% Interface gráfica
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
    new(VerboseMenuItem,             menu_item('Verbose mode (off)',     message(@prolog, verbose_mode))),
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
    send(Column_Left, append, bitmap(image('images/column_p.bmp'))),
    send(Frame, open, point(50, 70)).

% Função: Gera todos os argumentos.
% Saídas: Mensagem confirmando a geração de argumentos.
% Opções: Botão "Ok" para fechar a caixa de diálogo.
arguments_click :-
    arguments(yes),
    not(exists_dialog),
    new(Dialog, dialog('Argumentational System')),
    send(Dialog, append, text('All arguments were successfully generated.')),
    send(Dialog, append, button(ok, message(Dialog, destroy))),
    var(plato:dialog, Dialog),
    send(Dialog, open, point(880, 280)).

% Verifica se o frame já existe (está em exibição).
exists_frame :-
    var(plato:frame, Frame),
    catch(send(Frame, expose), Error, true),
    var(Error).

% Verifica se o dialog já existe (está em exibição).
exists_dialog :-
    var(plato:dialog, Dialog),
    catch(send(Dialog, expose), Error, true),
    var(Error).

% Fecha a janela do módulo plato.
close :-
    var(plato:frame, Frame),
    send(Frame, destroy).

% Função: Verifica se as regras de precedência explícita estão corretas.
% Saídas: (1) Mensagem indicando a existência de ciclos.
%         (2) Mensagem indicando a verificação de consistência.
%         (3) Mensagem indicando que os conflitos não podem ser gerados.
consist_prec :-
    vértices(Vs),
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

vértices(Vértices) :-
    findall([V,W], plato:precedes(V, W), L),
    flatten(L, F),
    sort(F, Vértices).

acessa(V,L) :- acessa([V], nil, [], L).

acessa([], _, A, acessa(L)) :- reverse(A, L), !.

acessa(_, A, A, ciclo(L)) :- reverse(A, L), !.

acessa([V|Vs], _, A, L) :-
    findall(W, plato:precedes(V, W), F),
    union(Vs, F, N),
    union([V], A, M),
    acessa(N, A, M, L).

% ------------------------------------------------------------------
% Geração de argumentos e atualizações
% ------------------------------------------------------------------

% arguments(+File): gera os argumentos a partir das regras.
% +File: define se as regras serão coletadas do arquivo (yes) ou,
% caso contrário (no), da memória.
arguments(File) :-
    var(plato:display_editor, DisplayEditor),
    (File = yes -> updateKB(plato); true),
    (get(DisplayEditor, selected, _)
    ->  var(plato:display_tab, DisplayTab),
        var(plato:tab_stack, TabStack),
        send(TabStack, on_top, DisplayTab)
    ;   updateArgs).

% Atualiza a visualização dos argumentos.
updateArgs :-
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

% Junta as claúsulas em uma lista com uma nova linha.
join(_, [N: Last], [Atom,']\n\n']) :- !, term_to_atom(N:Last, Atom).

join(NewLine, [N:First|Rest], [Atom,NewLine|Atoms]) :-
    term_to_atom(N:First, Atom),
    join(NewLine, Rest, Atoms).

% Gera todos os argumentos possíveis.
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

% Gera todas as locuções possíveis.
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

% Anexa duas fórmulas conjuntivas.
add(true, B, B) :- !.
add(A and B, C, A and D) :- add(B, C, D).
add(A, B, A and B).

%+-----------------------------------------------------------------+
%¦                  GRAFO DE DECISÃO DE COERÊNCIA                  ¦
%¦ amarelo (bloqueado), vermelho(derrotado) e verde(não derrotado) ¦
%+-----------------------------------------------------------------+

% Exibe o grafo de decisão de coerência dos argumentos da base de
% conhecimentos.
conflicts :-
    arguments(no),
    var(plato:picture, Picture),
    send(Picture, clear),
    draw_vertices,
    draw_links.

% Desenha os vértices.
draw_vertices :-
    initiate_position_variables,
    retractall(vertex(_, _)),
    forall(vert(_, Label),
	  (draw_vertex(Label, ObjectId),
	   assertz(vertex(Label, ObjectId)))),
    colour_vertices.

% Inicialização da posição global das variáveis.
initiate_position_variables :-
    findall(Vertex, vert(_, Vertex), Vertices),
    length(Vertices, Length),
    Height is 75 + (ceil(sqrt(Length) -1)) * 75,
    flag(current_x, _, 50),
    flag(current_y, _, 50),
    flag(picture_h, _, Height).

% Desenha um único vértice e retorna seu identificador.
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

% Retorna a posição onde o próximo vértice será desenhado.
get_position(X, Y) :-
    flag(picture_h, H, H),
    flag(current_x, X, X),
    flag(current_y, Y, Y),
    (Y+75 < H -> flag(current_y, _, Y+75)
    ;flag(current_x, _, X+75),
     flag(current_y, _, 50)).

% Desenha as arestas entre os vértices.
draw_links :-
    forall(edge(Label1,Label2),
	  (vertex(Label1,Vertex1),
	   vertex(Label2,Vertex2),
	   handles(Vertex1, Vertex2, Out, In),
	   new(Link, link(Out, In, line(arrows := second))),
	   send(Vertex1, connect, Vertex2, Link))).

% Define qual o formato da aresta entre dois vértices.
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

% Colore os vértices para indicar a coerência dos argumentos.
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
%¦                     RELAÇÕES DE PRECEDÊNCIA                     ¦
%¦	       (explícita, implícita, mista e mediada)             ¦
%+-----------------------------------------------------------------+

% ------------------------------------------------------------------
% Relação de precedência vazia
% ------------------------------------------------------------------

precedence_relation(none, []).

% ------------------------------------------------------------------
% Relação de precedência explícita
% ------------------------------------------------------------------

% precedence_relation(+explicit,-Precedences): verdade se
% Precedences é um conjunto de regras de precedência explícita
% sintetizado das regras de precedência declaradas na base de
% conhecimentos.
precedence_relation(explicit, Precedences) :-
    findall(L1<L2, precedes(L1, L2), Explicit),
    transitive_closure(Explicit, Closure),
    filtered(Closure, Precedences).

% transitive_closure(+R,-T): verdade se o fecho transitivo da
% relação binária acíclica R é T.
transitive_closure(R, T) :-
    acyclic(R),
    setof(L1<L2, transition(L1, L2, R), T), !; T = [].

% acyclic(+Relation): verdade se Relation é acíclico.
% +Relation: relação de regras de precedência.
acyclic([]) :- !.

acyclic(Relation) :-
    select(L1<_, Relation, NewRelation),
    not(member(_<L1, Relation)),
    acyclic(NewRelation), !.

% transition(+L1,+L2,+R): verdade se a regra de precedência L1<L2
% pode ser inferida da relação R, seja diretamente ou por
% transitividade, ou seja, L1<Lx e Lx<L2.
% +L1: Rótulo de uma regra revogável
% +L2: Rótulo de uma regra revogável
% +R:  Relação de regras de precedência
transition(L1, L2, R) :- member(L1<L2, R).
transition(L1, L2, R) :- member(L1<Lx, R), transition(Lx, L2, R).

% filtered(+Closure,-Precedences): remove as regras de precedência entre
% regras revogáveis que não têm consequentes complementares.
% +Closure: fecho transitivo de regras de precedência
% -Precedences: relação de regras de precedência filtrada
filtered(Closure, Precedences) :-
    findall(L1<L2,
        (member(L1<L2, Closure),
         believes(_, then(L1, _, Consequent1)),
         believes(_, then(L2, _, Consequent2)),
         neg(Consequent1, Consequent2)),
	Precedences).

% ------------------------------------------------------------------
% Relação de precedência implícita
% ------------------------------------------------------------------

% precedence_relation(+implicit,-Precedences): é verdadeiro se
% Precedences é o conjunto de regras de precedência implícita
% sintetizado de regras revogáveis declaradas na base de
% conhecimentos.
precedence_relation(implicit, Precedences) :-
    findall(Label, believes(_, then(Label,_,_)), Labels),
    findall(L1<L2,
	   (member(L1, Labels),
	    member(L2, Labels),
	    L1 \= L2,
	    more_specific(L1, L2)),
	   Implicit),
    transitive_closure(Implicit, Closure),
    filtered(Closure, Precedences).

% more_specific(+L1,+L2): verdade se:
% (1) L1 e L2 são rótulos de regras revogáveis
% (2) L1 não é uma presunção
% (3) L1 e L2 têm consequentes complementares
% (4) O antecedente de L2 pode ser derivado do antecedente de L1
% (5) O antecedente de L1 não pode ser derivado do antecedente de L2
more_specific(L1, L2) :-
    rule(L1: A1 then C1),
    rule(L2: A2 then C2),
    A1 \= true,
    neg(C1, C2),
    der(A2, A1),
    not(der(A1, A2)), !.

% der(+C1,+C2): verdade se a conjunção C1 é revogavelmente
% derivável da conjunção C2.
der(C1, C2) :-
    setof(L, in(L, C2), Ls),
    forall(in(L, C1), ddt([L], Ls)).

% Árvore de derivação revogável.
ddt([L|H], _)  :- memberchk(L, H), !, fail. % Evita regras cíclicas
ddt([L|_], Ls) :- memberchk(L, Ls), !.
ddt([L|H], Ls) :-
    rule(_: A then L), in(X, A),
    ddt([X,L|H], Ls).

% ------------------------------------------------------------------
% Relação de precedência mista
% ------------------------------------------------------------------

% precedence_relation(+mixed,-Precedences): verdade se Precedences
% é o conjunto de regras de precedência mista sintetizado da
% integração das relações de precedência implícita e explícita.
precedence_relation(mixed, Precedences) :-
    precedence_relation(explicit, Explicit),
    precedence_relation(implicit, Implicit),
    integrate(Explicit, Implicit, Mixed),
    transitive_closure(Mixed, Closure),
    filtered(Closure, Precedences).

% integrate(+E, +I, -M): Integra E e I em M (priorizando E).
% +E: Lista com a relação de precedência explícita.
% +I: Lista com a relação de precedência implícita.
% -M: Lista com a relação de precedência mista.
integrate(E, I, M) :-
    union(E, I, U), % U será a união de E e I.
    purge(U, E, M). % M será U purificado.

% purge(+R, +E, -P): Gera P eliminando os cíclos de R (priorizando E).
% +R: União das relações de precedências explícitas e implícitas.
% +E: Lista com as relações de precedências explícitas.
% -P: União purificada.
purge(Relation, Explicit, Purged) :-
    reject(Relation, Explicit, Links), !,
    subtract(Relation, Links, NewRelation),
    purge(NewRelation, Explicit, Purged).

purge(Relation, _, Relation).

% reject(+Relation,+Explicit,-Link): verdade se Relation é
% cíclico e Link é o elo mais fraco em um cíclo mais curto nesta relação
reject(Relation, Explicit, Links) :-
    setof([L1], L2^member(L1<L2, Explicit), Sources),
    shortest(Sources, Relation, Cycle),
    weakest(Cycle, Explicit, Links), !,
    Links \= [].

% shortest(+ListOfSources,+Relation,-Cycle): verdade se Cycle é
% um ciclo mais curto em Relation, começando de um dos rótulos
% ListOfSources. Se Relation é acíclico, Cycle é um caminho vazio.
shortest([], _, []) :- !.

shortest([[Label|Labels]|_Paths], _Relation, [Label|Labels]) :-
    member(Label, Labels), !.

shortest([Path|Paths], Relation, Cycle) :-
    Path = [Label|_],
    findall([NextLabel|Path], member(Label<NextLabel, Relation), Successors),
    append(Paths, Successors, NewForest),
    shortest(NewForest, Relation, Cycle).

% weakest(+Cycle, +Explicit, -Links): verdade se Links é o
% conjunto de elos mais fracos em Cycle. Os elos mais fracos em Cycle
% são todas as ligações implícitas (derivadas de precedências implícitas)
% que precedem imediatamente uma ligação explícita (derivada de uma
% precedência explícita).
weakest(Cycle, Explicit, Links) :-
    findall(L1<L2,
        (append(_, [L2,L1|_], Cycle),
	 not(member(L1<L2, Explicit)),
	 memberchk(L2<_, Explicit)),
	Links).

% ------------------------------------------------------------------
% Relação de precedência mediada
% ------------------------------------------------------------------

% precedence_relation(+mediated,-Precedences): verdade se
% Precedences é um conjunto de regras de precedência mediadas
% pelo usuário partindo dos conflitos da base de conhecimentos.
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

% reverse_pair(?A,?B): gera B com a inversão dos pares de A.
% ?A: Lista com relações de precedência.
% ?B: Lista com relações de precedência.
reverse_pairs([], []) :- !.
reverse_pairs([(X<Y)|Resto1], [(Y<X)|Resto2]) :- reverse_pairs(Resto1, Resto2).

% escolhe(+O, +A, -E): gera E a partir das opções escolhidas.
% +O: lista de opções de regras conflitantes passíveis de mediação.
% +A: acumulador de regras de precedência escolhidas.
% -E: lista final com as regras de precedência mediadas.
escolhe([], Acumulador, Escolhidas) :- !, reverse(Acumulador, Escolhidas).

escolhe([(L1<L2)|Opções], Acumulador, Escolhidas) :-
    ((acyclic([L1<L2|Acumulador]), acyclic([L2<L1|Acumulador]))
    -> choose([L1,L2])
    ;  true),
    repeat, sleep(1), retract(choice(L)), !,
    (L = L1 -> NovoAcumulador = [(L1<L2)|Acumulador]
    ;L = L2 -> NovoAcumulador = [(L2<L1)|Acumulador]
    ;NovoAcumulador = Acumulador),
    subtract(Opções, [(L1<L2),(L2<L1)], NovaOpções),
    escolhe(NovaOpções, NovoAcumulador, Escolhidas).

% choose(+[L1,L2]): Abre uma janela para escolha de uma precedência.
% +[L1,L2]: lista com um par de rótulos de regras conflitantes.
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

% unflawed(+Clauses): verdade se todas as regras revogáveis
% na lista Clauses são consistentes.
unflawed([]) :- !.

unflawed([precedes(_,_)|Clauses]) :- !, unflawed(Clauses).

unflawed([_ believes _ : then(A,C)|Clauses]) :-
    consistent(then(A,C)), !,
    unflawed(Clauses).

unflawed([Agent believes L : then(A,C)|_]) :-
    ansi_format([bold,fg(black)],'~nError: ', []),
    format('inconsistent rule (~w)~n~n', [Agent believes L : then(A,C)]),
    !, fail.

% consistent(+then(Antecedent, Consequent)): verdade se não existe um
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

% rule(+L: A then C): verdade se existe a regra de rótulo L, com
% antecedente A e consequente C, na base de conhecimentos.
rule(Label: Antecedent then Consequent) :-
    believes(_, then(Label, Antecedent, Consequent)).

% in(?L, +C): verdade se L é um literal na conjunção C.
in(L, L) :- L \= true, L \= and(_, _).
in(L, and(L, _)).
in(L, and(_, C)) :- in(L, C).

% sset(+A, +B): verdade se cada literal na conjunção A é também
% um literal na conjunção B, ou seja, A é um subconjunto de B.
sset(A, B) :- forall(in(X, A), in(X, B)).

% Define a relação de precedência que será usada.
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

% Define que não será usada relação de precedência.
:- retractall(ptype(_)), assert(ptype(none)).

% Define que não será permitido o aprendizado no diálogo.
:- retractall(aprende(_)), assert(aprende(no)).

% Define que toda alegação do oponente deve ser justificada.
:- retractall(explain(_)), assert(explain(no)).

% Define o modo de diálogo como não detalhado.
:- retractall(verbose(_)), assert(verbose(no)).

% Define a relação de precedência que será usada.
def_prec(Precedence) :- retractall(ptype(_)), assert(ptype(Precedence)).

% Define a possibilidade de aprendizado no diálogo.
learning_mode :- retract(aprende(no)), assert(aprende(yes)),
	modify_name(plato:learning_menu_item, 'Learning mode (on)'), !.

learning_mode :- retract(aprende(yes)), assert(aprende(no)),
	modify_name(plato:learning_menu_item, 'Learning mode (off)'), !.

% Define a possibilidade de aceitar alegações já conhecidas sem explicações.
explanation_mode :- retract(explain(no)), assert(explain(yes)),
	modify_name(plato:explanation_menu_item, 'Explanation mode (on)'), !.

explanation_mode :- retract(explain(yes)), assert(explain(no)),
	modify_name(plato:explanation_menu_item, 'Explanation mode (off)'), !.

% Define o modo de diálogo como detalhado ou não.
verbose_mode :- retract(verbose(no)), assert(verbose(yes)),
	modify_name(plato:verbose_menu_item, 'Verbose mode (on)'), !.

verbose_mode :- retract(verbose(yes)), assert(verbose(no)),
	modify_name(plato:verbose_menu_item, 'Verbose mode (off)'), !.

% modify_name(+C, +N): altera o nome de C para N.
% +C: controle
% +N: novo nome
modify_name(Control, Name) :- var(Control, C), send(C, value, Name).

% Exibe todas as possíveis precedências entre regras revogáveis.
show_precedences :-
   updateKB(plato), % remover-----------------------------------------------------------------------------------------
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
%¦                        DIÁLOGO PERSUASIVO                        ¦
%¦          Dois agentes no diálogo (proponente e oponente)         ¦
%+------------------------------------------------------------------+

% start_dialogue(+L): inicia o diálogo persuasivo cujo objetivo
% é provar que L é coerente.
% +L: literal que deverá ser defendido
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
% Diálogo persuasivo e início da locução
% ------------------------------------------------------------------

% Abre uma janela para a inserção do literal que será defendido.
dialogue :-
    not(exists_dialog),
    new(D, dialog('Persuasion Dialog')),
    new(T, text_item('Insert Claim')),
    send(D, append, T),
    send(D, append, button(show, message(@prolog, show, T?selection))),
    send(D, append, button(clear, message(T, clear))),
    var(plato:dialog, D),
    send(D, open, point(880, 280)).

% dialogue(+Act): simula um diálogo persuasivo entre paul e olga
% +Act: para começar o diálogo deverá ser 'start'.
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

% Exibe todo o diálogo de uma só vez
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

% new(+Agent:Locution): verdade se a locução é inédita na narrativa.
% Obs.: Somente o agente 'olga' pode repetir locuções.
new(Agent:Locution) :-
    (Agent = olga, Locution = since(_)
    ;nb_getval(narrative, Narrative),
    not(memberchk(_:Locution, Narrative))).

% say(+Agent:Locution): exibe a locução do agente.
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

% upd(+Agent:Locution): atualiza a narrativa com a locução do agente.
upd(Agent:Locution) :-
    nb_getval(narrative, Narrative),
    nb_setval(narrative, [Agent:Locution|Narrative]).

% ------------------------------------------------------------------
% Regras de protocolo
% ------------------------------------------------------------------

% next(+L1, +L2): verdade se L1 é uma locução com L2 como contra
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

% final(+L): verdade se L é uma locução de finalização.
% +L: locução
final(_:agree(_)).
final(_:learn(_)).
final(_:retract(_)).

% adv(?A,?B): verdade se A e B são adversários.
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

% learn(+A, +C): verdade se A não tem um argumento para C.
% +A: agente
% +C: consequente
learn(A, C) :-
    not(argument(A, C, _)),
    updateKB:insertKB(believes(A, then(true, C))),
    conflicts.

% outcome(+L, -O, -C): define o resultado do diálogo e a cor
% que será exibida.
% +L: literal
% -O: resultado
% -C: cor
outcome(Literal, Outcome, Color) :-
    nb_getval(narrative, Narrative),
    Narrative \= [],
    (member(olga:agree(Literal), Narrative)
    -> (Outcome, Color) = ('Accepted', blue)
    ;  (Outcome, Color) = ('Rejected', red)).

% neg(?L, ?C): verdade se L e C são complementares.
% ?L: literal
% ?C: literal
neg(not(Proposition), Proposition) :- !.
neg(Proposition, not(Proposition)).

% coherent(+A then +C): verdade se A alegar C é coerente com a
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
% +R:     regra revogável
% +C:	  consequente da regra revogável
adduces(Agent, R: A then C, C) :-
    believes(Agent, then(R, A, C)),
    coherent(A then C).

% premise(-L, +R): verdade se L é uma premissa de R.
% -L: literal
% +R: regra revogável
premise(Literal, _:Antecedent then _) :-
    in(Literal, Antecedent),
    neg(Literal, Complement),
    coherent(Complement then nil).

% attacks(+A, +R1, +R2): verdade se A pode usar R1 para atacar R2.
% +A:  agente
% +R1: rótulo de uma regra revogável
% +R2: rótulo de uma regra revogável
attacks(Agent, R1: C then N, R2: _ then F) :-
    neg(F, N),
    believes(Agent, then(R1, C, N)),
    coherent(C then N),
    defeats(Agent, R1, R2).

% defeats(+A, +R1, +R2): verdade se R1 derrota R2.
% +A:  agente
% +R1: rótulo de uma regra revogável
% +R2: rótulo de uma regra revogável
defeats(paul, R1, R2) :- prec(R1, R2), !.
defeats(olga, R1, R2) :- prec(R1, R2), !.
defeats(olga, R1, R2) :- not(prec(R1, R2)), not(prec(R2, R1)).

% prec(+R1, +R2): verdade se R1 precede R2.
% +R1: rótulo de uma regra revogável
% +R2: rótulo de uma regra revogável
prec(R1, R2) :-
    nb_getval(precedences, Precedences),
    memberchk(R1<R2, Precedences).

% claim(+R, -C): verdade se há regra revogável com consequente C.
% +R: regra revogável
% -C: consequente da regra revogável
claim(_ : _ then C, C).


















