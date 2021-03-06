/*----------------------------------------------------------------+
|* Authors: Silvio do Lago, Luiz F. Zarco & Lucio N. Lira.        |
|* Institution: S�o Paulo Technological College (FATEC-SP).       |
|* Copyright (c) 2013-2016					  |
|*								  |
|* This file is part of aristotle software source code.           |
|* The software source code is distributed under MIT License.     |
|* Read LICENSE.txt for further information.			  |
+----------------------------------------------------------------*/

/*----------------------------------------------------------------+
| File Browser							  |
+----------------------------------------------------------------*/

:- module( fbrowser, [ loadfile/2, newfile/2, savefile/1, saveasfile/2 ] ).
:- use_module( globvars ).
:- pce_autoload( finder, library(find_file) ).

loadfile( Caller, Extension ) :-
   var( Caller:knowledgebase_editor, KnowledgebaseEditor ),
   var( Caller:knowledgebase_tab, KnowledgebaseTab ),
   var( Caller:tab_stack, TabStack ),
   var( Caller:picture, Picture ),
   filetype( Extension, Description ),
   new( Finder, finder),
   get( Finder, file, open, [tuple(Description, Extension), tuple('Prolog', 'pl'),   tuple('All files', '*')], FileName ),
   get( file( FileName ), base_name, BaseName ),
   var( Caller:basename, @BaseName ),
   var( Caller:current_path, @(FileName)),
   atomic_list_concat( [ 'Knowledgebase [', BaseName, ']' ], TabName),
   size_file( FileName, Size),
   send( KnowledgebaseEditor, load, FileName ),
   send( KnowledgebaseEditor, caret, Size),
   send( KnowledgebaseTab, name, TabName ),
   send( TabStack, on_top, KnowledgebaseTab ),
   send( KnowledgebaseEditor, editable, @on ),
   send( Picture, clear ),
   send( KnowledgebaseEditor, modified, @on ).

newfile( Caller, Extension ) :-
   var( Caller:knowledgebase_editor, KnowledgebaseEditor ),
   var( Caller:knowledgebase_tab, KnowledgebaseTab ),
   var( Caller:tab_stack, TabStack ),
   var( Caller:picture, Picture ),
   send( Picture, clear ),
   send( KnowledgebaseEditor, clear ),
   send( KnowledgebaseTab, name, knowledgebase ),
   send( TabStack, on_top, KnowledgebaseTab ),
   send( KnowledgebaseEditor, editable, @on ),
   saveasfile( Caller,Extension ).

savefile( Caller ) :-
   var( Caller:knowledgebase_editor, KnowledgebaseEditor ),
   var( Caller:knowledgebase_tab, KnowledgebaseTab ),
   var( Caller:current_path, @(FileName)),
   var( Caller:tab_stack, TabStack ),
   send( KnowledgebaseEditor, save, FileName),
   size_file(FileName,Size),
   send( KnowledgebaseEditor, caret, Size),
   send( TabStack, on_top, KnowledgebaseTab ),
   send( KnowledgebaseEditor, editable, @on ), !.

savefile( Caller ):-
	( Caller = socrates
	-> saveasfile(Caller,mkb)
	;  saveasfile(Caller,dkb) ).

saveasfile( Caller, Extension ) :-
   var( Caller:knowledgebase_editor, KnowledgebaseEditor ),
   var( Caller:knowledgebase_tab, KnowledgebaseTab ),
   var( Caller:tab_stack, TabStack ),
   filetype( Extension, Description ),
   new( Finder, finder),
   get( Finder, file, save, [tuple(Description, Extension), tuple('Prolog', 'pl'),   tuple('All files', '*')], FileName ),
   get( file( FileName ), base_name, BaseName ),
   var( Caller:current_path, @(FileName)),
   atomic_list_concat( [ 'Knowledgebase [', BaseName, ']' ], TabName),
   send( KnowledgebaseEditor, save, FileName ),
   size_file(FileName,Size),
   send( KnowledgebaseEditor, caret, Size),
   send( KnowledgebaseTab, name, TabName ),
   send( TabStack, on_top, KnowledgebaseTab ),
   send( KnowledgebaseEditor, editable, @on ).

filetype( mkb, 'Monological KBs' ).
filetype( dkb, 'Dialogical KBs'  ).




