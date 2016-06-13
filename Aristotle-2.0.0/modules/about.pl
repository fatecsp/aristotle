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
| About the Software						  |
+----------------------------------------------------------------*/

:- module(about,[about/0]).

about :-
   not( exists_frame ),
   new( Frame,  frame( 'About - Aristotle' ) ),
   send( Frame, append, new( Dialog, dialog ) ),
   send( Dialog, append, text('Aristotle - Computational Argumentation')),
   send( Dialog, append, new( Editor, editor( new( text_buffer ), 64,10) ) ),
   send( Dialog, append, button( close, message( @prolog, close ) ) ),
   send_list( Editor, append,
   ['Aristotle - Computational Argumentation (Version 2)\n
     Copyright (c) 2013-2016 PEREIRA, SANTOS & LIRA.\n
     Available at http://www.ime.usp.br/~slago/aristotle.zip\n
     This is free software. There is ABSOLUTELY NO WARRANTY.
     Read LICENSE.txt for further information.
       '] ),
   var( about:frame, Frame ),
   send( Editor, font, font( screen, normal, 12 ) ),
   send( Editor, caret, 0),
   send( Editor, editable, @off),
   send( Frame,  open, point( 350, 350 ) ).

exists_frame :-
   var( about:frame, Frame ),
   catch( send( Frame, expose ), Error, true ),
   var( Error ).

close :-
   var( about:frame, Frame ),
   send( Frame, destroy ).










