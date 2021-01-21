:- encoding(utf8).
:- use_module(library(pce)).
:- pce_image_directory('./pictures/').
:- write('\033[2J').    
:- write('[p] Creating images...\n').
resource(menu, menu, image('menu_oca.jpg')).
resource(salida, salida, image('salida.jpg')).
resource(iniciar, iniciarjuego, image('inciar_juego.jpg')).
resource(ayuda, ayuda, image('ayuda.jpg')).
resource(instrucciones, instrucciones, image('instrucciones.jpg')).
resource(ayuda_menu, ayudam, image('ayuda_menu.jpg')).
resource(ayuda_back, ayudab, image('back_ayuda.jpg')).
resource(config, config, image('config.jpg')).
resource(instrucciones_v, image, image('instrucciones_v.jpg')).
:- write('[p] Creating images of the game...\n').
resource(tablero, image, image('tablero.jpg')).     
resource(dado1, image, image('dado1.gif')).          
resource(dado2, image, image('dado2.gif')).
resource(dado3, image, image('dado3.gif')).
resource(dado4, image, image('dado4.gif')).
resource(dado5, image, image('dado5.gif')).
resource(dado6, image, image('dado6.gif')).
resource(actual, image, image('actual2.gif')).       
resource(fondo_oca, image, image('fondo.jpg')).      
resource(oca_gif, image, image('ocanormal.gif')).    
resource(oca_act, image, image('ocaact.gif')).
resource(dialogo, imagen, image('dialogo.gif')).    
resource(fantasma, imagen, image('fantasma.gif')).  
resource(estrella, imagen, image('estrella.gif')).   
:- write('[p] Creating players chips...\n').
resource(fichaazul, fichaazul, image('ficha_azul.gif')).
resource(fichaamari, image, image('ficha_amarillo.gif')).
resource(ficharoja, image, image('ficha_roja.gif')).
resource(fichaverde, image, image('ficha_verde.gif')).
imagen(Window, Figure, Imagen, Position) :-
	new(Figure, figure),
	new(Bitmap, bitmap(resource(Imagen),@on)),
	send(Bitmap, name, 1),
	send(Figure, display, Bitmap),
	send(Figure, status, 1),
	send(Window, display, Figure, Position).
nada(_).
juntar(Item1, Item2, Item3, Item4, Item5, Item6, Resultado):-
	atom_concat(Item1,Item2,Item11),
	atom_concat(Item11,Item3,Item22),
        atom_concat(Item22,Item4,Item33),
	atom_concat(Item33,Item5,Item44),
	atom_concat(Item44,Item6,Resultado).
:- write('[p] Creating dynamic predicates...\n').
:-dynamic turno/1.              
:-dynamic location/2.         
:-dynamic endgame/0.           
:-dynamic tiradas/1.           
:-dynamic numjug/1.            
:-dynamic turnosSinJugar/2.    
:-dynamic nameplayers/3.       
inicializar:-
	retractall(turno(_)),
	assert(turno(1)),
	retractall(location(_,_)),
	assert(location(1, 1)),
	assert(location(2, 1)),
	assert(location(3, 1)),
	assert(location(4, 1)),
	retractall(endgame),
	retractall(tiradas(_)),
	assert(tiradas(1)),
	retractall(turnosSinJugar(_,_)),
	assert(turnosSinJugar(1, 0)),
	assert(turnosSinJugar(2, 0)),
	assert(turnosSinJugar(3, 0)),
	assert(turnosSinJugar(4, 0)),
	retractall(carcel(_)).
free_menu:-
	free(@ayudam),
	free(@ayudab),
	free(@menu),
	free(@mprincipal),
	free(@inst),
	free(@ayuda),
	free(@salir),
	free(@config),
	free(@labelj1),
	free(@labelj2),
	free(@labelj3),	
	free(@labelj4),
	free(@numJug),
	free(@submit),
	free(@instrucciones).
:- write('[p] Launching main menu...\n').
:-
	free(@menu),
	free(@mprincipal),
	free(@inst),
	free(@ayuda),
	free(@salir),
	free(@config),
	new(Menu, window('The Lucky Rabbit Game: Main Menu', size(800, 600))),      
	imagen(Menu, @menu, menu, point(0,0)),
	imagen(Menu, @mprincipal, iniciar, point(33, 186)),
	imagen(Menu, @inst, instrucciones, point(32, 238)),
	imagen(Menu, @ayuda, ayuda, point(82, 284)),
	imagen(Menu, @salir, salida, point(84, 336)),
	send(@mprincipal, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, config, Menu))),
	
	send(@inst, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, instrucciones, Menu))),	
	
	send(@ayuda, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, lanzar_ayuda, Menu))),	
	
	send(@salir, recogniser,
	     click_gesture(left, '', single,
			   message(Menu, destroy))),
	send(@mprincipal, cursor, hand2),
	send(@ayuda, cursor, hand2),
	send(@salir, cursor, hand2),
	send(@inst, cursor, hand2),	
	
	send(Menu, open_centered).      
remenu2:-
	retractall(nameplayers(_, _, _)),
	
	free_menu, 
	
	new(Menu, window('The Lucky Rabbit Game: Menú Principal', size(800, 600))),
	
	imagen(Menu, @menu, menu, point(0,0)),
	imagen(Menu, @mprincipal, iniciar, point(33, 186)),
	imagen(Menu, @inst, instrucciones, point(32, 238)),
	imagen(Menu, @ayuda, ayuda, point(82, 284)),
	imagen(Menu, @salir, salida, point(84, 336)),
	
	send(@mprincipal, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, config, Menu))),
	
	send(@inst, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, instrucciones, Menu))),	
	
	send(@ayuda, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, lanzar_ayuda, Menu))),	
	
	send(@salir, recogniser,
	     click_gesture(left, '', single,
			   message(Menu, destroy))),
	
	send(@mprincipal, cursor, hand2),
	send(@ayuda, cursor, hand2),
	send(@salir, cursor, hand2),
	send(@inst, cursor, hand2),	
	
	send(Menu, open_centered).

remenu(Menu):-
	
	free_menu,
	
	imagen(Menu, @menu, menu, point(0,0)),
	imagen(Menu, @mprincipal, iniciar, point(33, 186)),
	imagen(Menu, @inst, instrucciones, point(32, 238)),
	imagen(Menu, @ayuda, ayuda, point(82, 284)),
	imagen(Menu, @salir, salida, point(84, 336)),
	
	send(@mprincipal, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, config, Menu))),
	
	send(@inst, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, instrucciones, Menu))),	
	
	send(@ayuda, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, lanzar_ayuda, Menu))),	
	
	send(@salir, recogniser,
	     click_gesture(left, '', single,
			   message(Menu, destroy))),
	
	send(@mprincipal, cursor, hand2),
	send(@ayuda, cursor, hand2),
	send(@salir, cursor, hand2),
	send(@inst, cursor, hand2).	

lanzar_ayuda(Menu):-
	
	free_menu,
	
	imagen(Menu, @ayudam, ayuda_menu, point(0,0)),
	imagen(Menu, @ayudab, ayuda_back, point(578, 538)),
	
	send(@ayudab, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, remenu, Menu))),
	
	send(@ayudab, cursor, hand2).

instrucciones(Menu):-
	
	free_menu,
	
	imagen(Menu, @instrucciones, instrucciones_v, point(0,0)),
	imagen(Menu, @ayudab, ayuda_back, point(578, 538)),
	
	send(@ayudab, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, remenu, Menu))),
	
	send(@ayudab, cursor, hand2).

poss_textitem(0, 29, 186, @labelint).
poss_textitem(1, 29, 235, @labelj1).
poss_textitem(2, 29, 270, @labelj2).
poss_textitem(3, 29, 305, @labelj3).
poss_textitem(4, 29, 340, @labelj4).

load_textitem(Num, Menu):-
	
	poss_textitem(Num, X, Y, Var),
	
	atom_concat('PLAYER ', Num, Label),
	
	send(Menu, display, new(Var, text_item(Label)), point(X, Y)),
	send(Var, length, 40),      
	send(Var, editable, true). 

free_textitems(Num):-
	
	poss_textitem(Num, _, _, Label),
	
	send(Label, editable, false).

unfree_textitems(Num):-
	
	poss_textitem(Num, _, _, Label),
	
	send(Label, editable, true).

load_textitems(2):-
	
	free_textitems(3),
	free_textitems(4),
	num_players(2).

load_textitems(3):-
	
	unfree_textitems(3),
	free_textitems(4),
	num_players(3).

load_textitems(4):-
	
	unfree_textitems(3),
	unfree_textitems(4),
	num_players(4).

load_start(Menu):-
	
	load_textitem(1, Menu),
	load_textitem(2, Menu),
	load_textitem(3, Menu),
	load_textitem(4, Menu),
	num_players(4).

num_players(Num):-
	
	retractall(numjug(_)),
	assert(numjug(Num)).

insert_players(2, P1, P2, _, _):-
	
	assert(nameplayers(1, P1, 'rojo')),
	assert(nameplayers(2, P2, 'rojo')),
	assert(nameplayers(3, '', '')),
	assert(nameplayers(4, '', '')).

insert_players(3, P1, P2, P3, _):-
	
	assert(nameplayers(1, P1, 'rojo')),
	assert(nameplayers(2, P2, 'rojo')),
	assert(nameplayers(3, P3, 'rojo')),
	assert(nameplayers(4, '', '')).

insert_players(4, P1, P2, P3, P4):-
	
	assert(nameplayers(1, P1, 'rojo')),
	assert(nameplayers(2, P2, 'rojo')),
	assert(nameplayers(3, P3, 'rojo')),
	assert(nameplayers(4, P4, 'rojo')).

config(Menu):-
	
	free_menu,
	
	imagen(Menu, @config, config, point(0,0)),
	imagen(Menu, @ayudab, ayuda_back, point(578, 538)),
	
	send(@ayudab, recogniser,
	     click_gesture(left, '', single,
			   message(@prolog, remenu, Menu))),
	
	send(@ayudab, cursor, hand2),
	
	send(Menu, display, new(@numJug, 
				int_item('Number of Players', 4, 
					 message(@prolog, load_textitems, @numJug?selection))),
	     point(30, 200)),
	
	send(@numJug, range(low := 2, high := 4)),    
	
	load_start(Menu),
       	
	send(Menu, display, new(@submit, button('START GAME',
						and(
						    message(@prolog, insert_players, @numJug?selection,
							    @labelj1?value, @labelj2?value, 
							    @labelj3?value, @labelj4?value),
						    message(@prolog, go),
						    message(Menu, destroy)))),
	     point(50, 500)).
go :-
	inicializar,
	new(Rab, window('The Lucky Rabbit Game', size(1000, 566))),
	free_all,
	imagen(Rab, @fondo, fondo_oca, point(0,0)),
	imagen(Rab, @tablerooca, tablero, point(0, 0)),
	imagen(Rab, @actual, actual, point(770, 30)),
	crear_fichas(Rab),	
	send(Rab, display, new(@lanzardado, 
			       button('Throw',
				      message(@prolog, empezar_todo, Rab))),
	     point(900, 8)),
	send(Rab, display, new(@reiniciar, 
			       button('Restart',
				      and(
					  message(Rab, destroy),
					  message(@prolog, go)))), 
	     point(500, 600)),
	
	send(Rab, display, new(@salir_button, 
			       button('Go out',
				      and(
					  message(Rab, destroy),
					  message(@prolog, remenu2)))),
	     point(600, 600)),
	
	imagen(Rab, @dialogo, dialogo, point(750, 175)),
	imagen(Rab, @ocan2, oca_act, point(800, 300)),
	
	nameplayers(1, Name, _),
	send_log('Welcome to The Lucky Rabbit Game. \n\nTry your luck player "', Name, '"', '', '', '', Rab),
	
	send(Rab, open_centered).
free_all:-
	free(@tablerooca),
	free(@fichaj1),
	free(@fichaj2),
	free(@fichaj3),
	free(@fichaj4),
	free(@fichaj12),
	free(@fichaj22),
	free(@fichaj32),
	free(@fichaj42),
	free(@txjug1),
	free(@txjug2),
	free(@txjug3),
	free(@txjug4),
	free(@lanzardado),
	free(@stargame),
	free(@actual),
	free(@dado),
	free(@logt2),
	free(@logs),
	free(@buffer),
	free(@reiniciar),
	free(@salir_button),
	free(@fondo),
	free(@ocan),
	free(@ocan2),
	free(@dialogo),
	free(@fantasma),
	free(@estrella).

crear_fichas(Rab):-
	
	imagen(Rab, @fichaj1, fichaverde, point(739,9)),
	imagen(Rab, @fichaj2, fichaazul, point(739,47)),
	imagen(Rab, @fichaj3, fichaamari, point(739,85)),
	imagen(Rab, @fichaj4, ficharoja, point(739,124)),
	imagen(Rab, @fichaj12, fichaverde, point(739,9)),
	imagen(Rab, @fichaj22, fichaazul, point(739,47)),
	imagen(Rab, @fichaj32, fichaamari, point(739,85)),
	imagen(Rab, @fichaj42, ficharoja, point(739,124)),
	
	nameplayers(1, Name1, _), nameplayers(2, Name2, _), nameplayers(3, Name3, _), nameplayers(4, Name4, _),
	send(Rab, display, new(@txjug1, text(Name1)), point(774,6)),
	send(Rab, display, new(@txjug2, text(Name2)), point(774,45)),
	send(Rab, display, new(@txjug3, text(Name3)), point(774,83)),
	send(Rab, display, new(@txjug4, text(Name4)), point(774,122)),
	
	send(@txjug1, font, font('Arial', sans, 14)),
	send(@txjug2, font, font('Arial', bold, 14)),
	send(@txjug3, font, font('Arial', bold, 14)),
	send(@txjug4, font, font('Arial', bold, 14)),
	
	numjug(NumJug),
	
	(   NumJug=2
	->  borrar_fichas(3),
	    borrar_fichas(4)
	;   NumJug=3
	->  borrar_fichas(4)
	;   nada(_)
	).

borrar_fichas(3):-
	
	free(@fichaj3),
	free(@fichaj4),
	free(@fichaj32),
        free(@fichaj42).

borrar_fichas(4):-
	
	free(@fichaj4),
	free(@fichaj42).

coords(0,0,0).
coords(1, 14, 22).
coords(2, 14, 102).
coords(3, 14, 190).
coords(4, 14, 263).
coords(5, 14, 339).
coords(6, 14, 425).
coords(7, 14, 498).
coords(8, 96, 501).
coords(9, 175, 501).
coords(10, 252, 501).
coords(11, 339, 501).
coords(12, 420, 501).
coords(13, 504, 501).
coords(14, 579, 501).
coords(15, 658, 501).
coords(16, 661, 422).
coords(17, 661, 343).
coords(18, 661, 259).
coords(19, 661, 184).
coords(20, 661, 98).
coords(21, 661, 22).
coords(22, 579, 22).
coords(23, 495, 22).
coords(24, 415, 22).
coords(25, 333, 22).
coords(26, 252, 22).
coords(27, 175, 22).
coords(28, 92, 22).
coords(29, 92, 99).
coords(30, 92, 175).
coords(31, 92, 260).
coords(32, 92, 340).
coords(33, 94, 421).
coords(34, 177, 421).
coords(35, 256, 421).
coords(36, 337, 421).
coords(37, 416, 421).
coords(38, 499, 421).
coords(39, 581, 421).
coords(40, 581, 346).
coords(41, 581, 258).
coords(42, 581, 177).
coords(43, 581, 99).
coords(44, 498, 99).
coords(45, 416, 99).
coords(46, 336, 99).
coords(47, 253, 99).
coords(48, 171, 99).
coords(49, 171, 178).
coords(50, 171, 256).
coords(51, 171, 341).
coords(52, 251, 341).
coords(53, 335, 341).
coords(54, 419, 341).
coords(55, 500, 341).
coords(56, 500, 252).
coords(57, 500, 180).
coords(58, 413, 180).
coords(59, 339, 180).
coords(60, 254, 180).
coords(61, 254, 258).
coords(62, 333, 258).
coords(63, 438, 273).

posicion_ficha(0, 0, 0).
posicion_ficha(1, 0, 0).
posicion_ficha(2, 15, 0).
posicion_ficha(3, 0, 15).
posicion_ficha(4, 15, 15).

pos_ficha(IDficha, Box, PosX, PosY):-
	
	coords(Box, X, Y),
	
	posicion_ficha(IDficha, Xpos, Ypos),
	
	PosX is Xpos+X,
	PosY is Ypos+Y.

identifica_ficha(@fantasma, 0, fantasma).
identifica_ficha(@fichaj1, 1, fichaverde).
identifica_ficha(@fichaj2, 2, fichaazul).
identifica_ficha(@fichaj3, 3, fichaamari).
identifica_ficha(@fichaj4, 4, ficharoja).

move_ficha(IDficha, X, Y, Rab):-
	
	identifica_ficha(Ficha, IDficha, Color),
	
	send(Ficha, destroy),
	imagen(Rab, Ficha, Color, point(X,Y)).

move_ficha_casilla(IDficha, Box, Rab):-
	pos_ficha(IDficha, Box, PosX, PosY),
	move_ficha(IDficha, PosX, PosY, Rab).
cara_dado(Num, Resource):-
	
	atom_concat('dado', Num, Resource).

dados(X):-
	
	X is random(6)+1.

send_log(Msg1, Msg2, Msg3, Msg4, Msg5, Msg6, Rab):-
	free(@logs),
	juntar(Msg1, Msg2, Msg3, Msg4, Msg5, Msg6, Resultado),	
        send(Rab, display, new(@logs, text(Resultado)), point(765, 185)),
        send(@logs, font, font('Arial', normal, 12)),          
        send(@logs, geometry(width := 255, height := 174)).    	

send_prolog(Msg1, Msg2, Msg3, Msg4, Msg5, Msg6):-
	juntar(Msg1, Msg2, Msg3, Msg4, Msg5, Msg6, Resultado),
	write(Resultado).
empezar_todo(_):-endgame.   
empezar_todo(Rab):-
	
	free(@fantasma),
	imagen(Rab, @fantasma, fantasma, point(923, 469)),
       
	send(@logs, clear),
	
	tiradas(Tiradas),
	
	TiradasNew is Tiradas+1,
	retractall(tiradas(_)),
	assert(tiradas(TiradasNew)),

	free(@dado),
	
	turno(TurnoA),
	
	dados(N),
	
	location(TurnoA, CarcelP),
	newpos(CarcelP, N, NewPossC),
	
	salir_carcel(1, CarcelP, NewPossC),
	salir_carcel(2, CarcelP, NewPossC),
	salir_carcel(3, CarcelP, NewPossC),
	salir_carcel(4, CarcelP, NewPossC),	
	
	cara_dado(N, Imagen),
	imagen(Rab, @dado, Imagen, point(900, 35)),
	
	moveplayer(TurnoA, N),
	
	check_casilla(TurnoA, Rab),
	
	turno(SiguienteJugador),	

	cambia_token(SiguienteJugador, Rab),
	
	location(TurnoA, Poss),
	
	move_ficha_casilla(TurnoA, Poss, Rab).

barras_turno(0, 0, 0).
barras_turno(1, 771, 31).
barras_turno(2, 771, 70).
barras_turno(3, 771, 110).
barras_turno(4, 771, 149).

cambia_token(Siguiente, Rab):-

	barras_turno(Siguiente, X, Y),
	
	send(@actual, destroy),
	imagen(Rab, @actual, actual, point(X,Y)).
animar_oca(Rab):-
	
	free(@ocan),
	free(@ocan2),
	
        imagen(Rab, @ocan2, oca_act, point(865, 390)).
 
fantasma(Box, Rab):-
	
	move_ficha_casilla(0, Box, Rab).

estrella_ganador(1, 739, 9).
estrella_ganador(2, 739, 47).
estrella_ganador(3, 739, 85).
estrella_ganador(4, 739, 124).

ganador_imagen(IDplayer, Rab):-
	
	estrella_ganador(IDplayer, X, Y),
	imagen(Rab, @estrella, estrella, point(X, Y)).

newpos(PosA,N,NewPos):-
	
	(   PosA+N>63
	->  NewPos is 63- (PosA+N-63)
	;   NewPos is PosA+N
	).
moveplayer(Jug,N):-
	    location(Jug, X),
	(   X=63
	->  NewPos=63
	;   newpos(X, N, NewPos)
	),
        retract(location(Jug, X)),
        assert(location(Jug, NewPos)).
moveplayer_casilla(Jug, N):-
	location(Jug, X),
	retract(location(Jug, X)),
	assert(location(Jug, N)).
siguiente_players(Actual,Siguiente):-
	
	numjug(Act),
	(   Actual is Act
	->  Siguiente is 1
	;   Siguiente is Actual+1
	).
salir_carcel(IDplayer, PossAct, Next):-
	turnosSinJugar(IDplayer, T),
	
	(   PossAct<52
	->  (   Next>52
	    ->  (   T>50
		->  retractall(turnosSinJugar(IDplayer, _)),
		    assert(turnosSinJugar(IDplayer, 0))
		;   nada(_)
		)
	    ;   nada(_)
	    )
	;   nada(_)
	).

meta(IDplayer, Rab):-
	
	animar_oca(Rab),
	assert(endgame),

        nameplayers(IDplayer, Name, _),
        tiradas(Tiradas),
	
	send_log('¡"', Name, '" has won the game! =\'D', '\n\nTotal rolls: ', Tiradas, '', Rab),
	cambia_token(IDplayer, Rab),
	
	ganador_imagen(IDplayer, Rab).
meta_oca(IDplayer, Rab):-
	moveplayer_casilla(IDplayer, 63),
	meta(IDplayer, Rab).
siguiente_oca(Actual, Siguiente):-
	
	boxjug(Siguiente, Box),
	
	Siguiente > Actual,
	(Box=oca;Box=meta), !.
oca(IDplayer, Rab):-
	
	animar_oca(Rab),
	
	location(IDplayer, RabActual),	
	nameplayers(IDplayer, Name, _),
	
	siguiente_oca(RabActual, SiguienteRab),
	fantasma(RabActual, Rab),
	
	moveplayer_casilla(IDplayer, SiguienteRab),
       
	send_log('"', Name, '" has bumped into the super Rabbit \n and jumps out of the box', RabActual, '\nto the box', SiguienteRab, '\n\n¡Tira de nuevo!', Rab).
calavera(IDplayer, Rab):-
	animar_oca(Rab),
        fantasma(58, Rab),
	nameplayers(IDplayer, Name, _),
        moveplayer_casilla(IDplayer, 1),
	send_log('"', Name, '" has fallen on the skull \n and returns to the beginning = (', '', '\n\n', '', Rab).
siguiente_puente(Actual, Siguiente):-
	
	boxjug(Siguiente, puente),
	Siguiente \= Actual.
puente(IDplayer, Rab):-
	
	animar_oca(Rab),
	
        nameplayers(IDplayer, Name, _),
	location(IDplayer, PuenteActual),
	
	fantasma(PuenteActual, Rab),
	siguiente_puente(PuenteActual, Next),
	
	atom_concat('\nto the box ', Next, Concat),
	send_log('"', Name, '" has fallen on the bridge \n and jumps out of the box ', PuenteActual, Concat, '\n\nPull again!', Rab),
	
	moveplayer_casilla(IDplayer, Next).
siguiente_dados(Actual, Siguiente):-
	
	boxjug(Siguiente, losdados),
	Siguiente \= Actual.
losdados(IDplayer, Rab):-
	
	animar_oca(Rab),
	
	nameplayers(IDplayer, Name, _),
	location(IDplayer, DadosActual),
	
	fantasma(DadosActual, Rab),	
	siguiente_dados(DadosActual, Next),
	
	send_log('"', Name, '" it has fallen on the dice \n and it jumps out of the box ', DadosActual, '\nto the box ', Next, Rab),
	
	moveplayer_casilla(IDplayer, Next).
laberinto(IDplayer, Rab):-
	
	animar_oca(Rab),
	
	nameplayers(IDplayer, Name, _),
	
	send_log('"', Name, '" you get lost in the maze \n and go back to box 30\n', '', '\n\n', '', Rab),
	
	moveplayer_casilla(IDplayer, 30),
	
	turno(TurnoA),
	siguiente_players(TurnoA, SiguienteJugador),
	turnosSinJugar(SiguienteJugador, SinJugar),
	siguiente_players(SiguienteJugador, SigSigJugador),
	
	(   SinJugar>0
	->  Turno=SigSigJugador,
	    NuevosTurnos is SinJugar-1,
	    retract(turnosSinJugar(SiguienteJugador, _)),
	    assert(turnosSinJugar(SiguienteJugador, NuevosTurnos))
	;   Turno=SiguienteJugador
	),
	
	retractall(turno(_)),
	assert(turno(Turno)).	

posada(IDplayer, Rab):-
	
	animar_oca(Rab),
	
	retract(turnosSinJugar(IDplayer, _)),
	assert(turnosSinJugar(IDplayer, 1)),   
	
	nameplayers(IDplayer, Name, _),
	
	send_log('"', Name, '" He has fallen at the inn \n and will be 1 turn without playing. ','', '', '', Rab),
	
	siguiente_players(IDplayer, SiguienteJugador),
	
	retractall(turno(_)),
	assert(turno(SiguienteJugador)).

pozo(IDplayer, Rab):-
	
	animar_oca(Rab),
	
	retract(turnosSinJugar(IDplayer, _)),
	assert(turnosSinJugar(IDplayer, 2)),
	
	nameplayers(IDplayer, Name, _),
	
	send_log('"', Name, '" has fallen into the pot \n and will be 2 turns without playing =( ','', '', '', Rab),
	
	siguiente_players(IDplayer, SiguienteJugador),
	
	retractall(turno(_)),
	assert(turno(SiguienteJugador)).

lacarcel(IDplayer, Rab):-
	
	animar_oca(Rab),
	
	retract(turnosSinJugar(IDplayer, _)),
	assert(turnosSinJugar(IDplayer, 999)),    
	
	nameplayers(IDplayer, Name, _),
	
	send_log('"', Name, '" is locked in the jail \n and will come out when someone passes \nthrough that position =( ','', '', '', Rab),
	
	siguiente_players(IDplayer, SiguienteJugador),
	retractall(turno(_)),
	assert(turno(SiguienteJugador)).	
noact(_, Rab):-
	
	free(@ocan2),
	free(@ocan),
	
	imagen(Rab, @ocan, oca_gif, point(865, 390)),
	
	turno(TurnoA),
	
	siguiente_players(TurnoA, SiguienteJugador),
	turnosSinJugar(SiguienteJugador, SinJugar),
	siguiente_players(SiguienteJugador, SigSigJugador),
	
	(   SinJugar>0
	->  Turno=SigSigJugador,
	    NuevosTurnos is SinJugar-1,
	    retract(turnosSinJugar(SiguienteJugador, _)),
	    assert(turnosSinJugar(SiguienteJugador, NuevosTurnos))
	;   Turno=SiguienteJugador
	),
	retractall(turno(_)),
	assert(turno(Turno)).
boxjug(1, noact).
boxjug(2, noact).
boxjug(3, noact).
boxjug(4, noact).
boxjug(5, oca).
boxjug(6, puente).
boxjug(7, noact).
boxjug(8, noact).
boxjug(9, oca).
boxjug(10, noact).
boxjug(11, noact).
boxjug(12, puente).
boxjug(13, noact).
boxjug(14, oca).
boxjug(15, noact).
boxjug(16, noact).
boxjug(17, noact).
boxjug(18, oca).
boxjug(19, posada).
boxjug(20, noact).
boxjug(21, noact).
boxjug(22, noact).
boxjug(23, oca).
boxjug(24, noact).
boxjug(25, noact).
boxjug(26, losdados).
boxjug(27, oca).
boxjug(28, noact).
boxjug(29, noact).
boxjug(30, noact).
boxjug(31, pozo).
boxjug(32, oca).
boxjug(33, noact).
boxjug(34, noact).
boxjug(35, noact).
boxjug(36, oca).
boxjug(37, noact).
boxjug(38, noact).
boxjug(39, noact).
boxjug(40, noact).
boxjug(41, oca).
boxjug(42, laberinto).
boxjug(43, noact).
boxjug(44, noact).
boxjug(45, oca).
boxjug(46, noact).
boxjug(47, noact).
boxjug(48, noact).
boxjug(49, noact).
boxjug(50, oca).
boxjug(51, noact).
boxjug(52, lacarcel).
boxjug(53, losdados).
boxjug(54, oca).
boxjug(55, noact).
boxjug(56, noact).
boxjug(57, noact).
boxjug(58, calavera).
boxjug(59, meta_oca).
boxjug(60, noact).
boxjug(61, noact).
boxjug(62, noact).
boxjug(63, meta).

check_casilla(Jug, Rab):-
	
	location(Jug, Poss),
	boxjug(Poss, Box),
	call(Box, Jug, Rab).
