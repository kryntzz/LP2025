% lp24 - ist1113637 - projecto 
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- [puzzles]. % Ficheiro dado. A avaliação terá mais puzzles.
:- [codigoAuxiliar]. % Ficheiro dado. Não alterar.

%---------------------------------------------------------------1.VISUALIZAÇÃO----------------------------------------------------------%

/*
    visualiza(Lista)
    
    Verdadeiro se Lista for uma lista válida. Este predicado exibe cada elemento 
    da lista em uma nova linha.

    Lista - A lista de elementos a que vai ser verificada e exibida.

*/

visualiza([]).

visualiza([H|T]):-
  writeln(H),
  visualiza(T).

/*
    visualizaLinha(Lista)
    
    Verdadeiro se Lista for uma lista válida. Este predicado exibe cada elemento 
    da lista em uma nova linha, aparecendo antes o número da linha em causa, um 
    ":" e um espaço.

    Lista - A lista de elementos a que vai ser verificada e exibida.

*/

visualizaLinha(Lst):-
  visualizaLinha(Lst,1).

visualizaLinha([],_).

visualizaLinha([H|T], Linha):-
  write(Linha),
  write(': '),
  writeln(H),
  NovaLinha is Linha + 1,
  visualizaLinha(T,NovaLinha).

%---------------------------------------------------------------2.INSERÇÃO----------------------------------------------------------%

/*
    insereObjecto((L, C), Tabuleiro, Obj)
    
    Devolve verdadeiro se Tabuleiro é um tabuleiro que após a aplicação deste 
    predicado passa a ter o Obj nas coordenadas (L,C), caso nestas
    se encontre uma variável.

    (L,C) - Coordenadas onde se pretende meter o objeto;
    Tabuleiro - Tabuleiro atual do jogo;
    Obj - Objeto que se pretende meter nas coordenadas do tabuleiro.
    
*/

insereObjecto((L, C), Tabuleiro, Obj) :-
  nth1(L, Tabuleiro, Linha),      
  nth1(C, Linha, Celula),       
  var(Celula),                    
  Celula = Obj,                   
  !.                               

insereObjecto(_,_,_).

/*
    comparaTamanhos(Lst1,Lst2)

    Devolve verdadeiro se ambas as listas tiverem igual
    tamanho.

    Lst1 - Primeira lista a ser verificada;
    LSt2 - Segunda lista a ser verificada.

*/

comparaTamanhos(Lst1,Lst2):-
  length(Lst1,Tam1),
  length(Lst2,Tam2),
  Tam1 =:= Tam2.

/*
  insereVariosObjectos(ListaCoords,Tabuleiro,ListaObjs)

  Devolve verdadeiro se ListaCoords for uma lista de coordenadas, ListaObjs uma
  lista de objectos e Tabuleiro um tabuleiro que após a aplicação do predicado,
  passa a ter nas coordenadas de ListaCoords os objectos de ListaObjs.

  ListaCoords - Lista de coordenadas;
  Tabuleiro - Tabuleiro atual do jogo;
  ListaObjs - Lista de objectos.

*/

insereVariosObjectos([],_,[]).

insereVariosObjectos([H|T],Tabuleiro,[H1|T1]):-
  comparaTamanhos([H|T],[H1|T1]),
  insereObjecto(H,Tabuleiro,H1),
  insereVariosObjectos(T,Tabuleiro,T1).

/*
  inserePontosVolta(Tabuleiro, (L,C))

  Devolve verdadeiro se Tabuleiro for um tabuleiro que após a aplicação
  do predicado passa a ter pontos (p), à volta das coordenadas (L,C),
  em todas as direções.

  Tabuleiro - Tabuleiro atual do jogo;
  (L,C) - Coordenadas a aplicar o predicado.

*/

inserePontosVolta(Tabuleiro,(L,C)):-
  Vet= [(-1,1),(0,1),(1,1),(1,0),(1,-1),(0,-1),(-1,-1),(-1,0)],         %Lista de Vetores necessarios para o predicado.
  inserePontosVoltaAux(Tabuleiro,(L,C),Vet).

/*
  inserePontosVoltaAux(Tabuleiro,(L,C),Vet)

  Predicado auxiliar de "InserePontosVolta", mas com os Vetores a aplicar.

  Tabuleiro - Tabuleiro atual do jogo;
  (L,C) - Coordenadas a aplicar o predicado;
  Vet - Vetores necessarios para aplicar o predicado em todas as direções.

*/
inserePontosVoltaAux(_,_,[]):- !.

inserePontosVoltaAux(Tabuleiro,(L,C),[(C1,L1)|T]):-

  %Soma dos vetores de modo a obter os pontos necessarios.
  X is C + C1,
  Y is L + L1,
  insereObjecto((Y,X),Tabuleiro,p),
  inserePontosVoltaAux(Tabuleiro,(L,C),T).

/*
  inserePontos(Tabuleiro, ListaCoord)

  Devolve verdadeiro se Tabuleiro for um tabuleiro que, após
  a aplicação do predicado, passa a ter pontos (p) em todas as
  coordenadas de ListaCoord.

  Tabuleiro - Tabuleiro atual do jogo;
  ListaCoord - Lista de coordenadas para aplicar o predicado.

*/

inserePontos(_,[]):- !.

inserePontos(Tabuleiro,[H|T]):-
  insereObjecto(H,Tabuleiro,p),
  inserePontos(Tabuleiro,T).

/*
  objectosEmCoordenadas(ListaCoords, Tabuleiro, ListaObjs)

  Devolve verdadeiro se ListaObjs for a lista de objectos das
  coordenadas ListaCoords no tabuleiro Tabuleiro.

  ListaCoords - Lista de coordenadas a aplicar o predicado;
  Tabuleiro - Tabuleiro atual do jogo;
  Lista Objs - Lista de objectos presentes apos aplicar o predicado.

*/

%---------------------------------------------------------------3.CONSULTAS----------------------------------------------------------%

objectosEmCoordenadas(ListaCoords, Tabuleiro, ListaObjs):-
  objectosEmCoordenadas(ListaCoords, Tabuleiro, [], ListaObjs).

objectosEmCoordenadas([],_,ListaObjs,ListaObjs).

objectosEmCoordenadas([(L,C)|Resto],Tabuleiro, Aux, ListaObjs):-
  nth1(L,Tabuleiro,Linha),
  nth1(C,Linha,Obj),
  append(Aux,[Obj],Aux2),
  objectosEmCoordenadas(Resto, Tabuleiro, Aux2, ListaObjs).

/*
  coordObjectos(Objecto, Tabuleiro, ListaCoords, ListaCoordObjs,NumObjectos)

  Devolve verdadeiro se Tabuleiro for um tabuleiro, ListaCoords uma lista 
  de coordenadas e ListaCoordObjs uma lista que contem as coordenadas dos
  objectos do tipo Objecto.
  NumObjectos é o numero de objectos Objecto encontrados.

  Objecto - Objecto a ser encontrado na lista ListaCoords;
  Tabuleiro - Tabuleiro atual do jogo;
  ListaCoords - Lista de coordenadads a aplicar o predicado;
  ListaCoordObjs - Lista de coordenadas presente apos a aplicação do predicado;
  NumObjectos - Numero de objectos do tipo Objecto presentes em ListaCoords.

*/

coordObjectos(Obj, Tabuleiro, ListaCoords, ListaCoordObjs, NumObjectos):-
    objectosEmCoordenadas(ListaCoords, Tabuleiro, ListaObjs),
    coordObjectosAux(ListaCoords, ListaObjs, Obj, ListaCoordObjsAux),
    sort(ListaCoordObjsAux, ListaCoordObjs),
    length(ListaCoordObjs, NumObjectos).

coordObjectosAux([], [], _, []).

coordObjectosAux([(L, C) | TCoords], [H | TObjs], Obj, [(L, C) | TResultado]) :-
  var(H),
  var(Obj),
  coordObjectosAux(TCoords, TObjs, Obj, TResultado),!.
coordObjectosAux([(L, C) | TCoords], [H | TObjs], Obj, [(L, C) | TResultado]) :-
    H == Obj,
    coordObjectosAux(TCoords, TObjs, Obj, TResultado),!.

coordObjectosAux([_ | TCoords], [_ | TObjs], Obj, TResultado) :-
    coordObjectosAux(TCoords, TObjs, Obj, TResultado),!.

/*
    coordenadasVars(Tabuleiro, ListaVars)
    
    Devolve Verdadeiro se ListaVars forem as coordenadas das variáveis do Tabuleiro.
    
    Tabuleiro - Tabuleiro atual do jogo;
    ListaVars - Lista com as coordenadas das variáveis do tabuleiro.
    
*/

coordenadasVars(Tabuleiro, ListaVars):-

  findall((L,C),(nth1(L,Tabuleiro,Linha),nth1(C,Linha,Var),var(Var)),L),
  sort(L,ListaVars).

%---------------------------------------------------------------4.ESTRATÉGIAS----------------------------------------------------------%

/*
    fechaListaCoordenadas(Tabuleiro, ListaCoord)
    
    Devolve Verdadeiro se Tabuleiro for um Tabuleiro e ListaCoord for uma lista 
    de coordenadas que após a aplicação deste predicado as coordenas dessa ListaCoord
    são apenas estrelas e pontos. 
    Considerando as hipoteses dos enunciados.
    
    Tabuleiro - Tabuleiro atual do jogo;
    ListaCoord - Lista de coordenadas a aplicar o predicado.

*/


fechaListaCoordenadas(Tabuleiro,ListaCoord):-

  coordObjectos(e,Tabuleiro,ListaCoord,LstObj,2),

  findall(Coords,(member(Coords,ListaCoord),\+member(Coords,LstObj)),Coords_mdr),
  inserePontos(Tabuleiro,Coords_mdr), 
  !.

fechaListaCoordenadas(Tabuleiro,ListaCoord):-

  coordObjectos(e,Tabuleiro,ListaCoord,_,1),

  findall(Coord,(coordenadasVars(Tabuleiro,ListaVars),
  member(Coord,ListaCoord),member(Coord,ListaVars)),ListaVar),

  length(ListaVar,1),
  [CoordVar] = ListaVar,
  insereObjecto(CoordVar,Tabuleiro,e),
  inserePontosVolta(Tabuleiro,CoordVar),
  !.

fechaListaCoordenadas(Tabuleiro, ListaCoord) :-

    coordObjectos(e, Tabuleiro, ListaCoord, _, 0),

    coordenadasVars(Tabuleiro, ListaVars),
    findall(Coord, (member(Coord, ListaCoord), member(Coord, ListaVars)), ListaVar),

    length(ListaVar, 2),
    ListaVar = [C1, C2],
    insereVariosObjectos(ListaVar, Tabuleiro, [e, e]),
    inserePontosVolta(Tabuleiro, C1),
    inserePontosVolta(Tabuleiro, C2),
    !.

fechaListaCoordenadas(_, _).

/*
    fecha(Tabuleiro, ListaListasCoord)
    
    Devolve Verdadeiro se Tabuleiro for um Tabuleiro e ListaListasCoord for uma
    lista de listas de coordenadas que após a aplicação deste predicado 
    Tabuleiro será o resultado de aplicar o predicado fechaListaCoordenadas,
    a cada lista de coordenadas.

    Tabuleiro - Tabuleiro atual do jogo;
    ListaCoord - Lista de Listas de coordenadas a aplicar o predicado fechaListaCoordenadas.

*/

fecha(_,[]):-!.

fecha(Tabuleiro,[L1|Resto]):-
  fechaListaCoordenadas(Tabuleiro,L1),
  fecha(Tabuleiro,Resto).

/*
    encontraSequencia(Tabuleiro, N, ListaCoord, Seq)
    
    Devolve verdadeiro se Tabuleiro for um tabuleiro, ListaCoords
    for uma lista de coordenadas e N o tamanho de Seq, que e uma sublista de 
    ListaCoords (portanto, com as coordenadas na mesma ordem) que verifica as 
    seguintes condições: 
    As suas coordenadas representam posicoes com variaveis;
    As suas coordenadas aparecem seguidas numa linha coluna ou regiao; 
    Seq pode ser concatenada com duas listas para chegar a ListaCoord.

    
    Tabuleiro - Tabuleiro atual do jogo;
    N - Tamanho de Seq;
    ListaCoord - Lista inicial de coordenadas;
    Seq - Sublista de ListaCoord.

*/

encontraSequencia(Tabuleiro,N,ListaCoords, Seq):-
  
  %Numero de pontos tem que ser igual ao tamanho total de ListaCoords
  %menos o número de variaveis, N.
  length(ListaCoords,NumTotal),
  NumPontos is NumTotal - N,
  coordObjectos(p,Tabuleiro,ListaCoords,_,NumPontos),
  
  %Verifica que não existem estrelas em ListaCoords.
  coordObjectos(e,Tabuleiro,ListaCoords,[],0),
  
  %Sequencia pode ser concatenada com duas listas, uma antes e uma 
  %depois, eventualmente vazias ou com pontos nas coordenadas respetivas.
  append(_,Seq,Aux),
  append(Aux,_,ListaCoords),

  coordObjectos(_,Tabuleiro,ListaCoords,SeqOrdenada,N),

  sort(Seq,SeqOrdenada),!.



/*
    aplicaPadraoI(Tabuleiro, [(L1, C1), (L2, C2), (L3, C3)])
    
    Devolve verdadeiro se Tabuleiro for um tabuleiro e [(L1, C1), (L2, C2), (L3, C3)] 
    for uma lista de coordenadas. 
    Após a aplicacao deste prediicado , tabuleiro sera o resultado de 
    colocar uma estrela em (L1, C1) e (L3, C3) e os pontos a volta de cada estrela.
    
    Tabuleiro - Tabuleiro atual do jogo;
    [(L1, C1), (L2, C2), (L3, C3)] - Lista de Coordenadas.

*/

aplicaPadraoI(Tabuleiro,[(L,C),_,(L3,C3)]):-
  insereObjecto((L,C),Tabuleiro,e),
  insereObjecto((L3,C3),Tabuleiro,e),
  inserePontosVolta(Tabuleiro,(L,C)),
  inserePontosVolta(Tabuleiro,(L3,C3)).


/*
    aplicaPadroes(Tabuleiro, ListaListaCoords)
    
    Devolve verdadeiro se Tabuleiro for um tabuleiro e 
    ListaListaCoords for uma lista de listas de coordenadas. 
    Após a aplicação deste prediicado , tabuleiro sera o resultado de aplicar o aplicaPadraoI
    a sequências de tabanho 3 e o aplicaPadraoT a sequencias de tamanho 4.
    
    Tabuleiro - Tabuleiro atual do jogo;
    ListaListaCoords - Lista de Listas de Coordenadas.

*/

aplicaPadroes(_,[]):- !.

aplicaPadroes(Tabuleiro,[H|T]):-
  length(H,3),
  coordObjectos(e,Tabuleiro,H,_,0),
  aplicaPadraoI(Tabuleiro,H), !,
  aplicaPadroes(Tabuleiro,T).

aplicaPadroes(Tabuleiro,[H|T]):-
  length(H,4),
  coordObjectos(e,Tabuleiro,H,_,0),
  aplicaPadraoT(Tabuleiro,H),!,
  aplicaPadroes(Tabuleiro,T).

aplicaPadroes(Tabuleiro, [_|T]):-
  aplicaPadroes(Tabuleiro,T).

/*
    resolve(Estrutura, Tabuleiro)
    
    Devolve verdadeiro se Estrutura for uma estrutura e Tabuleiro for um tabuleiro
    que resulta de aplicar os predicados aplicaPadroes/2 e fecha/2 até já não
    haver mais alterações nas variáveis do tabuleiro.
    
    Estruturas - Estruturas do tabuleiro.
    Tabuleiro - Tabuleiro atual do jogo.

*/

%---------------------------------------------------------------5.APOTEOSE-FINAL---------------------------------------------------------%

resolve(E,Tabuleiro):-
  coordTodas(E, CoordTodas),
  coordenadasVars(Tabuleiro,ListaVar),
  aplicaPadroes(Tabuleiro,CoordTodas),
  fecha(Tabuleiro,CoordTodas),
  coordenadasVars(Tabuleiro,ListaVar),!.

resolve(E,Tabuleiro):-
  coordTodas(E, CoordTodas),
  aplicaPadroes(Tabuleiro,CoordTodas),
  fecha(Tabuleiro,CoordTodas),
  resolve(E,Tabuleiro).

