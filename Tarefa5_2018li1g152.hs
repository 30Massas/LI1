-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

import           Data.Char
import           Data.List
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           LI11819
import           Tarefa0_2018li1g152
import           Tarefa1_2018li1g152
import           Tarefa2_2018li1g152
import           Tarefa3_2018li1g152
import           Tarefa4_2018li1g152

-- Module : Tarefa5_2018li1g152
-- Description : Design gráfico do jogo
-- Copyright : Luís Vieira <a89601@alunos.uminho.pt>
--             Paulo Sousa <a89465@alunos.uminho.pt>
--

-- * Introdução

-- $intro
-- Nesta tarefa, fomos propostos a criar a parte gráfica do jogo.
-- Este tarefa foi completamente livre, por isso cada grupo estruturava a tarefa como quisesse.

-- * Objetivos

-- $objs
-- No incio da tarefa, decidimos dividi-la em cinco partes, baseados na ficha gloss3: __estadoInicialGloss, desenhaEstadoGloss, reageEventoGloss, reageTempoGloss e main.__
-- Para o desenvolvimento da mesma, optámos por criar três novos tipos: EstadoGloss, Menu e PosicaoGloss.
-- O tipo EstadoGloss é composto por (Estado,[[Picture]],Menu,[Key]): o Estado representa o estado do jogo, [[Pictures]] a lista de imagens necessárias para o jogo, Menu o ecra em que o jogo se encontra e [Key] uma lista com as teclas pressionadas.
-- O tipo Menu é um inteiro. 0 corresponde ao jogo propiamente dito, 1 ao menu inicial, 2 ao menu dos comandos de jogo e 3 ao menu GameOver.
-- O tipo PosicaoGloss é um par de Floats utilizado para posicionar o mapa corretamente no ecrã.
--
-- == Estado Inicial Gloss
--
-- O __estadoInicalGloss__ recebe uma [[Picture]] (que corresponde às imagens de todas as componentes do jogo) e devolve um EstadoGloss.
-- Para o estado do EstadoGloss, criámos um estadoIncial que possui um mapa predefinido 12 por 12 (com labirintos de blocos destrutiveis e indestrutiveis) e 4 jogadores com posições iniciais nos 4 cantos do mapa.
-- A [[Picture]] do EstadoGloss é a lista que a função recebe, o Menu é 1 (para o jogo abrir no menu inicial) e a [Key] é uma lista vazia.
--
-- == Desenha Estado Gloss
--
-- A função __desenhaEstadoGloss__ recebe um EstadoGloss e devolve uma Picture.
-- Quando o Menu é 1, 2 ou 3 a função devolve a imagem associada ao respetivo menu.
-- Quando o Menu é 0, a função devolve o estado do jogo desenhado. Para isso foi nos essencial a utilização de duas funções: desenhaEstado e pictures.
-- A função desenhaEstado recebe uma [[Picture]] e devolve uma só lista, [Picture], que corresponde a todas as imagens necessárias para o jogo translacionadas para a posição correta.
-- Para isso, esta dividida em quatro funções: desenhaEstadoMapa, desenhaEstadoJogadores, desenhaEstadoCL e desenhaEstadoChoque.
-- A função desenhaEstadoMapa verifica qual a Peca de cada posicao do mapa e translaciona para o devido lugar.
-- A função desenhaEstadoJogador posiciona cada jogador na sua posicao.
-- As funções desenhaEstadoCL e desenhaEstadoChoque posicionam os disparos nos devidos sitios.
-- Após isso, é aplicada sobre a função desenhaEstado a função pictures.
-- Esta transforma a [Picture] em apenas uma imagem, Picture, com todos os elementos devidamente posicionados.
--
-- == Reage Evento Gloss
--
-- A função __reageEventoGloss__ recebe um Evento e um EstadoGloss e devolve um EstadoGloss.
-- Quando o Menu do EstadoGloss é 0, ou seja, estamos no jogo, a função reageMenu vai ser invocada para verificar se o jogador carregou no Enter ou no Espaço para voltar ao menu incial.
-- Se for verdadeiro, o jogo retorna ao estadoInicalGloss, com o utilizador no menu. Se for falso, o jogo vai avançar para a função reage Evento.
-- A função reageEvento recebe um evento e um EstadoGloss e devolve um EstadoGloss.
-- Enquanto o segundo argumento do Evento for Down, o jogo adiciona as teclas pressionadas à [Key]. Quando o segundo argumento do Evento for Up, a função remove todas as teclas adicionadas da lista.
-- Quando o Menu do EstadoGloss é 1 (menu Incial) a função reageMenu, perante o pressionar do Enter ou do Espaço, avança para o menu 2, o menu dos Comandos.
-- Quando o Menu do EstadoGloss é 2 (menu dos Comandos) a função reageMenu, que perante o pressionar do Enter ou do Espaço, avança para o menu 0, o jogo.
-- Quando o Menu do EstadoGloss é 3 (menu de Gameover) a função reageMenu, que perante o pressionar do Enter ou do Espaço, avança para o menu 1, o menu Inical.
--
-- == Reage Tempo Gloss
--
-- A função __reageTempoGloss__ recebe um float e um EstadoGloss, e devolve um EstadoGloss.
-- Quando o Menu do EstadoGloss é 1, 2 ou 3, a função devolve o próprio EstadoGloss, pois estes Menus não reagem ao Tempo.
-- Quando o Menu do EstadoGloss é 0, vai ser utilizada a função contaMortes.
-- Esta função precorre a Lista de jogadores e verifica quantos não possuem vidas restantes.
-- Quando este numero é 3 a função reageTempoGloss devolve um EstadoGloss no Menu 3, menu de GameOver.
-- Quando este numero não é 3, é invocada a função reageTempo.
-- Esta aplica a um estado um tick mas, antes disso, é aplicada a função reageKeyLista ao estado.
-- Esta última aplica todas as teclas pressionadas ao estado, usando a função reageKey, a qual associa a cada tecla uma jogada da Tarefa 2, fazendo mover os jogadores e disparar os Canhoes, Laseres e Choques.
--
-- == Função Principal
--
-- A função __main__, função principal da tarefa 5 é do tipo IO.
-- Primeiramente, foram importadas todas as imagens em formato bmp para o jogo.
-- Após, usando a função let foram criadas listas por categorias com as diferentes imagens do jogo (Lista com as imagens do mapa, jogadores, canhões, lasers, choques e menus)
-- Depois, estas listas foram postas mais uma vez dentro de uma lista p, usando a função let, facilitando a sua utilização.
-- Por fim, introduzimos a função play do gloss.
-- No Display, decidimos usar FullScreen de modo a que o jogo fique mais apelativo esteticamente.
-- Quando à cor do background usamos a função makeColor para criar um fundo Amarelo para o jogo.
-- A FrameRate do jogo escolhida foi 10, pois perante vários testes acordámos que era um meio-termo entre fluídez e jogabilidade.
-- Por último, foram invocadas na função play as 4 funções previamente indicadas: __estadoInicalGloss, desenhaEstadoGloss, reageEventoGloss e reageTempoGloss.__

-- * Conclusão

-- $conc
-- Ao observar o jogo final criado pelo grupo, averiguámos que, por um lado, fomos surpreendidos pela positiva com o resultado, mas por outro lado, os nossos objetivos não foram totalmente cumpridos.
-- Primeiramente, o grupo ficou surpreendido pelo facto de se conseguirem integrar Menus no jogo, algo que não estávamos à espera de conseguir.
-- Para além disso, o grupo ficou surpreendido pela forma como as imagens se completamentavam umas com as outras, criando um jogo estéticamente apelativo.
-- Por outro lado, o grupo ficou desiludido pelo facto de não conseguir implementar bots no jogo, algo que era um dos objetivos desde o início.
-- Além disso, alguns erros na tarefa 2 e na tarefa 4 que não conseguiram ser corrigidos fazem com que o jogo tenha alguns bugs.
-- Concluindo, de modo geral, a tarefa 5 foi um sucesso tendo em conta os objetivos do grupo, apesar de nem todos terem sido cumpridos.



-- * Tipos Gloss

-- | Estado Gloss utilizado para parte grafica do jogo
type EstadoGloss = (Estado,[[Picture]],Menu,[Key])

-- | Numero que corresponde ao menu em que o jogo se encontra
type Menu = Int

-- | Par de Floats utilizado para criar o mapa em Gloss
type PosicaoGloss = (Float,Float)



-- * Estado Inicial Gloss

-- | Função que devolve o estado inicial em gloss
estadoInicialGloss :: [[Picture]] -> EstadoGloss
estadoInicialGloss p = (estadoInicial, p, 1, [])

-- | Estado inicial do jogo
estadoInicial :: Estado
estadoInicial = Estado (mapaGloss) [Jogador (1,1) D 5 5 5, Jogador (1,9) B 5 5 5, Jogador (9,9) E 5 5 5, Jogador (9,1) C 5 5 5] []

-- | Mapa escolhido para o jogo
mapaGloss :: Mapa
mapaGloss = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]



-- * Desenha Estado Gloss

-- | Função que desenha o estado em Gloss
desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (e, [p1,p2,p3,p4,p5,[menu,gameover,comandos]], 0, k) = pictures (desenhaEstado e [p1,p2,p3,p4,p5])
desenhaEstadoGloss (e, [p1,p2,p3,p4,p5,[menu,gameover,comandos]], 1, k) = translate 0 0 menu
desenhaEstadoGloss (e, [p1,p2,p3,p4,p5,[menu,gameover,comandos]], 2, k) = translate 0 0 comandos
desenhaEstadoGloss (e, [p1,p2,p3,p4,p5,[menu,gameover,comandos]], 3, k) = translate 0 0 gameover

-- | Função que translaciiona todas as imagens do jogo
desenhaEstado :: Estado -> [[Picture]] -> [Picture]
desenhaEstado (Estado m j dj) [p1,p2,p3,p4,p5] = (desenhaEstadoMapa m p1 (-384,-384)) ++ (desenhaEstadoJogadores 0 j p2) ++ (desenhaEstadoCL dj p3 p4) ++ (desenhaEstadoChoque dj j p5)

-- | Função que posiciona as imagens do mapa
desenhaEstadoMapa :: Mapa -> [Picture] -> PosicaoGloss -> [Picture]
desenhaEstadoMapa [] p1 (x,y)    = []
desenhaEstadoMapa (h:t) p1 (x,y) = desenhaEstadoLinhas h p1 (x,y) ++ desenhaEstadoMapa t p1 (x+64,y)

-- | Função que posiciona as imagens das linhas do mapa
desenhaEstadoLinhas :: [Peca] -> [Picture] -> PosicaoGloss -> [Picture]
desenhaEstadoLinhas [] _ (x,y) = []
desenhaEstadoLinhas (h:t) [b1,b2,b3] (x,y) | h == (Bloco Indestrutivel) = (translate y (-x) (scale 0.125 0.125 b2)) : desenhaEstadoLinhas t [b1,b2,b3] (x,y+64)
                                           | h == (Bloco Destrutivel)   = (translate y (-x) (scale 0.125 0.125 b1)) : desenhaEstadoLinhas t [b1,b2,b3] (x,y+64)
                                           | h == (Vazia)               = (translate y (-x) (scale 0.125 0.125 b3)) : desenhaEstadoLinhas t [b1,b2,b3] (x,y+64)

-- | Função que posiciona as imagens dos Jogadores
desenhaEstadoJogadores :: Int -> [Jogador] -> [Picture] -> [Picture]
desenhaEstadoJogadores _ [] _ = []
desenhaEstadoJogadores x ((Jogador (a,b) d v l c):t) p2 | v/=0      = desenhaEstadoJogador x (Jogador (a,b) d v l c) p2 : desenhaEstadoJogadores (x+1) t p2
                                                        | otherwise = desenhaEstadoJogadores (x+1) t p2

-- | Função que posiciona a imagem de cada Jogador
desenhaEstadoJogador :: Int -> Jogador -> [Picture] -> Picture
desenhaEstadoJogador 0 (Jogador (x,y) B v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (scale 0.125 0.125 t0))
desenhaEstadoJogador 0 (Jogador (x,y) E v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (rotate 90 (scale 0.125 0.125 t0)))
desenhaEstadoJogador 0 (Jogador (x,y) C v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (rotate 180 (scale 0.125 0.125 t0)))
desenhaEstadoJogador 0 (Jogador (x,y) D v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (rotate 270 (scale 0.125 0.125 t0)))
desenhaEstadoJogador 1 (Jogador (x,y) B v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (scale 0.125 0.125 t1))
desenhaEstadoJogador 1 (Jogador (x,y) E v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (rotate 90 (scale 0.125 0.125 t1)))
desenhaEstadoJogador 1 (Jogador (x,y) C v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (rotate 180 (scale 0.125 0.125 t1)))
desenhaEstadoJogador 1 (Jogador (x,y) D v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (rotate 270 (scale 0.125 0.125 t1)))
desenhaEstadoJogador 2 (Jogador (x,y) B v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (scale 0.125 0.125 t2))
desenhaEstadoJogador 2 (Jogador (x,y) E v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (rotate 90 (scale 0.125 0.125 t2)))
desenhaEstadoJogador 2 (Jogador (x,y) C v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (rotate 180 (scale 0.125 0.125 t2)))
desenhaEstadoJogador 2 (Jogador (x,y) D v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (rotate 270 (scale 0.125 0.125 t2)))
desenhaEstadoJogador 3 (Jogador (x,y) B v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (scale 0.125 0.125 t3))
desenhaEstadoJogador 3 (Jogador (x,y) E v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (rotate 90 (scale 0.125 0.125 t3)))
desenhaEstadoJogador 3 (Jogador (x,y) C v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (rotate 180 (scale 0.125 0.125 t3)))
desenhaEstadoJogador 3 (Jogador (x,y) D v l c) [t0, t1, t2, t3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x)))+352) (rotate 270 (scale 0.125 0.125 t3)))

-- | Função que posiciona as imagens dos Canhoes e dos Lasers
desenhaEstadoCL :: [Disparo] -> [Picture] -> [Picture] -> [Picture]
desenhaEstadoCL [] _ _ = []
desenhaEstadoCL ((DisparoCanhao 0 (x,y) C):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (scale 0.125 0.125 c0)) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 0 (x,y) D):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 90 (scale 0.125 0.125 c0))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 0 (x,y) B):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 180 (scale 0.125 0.125 c0))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 0 (x,y) E):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 270 (scale 0.125 0.125 c0))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 1 (x,y) C):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (scale 0.125 0.125 c1)) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 1 (x,y) D):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 90 (scale 0.125 0.125 c1))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 1 (x,y) B):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 180 (scale 0.125 0.125 c1))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 1 (x,y) E):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 270 (scale 0.125 0.125 c1))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 2 (x,y) C):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (scale 0.125 0.125 c2)) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 2 (x,y) D):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 90 (scale 0.125 0.125 c2))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 2 (x,y) B):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 180 (scale 0.125 0.125 c2))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 2 (x,y) E):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 270 (scale 0.125 0.125 c2))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 3 (x,y) C):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (scale 0.125 0.125 c3)) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 3 (x,y) D):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 90 (scale 0.125 0.125 c3))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 3 (x,y) B):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 180 (scale 0.125 0.125 c3))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoCanhao 3 (x,y) E):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 270 (scale 0.125 0.125 c3))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 0 (x,y) C):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (scale 0.125 0.125 l0)) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 0 (x,y) D):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 90 (scale 0.125 0.125 l0))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 0 (x,y) B):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 180 (scale 0.125 0.125 l0))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 0 (x,y) E):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 270 (scale 0.125 0.125 l0))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 1 (x,y) C):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (scale 0.125 0.125 l1)) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 1 (x,y) D):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 90 (scale 0.125 0.125 l1))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 1 (x,y) B):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 180 (scale 0.125 0.125 l1))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 1 (x,y) E):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 270 (scale 0.125 0.125 l1))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 2 (x,y) C):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (scale 0.125 0.125 l2)) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 2 (x,y) D):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 90 (scale 0.125 0.125 l2))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 2 (x,y) B):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 180 (scale 0.125 0.125 l2))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 2 (x,y) E):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 270 (scale 0.125 0.125 l2))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 3 (x,y) C):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (scale 0.125 0.125 l3)) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 3 (x,y) D):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 90 (scale 0.125 0.125 l3))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 3 (x,y) B):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 180 (scale 0.125 0.125 l3))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL ((DisparoLaser 3 (x,y) E):t) [c0,c1,c2,c3] [l0,l1,l2,l3] = (translate (fromInteger(toInteger (64*y))-352) (fromInteger(toInteger (64*(-x))+352)) (rotate 270 (scale 0.125 0.125 l3))) : desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]
desenhaEstadoCL (h:t) [c0,c1,c2,c3] [l0,l1,l2,l3]                        = desenhaEstadoCL t [c0,c1,c2,c3] [l0,l1,l2,l3]

-- | Função que posiciona as imagens dos Choques
desenhaEstadoChoque :: [Disparo] -> [Jogador] -> [Picture] -> [Picture]
desenhaEstadoChoque [] _ _ = []
desenhaEstadoChoque ((DisparoChoque 0 ti):t) j [ch0,ch1,ch2,ch3] = translate (fromInteger(toInteger (64 * (posY (encontraJogador 0 j))))-352) (fromInteger(toInteger (-64 * (posX (encontraJogador 0 j))))+352) ch0 : desenhaEstadoChoque t j [ch0,ch1,ch2,ch3]
desenhaEstadoChoque ((DisparoChoque 1 ti):t) j [ch0,ch1,ch2,ch3] = translate (fromInteger(toInteger (64 * (posY (encontraJogador 1 j))))-352) (fromInteger(toInteger (-64 * (posX (encontraJogador 1 j))))+352) ch1 : desenhaEstadoChoque t j [ch0,ch1,ch2,ch3]
desenhaEstadoChoque ((DisparoChoque 2 ti):t) j [ch0,ch1,ch2,ch3] = translate (fromInteger(toInteger (64 * (posY (encontraJogador 2 j))))-352) (fromInteger(toInteger (-64 * (posX (encontraJogador 2 j))))+352) ch2 : desenhaEstadoChoque t j [ch0,ch1,ch2,ch3]
desenhaEstadoChoque ((DisparoChoque 3 ti):t) j [ch0,ch1,ch2,ch3] = translate (fromInteger(toInteger (64 * (posY (encontraJogador 3 j))))-352) (fromInteger(toInteger (-64 * (posX (encontraJogador 3 j))))+352) ch3 : desenhaEstadoChoque t j [ch0,ch1,ch2,ch3]
desenhaEstadoChoque (h:t) j [ch0,ch1,ch2,ch3]                    = desenhaEstadoChoque t j [ch0,ch1,ch2,ch3]

-- | Posição em x de um Jogador
posX :: Jogador -> Int
posX (Jogador (x,y) _ _ _ _) = x

-- | Posição em y de um Jogador
posY :: Jogador -> Int
posY (Jogador (x,y) _ _ _ _) = y



-- * Reage Evento Gloss

-- | Função que reage aos eventos realizados pelo utilizador em Gloss
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss ev (e, p, 0, k) | reageMenu ev 0 == 1 = (estadoInicial, p, 1, [])
                                 | otherwise           = reageEvento ev (e, p, 0, k)
reageEventoGloss ev (e, p, 1, k) = (e, p, reageMenu ev 1, k)
reageEventoGloss ev (e, p, 2, k) = (e, p, reageMenu ev 2, k)
reageEventoGloss ev (e, p, 3, k) | reageMenu ev 3 == 1 = (estadoInicial, p, 1, [])
                                 | otherwise           = (e, p, 3, k)

-- | Função que reage aos eventos realizados pelo utilizador
reageEvento :: Event -> EstadoGloss -> EstadoGloss
reageEvento (EventKey t Down _ _) (e, p, m, k) = (e, p, m, k++[t])
reageEvento (EventKey t Up _ _) (e, p, m, k)   = (e, p, m, removeKey t k)

-- | Função que remove as teclas da lista quando estas deixam de ser pressionadas
removeKey :: Key -> [Key] -> [Key]
removeKey a [] = []
removeKey a (h : t) | a==h      = removeKey a t
                    | otherwise = a : removeKey a t

-- | Função que alterna entre Menus do jogo
reageMenu :: Event -> Menu -> Menu
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) 0 = 1
reageMenu (EventKey (SpecialKey KeySpace) Down _ _) 0 = 1
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) 1 = 2
reageMenu (EventKey (SpecialKey KeySpace) Down _ _) 1 = 2
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) 2 = 0
reageMenu (EventKey (SpecialKey KeySpace) Down _ _) 2 = 0
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) 3 = 1
reageMenu (EventKey (SpecialKey KeySpace) Down _ _) 3 = 1
reageMenu _ m                                         = m



-- * Reage Tempo Gloss

-- | Função que reage à passagem do tempo em Gloss
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss t ((Estado m j dj), p, 0, k) | contaMortes 0 j == 3 = ((Estado m j dj), p, 3, k)
                                             | otherwise            = (reageTempo t k (Estado m j dj), p, 0, k)
reageTempoGloss t (e, p, 1, k) = (e, p, 1, k)
reageTempoGloss t (e, p, 2, k) = (e, p, 2, k)
reageTempoGloss t (e, p, 3, k) = (e, p, 3, k)

-- | Função que conta o numero de mortos
contaMortes :: Int -> [Jogador] -> Int
contaMortes x [] = x
contaMortes x ((Jogador (a,b) _ v _ _) : t) | v == 0    = contaMortes (x+1) t
                                            | otherwise = contaMortes x t

-- | Função que reage a passagem do tempo
reageTempo :: Float -> [Key] -> Estado -> Estado
reageTempo t k e = tick (reageKeyLista k e)

-- | Função que executa os estados de uma [Key]
reageKeyLista ::  [Key] -> Estado -> Estado
reageKeyLista [] e    = e
reageKeyLista (h:t) e = reageKeyLista t (reageKey h e)

-- | Função que executa os estados de uma Key
reageKey :: Key -> Estado -> Estado
reageKey (Char 'w') e            = jogada 0 (Movimenta C) e
reageKey (Char 's') e            = jogada 0 (Movimenta B) e
reageKey (Char 'd') e            = jogada 0 (Movimenta D) e
reageKey (Char 'a') e            = jogada 0 (Movimenta E) e
reageKey (Char '1') e            = jogada 0 (Dispara Canhao) e
reageKey (Char '2') e            = jogada 0 (Dispara Laser) e
reageKey (Char '3') e            = jogada 0 (Dispara Choque) e
reageKey (Char 't') e            = jogada 1 (Movimenta C) e
reageKey (Char 'g') e            = jogada 1 (Movimenta B) e
reageKey (Char 'h') e            = jogada 1 (Movimenta D) e
reageKey (Char 'f') e            = jogada 1 (Movimenta E) e
reageKey (Char '4') e            = jogada 1 (Dispara Canhao) e
reageKey (Char '5') e            = jogada 1 (Dispara Laser) e
reageKey (Char '6') e            = jogada 1 (Dispara Choque) e
reageKey (Char 'i') e            = jogada 2 (Movimenta C) e
reageKey (Char 'k') e            = jogada 2 (Movimenta B) e
reageKey (Char 'l') e            = jogada 2 (Movimenta D) e
reageKey (Char 'j') e            = jogada 2 (Movimenta E) e
reageKey (Char '7') e            = jogada 2 (Dispara Canhao) e
reageKey (Char '8') e            = jogada 2 (Dispara Laser) e
reageKey (Char '9') e            = jogada 2 (Dispara Choque) e
reageKey (SpecialKey KeyUp) e    = jogada 3 (Movimenta C) e
reageKey (SpecialKey KeyDown) e  = jogada 3 (Movimenta B) e
reageKey (SpecialKey KeyRight) e = jogada 3 (Movimenta D) e
reageKey (SpecialKey KeyLeft) e  = jogada 3 (Movimenta E) e
reageKey (Char ',') e            = jogada 3 (Dispara Canhao) e
reageKey (Char '.') e            = jogada 3 (Dispara Laser) e
reageKey (Char '-') e            = jogada 3 (Dispara Choque) e
reageKey _ e                     = e



-- * Função Principal

-- | Display mode
dm :: Display
dm = FullScreen

-- | Frame rate
fr :: Int
fr = 10

-- | Função principal que faz correr o jogo
main :: IO ()
main = do b1 <- loadBMP "destrutivel.bmp"
          b2 <- loadBMP "indestrutivel.bmp"
          b3 <- loadBMP "vazia.bmp"
          t0 <- loadBMP "tank0.bmp"
          t1 <- loadBMP "tank1.bmp"
          t2 <- loadBMP "tank2.bmp"
          t3 <- loadBMP "tank3.bmp"
          c0 <- loadBMP "canhao0.bmp"
          c1 <- loadBMP "canhao1.bmp"
          c2 <- loadBMP "canhao2.bmp"
          c3 <- loadBMP "canhao3.bmp"
          l0 <- loadBMP "laser0.bmp"
          l1 <- loadBMP "laser1.bmp"
          l2 <- loadBMP "laser2.bmp"
          l3 <- loadBMP "laser3.bmp"
          ch0 <- loadBMP "choque0.bmp"
          ch1 <- loadBMP "choque1.bmp"
          ch2 <- loadBMP "choque2.bmp"
          ch3 <- loadBMP "choque3.bmp"
          menu <- loadBMP "mainscreen.bmp"
          comandos <- loadBMP "comandos.bmp"
          gameover <- loadBMP "gameover.bmp"
          let p1 = [b1, b2, b3]
          let p2 = [t0, t1, t2, t3]
          let p3 = [c0, c1, c2, c3]
          let p4 = [l0, l1, l2, l3]
          let p5 = [ch0, ch1, ch2, ch3]
          let p = [p1,p2,p3,p4,p5,[menu,gameover,comandos]]
          play dm
               (makeColor (0.9) (0.7) (0) (0.2))
               fr
               (estadoInicialGloss p)
               desenhaEstadoGloss
               reageEventoGloss
               reageTempoGloss
