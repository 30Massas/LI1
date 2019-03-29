-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g152 where

import           LI11819
import           Tarefa0_2018li1g152
import           Tarefa1_2018li1g152
import           Tarefa2_2018li1g152

-- Module : Tarefa4_2018li1g152
-- Description : Criação da função tick que avança o jogo no tempo
-- Copyright : Luís Vieira <a89601@alunos.uminho.pt>
--             Paulo Sousa <a89465@alunos.uminho.pt>
--

-- * Testes

-- | Testes da Tarefa 4
testesT4 :: [Estado]
testesT4 = [(Estado (mapaInicial (6,6)) [Jogador (1,1) C 5 3 3, Jogador (3,3) B 5 2 2] [DisparoChoque 0 5, DisparoChoque 1 1]),
            (Estado (mapaInicial (15,15)) l1 [DisparoCanhao 0 (3,3) D, DisparoCanhao 1 (3,5) E]),
            (Estado (mapaInicial (15,15)) l1 [DisparoCanhao 0 (3,3) B, DisparoCanhao 1 (5,3) C]),
            (Estado (mapaInicial (15,15)) l1 [DisparoCanhao 0 (3,3) D, DisparoCanhao 1 (3,4) E]),
            (Estado (mapaInicial (15,15)) l1 [DisparoCanhao 0 (3,3) B, DisparoCanhao 1 (4,3) C]),
            (Estado (mapaInicial (15,15)) l1 [DisparoCanhao 0 (3,3) B, DisparoLaser 1 (6,3) C]),
            (Estado (mapaInicial (15,15)) l1 [DisparoLaser 2 (11,1) C]),
            (Estado (mapaInicial (15,15)) l1 [DisparoLaser 0 (3,3) B, DisparoLaser 1 (6,3) C]),
            (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]) [Jogador (8,3) C 1 2 2] [DisparoLaser 0 (7,3) C]),
            (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]) [Jogador (8,3) C 1 2 2] [DisparoCanhao 0 (5,3) C]),
            (Estado ([[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]) [Jogador (1,3) B 1 2 2] [DisparoLaser 0 (3,3) B]),
            (Estado (mapaInicial (10,10)) [Jogador (3,3) C 3 4 5] [DisparoCanhao 0 (2,1) D, DisparoCanhao 0 (2,2) E]),
            (Estado (mapaInicial (8,8)) [Jogador (3,3) D 3 3 4] [DisparoCanhao 0 (4,3) C]),
            (Estado (mapaInicial (10,10)) [Jogador (7,3) D 3 5 4] [DisparoChoque 0 0]),
            (Estado (mapaInicial (10,10)) [Jogador (5,5) C 2 2 2] [DisparoCanhao 0 (2,2) C, DisparoCanhao 0 (2,2) E]),
            (Estado (mapaInicial (8,8)) [Jogador (4,4) D 4 2 1] [DisparoChoque 0 1]),
            (Estado (mapaInicial (7,7)) [Jogador (3,2) D 2 2 2, Jogador (1,3) B 3 3 3] [DisparoLaser 0 (1,4) B]),
            (Estado (mapaInicial (15,15)) [Jogador (5,5) C 1 1 1] [DisparoLaser 0 (2,2) D, DisparoCanhao 0 (2,3) B, DisparoCanhao 0 (1,3) B]),
            (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [Jogador (3,5) E 5 5 5] [DisparoCanhao 0 (3,3) E]),
            (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [Jogador (3,5) E 5 5 5] [DisparoCanhao 0 (5,3) E])
            ]

-- | Lista de Jogadores usada para os testes
l1 :: [Jogador]
l1 = [Jogador (1,1) D 2 3 1, Jogador (1,12) B 5 4 10, Jogador (12,12) E 10 4 3, Jogador (12,1) C 1 2 3]



-- * Ticks

-- | Avança o Estado do jogo um Tick de tempo.
tick :: Estado -> Estado
tick = tickChoques . tickCanhoes . tickLasers



-- * Ticks Lasers

-- | Avança o 'Estado' do jogo um Tick de tempo, considerando apenas os efeitos dos tiros de Laser disparados.
tickLasers :: Estado -> Estado
tickLasers (Estado m j dj) = Estado (alteraMapaLaser dj m) (editaJogadorDisparo dj j) (removeLaser dj)

-- | Altera o mapa quando algum bloco é atingido por um Laser
alteraMapaLaser :: [Disparo] -> Mapa -> Mapa
alteraMapaLaser [] m = m
alteraMapaLaser ((DisparoLaser a (x,y) C):t) m | (eBordaMatriz (x,y) m)                                                                                             = alteraMapaLaser t m
                                               | (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel) || (encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel) = alteraMapaLaser t m
                                               | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel) && (encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel)     = alteraMapaLaser ((DisparoLaser a (x-1,y) C):t) (atualizaPosicaoMatriz (x,y+1) Vazia (atualizaPosicaoMatriz (x,y) Vazia m))
                                               | otherwise                                                                                                          = alteraMapaLaser ((DisparoLaser a (x-1,y) C):t) m
alteraMapaLaser ((DisparoLaser a (x,y) B):t) m | (eBordaMatriz (x,y) m)                                                                                             = alteraMapaLaser t m
                                               | (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel) || (encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel) = alteraMapaLaser t m
                                               | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel) && (encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel)     = alteraMapaLaser ((DisparoLaser a (x+1,y) B):t) (atualizaPosicaoMatriz (x,y+1) Vazia (atualizaPosicaoMatriz (x,y) Vazia m))
                                               | otherwise                                                                                                          = alteraMapaLaser ((DisparoLaser a (x+1,y) B):t) m
alteraMapaLaser ((DisparoLaser a (x,y) D):t) m | (eBordaMatriz (x,y) m)                                                                                             = alteraMapaLaser t m
                                               | (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel) || (encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel) = alteraMapaLaser t m
                                               | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel) && (encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel)     = alteraMapaLaser ((DisparoLaser a (x,y+1) D):t) (atualizaPosicaoMatriz (x+1,y) Vazia (atualizaPosicaoMatriz (x,y) Vazia m))
                                               | otherwise                                                                                                          = alteraMapaLaser ((DisparoLaser a (x,y+1) D):t) m
alteraMapaLaser ((DisparoLaser a (x,y) E):t) m | (eBordaMatriz (x,y) m)                                                                                             = alteraMapaLaser t m
                                               | (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel) || (encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel) = alteraMapaLaser t m
                                               | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel) && (encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel)     = alteraMapaLaser ((DisparoLaser a (x,y-1) E):t) (atualizaPosicaoMatriz (x+1,y) Vazia (atualizaPosicaoMatriz (x,y) Vazia m))
                                               | otherwise                                                                                                          = alteraMapaLaser ((DisparoLaser a (x,y-1) E):t) m
alteraMapaLaser (h:t) m = alteraMapaLaser t m

-- | Verifica todos os disparos de Canhao para todos os Jogadores
editaJogadorDisparo :: [Disparo] -> [Jogador] -> [Jogador]
editaJogadorDisparo [] j                            = j
editaJogadorDisparo ((DisparoCanhao a (x,y) d):t) j = editaJogadorDisparo t (jogadorDisparoAux (DisparoCanhao a (x,y) d) j)
editaJogadorDisparo ((DisparoLaser a (x,y) d):t) j  = editaJogadorDisparo t (jogadorDisparoAux (DisparoLaser a (x,y) d) j)
editaJogadorDisparo (h:t) j                         = editaJogadorDisparo t j

-- | Verifica se um jogador colide com um disparo canhao
jogadorDisparoAux :: Disparo -> [Jogador] -> [Jogador]
jogadorDisparoAux _ [] = []
jogadorDisparoAux (DisparoCanhao n (a,b) C) ((Jogador (x,y) d v l c):t) | (a-1)==x && b==y && v>=1 = (Jogador (x,y) d (v-1) l c) : t
                                                                        | otherwise                = (Jogador (x,y) d v l c) : jogadorDisparoAux (DisparoCanhao n (a,b) C) t
jogadorDisparoAux (DisparoCanhao n (a,b) B) ((Jogador (x,y) d v l c):t) | (a+1)==x && b==y && v>=1 = (Jogador (x,y) d (v-1) l c) : t
                                                                        | otherwise                = (Jogador (x,y) d v l c) : jogadorDisparoAux (DisparoCanhao n (a,b) B) t
jogadorDisparoAux (DisparoCanhao n (a,b) D) ((Jogador (x,y) d v l c):t) | a==x && (b+1)==y && v>=1 = (Jogador (x,y) d (v-1) l c) : t
                                                                        | otherwise                = (Jogador (x,y) d v l c) : jogadorDisparoAux (DisparoCanhao n (a,b) D) t
jogadorDisparoAux (DisparoCanhao n (a,b) E) ((Jogador (x,y) d v l c):t) | a==x && (b-1)==y && v>=1 = (Jogador (x,y) d (v-1) l c) : t
                                                                        | otherwise                = (Jogador (x,y) d v l c) : jogadorDisparoAux (DisparoCanhao n (a,b) E) t
jogadorDisparoAux (DisparoLaser n (a,b) C) ((Jogador (x,y) d v l c):t) | x<=a && y==b && v>=1 = (Jogador (x,y) d (v-1) l c) : jogadorDisparoAux (DisparoLaser n (a,b) C) t
                                                                       | otherwise            = (Jogador (x,y) d v l c) : jogadorDisparoAux (DisparoLaser n (a,b) C) t
jogadorDisparoAux (DisparoLaser n (a,b) B) ((Jogador (x,y) d v l c):t) | x>=a && y==b && v>=1 = (Jogador (x,y) d (v-1) l c) : jogadorDisparoAux (DisparoLaser n (a,b) B) t
                                                                       | otherwise            = (Jogador (x,y) d v l c) : jogadorDisparoAux (DisparoLaser n (a,b) B) t
jogadorDisparoAux (DisparoLaser n (a,b) D) ((Jogador (x,y) d v l c):t) | x==a && y>=b && v>=1 = (Jogador (x,y) d (v-1) l c) : jogadorDisparoAux (DisparoLaser n (a,b) D) t
                                                                       | otherwise            = (Jogador (x,y) d v l c) : jogadorDisparoAux (DisparoLaser n (a,b) D) t
jogadorDisparoAux (DisparoLaser n (a,b) E) ((Jogador (x,y) d v l c):t) | x==a && y<=b && v>=1 = (Jogador (x,y) d (v-1) l c) : jogadorDisparoAux (DisparoLaser n (a,b) E) t
                                                                       | otherwise            = (Jogador (x,y) d v l c) : jogadorDisparoAux (DisparoLaser n (a,b) E) t

-- | Remove Laser da lista de disparos
removeLaser :: [Disparo] -> [Disparo]
removeLaser []                           = []
removeLaser ((DisparoLaser a (x,y) d):t) = removeLaser t
removeLaser (h:t)                        = h : removeLaser t



-- * Ticks Canhoes

-- | Avança o Estado do jogo um Tick de tempo, considerando apenas os efeitos das balas de Canhao disparadas.
tickCanhoes :: Estado -> Estado
tickCanhoes (Estado m j dj) = Estado (alteraMapaCanhao dj m) (editaJogadorDisparo dj j) (editaListaCanhao dj m j)

-- | Altera o mapa quando algum bloco é atingido por um Canhao
alteraMapaCanhao :: [Disparo] -> Mapa -> Mapa
alteraMapaCanhao [] m  = m
alteraMapaCanhao ((DisparoCanhao a (x,y) C):t) m | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel) && (encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel) = alteraMapaCanhao t (atualizaPosicaoMatriz (x,y+1) Vazia (atualizaPosicaoMatriz (x,y) Vazia m))
                                                 | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel)                                                           = alteraMapaCanhao t (atualizaPosicaoMatriz (x,y) Vazia m)
                                                 | otherwise                                                                                                      = alteraMapaCanhao t m
alteraMapaCanhao ((DisparoCanhao a (x,y) B):t) m | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel) && (encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel) = alteraMapaCanhao t (atualizaPosicaoMatriz (x,y+1) Vazia (atualizaPosicaoMatriz (x,y) Vazia m))
                                                 | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel)                                                           = alteraMapaCanhao t (atualizaPosicaoMatriz (x,y) Vazia m)
                                                 | otherwise                                                                                                      = alteraMapaCanhao t m
alteraMapaCanhao ((DisparoCanhao a (x,y) D):t) m | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel) && (encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel) = alteraMapaCanhao t (atualizaPosicaoMatriz (x+1,y) Vazia (atualizaPosicaoMatriz (x,y) Vazia m))
                                                 | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel)                                                           = alteraMapaCanhao t (atualizaPosicaoMatriz (x,y) Vazia m)
                                                 | otherwise                                                                                                      = alteraMapaCanhao t m
alteraMapaCanhao ((DisparoCanhao a (x,y) E):t) m | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel) && (encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel) = alteraMapaCanhao t (atualizaPosicaoMatriz (x+1,y) Vazia (atualizaPosicaoMatriz (x,y) Vazia m))
                                                 | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel)                                                           = alteraMapaCanhao t (atualizaPosicaoMatriz (x,y) Vazia m)
                                                 | otherwise                                                                                                      = alteraMapaCanhao t m
alteraMapaCanhao (h:t) m  = alteraMapaCanhao t m

-- | Verifica a existencia de obstaculos e altera a posicao do disparo com o avanço do tempo
editaListaCanhao :: [Disparo] -> Mapa -> [Jogador] -> [Disparo]
editaListaCanhao [] m j = []
editaListaCanhao ((DisparoCanhao a (x,y) C):t) m j | (encontraOutraBalaCanhao (DisparoCanhao a (x,y) C) t)                                                              = editaListaCanhao (removeOutraBala (DisparoCanhao a (x,y) C) t) m j
                                                   | (encontraPosicaoJogador (x,y) j)                                                                                   = editaListaCanhao t m j
                                                   | (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel) || (encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel) = editaListaCanhao t m j
                                                   | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel) || (encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel)     = editaListaCanhao t m j
                                                   | otherwise                                                                                                          = editaTickCanhao (DisparoCanhao a (x,y) C) : editaListaCanhao t m j
editaListaCanhao ((DisparoCanhao a (x,y) B):t) m j | (encontraOutraBalaCanhao (DisparoCanhao a (x,y) B) t)                                                              = editaListaCanhao (removeOutraBala (DisparoCanhao a (x,y) B) t) m j
                                                   | (encontraPosicaoJogador (x,y) j)                                                                                   = editaListaCanhao t m j
                                                   | (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel) || (encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel) = editaListaCanhao t m j
                                                   | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel) || (encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel)     = editaListaCanhao t m j
                                                   | otherwise                                                                                                          = editaTickCanhao (DisparoCanhao a (x,y) B) : editaListaCanhao t m j
editaListaCanhao ((DisparoCanhao a (x,y) D):t) m j | (encontraOutraBalaCanhao (DisparoCanhao a (x,y) D) t)                                                              = editaListaCanhao (removeOutraBala (DisparoCanhao a (x,y) D) t) m j
                                                   | (encontraPosicaoJogador (x,y) j)                                                                                   = editaListaCanhao t m j
                                                   | (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel) || (encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel) = editaListaCanhao t m j
                                                   | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel) || (encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel)     = editaListaCanhao t m j
                                                   | otherwise                                                                                                          = editaTickCanhao (DisparoCanhao a (x,y) D) : editaListaCanhao t m j
editaListaCanhao ((DisparoCanhao a (x,y) E):t) m j | (encontraOutraBalaCanhao (DisparoCanhao a (x,y) E) t)                                                              = editaListaCanhao (removeOutraBala (DisparoCanhao a (x,y) E) t) m j
                                                   | (encontraPosicaoJogador (x,y) j)                                                                                   = editaListaCanhao t m j
                                                   | (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel) || (encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel) = editaListaCanhao t m j
                                                   | (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel) || (encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel)     = editaListaCanhao t m j
                                                   | otherwise                                                                                                          = editaTickCanhao (DisparoCanhao a (x,y) E) : editaListaCanhao t m j
editaListaCanhao (h : t) m j = h : editaListaCanhao t m j

-- | Compara um Disparo Canhao com outro Disparo Canhao ou com um Disparo Laser
encontraOutraBalaCanhao :: Disparo -> [Disparo] -> Bool
encontraOutraBalaCanhao _ [] = False
encontraOutraBalaCanhao (DisparoCanhao n (a,b) d) ((DisparoCanhao p (x,y) _):t) | x==a && y==b = True
                                                                                | otherwise    = encontraOutraBalaCanhao (DisparoCanhao n (a,b) d) t
encontraOutraBalaCanhao (DisparoCanhao n (a,b) d) ((DisparoLaser p (x,y) e):t) | e==C && x>a && y==b = True
                                                                               | e==B && x<a && y==b = True
                                                                               | e==D && x==a && y<b = True
                                                                               | e==E && x==a && y>b = True
                                                                               | otherwise           = encontraOutraBalaCanhao (DisparoCanhao n (a,b) d) t
encontraOutraBalaCanhao (DisparoCanhao n (a,b) d) (h:t) = encontraOutraBalaCanhao (DisparoCanhao n (a,b) d) t

-- | Remove uma Bala quando vai contra outra
removeOutraBala :: Disparo -> [Disparo] -> [Disparo]
removeOutraBala _ [] = []
removeOutraBala (DisparoCanhao n (a,b) d) ((DisparoCanhao p (x,y) e):t) | x==a && y==b = t
                                                                        | otherwise    = (DisparoCanhao p (x,y) e): removeOutraBala (DisparoCanhao n (a,b) d) t

-- | Verifica se um jogador se encontra numa dada posicao
encontraPosicaoJogador :: Posicao -> [Jogador] -> Bool
encontraPosicaoJogador (a,b) [] = False
encontraPosicaoJogador (a,b) ((Jogador (x,y) d v _ _):t) | (a==x && b==y) || (a==x-1) && b==y = True
                                                         | otherwise = encontraPosicaoJogador (a,b) t

-- | Altera a posicao do disparo com o avanço do tempo
editaTickCanhao :: Disparo -> Disparo
editaTickCanhao (DisparoCanhao a (x,y) C) = (DisparoCanhao a (x-1,y) C)
editaTickCanhao (DisparoCanhao a (x,y) B) = (DisparoCanhao a (x+1,y) B)
editaTickCanhao (DisparoCanhao a (x,y) D) = (DisparoCanhao a (x,y+1) D)
editaTickCanhao (DisparoCanhao a (x,y) E) = (DisparoCanhao a (x,y-1) E)



-- * Ticks Choques

-- | Avança o Estado do jogo um Tick de tempo, considerando apenas os efeitos dos campos de Choque disparados.
tickChoques :: Estado -> Estado
tickChoques (Estado m j dj) = Estado m j (editaListaChoque dj)

-- | Avança os Ticks do Choque
editaListaChoque :: [Disparo] -> [Disparo]
editaListaChoque [] = []
editaListaChoque ((DisparoChoque x 0):t)  = editaListaChoque t
editaListaChoque ((DisparoChoque x ti):t) = (DisparoChoque x (ti-1)) : editaListaChoque t
editaListaChoque (h : t)                  = h : editaListaChoque t
