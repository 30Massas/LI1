-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g152 where

import           Data.List
import           LI11819
import           Tarefa0_2018li1g152
import           Tarefa1_2018li1g152

-- Module : Tarefa2_2018li1g152
-- Description : Criação da função jogada que movimenta o jogador e dispara os canhões, ticks e laseres
-- Copyright : Luís Vieira <a89601@alunos.uminho.pt>
--             Paulo Sousa <a89465@alunos.uminho.pt>
--

-- * Testes

-- | Testes da Tarefa 2.
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0 , Dispara Choque, (Estado (mapaInicial (6,6)) [Jogador (1,1) C 5 3 3, Jogador (3,3) B 5 2 2] [DisparoChoque 0 5])),
            (1 , Movimenta B, (Estado (mapaInicial (6,6)) [Jogador (1,1) C 5 3 3, Jogador (3,3) B 5 2 2] [])),
            (0 , Dispara Laser, (Estado (mapaInicial (6,6)) [Jogador (2,2) C 5 3 2, Jogador (1,3) D 4 2 2] [] )),
            (2 , Movimenta D, (Estado (mapaInicial (8,8)) [Jogador (1,4) C 5 1 1, Jogador (3,1) E 2 3 3, Jogador (3,5) B 4 3 1] [] )),
            (0 , Dispara Canhao, (Estado (mapaInicial (7,5)) [Jogador (1,1) B 5 3 3, Jogador (3,1) C 5 2 2] [])),
            (1 , Dispara Laser, (Estado (mapaInicial (7,7)) [Jogador (2,2) C 5 3 3, Jogador (3,3) E 4 2 2] [] )),
            (0 , Movimenta C, (Estado (mapaInicial (10,10)) [Jogador (5,3) C 1 2 3, Jogador (3,3) B 1 2 3] [])),
            (0 , Movimenta C, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) B 1 2 3] [])),
            (1 , Movimenta C , (Estado (mapaInicial (10,10)) [Jogador (2,3) C 1 1 1, Jogador (2,7) C 1 1 1] [DisparoChoque 0 1])),
            (1 , Movimenta E , (Estado (mapaInicial (10,10)) [Jogador (2,3) C 1 1 1, Jogador (4,5) D 1 1 1] [DisparoChoque 0 1])),
            (1 , Movimenta E , (Estado (mapaInicial (10,10)) [Jogador (2,3) C 1 1 1, Jogador (1,1) C 1 1 1] [DisparoChoque 0 1])),
            (1 , Movimenta C , (Estado (mapaInicial (10,10)) [Jogador (1,1) C 1 1 1, Jogador (3,1) C 1 1 1] []))]



-- * Função Principal

-- | Efetua uma jogada.
jogada :: Int -> Jogada -> Estado -> Estado
jogada x (Movimenta C) (Estado m j dj) = Estado m (atualizaListaJogador x (editaJogador  C (Estado m j dj) (encontraJogador x j)) j) dj
jogada x (Movimenta B) (Estado m j dj) = Estado m (atualizaListaJogador x (editaJogador  B (Estado m j dj) (encontraJogador x j)) j) dj
jogada x (Movimenta D) (Estado m j dj) = Estado m (atualizaListaJogador x (editaJogador  D (Estado m j dj) (encontraJogador x j)) j) dj
jogada x (Movimenta E) (Estado m j dj) = Estado m (atualizaListaJogador x (editaJogador  E (Estado m j dj) (encontraJogador x j)) j) dj
jogada x (Dispara Canhao) (Estado m j dj) = Estado m j (dj ++ (editaDisparo x Canhao (encontraJogador x j)))
jogada x (Dispara Laser) (Estado m j dj) = Estado m (atualizaListaJogador x (editaJogadorLasers (encontraJogador x j)) j) (dj ++ (editaDisparo x Laser (encontraJogador x j)))
jogada x (Dispara Choque) (Estado m j dj) = Estado m (atualizaListaJogador x (editaJogadorChoques (encontraJogador x j)) j) (dj ++ (editaDisparoChoque x 5 (encontraJogador x j)))



-- * Funções auxiliares da jogada Movimenta

-- | Procura o "Jogador" na lista de jogadores e "remove-o" da lista para poder editar o seu estado
encontraJogador :: Int -> [Jogador] -> Jogador
encontraJogador 0 (h:t) = h
encontraJogador x (h:t) = encontraJogador (x-1) t

-- | Atualiza a posicao do "Jogador" retirado da lista consoante a direcao
editaJogador :: Direcao -> Estado -> Jogador -> Jogador
editaJogador C (Estado m j dj) (Jogador (x,y) d v l c) | d == C && v==0 = Jogador (x,y) d v l c
                                                       | d == C && (eBordaMatriz (x-1,y) m) = Jogador (x,y) d v l c
                                                       | d == C && (encontraPosicaoMatriz (x-1,y) m == Bloco Indestrutivel) || (encontraPosicaoMatriz (x-1,y+1) m == Bloco Indestrutivel) = Jogador (x,y) d v l c
                                                       | d == C && (encontraPosicaoMatriz (x-1,y) m == Bloco Destrutivel) || (encontraPosicaoMatriz (x-1,y+1) m == Bloco Destrutivel) = Jogador (x,y) d v l c
                                                       | d == C && (temOutroTanque (Jogador (x,y) d v l c) C j) = Jogador (x,y) d v l c
                                                       | d == C && (temChoque (Jogador (x,y) d v l c) j dj) = (Jogador (x,y) d v l c)
                                                       | d == C = Jogador (x-1,y) d v l c
                                                       | otherwise = Jogador (x,y) C v l c
editaJogador B (Estado m j dj) (Jogador (x,y) d v l c) | d == B && v==0 = Jogador (x,y) d v l c
                                                       | d == B && (eBordaMatriz (x+2,y) m) = Jogador (x,y) d v l c
                                                       | d == B && (encontraPosicaoMatriz (x+2,y) m == Bloco Indestrutivel) || (encontraPosicaoMatriz (x+2,y+1) m == Bloco Indestrutivel) = Jogador (x,y) d v l c
                                                       | d == B && (encontraPosicaoMatriz (x+2,y) m == Bloco Destrutivel) || (encontraPosicaoMatriz (x+2,y+1) m == Bloco Destrutivel) = Jogador (x,y) d v l c
                                                       | d == B && (temOutroTanque (Jogador (x,y) d v l c) B j) == True = Jogador (x,y) d v l c
                                                       | d == B && (temChoque (Jogador (x,y) d v l c) j dj) = (Jogador (x,y) d v l c)
                                                       | d == B = Jogador (x+1,y) d v l c
                                                       | otherwise = Jogador (x,y) B v l c
editaJogador D (Estado m j dj) (Jogador (x,y) d v l c) | d == D && v==0 = Jogador (x,y) d v l c
                                                       | d == D && (eBordaMatriz (x,y+2) m) = Jogador (x,y) d v l c
                                                       | d == D && (encontraPosicaoMatriz (x,y+2) m == Bloco Indestrutivel) || (encontraPosicaoMatriz (x+1,y+2) m == Bloco Indestrutivel) = Jogador (x,y) d v l c
                                                       | d == D && (encontraPosicaoMatriz (x,y+2) m == Bloco Destrutivel) || (encontraPosicaoMatriz (x+1,y+2) m == Bloco Destrutivel) = Jogador (x,y) d v l c
                                                       | d == D && (temOutroTanque (Jogador (x,y) d v l c) D j) = Jogador (x,y) d v l c
                                                       | d == D && (temChoque (Jogador (x,y) d v l c) j dj) = (Jogador (x,y) d v l c)
                                                       | d == D = Jogador (x,y+1) d v l c
                                                       | otherwise = Jogador (x,y) D v l c
editaJogador E (Estado m j dj) (Jogador (x,y) d v l c) | d == E && v==0 = Jogador (x,y) d v l c
                                                       | d == E && (eBordaMatriz (x,y-1) m) = Jogador (x,y) d v l c
                                                       | d == E && (encontraPosicaoMatriz (x,y-1) m == Bloco Indestrutivel) || (encontraPosicaoMatriz (x+1,y-1) m == Bloco Indestrutivel) = Jogador (x,y) d v l c
                                                       | d == E && (encontraPosicaoMatriz (x,y-1) m == Bloco Destrutivel) || (encontraPosicaoMatriz (x+1,y-1) m == Bloco Destrutivel) = Jogador (x,y) d v l c
                                                       | d == E && (temOutroTanque (Jogador (x,y) d v l c) E j) = Jogador (x,y) d v l c
                                                       | d == E && (temChoque (Jogador (x,y) d v l c) j dj) = (Jogador (x,y) d v l c)
                                                       | d == E = Jogador (x,y-1) d v l c
                                                       | otherwise = Jogador (x,y) E v l c

-- | Dado um Jogador, uma lista de Jogadores e uma lista de Disparos, diz se o Jogador está sobre o efeito de Choque ou não
temChoque :: Jogador -> [Jogador] -> [Disparo] -> Bool
temChoque (Jogador (x,y) d v l c) j dj = (existeDisparo dj) && (jogadorEmChoque (Jogador (x,y) d v l c) (encontraJogador (numeroJogadorChoque (encontraChoque dj)) j))

-- | Dado um DisparoChoque devlove o numero do jogador
numeroJogadorChoque :: Disparo -> Int
numeroJogadorChoque (DisparoChoque x t) = x

-- | Dado uma lista de Disparos devolve o DisparoChoque
encontraChoque :: [Disparo] -> Disparo
encontraChoque ((DisparoChoque x ti):t) = DisparoChoque x ti
encontraChoque (h:t)                    = encontraChoque t

-- | Dado o Jogador que dispara o choque e dado outro Jogador, verifica se este ultimo está em zona de choque
jogadorEmChoque :: Jogador -> Jogador -> Bool
jogadorEmChoque (Jogador (x,y) d v l c) (Jogador (a,b) _ _ _ _) | (x <= a+3) && (x >= a-3) && (y <= a+3) && (y >= a-3) = True
                                                                | otherwise = False

-- | Dado uma lista de Disparos, diz se tem DisparoChoque na lista
existeDisparo :: [Disparo] -> Bool
existeDisparo []                       = False
existeDisparo ((DisparoChoque x ti):t) = True
existeDisparo (h:t)                    = existeDisparo t

-- | Verifica se existe outro "Jogador" na posição para a qual se pretende mover
temOutroTanque :: Jogador -> Direcao -> [Jogador] -> Bool
temOutroTanque (Jogador (x,y) d v l c) C [] = False
temOutroTanque (Jogador (x,y) d v l c) C ((Jogador (a,b) _ e _ _):t) | e == 0 = False
                                                                     | d /= C = False
                                                                     | d == C && ((x-2)==a && y==b) = True
                                                                     | d == C && ((x-2)==a && y==(b-1)) = True
                                                                     | d == C && ((x-2)==a && y==(b+1)) = True
                                                                     | d == C && ((x-1)==a && y==b) = True
                                                                     | d == C && ((x-1)==a && y==(b-1)) = True
                                                                     | d == C && ((x-1)==a && y==(b+1)) = True
                                                                     | otherwise = temOutroTanque (Jogador (x,y) d v l c) C t
temOutroTanque (Jogador (x,y) d v l c) B [] = False
temOutroTanque (Jogador (x,y) d v l c) B ((Jogador (a,b) _ e _ _):t) | e == 0 = False
                                                                     | d /= B = False
                                                                     | d == B && ((x+2)==a && y==b) = True
                                                                     | d == B && ((x+2)==a && y==(b-1)) = True
                                                                     | d == B && ((x+2)==a && y==(b+1)) = True
                                                                     | d == B && ((x+1)==a && y==b) = True
                                                                     | d == B && ((x+1)==a && y==(b-1)) = True
                                                                     | d == B && ((x+1)==a && y==(b+1)) = True
                                                                     | otherwise = temOutroTanque (Jogador (x,y) d v l c) B t
temOutroTanque (Jogador (x,y) d v l c) D [] = False
temOutroTanque (Jogador (x,y) d v l c) D ((Jogador (a,b) _ e _ _):t) | e == 0 = False
                                                                     | d /= D = False
                                                                     | d == D && (x==a && (y+2)==b) = True
                                                                     | d == D && (x==(a-1) && (y+2)==b) = True
                                                                     | d == D && (x==(a+1) && (y+2)==b) = True
                                                                     | d == D && (x==a && (y+1)==b) = True
                                                                     | d == D && (x==(a-1) && (y+1)==b) = True
                                                                     | d == D && (x==(a+1) && (y+1)==b) = True
                                                                     | otherwise = temOutroTanque (Jogador (x,y) d v l c) D t
temOutroTanque (Jogador (x,y) d v l c) E [] = False
temOutroTanque (Jogador (x,y) d v l c) E ((Jogador (a,b) _ e _ _):t) | e == 0 = False
                                                                     | d /= E = False
                                                                     | d == E && (x==a && (y-2)==b) = True
                                                                     | d == E && (x==(a-1) && (y-2)==b) = True
                                                                     | d == E && (x==(a+1) && (y-2)==b) = True
                                                                     | d == E && (x==a && (y-1)==b) = True
                                                                     | d == E && (x==(a-1) && (y-1)==b) = True
                                                                     | d == E && (x==(a+1) && (y-1)==b) = True
                                                                     | otherwise = temOutroTanque (Jogador (x,y) d v l c) E t

-- | Reintroduz o "Jogador" na lista após ter o estado modificado
atualizaListaJogador :: Int -> Jogador -> [Jogador] -> [Jogador]
atualizaListaJogador _ j [] = [j]
atualizaListaJogador 0 j (h : t) = j : t
atualizaListaJogador x j (h:t)   = h : atualizaListaJogador (x-1) j t



-- * Funções auxiliares da jogada Disparo

-- | Realiza a ação de disparo consoante o estado do jogador
editaDisparo :: Int -> Arma -> Jogador -> [Disparo]
editaDisparo a Canhao (Jogador (x,y) C v l c) | v==0 = []
                                              | otherwise = [DisparoCanhao a (x-1,y) C]
editaDisparo a Canhao (Jogador (x,y) B v l c) | v==0 = []
                                              | otherwise = [DisparoCanhao a (x+1,y) B]
editaDisparo a Canhao (Jogador (x,y) D v l c) | v==0 = []
                                              | otherwise = [DisparoCanhao a (x,y+1) D]
editaDisparo a Canhao (Jogador (x,y) E v l c) | v==0 = []
                                              | otherwise = [DisparoCanhao a (x,y-1) E]
editaDisparo a Laser (Jogador (x,y) C v l c) | v==0 = []
                                             | l <= 0 = []
                                             | otherwise = [DisparoLaser a (x-1,y) C]
editaDisparo a Laser (Jogador (x,y) B v l c) | v==0 = []
                                             | l <= 0 = []
                                             | otherwise = [DisparoLaser a (x+1,y) B]
editaDisparo a Laser (Jogador (x,y) D v l c) | v==0 = []
                                             | l <= 0 = []
                                             | otherwise = [DisparoLaser a (x,y+1) D]
editaDisparo a Laser (Jogador (x,y) E v l c) | v==0 = []
                                             | l <= 0 = []
                                             | otherwise = [DisparoLaser a (x,y-1) E]

-- | Realiza a ação de disparo consoante o jogador
editaDisparoChoque :: Int -> Ticks -> Jogador -> [Disparo]
editaDisparoChoque x t (Jogador p d v l c) | v==0 = []
                                           | c <= 0 = []
                                           | otherwise = [DisparoChoque x t]

-- | Atualiza a informação acerca da quantidade de lasers disponiveis
editaJogadorLasers :: Jogador -> Jogador
editaJogadorLasers (Jogador p d v l c) = Jogador p d v (l-1) c

-- | Atualiza a informação acerca da quantidade de choques disponiveis
editaJogadorChoques :: Jogador -> Jogador
editaJogadorChoques (Jogador p d v l c) = Jogador p d v l (c-1)
