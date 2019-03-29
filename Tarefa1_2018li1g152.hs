-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g152 where

import           Data.List           (transpose)
import           LI11819
import           Tarefa0_2018li1g152

-- Module : Tarefa1_2018li1g152
-- Description : Criação do editor de mapas
-- Copyright : Luís Vieira <a89601@alunos.uminho.pt>
--             Paulo Sousa <a89465@alunos.uminho.pt>
--

-- * Testes

-- | Testes da Tarefa 1
testesT1 :: [Instrucoes]
testesT1 = [
            [Move D, Roda, MudaTetromino, MudaParede, Desenha],
            [Move C,Move E, Move E, MudaTetromino, MudaTetromino, MudaTetromino, Roda, Roda, Desenha],
            [Move C, Move C, Move D, Move E, Move C, MudaParede, MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino, Roda, Desenha],
            [Move B, Move B, Desenha, Move D, Move D, Roda, Desenha, MudaTetromino, MudaParede, Move D, Desenha],
            [Roda, Desenha, MudaTetromino, Roda, Roda, Move E, Move E, Desenha, MudaParede, MudaTetromino, MudaTetromino, Roda, Desenha],
            [Move E, Move E, Move B, Move B, MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino, MudaTetromino, Desenha],
            [Move B,Move B,Move B,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move C,Move C,Move C,Move B,Move B,Move B,Desenha,Move E,Move E,Move E,Move C,Desenha,Move E,Move E,Move E,Desenha,MudaTetromino,MudaTetromino,Desenha,Move E,Move E,Move E,Move C,MudaParede,Desenha,Move B,Move B,Desenha,Move B,Move D,Desenha,Move D,Move C,Desenha,Move C,Move C,Desenha,Move D,Move D,Move D,Desenha,Move D,Move D,Desenha]
            ]

-- * Funções Principais

-- | Função que aplica Instrucoes num Editor
instrucao :: Instrucao -> Editor -> Editor

-- Move o cursor numa dada direcao
instrucao (Move C) (Editor (x,y) d t p m) = Editor (x-1,y) d t p m
instrucao (Move B) (Editor (x,y) d t p m) = Editor (x+1,y) d t p m
instrucao (Move D) (Editor (x,y) d t p m) = Editor (x,y+1) d t p m
instrucao (Move E) (Editor (x,y) d t p m) = Editor (x,y-1) d t p m

-- Avança para o tetronimo seguinte
instrucao MudaTetromino (Editor c d I p m) = Editor c d J p m
instrucao MudaTetromino (Editor c d J p m) = Editor c d L p m
instrucao MudaTetromino (Editor c d L p m) = Editor c d O p m
instrucao MudaTetromino (Editor c d O p m) = Editor c d S p m
instrucao MudaTetromino (Editor c d S p m) = Editor c d T p m
instrucao MudaTetromino (Editor c d T p m) = Editor c d Z p m
instrucao MudaTetromino (Editor c d Z p m) = Editor c d I p m

-- Roda o tetronimo atual segundo uma direcao
instrucao Roda (Editor c C t p m) = Editor c D t p m
instrucao Roda (Editor c D t p m) = Editor c B t p m
instrucao Roda (Editor c B t p m) = Editor c E t p m
instrucao Roda (Editor c E t p m) = Editor c C t p m

-- Muda o tipo de parede
instrucao MudaParede (Editor c d t Destrutivel m) = Editor c d t Indestrutivel m
instrucao MudaParede (Editor c d t Indestrutivel m) = Editor c d t Destrutivel m

-- Altera o mapa sem modificar parâmetros
instrucao Desenha (Editor (x,y) d t p c) = Editor (x,y) d t p (desenhar (x,y) d t p c (0,0))

                                            where desenhar (x,y) d t p c (a,b) | a == length (tetrominoParaMatriz t) = c -- Caso a seja igual ao comprimento do tetromino, devolve se o mapa
                                                                               | ePosicaoMatrizValida (a,b) (tetrominoParaMatriz t) = auxDesenha (x,y) d t p c (a,b) -- Verifica se a posiçao (a,b) é valida para o tetromino dado
                                                                               | otherwise = desenhar (x+1,y-b) d t p c (a+1,0) -- Se a posicao (a,b) não for valida avançamos para a proxima linha do tetromino


                                                                                where auxDesenha (x,y) C t p c (a,b) | encontraPosicaoMatriz (a,b) (tetrominoParaMatriz t) = desenhar (x,y+1) d t p (atualizaPosicaoMatriz (x,y) (Bloco p) c) (a,b+1) -- Se a posiçao (a,b) do tetromino dado for True, atualiza se a posicao no mapa com o tipo de parede dado
                                                                                                                     | otherwise = desenhar (x,y+1) d t p c (a,b+1) -- Se a posicao (a,b) do tetromino dado for False, nao se atualiza a posicao e verifica para a posicao seguinte
                                                                                      auxDesenha (x,y) D t p c (a,b) | encontraPosicaoMatriz (a,b) (rodaMatriz (tetrominoParaMatriz t)) = desenhar (x,y+1) d t p (atualizaPosicaoMatriz (x,y) (Bloco p) c) (a,b+1) -- Se a posiçao (a,b) do tetromino dado for True, atualiza se a posicao no mapa com o tipo de parede dado
                                                                                                                     | otherwise = desenhar (x,y+1) d t p c (a,b+1) -- Se a posicao (a,b) do tetromino dado for False, nao se atualiza a posicao e verifica para a posicao seguinte
                                                                                      auxDesenha (x,y) B t p c (a,b) | encontraPosicaoMatriz (a,b) (rodaMatriz (rodaMatriz (tetrominoParaMatriz t))) = desenhar (x,y+1) d t p (atualizaPosicaoMatriz (x,y) (Bloco p) c) (a,b+1) -- Se a posiçao (a,b) do tetromino dado for True, atualiza se a posicao no mapa com o tipo de parede dado
                                                                                                                     | otherwise = desenhar (x,y+1) d t p c (a,b+1) -- Se a posicao (a,b) do tetromino dado for False, nao se atualiza a posicao e verifica para a posicao seguinte
                                                                                      auxDesenha (x,y) E t p c (a,b) | encontraPosicaoMatriz (a,b) (rodaMatriz (rodaMatriz (rodaMatriz (tetrominoParaMatriz t)))) = desenhar (x,y+1) d t p (atualizaPosicaoMatriz (x,y) (Bloco p) c) (a,b+1) -- Se a posiçao (a,b) do tetromino dado for True, atualiza se a posicao no mapa com o tipo de parede dado
                                                                                                                     | otherwise = desenhar (x,y+1) d t p c (a,b+1) -- Se a posicao (a,b) do tetromino dado for False, nao se atualiza a posicao e verifica para a posicao seguinte

-- | Função que aplica uma sequência de Instrucoes num Editor
instrucoes :: Instrucoes -> Editor -> Editor
instrucoes [] (Editor c d t p m)    = Editor c d t p m
instrucoes (h:i) (Editor c d t p m) = instrucoes i (instrucao h (Editor c d t p m))

-- | Cria um Mapa inicial com Paredes nas bordas e o resto vazio.
mapaInicial :: Dimensao -> Mapa
mapaInicial (0,_) = []
mapaInicial (_,0) = []
mapaInicial (x,y) = criaLinha (Bloco Indestrutivel) y : restoMatriz (x-1,y)

-- | Produz todas as linhas do mapa exceto a primeira
restoMatriz :: Dimensao -> Mapa
restoMatriz (0,y) = []
restoMatriz (1,y) = criaLinha (Bloco Indestrutivel) y : []
restoMatriz (x,y) = criaMapa (x,y) (1,1) : restoMatriz (x-1,y)

-- | Cria uma linha com o primeiro e o ultimo elemento Blocos Indestrutiveis e os restantes vazios
criaMapa :: Dimensao -> Dimensao -> [Peca]
criaMapa (x,y) (a,b) | b==1 = (Bloco Indestrutivel) : criaMapa (x,y) (a,b+1)
                     | b<y = Vazia : criaMapa (x,y) (a,b+1)
                     | b==y = (Bloco Indestrutivel) : criaMapa (x,y) (a,b+1)
                     | b>y = []

-- | Cria uma linha com apenas Blocos Indestrutíveis
criaLinha :: Peca -> Int -> [Peca]
criaLinha a 0 = []
criaLinha a b = a : criaLinha a (b-1)

-- | Cria um Editor inicial.
editorInicial :: Instrucoes -> Editor
editorInicial x = Editor (posicaoInicial x) C I Indestrutivel (mapaInicial(dimensaoInicial x))

-- | Constrói um Mapa dada uma sequência de 'Instrucoes'.
constroi :: Instrucoes -> Mapa
constroi is = editorMapa (instrucoes is (editorInicial is))

-- | Dado um Editor devolve apenas o Mapa
editorMapa :: Editor -> Mapa
editorMapa (Editor a b c d e) = e
