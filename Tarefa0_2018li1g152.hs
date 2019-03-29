-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g152 where

import           Data.List (transpose)
import           LI11819
-- * Funções não-recursivas.

-- | Um 'Vetor' é uma 'Posicao' em relação à origem.
type Vetor = Posicao
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>

-- ** Funções sobre vetores

-- *** Funções gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (x1,y1)(x2,y2) = (x1 + x2, y1 + y2)

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (x1,y1)(x2,y2) = (x1 - x2, y1 - y2)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Int -> Vetor -> Vetor
multiplicaVetor m (x,y) =  (m*x, m*y)

-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distância à origem).
--
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
rodaVetor :: Vetor -> Vetor
rodaVetor (x,y) = (y,-x)

-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
--
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
inverteVetorH :: Vetor -> Vetor
inverteVetorH (x,y) = (x,-y)

-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
--
-- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
inverteVetorV :: Vetor -> Vetor
inverteVetorV (x,y) = (-x,y)

-- *** Funções do trabalho sobre 'Vetor'es.

-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor C = (-1,0)
direcaoParaVetor B = (1,0)
direcaoParaVetor D = (0,1)
direcaoParaVetor E = (0,-1)

-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido _ [] = False
eIndiceListaValido c a  = c>=0 && c<length a
-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz a | null a || null (head a) = (0,0)
                 | otherwise = (length a,length (head a))

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida (x1,x2) [] = False
ePosicaoMatrizValida (x1,x2) a = x1>=0 && x1<=length a && x2>=0 && x2<length (head a)

-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -> Matriz a -> Bool
eBordaMatriz (x,y) a | x == 0 = True
                     | y == 0 = True
                     | x == length a - 1  = True
                     | y == length (head a) - 1 = True
                     | otherwise = False

-- *** Funções do trabalho sobre matrizes.

-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
--
-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -> Matriz Bool
tetrominoParaMatriz I = replicate 4 [False,True,False,False]
tetrominoParaMatriz J = [
       [False,True,False],
       [False,True,False],
       [True,True,False]
 ]
tetrominoParaMatriz L = [
       [False,True,False],
       [False,True,False],
       [False,True,True]
 ]
tetrominoParaMatriz O = replicate 2 [True,True]
tetrominoParaMatriz S = [
       [False,True,True],
       [True,True,False],
       [False,False,False]
 ]
tetrominoParaMatriz T = [
       [False,False,False],
       [True,True,True],
       [False,True,False]
 ]
tetrominoParaMatriz Z = [
       [True,True,False],
       [False,True,True],
       [False,False,False]
 ]

-- * Funções recursivas.

-- ** Funções sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 (h:t) = h
encontraIndiceLista x (h:t) = encontraIndiceLista (x-1) t

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista x c a | eIndiceListaValido x a = atualizarElemento x c a
                          | otherwise = a

-- | Substitui o  elemento na lista no índice correspondente
atualizarElemento :: Int -> a -> [a] -> [a]
atualizarElemento _ _ [] = []
atualizarElemento x c (h:t) | x == 0 = c:t
                            | otherwise = h : atualizarElemento (x-1) c t



-- ** Funções sobre matrizes.

-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
--
-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>
rodaMatriz :: Matriz a -> Matriz a
rodaMatriz l = transpose (reverse l)

-- | Inverte uma 'Matriz' na horizontal.
--
-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
inverteMatrizH :: Matriz a -> Matriz a
inverteMatrizH []    = []
inverteMatrizH (h:t) = reverse h : inverteMatrizH t

-- | Inverte uma 'Matriz' na vertical.
--
-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
inverteMatrizV :: Matriz a -> Matriz a
inverteMatrizV [] = []
inverteMatrizV a  = reverse a

-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -> a -> Matriz a
criaMatriz (0,y) _ = []
criaMatriz (x,y) c = criaLinhaMatriz y c : criaMatriz (x-1,y) c

-- | Função que cria uma Linha de Uma matriz com y elementos c
criaLinhaMatriz :: Int -> a -> [a]
criaLinhaMatriz 0 _ = []
criaLinhaMatriz y c = c : criaLinhaMatriz (y-1) c

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz (x,y) l = encontraIndiceLista y (encontraIndiceLista x l)

-- | Modifica um elemento numa dada 'Posicao'
--Posicao -> (Int,Int)
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.

atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz _ _ [] = []
atualizaPosicaoMatriz (x,y) c a | ePosicaoMatrizValida (x,y) a = atualizarPosicao (x,y) c a
                                | otherwise = a

-- | Corre a matriz até encontrar a posição desejada, onde substitui o antigo elemento por c
atualizarPosicao :: Posicao -> a -> Matriz a -> Matriz a
atualizarPosicao (0,y) c (h:t) = atualizaIndiceLista y c h : t
atualizarPosicao _ _ []        = []
atualizarPosicao (x,y) c (h:t) = h : atualizaPosicaoMatriz (x-1,y) c t
