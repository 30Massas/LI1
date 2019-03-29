-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2018li1g152 where

import           LI11819
import           Tarefa0_2018li1g152
import           Tarefa1_2018li1g152
import           Tarefa2_2018li1g152

-- Module      : Tarefa3_2018li1g152
-- Description : Compressão e um Estado numa String, e descompressão da String no mesmo Estado
-- Copyright   : Luís Vieira <a89601@alunos.di.uminho.pt>
--               Paulo Sousa <a89465@alunos.di.uminho.pt>

-- * Introdução

-- $intro
-- * Nesta tarefa, fomos propostos a criar duas funções: uma que comprime um Estado do Jogo numa String, de forma a obter a maior taxa de compressão possível,
-- e outra que descomprimisse e obtivesse o mesmo Estado do Jogo.

-- * Objetivos

-- $objs
-- * Quando iniciámos esta tarefa, começámos por discutir como iríamos comprimir o estado do jogo numa String.
-- Começámos por tentar fazer tudo na mesma função, ou seja, iríamos, de acordo com o Estado do Jogo apresentado, definir carateres para cada elemento diferente e assim tentar abranger todos.
-- No entanto, reparámos que seria pouco eficiente dessa maneira e decidimos dividir a compressão do Estado em três funções: uma para o mapa, outra para a lista de jogadores e respetivas informações
-- e , por fim, outra para a lista de disparos ocorrentes. Na função que comprime o mapa acabámos por utilizar uma outra função que comprime linha a linha, o mapa que nos é dado. Assim distinguimos os três tipos de peças possíveis da seguinte forma:
-- A peça Vazia seria representada pelo carater 'v', a peça Bloco Destrutível seria representada pelo carater 'd' e a peça Bloco Indestrutível seria representada pelo carater 'i'.
-- * A função principal que comprime o mapa, acaba por separar cada linha pelo carater '/' de forma a ser possível a distinção das diferentes linhas. Quando o mapa fosse vazio apresentaria o carater '%'.
-- Já na função que comprime a lista de jogadores e as suas informações, utilizando a função pré definida __show__, comprimimos numa String as informações de cada Jogador, pela ordem que aparecem na lista.
-- Assim, as coordenadas de um jogador separámos pelo carater '!' para prevenir casos em que as mesmas possuam dois algarismos, facilitando a sua leitura. De seguida separámos as vidas do jogador
-- do número de lasers pelo carater '?', e separámos o número de lasers do número de choques pelo carater ','. Na função principal que comprime a lista de jogadores, separámos cada jogador pelo carater '#'.
-- Quando a lista de jogadores se encontra vazia, a função representa-a pelo carater '€'.
-- A última função, destinada à compressão da lista de disparos, representa um disparo de canhão pelo carater 'c', um disparo laser pelo carater 'l' e um disparo choque pelo carater 'h'.
-- Quando se encontra a comprimir quer disparos canhão, quer disparos laser, e recorrendo novamente ao uso da função show, separa as coordenadas de disparo pelo carater '!' de forma a ter em consideração
-- casos em que estas possam ser de dois algarismos. Por outro lado, quando se encontra a comprimir um disparo choque, separa o identificador do jogador dos ticks de choque pelo carater '?'
-- Feita a compressão de cada um destes componentes em separado, a função principal separa cada String gerada pelas funções auxiliares pelo carater '*'.
-- Da mesma forma que ocorreu com a compressão, pensámos inicialmente para a descompressão juntar tudo na mesma função, acabando por tomar a mesma decisão e dividir em três funções diferentes.
-- * Na descompressão, de forma geral, cada função pega na string que é referente ao mapa, lista de jogadores e lista de disparos e faz corresponder a cada carater o seu elemento respetivo,
-- usando a função pré definida __read__. A função que descomprime o mapa, separa cada linha e de seguida faz corresponder ao carater designado na função de compressão, o elemento pŕoprio.
-- O mesmo ocorre nas funções que descomprimem a lista de jogadores e a lista de disparos. Inicialmente separam cada jogador e cada disparo dos restantes, e fazem corresponder ao carater designado na
-- função compressão o elemento previamente destinado.
-- A função principal de descompressão, recorrendo às três auxiliares, devolve assim o Estado inicialmente convertido para uma String de somente carateres.

-- * Conclusão

-- $conc
-- * Idealizámos e realizámos vários testes para podermos testar diferentes componentes das funções comprimir e descomprimir como: mapas com variadas dimensões, vários disparos ao mesmo tempo,
-- Estados sem jogadores e sem disparos.
-- Verificámos, então, que obtivemos uma taxa de compressão superior a 90% e que a função de descompressão retornava com sucesso o Estado que era comprimido.
-- * Concluímos então que o pretendido para esta Tarefa foi sucedido. No entanto tentámos melhorar a taxa de compressão ao fazer corresponder um carater para casos em que se repetissem por exemplo
-- várias peças Vazia seguidas. No entanto não fomos capazes de realizar o que pretendíamos e acabámos por deixar as funções de compressão e descompressão como se encontravam antes, obtendo como
-- resultado final uma taxa de compressão superior a 90%.



-- * Testes

-- | Testes da Tarefa 3
testesT3 :: [Estado]
testesT3 = [(Estado (mapaInicial (10,10)) [Jogador (3,3) D 3 3 3, Jogador (6,6) B 1 2 1] [DisparoChoque 1 5]),
            (Estado (mapaInicial (8,7)) [Jogador (5,4) E 5 10 4, Jogador (2,3) D 1 5 4] [DisparoCanhao 0 (5,3) E, DisparoLaser 1 (2,4) B]),
            (Estado (mapaInicial (15,15)) [Jogador (1,1) D 2 3 1, Jogador (1,12) B 5 4 10, Jogador (12,12) E 10 4 3, Jogador (12,1) C 1 2 3] [DisparoLaser 0 (1,2) D, DisparoCanhao 1 (2,12) B, DisparoChoque 2 5, DisparoLaser 3 (11,1) C]),
            (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [Jogador (1,8) D 1 2 3, Jogador (7,1) C 12 1 4] [DisparoCanhao 1 (2,1) D, DisparoLaser 0 (3,1) E, DisparoChoque 1 5]),
            (Estado (mapaInicial (12,12)) [Jogador (1,1) E 1 11 2] [DisparoLaser 0 (1,1) D, DisparoLaser 0 (1,2) D, DisparoLaser 0 (1,3) D, DisparoLaser 0 (1,4) D, DisparoLaser 0 (1,5) D, DisparoLaser 0 (1,6) D, DisparoLaser 0 (2,1) D, DisparoLaser 0 (2,2) D, DisparoLaser 0 (2,3) D, DisparoLaser 0 (2,4) D, DisparoLaser 0 (2,5) D, DisparoLaser 0 (2,6) D]),
            (Estado (mapaInicial (6,6)) [(Jogador (1,1) D 1 1 1),(Jogador (2,2) C 5 0 0)] [(DisparoChoque 1 0),(DisparoCanhao 1 (1,1) E)]),
            (Estado (mapaInicial (6,6)) [(Jogador (1,1) D 1 1 1),(Jogador (2,2) C 5 0 0)] [(DisparoChoque 1 5)]),
            (Estado (mapaInicial (12,20)) [Jogador (2,2) E 11 13 15, Jogador (5,5) B 11 12 13] [DisparoChoque 0 5, DisparoLaser 0 (2,4) D, DisparoCanhao 1 (1,1) C]),
            (Estado (mapaInicial (6,6)) [(Jogador (1,1) D 1 1 1),(Jogador (2,2) C 5 0 0)] []),
            (Estado (mapaInicial (6,6)) [] [])
            ]



-- * Comprime

-- | Comprime um Estado do Jogo numa String
comprime :: Estado -> String
comprime (Estado m j dj) = comprimir m j dj

-- | Comprime o Estado juntando as Strings do Mapa, [Jogador] e [Disparo], separadas por um '*'
comprimir :: Mapa -> [Jogador] -> [Disparo] -> String
comprimir m j dj = (comprimeMapa m) ++ "*" ++ (comprimeListaJogador j) ++ "*" ++ (comprimeListaDisparo dj)



-- * Comprime Mapa

-- | Comprime um Mapa numa String, juntado as Strings das Linhas separadas por um '/'
comprimeMapa :: Mapa -> String
comprimeMapa []    = "%"
comprimeMapa [x]   = comprimeLinha x ++ []
comprimeMapa (h:t) = (comprimeLinha h) ++ "/" ++ (comprimeMapa t)

-- | Comprime uma Linha numa String
comprimeLinha :: [Peca] -> String
comprimeLinha [] = ""
comprimeLinha (h:t) | h == Bloco Indestrutivel = "i" ++ (comprimeLinha t)
                    | h == Bloco Destrutivel = "d" ++ (comprimeLinha t)
                    | h == Vazia = "v" ++ (comprimeLinha t)



-- * Comprime Jogador

-- | Comprime uma [Jogador] numa String, juntando as Strings dos Jogadores separados por um '#'
comprimeListaJogador :: [Jogador] -> String
comprimeListaJogador [] = "€"
comprimeListaJogador [x] = comprimeJogador x ++ []
comprimeListaJogador (h:t) = (comprimeJogador h) ++ "#" ++ (comprimeListaJogador t)

-- | Comprime um Jogador numa String
comprimeJogador :: Jogador -> String
comprimeJogador (Jogador (x,y) d v l c) = (show x) ++ "!" ++ (show y) ++ (show d) ++ (show v) ++ "?" ++ (show l) ++ "," ++ (show c)



-- * Comprime Disparo

-- | Comprime uma [Disparo] numa String, juntando as Strings dos Disparos separados por um '.'
comprimeListaDisparo :: [Disparo] -> String
comprimeListaDisparo [] = "$"
comprimeListaDisparo [x] = comprimeDisparo x ++ ""
comprimeListaDisparo (h:t) = (comprimeDisparo h) ++ "." ++ (comprimeListaDisparo t)

-- | Comprime um Disparo numa String
comprimeDisparo :: Disparo -> String
comprimeDisparo (DisparoCanhao a (x,y) d) = "c" ++ (show a) ++ (show x) ++ "!" ++ (show y) ++ (show d)
comprimeDisparo (DisparoLaser a (x,y) d) = "l" ++ (show a) ++ (show x) ++ "!" ++ (show y) ++ (show d)
comprimeDisparo (DisparoChoque a t) = "h" ++ (show a) ++ "?" ++ (show t)



-- * Descomprime

-- | Descomprime uma String num Estado
descomprime :: String -> Estado
descomprime a = Estado (estadoMapa a (descobreQuantidadeLinhas 1 (descomprimir 0 a))) (estadoJogador a (descobreQuantidadeJogadores 1 (descomprimir 1 a))) (estadoDisparo a ((descobreQuantidadeDisparos 1 (descomprimir 2 a))-1))

-- | Descomprime a String inical em strings mais pequenas separadas por um *, que correspondem à String do Mapa, da [Jogador] e da [Disparo]
descomprimir :: Int -> String -> String
descomprimir _ [] = []
descomprimir x (h:t) | h/='*' && x==0 = h : descomprimir x t
                     | h=='*' && x==0 = []
                     | h/='*' && x>0 = descomprimir x t
                     | h=='*' && x>0 = descomprimir (x-1) t



-- * Desomprime Mapa

-- | Dado a String correspondente ao Mapa e o numero de Linhas do Mapa devolve o mapa
estadoMapa :: String -> Int -> Mapa
estadoMapa a x | (descomprimir 0 a) == "%" = []
               | x==0 = []
               | otherwise =  estadoMapa a (x-1) ++ [descomprimirLinhas (descomprimirMapa 1 x (descomprimir 0 a))]

-- | Descomprime a String do Mapa em Strings mais pequenas separadas por '/', que correspondem às linhas do mapa
descomprimirMapa :: Int -> Int -> String -> String
descomprimirMapa _ _ [] = []
descomprimirMapa a x (h:t) | h/='/' && a==x = h : descomprimirMapa a x t
                           | h=='/' && a==x = []
                           | h/='/' && x>a = descomprimirMapa a x t
                           | h=='/' && x>a = descomprimirMapa (a+1) x t

-- | Descomprime a String das linhas do Mapa em [Peca]
descomprimirLinhas :: String -> [Peca]
descomprimirLinhas "" = []
descomprimirLinhas (h:t) | h=='v' = Vazia : descomprimirLinhas t
                         | h=='i' = (Bloco Indestrutivel) : descomprimirLinhas t
                         | h=='d' = (Bloco Destrutivel) : descomprimirLinhas t

-- | Descobre a quantidade de linhas do mapa a partir da String do Mapa
descobreQuantidadeLinhas :: Int -> String -> Int
descobreQuantidadeLinhas x "" = x
descobreQuantidadeLinhas x (h:t) | h=='/' = descobreQuantidadeLinhas (x+1) t
                                 | otherwise = descobreQuantidadeLinhas x t



-- * Descomprime Jogador

-- | Dado a String correspondente à [Jogador] e o numero de jogadores do estado devolve a [Jogador]
estadoJogador :: String -> Int -> [Jogador]
estadoJogador a x | (descomprimir 1 a) == "€" = []
                  | x==0 = []
                  | otherwise = estadoJogador a (x-1) ++ [descomprimirJogador (descomprimirListaJogador 1 x (descomprimir 1 a))]

-- | Descomprime a String do [Jogador] em Strings mais pequenas separadas por '#', que correspondem aos Jogadores
descomprimirListaJogador :: Int -> Int -> String -> String
descomprimirListaJogador _ _ [] = []
descomprimirListaJogador a x (h:t) | h/='#' && x==a = h : descomprimirListaJogador a x t
                                   | h=='#' && x==a = []
                                   | h/='#' && x>a = descomprimirListaJogador a x t
                                   | h=='#' && x>a = descomprimirListaJogador (a+1) x t

-- | Descomprime a String do Jogador em Jogador
descomprimirJogador :: String -> Jogador
descomprimirJogador ('!':t) = descomprimirJogador t
descomprimirJogador ('?':t) = descomprimirJogador t
descomprimirJogador (h:t) = Jogador (read (descomprimirPosicaoX (h:t)), read (descomprimirPosicaoY (drop(length(descomprimirPosicaoX (h:t))+1) (h:t)))) (read (descomprimirDirecao (h:t))) (read (descomprimirVida (h:t))) (read (descomprimirLaser (h:t))) (read (descomprimirChoque (h:t)))

-- | Descobre a quantidade de Jogadores a partir da String da [Jogador]
descobreQuantidadeJogadores :: Int -> String -> Int
descobreQuantidadeJogadores x "" = x
descobreQuantidadeJogadores x (h:t) | h=='#' = descobreQuantidadeJogadores (x+1) t
                                    | otherwise = descobreQuantidadeJogadores x t



-- * Descomprime Disparo

-- | Dado a String correspondente à [Disparo] e o numero de disparos do estado devolve a [Disparo]
estadoDisparo :: String -> Int -> [Disparo]
estadoDisparo a x | (descomprimir 2 a) == "$" = []
                  | x<0 = []
                  | otherwise = estadoDisparo a (x-1) ++ [descomprimirDisparo (descomprimirListaDisparo 0 x (descomprimir 2 a))]

-- | Descomprime a String do [Disparo] em Strings mais pequenas separadas por '.', que correspondem aos Disparos
descomprimirListaDisparo :: Int -> Int -> String -> String
descomprimirListaDisparo _ _ [] = []
descomprimirListaDisparo a x (h:t) | h/='.' && x==a = h : descomprimirListaDisparo x a t
                                   | h=='.' && x==a = []
                                   | h/='.' && x>a = descomprimirListaDisparo a x t
                                   | h=='.' && x>a = descomprimirListaDisparo (a+1) x t

-- | Descomprime a String do Disparo em Disparo
descomprimirDisparo :: String -> Disparo
descomprimirDisparo ('c':h:t) = DisparoCanhao (read [h]) (read (descomprimirPosicaoX t), read (descomprimirPosicaoY (drop (length (descomprimirPosicaoX t)+1) t))) (read (descomprimirDirecao t))
descomprimirDisparo ('l':h:t) = DisparoLaser (read [h]) (read (descomprimirPosicaoX t), read (descomprimirPosicaoY (drop (length (descomprimirPosicaoX t)+1) t))) (read (descomprimirDirecao t))
descomprimirDisparo ('h':h:t) = DisparoChoque (read [h]) (read (descomprimirTicks t))

-- | Descobre a quantidade de Disparos a partir da String da [Disparo]
descobreQuantidadeDisparos :: Int -> String -> Int
descobreQuantidadeDisparos x "" = x
descobreQuantidadeDisparos x (h:t) | h=='.' = descobreQuantidadeDisparos (x+1) t
                                   | otherwise = descobreQuantidadeDisparos x t



-- * Funções auxiliares da função descomprime

-- | Descomprime uma string do Jogador ou Disparo na abcissa da Posição
descomprimirPosicaoX :: String -> String
descomprimirPosicaoX [] = []
descomprimirPosicaoX (h:t) | h == '!' = []
                           | otherwise = h : descomprimirPosicaoX t

-- | Descomprime uma string do Jogador ou Disparo na ordenada da Posição
descomprimirPosicaoY :: String -> String
descomprimirPosicaoY [] = []
descomprimirPosicaoY (h:t) | h == 'C' || h == 'D' || h == 'B' || h == 'E' = []
                           | otherwise = h : descomprimirPosicaoY t

-- | Descomprime uma string do Jogador ou Disparo na Direção
descomprimirDirecao :: String -> String
descomprimirDirecao [] = []
descomprimirDirecao (h : t) | h == 'C' || h == 'D' || h == 'B' || h == 'E' = [h]
                            | otherwise = descomprimirDirecao t

-- | Descomprime a String do Jogador na vida do Jogador
descomprimirVida :: String -> String
descomprimirVida [] = []
descomprimirVida (h:i:t) | (h=='C' || h=='D'|| h=='B'|| h=='E') && i/='?' = i : descomprimirVida (h:t)
                         | (h=='C' || h=='D'|| h=='B'|| h=='E') && i=='?' = []
                         | otherwise = descomprimirVida (i:t)

-- | Descomprime a String do Jogador no numero de lasers do Jogador
descomprimirLaser :: String -> String
descomprimirLaser [] = []
descomprimirLaser (h:i:t) | (h == '?') && i/=',' = i : descomprimirLaser (h:t)
                          | (h == '?') && i==',' = []
                          | otherwise = descomprimirLaser (i:t)

-- | Descomprime a String do Jogador no numero de choques do Jogador
descomprimirChoque :: String -> String
descomprimirChoque [] = []
descomprimirChoque (h:t) | h == ',' = t
                         | otherwise = descomprimirChoque t

-- | Descomprime a String do Disparo nos Ticks do Choque
descomprimirTicks :: String -> String
descomprimirTicks [] = []
descomprimirTicks (h:t) | '?' == h = [head t]
                        | otherwise = descomprimirTicks t
