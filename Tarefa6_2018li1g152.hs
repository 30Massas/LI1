-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2018li1g152 where

import           LI11819
import           Tarefa0_2018li1g152
import           Tarefa1_2018li1g152
import           Tarefa2_2018li1g152
import           Tarefa4_2018li1g152

-- Module      : Tarefa6_2018li1g152
-- Description : Implementação de um ro'bot' no jogo
-- Copyright   : Luís Vieira <a89601@alunos.uminho.pt>
--               Paulo Sousa <a89465@alunos.uminho.pt>


-- * Introdução

-- $intro
-- * Nesta Tarefa, fomos desafiados a criar um ro'bot', em Haskell, que fosse capaz de jogar e ganhar o jogo quando competindo contra outros jogadores.
-- Tomando em conta cada Estado do jogo apresentado, o ro'bot' vai tomar a sua decisão, podendo jogar mais defensivamente ou agressivamente, conforme os dados que recolhe.

-- * Objetivos

-- $objs
-- * Quando iniciámos a tarefa, discutimos inicialmente se o nosso ro'bot' deveria ser mais agressivo ou mais defensivo, e chegámos à conclusão que deveria ser um intermédio entre ambos.
-- Isto é, não se encontra sempre a fugir dos jogadores quando se encontram próximos dele, mas também não os persegue constantemente de forma a retirar as suas vidas.
-- O ro'bot' recebe as informações disponíveis em cada Estado, e assim toma a decisão consoante o que nós programámos.
--
-- == Tomada de decisões
--
-- $decisões
--
-- * Dada a distância a outros jogadores, o nosso ro'bot' vai tentar deslocar-se na direção em que as posições seguintes contenham uma peça Vazia.
-- * Quando lhe for apresentado um Bloco Destrutível, este irá realizar um disparo de canhão de modo a destruir o bloco, quer para poder seguir o seu caminho, quer para angariar pontuação.
-- No entanto, o nosso ro'bot', apesar de muito díficl de ocorrer, tentará desviar-se dos lasers dos oponentes comparando as suas coordenadas às dos adversários.
-- Mais propriamente, se o adversário se encontrar acima ou abaixo dele, irá tentar desviar-se ou para a sua esquerda, ou para a sua direita.
-- Quando o adversário se encontrar numa das laterais do ro'bot', este tentará movimentar-se ou para cima, ou para baixo.
-- Da mesma forma que se tenta desviar de lasers, o nosso ro'bot' tenta também desviar-se das balas de canhão que venham na sua direção.
-- Assim, quando uma bala de canhão se encontrar a 4 posições do nosso ro'bot' este irá movimentar-se para tentar fugir à mesma.
-- Por fim, quando um adversário se encontrar a uma distância suficiente para potencialmente acertar com um choque, o nosso ro'bot' irá movimentar-se de forma a criar uma distância entre si e o adversário.
-- * Numa perspetiva mais ofensiva, o nosso ro'bot' vai averiguar se ainda tem lasers e/ou choques.
-- Se ainda tiver lasers, e estiver na mesma linha, quer horizontal, quer vertical, que um adversário, este deverá disparar o laser.
-- Se ainda tiver choques, e se se encontrar perto de um adversário, irá disparar o choque e tentar mover-se na sua direção para disparar o canhão e acertar.
-- Por fim, quando já só pode recorrer ao Canhão, o ro'bot' procura disparar somente quando se encontra a uma distância de um adversário que assegure contacto entre a bala disparada e o adversário.

-- * Conclusão

-- $conc
-- * Ao observar os variados testes feitos no simulador, e aquilo que ocorria no torneio experimental, averiguámos que o nosso ro'bot' não funciona exatamente como queremos.
-- Este, acaba por simplesmente disparar todos os lasers na direção em que nasce, e todos os choques (mesmo que não possua um jogador perto o suficiente para o atingir) no início do jogo.
-- * Para além disso, a tentativa de se desviar dos lasers que pudessem ocorrer acabou por se tornar obsoleta e acabámos por retirar a função, uma vez que o ro'bot' precisa de 2 ticks para se conseguir
-- eventualmente desviar, e o disparo de um laser ocorre somente num tick.
-- Outro ponto a notar, é que uma vez que o nosso ro'bot' encontre um jogador sobre o qual começa a disparar, mesmo que retire todas as vidas ao adversário, fica constantemente a disparar na direção
-- em que se encontrava o mesmo.
-- * Concluímos assim que, apesar de funcional, o nosso ro'bot' não é aquilo que era pretendido por nós.



-- * Função principal

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int -> Estado -> Maybe Jogada
bot x (Estado m j dj) | checkPath m (encontraJogador x j)        = Just (Dispara Canhao)
                      | condAtacar (encontraJogador x j) j dj    = atacar x (Estado m j dj)
                      | condDefender (encontraJogador x j) j dj  = defender x (Estado m j dj)
                      | otherwise                                = Just (Movimenta (escolheCaminho m (encontraJogador x j)))



-- * Destruir

-- | Função que verifica se nas posições à frente do Jogador se encontra um bloco destrutível
checkPath :: Mapa -> Jogador -> Bool
checkPath m (Jogador (x,y) C v l c) = encontraPosicaoMatriz (x-1,y) m == Bloco Destrutivel
checkPath m (Jogador (x,y) B v l c) = encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel
checkPath m (Jogador (x,y) D v l c) = encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel
checkPath m (Jogador (x,y) E v l c) = encontraPosicaoMatriz (x,y-1) m == Bloco Destrutivel

-- | Função auxiliar que verifica a existência de um jogador na linha vertical do bot
verificaX :: Jogador -> Posicao -> Bool
verificaX (Jogador (x,y) d v l c) (a,_) = ((x==a) || (x==a-1) || (x==a+1))

-- | Função auxiliar que verifica a existência de um jogador na linha horizontal do bot
verificaY :: Jogador -> Posicao -> Bool
verificaY (Jogador (x,y) d v l c) (_,b) = ((y==b) || (y==b-1) || (y==b+1))



-- * Defender

-- | Função que dadas certas condições, determina se o bot deve jogar defensivamente ou não
condDefender :: Jogador -> [Jogador] -> [Disparo] -> Bool
condDefender _ _ []  = False
condDefender j lj dj = (temJogadorPertoDef j lj) || (temCanhaoPerto j dj)

-- | Função que verifica se existe algum jogador numa distância de 4 casas
temJogadorPertoDef :: Jogador -> [Jogador] -> Bool
temJogadorPertoDef j []                                                                                     = False
temJogadorPertoDef (Jogador (x,y) d v l c) ((Jogador (a,b) _ _ _ _):t) | verificaDistanciasX d (x,y) (a,b)  = True
                                                                       | verificaDistanciasY d (x,y) (a,b)  = True
                                                                       | otherwise                          = temJogadorPertoDef (Jogador (x,y) d v l c) t

-- | Função que verifica se a distância entre dois jogadores na vertical é suficiente para acertar com um disparo de Canhão
verificaDistanciasX :: Direcao -> Posicao -> Posicao -> Bool
verificaDistanciasX C (x,y) (a,b) = (y==b || y==b-1 || y==b+1) && (x==a+4)
verificaDistanciasX B (x,y) (a,b) = (y==b || y==b-1 || y==b+1) && (x==a-4)
verificaDistanciasX D _ _         = False
verificaDistanciasX E _ _         = False

-- | Função que verifica se a distância entres dois jogadores na horizontal é suficiente para acertar com um disparo de Canhão
verificaDistanciasY :: Direcao -> Posicao -> Posicao -> Bool
verificaDistanciasY D (x,y) (a,b) = (x==a || x==a-1 || x==a+1) && (y==b-4)
verificaDistanciasY E (x,y) (a,b) = (x==a || x==a-1 || x==a+1) && (y==b+4)
verificaDistanciasY C _ _         = False
verificaDistanciasY B _ _         = False

-- | Função que verifica se existe uma bala de canhão em direção ao jogador
temCanhaoPerto :: Jogador -> [Disparo] -> Bool
temCanhaoPerto _ []                                                                                  = False
temCanhaoPerto j ((DisparoCanhao p (dx,dy) d):t) | verificaProximidade j (DisparoCanhao p (dx,dy) d) = True
                                                 | otherwise                                         = temCanhaoPerto j t
temCanhaoPerto j (h:t)                                                                               = temCanhaoPerto j t

-- | Função que verifica se a bala de canhão se encontra a uma distância de 3 casas do jogador
verificaProximidade :: Jogador -> Disparo -> Bool
verificaProximidade (Jogador (x,y) d _ _ _) (DisparoCanhao _ (dx,dy) _) | x - 1 <= dx + 3 = True
                                                                        | x + 1 >= dx - 3 = True
                                                                        | y - 1 <= dy + 3 = True
                                                                        | y + 1 >= dy - 3 = True
                                                                        | otherwise       = False

-- | Função que determina a Jogada a tomar face às condições de defesa apresentadas
defender :: Int -> Estado -> Maybe Jogada
defender x (Estado m j dj) | temCanhaoPerto b dj    = Just (Movimenta (escaparBala b m dj))
                           | temJogadorPertoDef b j = Just (Movimenta (escaparJogador b m j))
                           | otherwise              = Nothing

                           where b = encontraJogador x j

-- | Função que determina a direção em que o bot se vai movimentar de modo a escapar a um disparo de canhão nas suas proximidades
escaparBala :: Jogador -> Mapa -> [Disparo] -> Direcao
escaparBala (Jogador (x,y) d v l c) m [] = d
escaparBala (Jogador (x,y) d v l c) m ((DisparoCanhao p (dx,dy) C):t) | y == dy && (encontraPosicaoMatriz (x,y+1) m == Vazia) = D
                                                                      | y == dy && (encontraPosicaoMatriz (x,y+1) m /= Vazia) = E
                                                                      | y - 1 == dy                                           = D
                                                                      | y + 1 == dy                                           = E
                                                                      | otherwise                                             = escaparBala (Jogador (x,y) d v l c) m t
escaparBala (Jogador (x,y) d v l c) m ((DisparoCanhao p (dx,dy) B):t) | y == dy && (encontraPosicaoMatriz (x,y+1) m == Vazia) = D
                                                                      | y == dy && (encontraPosicaoMatriz (x,y+1) m /= Vazia) = E
                                                                      | y - 1 == dy                                           = D
                                                                      | y + 1 == dy                                           = E
                                                                      | otherwise                                             = escaparBala (Jogador (x,y) d v l c) m t
escaparBala (Jogador (x,y) d v l c) m ((DisparoCanhao p (dx,dy) D):t) | x == dx && (encontraPosicaoMatriz (x-1,y) m == Vazia) = C
                                                                      | x == dx && (encontraPosicaoMatriz (x-1,y) m /= Vazia) = B
                                                                      | x - 1 == dx                                           = B
                                                                      | x + 1 == dx                                           = C
                                                                      | otherwise                                             = escaparBala (Jogador (x,y) d v l c) m t
escaparBala (Jogador (x,y) d v l c) m ((DisparoCanhao p (dx,dy) E):t) | x == dx && (encontraPosicaoMatriz (x-1,y) m == Vazia) = C
                                                                      | x == dx && (encontraPosicaoMatriz (x-1,y) m /= Vazia) = B
                                                                      | x - 1 == dx                                           = B
                                                                      | x + 1 == dx                                           = C
                                                                      | otherwise                                             = escaparBala (Jogador (x,y) d v l c) m t
escaparBala (Jogador (x,y) d v l c) m (h:t)                                                                                   = escaparBala (Jogador (x,y) d v l c) m t

-- | Função que determina a direção em que o bot se vai movimentar de modo a escapar de um jogador que tenha potenciais choques, ou que se encontre perto o suficiente para atingir com disparos canhão
escaparJogador :: Jogador -> Mapa -> [Jogador] -> Direcao
escaparJogador (Jogador (x,y) d v l c) m ((Jogador (a,b) C _ _ _):t) | x <= a + 5 = escolheDirecao C (x,y) m
                                                                     | otherwise  = escaparJogador (Jogador (x,y) d v l c) m t
escaparJogador (Jogador (x,y) d v l c) m ((Jogador (a,b) B _ _ _):t) | x >= a - 5 = escolheDirecao B (x,y) m
                                                                     | otherwise  = escaparJogador (Jogador (x,y) d v l c) m t
escaparJogador (Jogador (x,y) d v l c) m ((Jogador (a,b) D _ _ _):t) | y >= b - 5 = escolheDirecao D (x,y) m
                                                                     | otherwise  = escaparJogador (Jogador (x,y) d v l c) m t
escaparJogador (Jogador (x,y) d v l c) m ((Jogador (a,b) E _ _ _):t) | y <= b + 5 = escolheDirecao E (x,y) m
                                                                     | otherwise  = escaparJogador (Jogador (x,y) d v l c) m t
escaparJogador (Jogador (x,y) d v l c) m []                                       = escolheDirecao d (x,y) m

-- | Função auxiliar que determina a direção a tomar
escolheDirecao :: Direcao -> Posicao -> Mapa -> Direcao
escolheDirecao C (x,y) m | encontraPosicaoMatriz (x-1,y) m == Vazia = C
                         | encontraPosicaoMatriz (x,y+1) m == Vazia = D
                         | encontraPosicaoMatriz (x,y-1) m == Vazia = E
                         | otherwise                                = B
escolheDirecao B (x,y) m | encontraPosicaoMatriz (x+1,y) m == Vazia = B
                         | encontraPosicaoMatriz (x,y+1) m == Vazia = D
                         | encontraPosicaoMatriz (x,y-1) m == Vazia = E
                         | otherwise                                = C
escolheDirecao D (x,y) m | encontraPosicaoMatriz (x,y+1) m == Vazia = D
                         | encontraPosicaoMatriz (x-1,y) m == Vazia = C
                         | encontraPosicaoMatriz (x+1,y) m == Vazia = B
                         | otherwise                                = E
escolheDirecao E (x,y) m | encontraPosicaoMatriz (x,y-1) m == Vazia = E
                         | encontraPosicaoMatriz (x-1,y) m == Vazia = C
                         | encontraPosicaoMatriz (x+1,y) m == Vazia = B
                         | otherwise                                = D



-- * Atacar

-- | Função que verifica se existem as condições necessárias para o bot jogar agressivamente
condAtacar :: Jogador -> [Jogador] -> [Disparo] -> Bool
condAtacar _ [] _  = False
condAtacar j lj dj = (temosLaser j)  || (temosChoque j) || (temJogadorParaAtk j lj)

-- | Função que verifica se temos lasers disponíveis para disparar
temosLaser :: Jogador -> Bool
temosLaser (Jogador (x,y) d v l c) = l>0

-- | Função que verifica se temos choques disponíveis para disparar
temosChoque :: Jogador -> Bool
temosChoque (Jogador (x,y) d v l c) = c>0

-- | Função que verifica se existe um jogador perto do bot de modo a dispararmos o canhão para acertar
temJogadorParaAtk :: Jogador -> [Jogador] -> Bool
temJogadorParaAtk j []                                                                                     = False
temJogadorParaAtk (Jogador (x,y) d v l c) ((Jogador (a,b) _ _ _ _):t) | verificaDistanciasX d (x,y) (a,b)  = True
                                                                      | verificaDistanciasY d (x,y) (a,b)  = True
                                                                      | otherwise                          = temJogadorParaAtk (Jogador (x,y) d v l c) t

-- | Função que determina a Jogada a tomar face as condições de ataque apresentadas
atacar :: Int -> Estado -> Maybe Jogada
atacar x (Estado m j dj) | (temosLaser (encontraJogador x j)) && (verLinhaLaser (encontraJogador x j) j)   = Just (Dispara Laser)
                         | (temJogadorParaAtk (encontraJogador x j) j)                                     = Just (Dispara Canhao)
                         | (jogProxChoque (encontraJogador x j) j) && (temosChoque (encontraJogador x j))  = Just (Dispara Choque)
                         | otherwise                                                                       = Nothing

-- | Função que verifica se temos um jogador nas linhas horizontal e vertical, para podermos disparar um laser
verLinhaLaser :: Jogador -> [Jogador] -> Bool
verLinhaLaser j []                                                                                         = False
verLinhaLaser (Jogador (x,y) C v l c) ((Jogador(a,b) _ _ _ _):t) | verificaY (Jogador (x,y) C v l c) (a,b) = True
                                                                 | otherwise                               = verLinhaLaser (Jogador (x,y) C v l c) t
verLinhaLaser (Jogador (x,y) B v l c) ((Jogador(a,b) _ _ _ _):t) | verificaY (Jogador (x,y) B v l c) (a,b) = True
                                                                 | otherwise                               = verLinhaLaser (Jogador (x,y) B v l c) t
verLinhaLaser (Jogador (x,y) D v l c) ((Jogador(a,b) _ _ _ _):t) | verificaX (Jogador (x,y) D v l c) (a,b) = True
                                                                 | otherwise                               = verLinhaLaser (Jogador (x,y) D v l c) t
verLinhaLaser (Jogador (x,y) E v l c) ((Jogador(a,b) _ _ _ _):t) | verificaX (Jogador (x,y) E v l c) (a,b) = True
                                                                 | otherwise                               = verLinhaLaser (Jogador (x,y) E v l c) t

-- | Função que verifica se existe um jogador perto o suficiente para ser atingido com um choque
jogProxChoque :: Jogador -> [Jogador] -> Bool
jogProxChoque j []                                                                                        = False
jogProxChoque (Jogador (x,y) d v l c) ((Jogador (a,b) _ _ _ _):t) | x+3<=a || x-3<=a || y+3<=b || y-3<=b  = True
                                                                  | otherwise                             = jogProxChoque (Jogador (x,y) d v l c) t


-- * Decidir

-- | Função que indica a direção a tomar quando a peça na posição à nossa frente for vazia, cas contrário roda o Jogador 90º no sentido dos ponteiros do relógio.
escolheCaminho :: Mapa -> Jogador -> Direcao
escolheCaminho m (Jogador (x,y) C v l c) | checkRoad m (Jogador (x,y) C v l c) = C
                                         | otherwise                           = escolheCaminho m (Jogador (x,y) D v l c)
escolheCaminho m (Jogador (x,y) D v l c) | checkRoad m (Jogador (x,y) D v l c) = D
                                         | otherwise                           = escolheCaminho m (Jogador (x,y) B v l c)
escolheCaminho m (Jogador (x,y) B v l c) | checkRoad m (Jogador (x,y) B v l c) = B
                                         | otherwise                           = escolheCaminho m (Jogador (x,y) E v l c)
escolheCaminho m (Jogador (x,y) E v l c) | checkRoad m (Jogador (x,y) E v l c) = E
                                         | otherwise                           = escolheCaminho m (Jogador (x,y) C v l c)

-- | Função que verifica se dada uma direção, a posição imediatamente à frente, se encontra Vazia
checkRoad :: Mapa -> Jogador -> Bool
checkRoad m (Jogador (x,y) C v l c) = encontraPosicaoMatriz (x-1,y) m == Vazia
checkRoad m (Jogador (x,y) B v l c) = encontraPosicaoMatriz (x+1,y) m == Vazia
checkRoad m (Jogador (x,y) D v l c) = encontraPosicaoMatriz (x,y+1) m == Vazia
checkRoad m (Jogador (x,y) E v l c) = encontraPosicaoMatriz (x,y-1) m == Vazia
