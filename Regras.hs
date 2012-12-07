module Regras where
	
import Tipos
import Aleatorio
import Graphics.UI.WX
import System.IO.Unsafe
import System.Random

--Tabuleiro zerado
tabZerado :: Tabuleiro
tabZerado = [(1,1,Vazio,[2,6,7]),(1,2,Vazio,[1,3,7]),(1,3,Vazio,[2,4,7,8,9]),
			(1,4,Vazio,[3,5,9]),(1,5,Vazio,[4,9,10]),(2,1,Vazio,[1,7,11]),
			(2,2,Vazio,[1,2,3,6,8,11,12,13]),(2,3,Vazio,[3,7,9,13]),(2,4,Vazio,[3,4,5,8,10,13,14,15]),
			(2,5,Vazio,[5,9,15]),(3,1,Vazio,[6,7,12,16,17]),(3,2,Vazio,[7,11,13,17]),
			(3,3,Vazio,[7,8,9,12,14,17,18,19]),(3,4,Vazio,[9,13,15,19]),(3,5,Vazio,[9,10,14,19,20]),
			(4,1,Vazio,[11,17,21]),(4,2,Vazio,[11,12,13,16,18,21,22,23]),(4,3,Vazio,[13,17,19,23]),
			(4,4,Vazio,[13,14,15,18,20,23,24,25]),(4,5,Vazio,[15,19,25]),(5,1,Vazio,[16,17,22]),
			(5,2,Vazio,[17,21,23]),(5,3,Vazio,[17,18,19,22,24]),(5,4,Vazio,[19,23,25]),
			(5,5,Vazio,[19,20,24])]
			
tabInicial :: Tabuleiro
tabInicial = [(1,1,Tigre,[2,6,7]),(1,2,Vazio,[1,3,7]),(1,3,Vazio,[2,4,7,8,9]),
			(1,4,Vazio,[3,5,9]),(1,5,Tigre,[4,9,10]),(2,1,Vazio,[1,7,11]),
			(2,2,Vazio,[1,2,3,6,8,11,12,13]),(2,3,Vazio,[3,7,9,13]),(2,4,Vazio,[3,4,5,8,10,13,14,15]),
			(2,5,Vazio,[5,9,15]),(3,1,Vazio,[6,7,12,16,17]),(3,2,Vazio,[7,11,13,17]),
			(3,3,Vazio,[7,8,9,12,14,17,18,19]),(3,4,Vazio,[9,13,15,19]),(3,5,Vazio,[9,10,14,19,20]),
			(4,1,Vazio,[11,17,21]),(4,2,Vazio,[11,12,13,16,18,21,22,23]),(4,3,Vazio,[13,17,19,23]),
			(4,4,Vazio,[13,14,15,18,20,23,24,25]),(4,5,Vazio,[15,19,25]),(5,1,Tigre,[16,17,22]),
			(5,2,Vazio,[17,21,23]),(5,3,Vazio,[17,18,19,22,24]),(5,4,Vazio,[19,23,25]),
			(5,5,Tigre,[19,20,24])]
			
tabAleatorio :: Tabuleiro
tabAleatorio = [(1,1,Tigre,[2,6,7]),(1,2,Vazio,[1,3,7]),(1,3,Vazio,[2,4,7,8,9]),
			(1,4,Vazio,[3,5,9]),(1,5,Tigre,[4,9,10]),(2,1,Vazio,[1,7,11]),
			(2,2,Vazio,[1,2,3,6,8,11,12,13]),(2,3,Vazio,[3,7,9,13]),(2,4,Vazio,[3,4,5,8,10,13,14,15]),
			(2,5,Vazio,[5,9,15]),(3,1,Vazio,[6,7,12,16,17]),(3,2,Vazio,[7,11,13,17]),
			(3,3,Cabra,[7,8,9,12,14,17,18,19]),(3,4,Vazio,[9,13,15,19]),(3,5,Vazio,[9,10,14,19,20]),
			(4,1,Vazio,[11,17,21]),(4,2,Vazio,[11,12,13,16,18,21,22,23]),(4,3,Vazio,[13,17,19,23]),
			(4,4,Vazio,[13,14,15,18,20,23,24,25]),(4,5,Vazio,[15,19,25]),(5,1,Tigre,[16,17,22]),
			(5,2,Vazio,[17,21,23]),(5,3,Vazio,[17,18,19,22,24]),(5,4,Vazio,[19,23,25]),
			(5,5,Tigre,[19,20,24])]


{-------------- INÍCIO DAS FUNÇÕES P/ MANIPULAÇÃO DOS DADOS --------------}

-- Retorna True se pelo menos um tigre estiver em xeque. False se contrário.
existeTigreEmXeque :: Tabuleiro -> Bool
existeTigreEmXeque g = foldr1 (||) (map (temQueComer g) (posicoesDosTigres g ))

-- Retorna a posição de todas os Tigres em uma lista.
posicoesDosTigres :: Tabuleiro -> [Int]
posicoesDosTigres g = [ x | x <- [1..25], temTigre g x]

-- Verifica se o jogo não acabou para a Cabra
verificaCabras :: Tabuleiro -> Int -> Bool
verificaCabras tab qtCabrasInseridas
	| (qtCabrasInseridas - (contaCabras tab)) < 5 = True
	| otherwise = False

contaCabras :: Tabuleiro -> Int
contaCabras tab = contaCabrasAux tab 0
		
contaCabrasAux :: Tabuleiro -> Int -> Int
contaCabrasAux [] qtdCabras = qtdCabras
contaCabrasAux ((x,y,e,d):tabs) qtdCabras
	| e == Cabra = contaCabrasAux tabs (qtdCabras + 1)
	| otherwise = contaCabrasAux tabs qtdCabras
	
-- Verifica quais são as casas que posso me mover para capturar uma ou mais ovelhas.
{- Estratégia: verifica se tem ovelhas nas casas vizinhas, se tiver, verifica se a casa depois delas estão vazias seguindo a mesma direção (A diferença
entre a posição do tigre e da ovelha, e da ovelha para a casa final tem que ser iguais).-}
posicoesComer :: Tabuleiro -> Int -> [Int]
posicoesComer g x
			   | not(temTigre g x) = []
			   | otherwise = [z | z <- map (calcPulo x )(cabrasVizinhas g x), temCasaVazia g z, z `elem` (camposVizinhos g (cabraDoMeio x z)) ]
					   where 
							   calcPulo x y = y + (y - x)
							   cabraDoMeio x z = ( x + (div (z - x) 2))	
							   
-- Verifica se dada posição é casa vazia ou não. Retorna True se a posição tiver um 'o' nela.
temCasaVazia :: Tabuleiro -> Int -> Bool
temCasaVazia [] _ = False
temCasaVazia ((x,y,e,d):tabs) pos
	| (indice x y) == pos && e == Vazio = True
	| otherwise = temCasaVazia tabs pos
			
-- Retorna as casas vizinhas que tem cabras.
cabrasVizinhas :: Tabuleiro -> Int -> [Int]
cabrasVizinhas g x = [x | x <-(camposVizinhos g x), temCabra g x]

-- Retorna as casas vizinhas da posicao escolhida.
camposVizinhos :: Tabuleiro -> Int -> [Int]
camposVizinhos [] _ = []
camposVizinhos ((x,y,e,d):tabs) pos 
				| (indice x y) == pos = d
				| otherwise = camposVizinhos tabs pos
				
-- Verifica se dada posição tem cabra ou não. Retorna True se a posição tiver um "T" nela.
temCabra :: Tabuleiro -> Int -> Bool
temCabra [] _ = False
temCabra ((x,y,e,d):tabs) pos
				| (indice x y) == pos && e == Cabra = True
				| otherwise = temCabra tabs pos
				
-- Verifica se dada posição tem um Tigre ou não. Retorna True se a posição tiver um "T" nela.
temTigre :: Tabuleiro -> Int -> Bool
temTigre [] _ = False
temTigre ((x,y,e,d):tabs) pos
				| (indice x y) == pos && e == Tigre = True
				| otherwise = temTigre tabs pos
				
-- Move o tigre da posição pT duas casas e come uma cabra na posição pC
comerCabra :: Tabuleiro -> Int -> Int -> Tabuleiro
comerCabra g pT pC = removerCabra novoGrafo pC
			where
				novoGrafo = mover g pT pFinal
					where
						pFinal = pT + 2*(pC - pT)
						
-- Movimentar peça da posição ini para fin no grafo g
movimentar :: Tabuleiro -> Int -> Int -> Tabuleiro
movimentar g ini fin
			| (movimentoValido g ini fin) && umPasso = mover g ini fin
			| (movimentoValido g ini fin) && doisPassos = comerCabra g ini posiCabra
			| otherwise = g
			where
				umPasso = fin `elem` (camposVizinhos g ini)
				doisPassos = fin `elem` (posicoesComer g ini)
				posiCabra = ini + (div (fin - ini) 2)

mover :: Tabuleiro -> Int -> Int -> Tabuleiro
mover grafo posIni posFim = moverAux grafo (retornaString(retornaVertice posIni grafo)) posIni posFim []

moverAux :: Tabuleiro -> Estado -> Int -> Int -> Tabuleiro -> Tabuleiro
moverAux [] _ _ _ resultado = resultado
moverAux ((x,y,e,d):tabs) peca posIni posFim resultado
	| (indice x y) == posIni = moverAux tabs peca posIni posFim (resultado++[(x, y, Vazio, d)])
	| (indice x y) == posFim = moverAux tabs peca posIni posFim (resultado++[(x, y, peca, d)])
	| otherwise = moverAux tabs peca posIni posFim (resultado++[(x, y, e, d)])

retornaString :: Jogada -> Estado
retornaString (x,y,e,d) = e

-- Inserir cabra
inserirCabra :: Tabuleiro ->  Int -> Tabuleiro -> Tabuleiro
inserirCabra [] _ resultado = resultado
inserirCabra ((x,y,e,d):tabs) dest resultado
	| (indice x y) == dest = inserirCabra tabs dest (resultado++[(x, y, Cabra, d)])
	| otherwise = inserirCabra tabs dest (resultado++[(x, y, e, d)])
	
-- Chama inserirVazio passando apenas o grafo e a posição.	
removerCabra :: Tabuleiro -> Int -> Tabuleiro
removerCabra g x = inserirVazio g x []

-- Coloca um espaço vazio no lugar para quando uma cabra é capturada.
inserirVazio :: Tabuleiro ->  Int -> Tabuleiro -> Tabuleiro
inserirVazio [] _ resultado = resultado
inserirVazio ((x,y,e,d):tabs) dest resultado
	| (indice x y) == dest = inserirVazio tabs dest (resultado++[(x, y, Vazio, d)])
	| otherwise = inserirVazio tabs dest (resultado++[(x, y, e, d)])
	
-- Diz se o movimento é permitido ou não (funciona para Cabras e Tigres)
movimentoValido :: Tabuleiro -> Int -> Int -> Bool
movimentoValido g ini fin = fin `elem` (posicoesPermitidas g ini)

-- Mostra todos os possíveis movimentos de uma peça qualquer na posição x (funciona tanto para cabras quanto tigres) ok
posicoesPermitidas :: Tabuleiro -> Int -> [Int]
posicoesPermitidas g x
	| temCabra g x = permitidasVizinhas g x
	| temTigre g x && temQueComer g x = posicoesComer g x
	| temTigre g x && not (temQueComer g x) = permitidasVizinhas g x
	| otherwise = []
	
-- Tigre  da posição x tem que comer, sim ou não
temQueComer :: Tabuleiro -> Int -> Bool
temQueComer g x
    | posicoesComer g x == [] = False
    | otherwise = True
	
-- Lista todas casas vizinhas para qual se pode mover apartir de x.
permitidasVizinhas g x = [x | x <-(camposVizinhos g x), temCasaVazia g x]	

-- Retorna uma lista com todos os tigres que estão cercados
listaTigreCercado :: Tabuleiro -> [Int]
listaTigreCercado tab = listaTigreCercadoAux tab [] tab

listaTigreCercadoAux :: Tabuleiro -> [Int] -> Tabuleiro -> [Int]
listaTigreCercadoAux [] resultado _ = resultado
listaTigreCercadoAux ((x1,y1,e1,(d1:ds1)):tabs1) resultado ((x2,y2,e2,(d2:ds2)):tabs2)
    | tigreEstaCercado (x1, y1, e1, (d1:ds1)) ((x2,y2,e2,(d2:ds2)):tabs2) == True = listaTigreCercadoAux tabs1 (resultado ++ [(indice x1 y1)]) ((x2,y2,e2,(d2:ds2)):tabs2)
    | otherwise = listaTigreCercadoAux tabs1 resultado ((x2,y2,e2,(d2:ds2)):tabs2)
    
tigreEstaCercado :: Jogada -> Tabuleiro -> Bool 
tigreEstaCercado (x,y,e,[]) tab = True
tigreEstaCercado (x,y,e,(d:ds)) tab
    | ((retornaStringDoVertice(retornaVertice d tab)) == Cabra) || ((retornaStringDoVertice(retornaVertice d tab)) == Tigre) = tigreEstaCercado (x, y, e, ds) tab
    | otherwise = False

retornaVertice :: Int -> Tabuleiro -> Jogada 
retornaVertice _ [] = (0,0,Vazio,[])
retornaVertice num ((x,y,e,d):tabs)
    | num == (indice x y) = (x,y,e,d)
    | otherwise = retornaVertice num tabs
	
retornaStringDoVertice :: Jogada -> Estado
retornaStringDoVertice (0,0,Vazio,[]) = Vazio
retornaStringDoVertice (x,y,e,d) = e

-- Recebe as coordenadas e retorna o estado naquela posição	
retornaEstado :: Tabuleiro -> Int -> Int -> Estado
retornaEstado [] _ _ = error "Tabuleiro Invalido!"
retornaEstado ((tx, ty, te, tr):ts) x y
    | tx == x && ty == y = te
    | otherwise          = retornaEstado ts x y
	
-- Função para testar se uma jogada é válida ou não	
testaJogada :: Tabuleiro -> (Int,Int,Estado) -> Estado
testaJogada t (x, y, e)
	|retornaEstado t x y == Tigre = Tigre
	|retornaEstado t x y == Cabra = Cabra
	|otherwise = Vazio
	
-- (igor) Retorna a posição de todas as Cabras em uma lista.
posicoesDasCabras :: Tabuleiro -> [Int]
posicoesDasCabras g = [ x | x <- [1..25], temCabra g x ]


-- (igor) Concatena a lista de movimentos possíveis de todos os tigres e se o resultado for vazio é porque as cabras venceram (True)
-- Se o tigre está preso, posicoesDosTigres retorna []. Se todos estão presos a função map retorna [[],[],[],[]] e depois de aplicar concat retorna []
cabraVenceu :: Tabuleiro -> Bool
cabraVenceu g =  [] == concat (map (posicoesPermitidas g) (posicoesDosTigres1 g))


-- (igor) Retorna a posição de todas os Tigres em uma lista.
posicoesDosTigres1 :: Tabuleiro -> [Int]
posicoesDosTigres1 g = [ x | x <- [1..25], temTigre1 g x]

-- (igor) Verifica se dada posição tem um Tigre ou não. Retorna True se a posição tiver um "T" nela. ok		
temTigre1 :: Tabuleiro -> Int -> Bool
temTigre1 g x = valorPosicao g x == Tigre

-- (igor) Valor guardado na posição
valorPosicao :: Tabuleiro -> Int -> Estado
valorPosicao [] _ = Vazio
valorPosicao ((x,y,e,d):tabs) pos
			| pos == (indice x y) = e
			| otherwise = valorPosicao tabs pos
	
{---------------- FIM DAS FUNÇÕES P/ MANIPULAÇÃO DOS DADOS ---------------}





{-------------------- INÍCIO DAS FUNÇÕES P/ 2 PLAYERS --------------------}
			
pick :: [a] -> IO a
pick xs = do
			r <- randomRIO (0, (length xs - 1))
			return $ xs !! r

movimentoCabraCPU :: Ambiente -> Tabuleiro -> IO Tabuleiro
movimentoCabraCPU a g = do
					--cabra aleatoria
					pCabra <- pick (posicoesDasCabras g)
					pFinal <- pick (posicoesPermitidas g pCabra)
					
					if (movimentoValido g pCabra pFinal == True)
						then do
						set (ambTbl a) [value := movimentar g pCabra pFinal]
						t1 <- get (ambTbl a) value
						return (t1)
						else do
						movimentoCabraCPU a g
						
inserirCabraCPU :: Ambiente -> Tabuleiro -> IO Tabuleiro
inserirCabraCPU a g = do
				pFinal <- pick (casasVazias g [])
				set (ambTbl a) [value := inserirCabra g pFinal []]
				t1 <- get (ambTbl a) value
				--count <- get (ambCon a) value
				--set (ambCon a) [value := count+1]
				return (t1)
				
casasVazias :: Tabuleiro -> [Int] -> [Int]
casasVazias [] resultado = resultado
casasVazias ((tx, ty, te, tr):ts) resultado
	| te == Vazio = casasVazias ts resultado++[(indice tx ty)]
	| otherwise = casasVazias ts resultado
		
{-------------------- FIM DAS FUNÇÕES P/ 2 PLAYERS --------------------}

-- Coordenadas de um valor da matriz do tabuleiro
quadrado :: Int -> (Int, Int)
quadrado x
	| x == 1 = (1,1)
	| x == 2 = (1,2)
	| x == 3 = (1,3)
	| x == 4 = (1,4)
	| x == 5 = (1,5)
	| x == 6 = (2,1)
	| x == 7 = (2,2)
	| x == 8 = (2,3)
	| x == 9 = (2,4)
	| x == 10 = (2,5)
	| x == 11 = (3,1)
	| x == 12 = (3,2)
	| x == 13 = (3,3)
	| x == 14 = (3,4)
	| x == 15 = (3,5)
	| x == 16 = (4,1)
	| x == 17 = (4,2)
	| x == 18 = (4,3)
	| x == 19 = (4,4)
	| x == 20 = (4,5)
	| x == 21 = (5,1)
	| x == 22 = (5,2)
	| x == 23 = (5,3)
	| x == 24 = (5,4)
	| x == 25 = (5,5)
	
-- Valor na matriz do tabuleiro
indice :: Int -> Int -> Int
indice x y
	| x == 1 && y == 1 = 1
	| x == 1 && y == 2 = 2
	| x == 1 && y == 3 = 3
	| x == 1 && y == 4 = 4
	| x == 1 && y == 5 = 5
	| x == 2 && y == 1 = 6
	| x == 2 && y == 2 = 7
	| x == 2 && y == 3 = 8
	| x == 2 && y == 4 = 9
	| x == 2 && y == 5 = 10
	| x == 3 && y == 1 = 11
	| x == 3 && y == 2 = 12
	| x == 3 && y == 3 = 13
	| x == 3 && y == 4 = 14
	| x == 3 && y == 5 = 15
	| x == 4 && y == 1 = 16
	| x == 4 && y == 2 = 17
	| x == 4 && y == 3 = 18
	| x == 4 && y == 4 = 19
	| x == 4 && y == 5 = 20
	| x == 5 && y == 1 = 21
	| x == 5 && y == 2 = 22
	| x == 5 && y == 3 = 23
	| x == 5 && y == 4 = 24
	| x == 5 && y == 5 = 25