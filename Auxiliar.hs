module Auxiliar where
	
import Graphics.UI.WX
import Graphics.UI.WXCore
import Tipos
import Regras
import Mensagens
import Sounds
import Aleatorio

-- Funções para atualização do tabuleiro

-- Atualiza a figura mostrada em uma posição do tabuleiro
atualizaPosicao :: Ambiente -> Panel () -> Jogada -> IO ()
atualizaPosicao a p (x,y,e,d) = do
    set p [on paint := aux a e]
    repaint p
    where
        aux a e dc _ = do
            s <- get (ambSkn a) value
            case e of
                Tigre -> do
                    bmp <- bitmapCreateFromFile ("skins/" ++ s ++ "/tabT" ++ (show x) ++ "_" ++ (show y) ++ ".bmp")
                    drawBitmap dc bmp (pt 0 0) False []
                Cabra -> do
                    bmp <- bitmapCreateFromFile ("skins/" ++ s ++ "/tabC" ++ (show x) ++ "_" ++ (show y) ++ ".bmp")
                    drawBitmap dc bmp (pt 0 0) False []
                Vazio -> do
                    bmp <- bitmapCreateFromFile ("skins/" ++ s ++ "/tab" ++ (show x) ++ "_" ++ (show y) ++ ".bmp")
                    drawBitmap dc bmp (pt 0 0) False []

-- Atualiza o título do jogo dependendo da skin					
atualizaTitulo :: Ambiente -> IO ()
atualizaTitulo a = do
	set (ambPn3 a) [on paint := aux a]
	repaint (ambPn3 a)
	where
		aux a dc _ = do
			s <- get (ambSkn a) value
			bmp <- bitmapCreateFromFile ("skins/" ++ s ++ "/titulo.bmp")
			drawBitmap dc bmp (pt 0 0) False []			

-- Atualiza a vez do jogador
atualizaVez :: Ambiente -> IO ()
atualizaVez a = do
		v <- get (ambVez a) value
		t <- get (ambTbl a) value
		s <- get (ambSkn a) value
		set (ambPn2 a) [on paint := aux1 s v]
		repaint (ambPn2 a)
		where
			aux1 s v dc viewArea = do
				if (strEstado v /= "vazio")
					then do
						bmp <- (bitmapCreateFromFile ("skins/" ++ s ++ "/" ++ strEstado v ++ ".bmp"))
						drawBitmap dc bmp (pt 0 0) True []
					else do
						bmp_fundo <- bitmapCreateFromFile ("skins/" ++ s ++ "/veznada.bmp")
						drawBitmap dc bmp_fundo (pt 0 0) False []

-- Atualiza todas as posições do tabuleiro de acordo com a situação do jogo						
atualiza :: Ambiente -> Tabuleiro -> [Panel ()] -> IO ()
atualiza _ [] [] = do {return ()}
atualiza a ((x, y, e, d):ts) (p:ps) = do
    atualizaPosicao a p (x,y,e,d)
    atualiza a ts ps									

-- Muda a skin do jogo e as figuras do tabuleiro	
aplicaSkin :: Ambiente -> String -> IO ()
aplicaSkin a s = do
    set (ambSkn a) [value := s]
    t1 <- get (ambTbl a) value
    atualiza a t1 (ambPos a)
    atualizaTitulo a
    atualizaVez a
	
-- Funções da jogada

-- Realiza ou rejeita uma jogada feita
jogar :: Ambiente -> (Int,Int) -> Estado -> [Int] -> Point -> IO()
jogar a (x,y) est direcao _ = do
	t0 <- get (ambTbl a) value
	e <- get (ambVez a) value
	m <- get (ambMod a) value
	count <- get (ambCon a) value
	if(e == Cabra)
		then do
			if(count < 20)
				then do
					if (temCasaVazia t0 (indice x y)) == True
						then do
							set (ambTbl a) [value := inserirCabra t0 (indice x y) []]
							set (ambCon a) [value := count+1]
							c1 <- get (ambCon a) value
							putStr (show c1 ++ "\n")
							t1 <- get (ambTbl a) value
							atualiza a t1 (ambPos a)
--							atualizaVez a
							--putStr (show (length(listaTigreCercado t1)) ++ "<-tigreCercado ")
							--if (length(listaTigreCercado t1) == 16)
							if(cabraVenceu t1 == True) 
								then do
									somVitoria (ambSom a)
									infoDialog (ambFrm a) dlgConcluidoT dlgVC
									resp <- confirmDialog (ambFrm a) dlgNovoJogoT dlgNovoJogo True
									if (resp)
										then do
											novoJogoTwoP a m est
										else do
											fecharJogo a
								else do
									set (ambVez a) [value := oposto e]
									atualizaVez a
									ativaJogo a t1 (ambPos a) e
						else do
							av <- get (ambAvs a) value
							if (av)
								then do
									warningDialog (ambFrm a) dlgInvalidaT dlgInvalidaC
								else do
									return ()		
				else do
					if((testaJogada t0 (x,y,e)) /= Cabra)
						then do
							av <- get (ambAvs a) value
							if (av)
								then do
									warningDialog (ambFrm a) dlgInvalidaT dlgInvalida
								else do
									return ()
						else do
							movimentarPeca a t0 (ambPos a) e (x, y) direcao
		else do
			if((testaJogada t0 (x,y,e)) /= Tigre)
				then do
					av <- get (ambAvs a) value
					if (av)
						then do
							warningDialog (ambFrm a) dlgInvalidaT dlgInvalida
						else do
							return ()
				else do
					if ((indice x y) `elem` listaTigreCercado t0 && temQueComer t0 (indice x y) == False)
						then do
							warningDialog (ambFrm a) dlgInvalidaT dlgInvalidaT4
							ativaJogo a t0 (ambPos a) e
						else do
							if (existeTigreEmXeque t0 == True)
								then do
									if (temQueComer t0 (indice x y) == True)
										then do
											movimentarPeca a t0 (ambPos a) e (x, y) direcao
										else do
											warningDialog (ambFrm a) dlgInvalidaT dlgInvalidaT3
											ativaJogo a t0 (ambPos a) e
								else
									movimentarPeca a t0 (ambPos a) e (x, y) direcao
											
movimentacao :: Ambiente -> (Int,Int) -> (Int,Int) -> Estado -> [Int] -> Point -> IO()
movimentacao a (x,y) (ini1, ini2) est direcao _ = do
	t0 <- get (ambTbl a) value
	e <- get (ambVez a) value
	m <- get (ambMod a) value
	count <- get (ambCon a) value
	if(e == Cabra)
		then do
			if (movimentoValido t0 (indice ini1 ini2) (indice x y) == True)
				then do
				set (ambTbl a) [value := movimentar t0 (indice ini1 ini2) (indice x y)]
				t1 <- get (ambTbl a) value
				atualiza a t1 (ambPos a)
				atualizaVez a
				--putStr (show (length(listaTigreCercado t1)) ++ "<-tigreCercadoMOVIMENTACAO ")
				--if (length(listaTigreCercado t1) == 16)
				if(cabraVenceu t1 == True)
					then do
						somVitoria (ambSom a)
						infoDialog (ambFrm a) dlgConcluidoT dlgVC
						resp <- confirmDialog (ambFrm a) dlgNovoJogoT dlgNovoJogo True
						if (resp)
							then do
								novoJogoTwoP a m est
							else do
								fecharJogo a
					else do
						set (ambVez a) [value := oposto e]
						atualizaVez a
						--putStr (show e ++ show m)
						ativaJogo a t1 (ambPos a) e
				else do
					av <- get (ambAvs a) value
					if (av)
						then do
							warningDialog (ambFrm a) dlgInvalidaT dlgInvalida
						else do
							return ()
		else do
			if (existeTigreEmXeque t0 == True)
				then do
					if (temQueComer t0 (indice ini1 ini2) == True)
						then do
							if (movimentoValido t0 (indice ini1 ini2) (indice x y) == True)
								then do
									set (ambTbl a) [value := movimentar t0 (indice ini1 ini2) (indice x y)]
									somComida (ambSom a)
									t1 <- get (ambTbl a) value
									atualiza a t1 (ambPos a)
									if (verificaCabras t1 count) == False 
										then do
											somVitoria (ambSom a)
											infoDialog (ambFrm a) dlgConcluidoT dlgVT
											resp <- confirmDialog (ambFrm a) dlgNovoJogoT dlgNovoJogo True
											if (resp)
												then do
													novoJogoTwoP a m est
												else do
													fecharJogo a
										else do
											if (temQueComer t1 (indice x y) == True)
												then do
													ativaJogo a t1 (ambPos a) e
												else do
													--set (ambVez a) [value := oposto e]
													--atualizaVez a
													set (ambVez a) [value := oposto e]
													vez <- get (ambVez a) value
													atualizaVez a
													if(m == 1 && vez == Cabra)
														then do
															if (verificaCabras t1 count) == False 
																then do
																	somVitoria (ambSom a)
																	infoDialog (ambFrm a) dlgConcluidoT dlgVT
																	resp <- confirmDialog (ambFrm a) dlgNovoJogoT dlgNovoJogo True
																	if (resp)
																		then do
																			novoJogoOneP a m est
																		else do
																			fecharJogo a
																else do
																	if(count < 20)
																		then do
																			--t3 <- get (ambTbl a) value
																			t2 <- inserirCabraCPU a t1
																			set (ambCon a) [value := count+1]
																			count <- get (ambCon a) value
																			putStr (show count ++ "\n")
																			atualiza a t2 (ambPos a)
																			set (ambVez a) [value := oposto Cabra]
																			atualizaVez a
																			ativaJogo a t2 (ambPos a) e
																		else do
																			--t3 <- get (ambTbl a) value
																			t2 <- movimentoCabraCPU a t1
																			atualiza a t2 (ambPos a)
																			--putStr (show t2)
																			set (ambVez a) [value := oposto Cabra]
																			atualizaVez a
																			ativaJogo a t2 (ambPos a) e
														else do
															--set (ambVez a) [value := oposto e]
															--atualizaVez a
															ativaJogo a t1 (ambPos a) e
								else do
									av <- get (ambAvs a) value
									if (av)
										then do
											warningDialog (ambFrm a) dlgInvalidaT dlgInvalidaT1
										else do
											return ()
						else do
							av <- get (ambAvs a) value
							if (av)
								then do
									warningDialog (ambFrm a) dlgInvalidaT dlgInvalidaT2 
								else do
									return ()									
				else do
					if (movimentoValido t0 (indice ini1 ini2) (indice x y) == True)
						then do
							set (ambTbl a) [value := movimentar t0 (indice ini1 ini2) (indice x y)]
							t1 <- get (ambTbl a) value
							atualiza a t1 (ambPos a)
							atualizaVez a
							set (ambVez a) [value := oposto e]
							vez <- get (ambVez a) value
							atualizaVez a
							--putStr (show e ++ show (oposto est) ++ show m)
							if(m == 1 && vez == Cabra)
								then do
									if (verificaCabras t1 count) == False 
										then do
											somVitoria (ambSom a)
											infoDialog (ambFrm a) dlgConcluidoT dlgVT
											resp <- confirmDialog (ambFrm a) dlgNovoJogoT dlgNovoJogo True
											if (resp)
												then do
													novoJogoOneP a m est
												else do
													fecharJogo a
										else do
											if(count < 20)
												then do
													--t3 <- get (ambTbl a) value
													t2 <- inserirCabraCPU a t1
													set (ambCon a) [value := count+1]
													count <- get (ambCon a) value
													atualiza a t2 (ambPos a)
													--putStr (show t2)
													set (ambVez a) [value := oposto Cabra]
													atualizaVez a
													ativaJogo a t2 (ambPos a) e
												else do
													--t3 <- get (ambTbl a) value
													t2 <- movimentoCabraCPU a t1
													atualiza a t2 (ambPos a)
													--putStr (show t2)
													set (ambVez a) [value := oposto Cabra]
													atualizaVez a
													ativaJogo a t2 (ambPos a) e
								else do
									--set (ambVez a) [value := oposto e]
									--atualizaVez a
									ativaJogo a t1 (ambPos a) e
						else do
							av <- get (ambAvs a) value
							if (av)
								then do
									warningDialog (ambFrm a) dlgInvalidaT dlgInvalida
								else do
									return ()
										
										
movimentarPeca :: Ambiente -> Tabuleiro -> [Panel()] -> Estado -> (Int, Int) -> [Int] -> IO()
movimentarPeca _ [] [] _ _ _ = do {return ()}
movimentarPeca a ((x,y,e,d):ts) (p:ps) est (iniX, iniY) dir = do
		set p [on click := movimentacao a (x,y) (iniX, iniY) est dir]
		movimentarPeca a ts ps est (iniX, iniY) dir	

-- Funções para mudar parâmetros do jogo
								
-- Ativa a ação do click nas posições do tabuleiro
ativaJogo :: Ambiente -> Tabuleiro -> [Panel()] -> Estado -> IO()
ativaJogo _ [] [] _ = do {return ()}
ativaJogo a ((x,y,e,d):ts) (p:ps) est = do
		set p [on click := jogar a (x,y) est gerarDirecao]
		ativaJogo a  ts ps est

-- Desativa a ação do click nas posições do tabuleiro	
desativaJogo :: [Panel ()] -> IO ()
desativaJogo [] = do {return ()}
desativaJogo (p:ps) = do
	set p [on click := nada]
	desativaJogo ps
	where
		nada _ = do {return ()}
		
-- Inicia um novo jogo de 2 players
novoJogoTwoP :: Ambiente -> Int -> Estado -> IO()
novoJogoTwoP a m e = do
	set (ambTbl a) [value := tabInicial]
	somInicio (ambSom a)
	t0 <- get (ambTbl a) value
	ativaJogo a t0 (ambPos a) e
	atualiza a t0 (ambPos a)
	set (ambFch a) [enabled := True]
	set (ambMod a) [value := m]
	set (ambTbl a) [value := tabInicial]
	set (ambVez a) [value := Cabra]
	set (ambCon a) [value := 0]
	atualizaVez a		
			
-- Inicia um novo jogo de 1 player
novoJogoOneP :: Ambiente -> Int -> Estado -> IO()
novoJogoOneP a m e = do
	set (ambTbl a) [value := tabAleatorio]
	somInicio (ambSom a)
	t0 <- get (ambTbl a) value
	ativaJogo a t0 (ambPos a) e
	atualiza a t0 (ambPos a)
	set (ambFch a) [enabled := True]
	set (ambMod a) [value := m]
	set (ambTbl a) [value := tabAleatorio]
	set (ambVez a) [value := Tigre]
	set (ambCon a) [value := 1]
	atualizaVez a		
		
-- Pergunta se o usuário deseja iniciar um novo jogo de 2 players
novoJogo2P :: Ambiente -> Int -> Estado -> IO()
novoJogo2P a m e = do
    resp <- confirmDialog (ambFrm a) dlgNovoJogoT dlgNovoJogo True
    if (resp)
        then do
            novoJogoTwoP a m e
        else do
            return ()
			
-- Pergunta se o usuário deseja iniciar um novo jogo de 1 player
novoJogo1P :: Ambiente -> Int -> Estado -> IO()
novoJogo1P a m e = do
    resp <- confirmDialog (ambFrm a) dlgNovoJogoT dlgNovoJogo True
    if (resp)
        then do
            novoJogoOneP a m e
        else do
            return ()
			
-- Encerra a partida ativa, caso exista			
fecharJogo :: Ambiente -> IO ()
fecharJogo a = do
	desativaJogo (ambPos a)
	atualiza a tabZerado (ambPos a)
	set (ambFch a) [enabled := False]
	set (ambMod a) [value := 0]
	set (ambTbl a) [value := tabZerado]
	set (ambCon a) [value := 0]
	set (ambVez a) [value := Vazio]
	atualizaVez a

-- Pergunta se o usuário deseja encerrar a partida ativa	
fecharJogoP :: Ambiente -> IO ()
fecharJogoP a = do
    resp <- confirmDialog (ambFrm a) dlgFecharT dlgFechar False
    if (resp)
        then do
            fecharJogo a
        else do
            return ()

-- Muda a variável que avisa quando uma jogada é inválida			
mudaAviso :: Var Bool -> MenuItem () -> IO ()
mudaAviso a m = do
    av <- get a value
    if (av)
        then do
            set a [value := False]
            set m [checked := False]
        else do
            set a [value := True]
            set m [checked := True]

-- Muda a variável que ativa ou desativa o som			
mudaSom :: Var Bool -> MenuItem() -> IO ()
mudaSom a m = do
	s <- get a value
	if(s)
		then do
			set a [value := False]
			set m [checked := False]
		else do
			set a [value := True]
			set m [checked := True]


-- Concatena das strings e retorna			
juntaString :: String -> String -> String
juntaString a b = a ++ b
