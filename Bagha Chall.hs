module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Mensagens
import Tipos
import Regras
import Auxiliar

main :: IO()
main = start gui

gui :: IO()
gui = do
	
	-- Cria a janela do jogo o parametro position equivale a posição na tela que ela aparecerá
	f 			<- frameFixed [text := tituloJanela, picture := "baghachall.ico",position:= pt 450 150]
	
	-- Cria os paineis da janela
	p3			<- panel f [clientSize := sz 320 38] -- painel do titulo
	p1 			<- panel f [clientSize := sz 320 384]
	p2			<- panel f [clientSize := sz 320 90] -- painel da vez do jogador
	
	-- Um painel para casa posição no tabuleiro
	p_1_1		<- panel p1 [clientSize := sz 64 64]
	p_1_2		<- panel p1 [clientSize := sz 64 64]
	p_1_3		<- panel p1 [clientSize := sz 64 64]
	p_1_4		<- panel p1 [clientSize := sz 64 64]
	p_1_5		<- panel p1 [clientSize := sz 64 64]
	
	p_2_1		<- panel p1 [clientSize := sz 64 64]
	p_2_2		<- panel p1 [clientSize := sz 64 64]
	p_2_3		<- panel p1 [clientSize := sz 64 64]
	p_2_4		<- panel p1 [clientSize := sz 64 64]
	p_2_5		<- panel p1 [clientSize := sz 64 64]
	
	p_3_1		<- panel p1 [clientSize := sz 64 64]
	p_3_2		<- panel p1 [clientSize := sz 64 64]
	p_3_3		<- panel p1 [clientSize := sz 64 64]
	p_3_4		<- panel p1 [clientSize := sz 64 64]
	p_3_5		<- panel p1 [clientSize := sz 64 64]
	
	p_4_1		<- panel p1 [clientSize := sz 64 64]
	p_4_2		<- panel p1 [clientSize := sz 64 64]
	p_4_3		<- panel p1 [clientSize := sz 64 64]
	p_4_4		<- panel p1 [clientSize := sz 64 64]
	p_4_5		<- panel p1 [clientSize := sz 64 64]
	
	p_5_1		<- panel p1 [clientSize := sz 64 64]
	p_5_2		<- panel p1 [clientSize := sz 64 64]
	p_5_3		<- panel p1 [clientSize := sz 64 64]
	p_5_4		<- panel p1 [clientSize := sz 64 64]
	p_5_5		<- panel p1 [clientSize := sz 64 64]
	
	-- MENU
	
	-- Jogo
	mJogo		<- menuPane [text := menuJogo]
	mNovo1		<- menuItem mJogo [text := menuNovoJogoCPU, help := ajudaNovoJogoCPU]
	mNovo2		<- menuItem mJogo [text := menuNovoJogo2P, help := ajudaNovoJogo2P]
	mFecha		<- menuItem mJogo [text := menuFecha, help := ajudaFecha]
	mJogoL		<- menuLine mJogo --separador
	mSair 		<- menuQuit mJogo [text := menuSair, help := ajudaSair]
	
	-- Opções
	mOpcoes		<- menuPane [text := menuOpcoes]
	mAvisar		<- menuItem mOpcoes [text := menuAvisar, help := ajudaAvisar]
	mSons		<- menuItem mOpcoes [text := menuSons, help := ajudaSons]
	mJogoL		<- menuLine mOpcoes --separador
	mSkins		<- menuPane [text := menuSkins]
	mSkinsSub	<- menuSub mOpcoes mSkins []
	mSkin1		<- menuRadioItem mSkins [text := menuSkin1, help := ajudaSkin1]
	mSkin2		<- menuRadioItem mSkins [text := menuSkin2, help := ajudaSkin2]
	mSkin3		<- menuRadioItem mSkins [text := menuSkin3, help := ajudaSkin3]

	-- Ajuda
	mAjuda		<- menuHelp [text := menuAjuda]
	mRegras		<- menuItem mAjuda [text := menuRegras, help := ajudaRegras]
	mSobre		<- menuAbout mAjuda [text := menuSobre, help := ajudaSobre]
	
	-- Barra de status
	status		<- statusField [text := ""]
	
	-- Variáveis do jogo
	
	-- Nenhum = 0
	-- Contra CPU = 1
	-- 2 jogadores = 2
	modo		<- variable [value := 0]
	
	-- Liga e desliga o aviso de jogada inválida
	aviso		<- variable [value := True]
	
	-- Armazena o tabuleiro atual do jogo
	tabuleiro	<- variable [value := tabZerado]
	
	-- Armazena a vez de jogar
	vez			<- variable [value := Vazio]
	
	-- Armazena a skin utilizada no momento
	skin		<- variable [value := ""]
	
	-- Lista de posições para ser utilizada por outras funções
	posicoes	<- toIO [p_1_1, p_1_2, p_1_3, p_1_4, p_1_5,
                         p_2_1, p_2_2, p_2_3, p_2_4, p_2_5,
                         p_3_1, p_3_2, p_3_3, p_3_4, p_3_5,
                         p_4_1, p_4_2, p_4_3, p_4_4, p_4_5,
                         p_5_1, p_5_2, p_5_3, p_5_4, p_5_5]
						
	-- Ativa ou desativa os sons do jogo
	sons		<- variable [value := True]
	
	-- Contador das cabras já inseridas
	contador	<- variable [value := 0]
	
	-- Cria a estrutura Ambiente com variáveis e elementos do jogo
	amb 		<- toIO (f, tabuleiro, modo, vez, aviso, skin, posicoes, p3, p1, p2, mFecha, sons, contador)
	
	--Propriedades do Elementos
	
	-- Menu Jogo
	set mSair 	[on command := close f]
	set mNovo1	[on command := novoJogo1P amb 1 Tigre]
	set mNovo2 [on command := novoJogo2P amb 2 Cabra]
	set mFecha 	[on command := fecharJogoP amb, enabled := False]
	
	-- Menu Opções
	set mAvisar [on command := mudaAviso aviso mAvisar, checkable := True, checked := True]
	set mSons [on command := mudaSom sons mSons, checkable := True, checked := True]
	set mSkin1 [on command := aplicaSkin amb "padrao", checked := True]
	set mSkin2 [on command := aplicaSkin amb "madeira"]
	set mSkin3 [on command := aplicaSkin amb "gelo"]
	
	-- Menu Ajuda
	set mRegras [on command := infoDialog f msgRegrasTitulo msgRegras]
	set mSobre [on command := infoDialog f msgSobreTitulo msgSobre]
	
	-- Define a disposição dos elementos na janela
	set f [statusBar := [status],
		menuBar := [mJogo, mOpcoes, mAjuda],
		layout := column 0 [row 0 [widget p3], row 1 [widget p1], row 2 [widget p2]],
		clientSize := sz 320 396]

	-- Define a disposição dos elementos no tabuleiro
	set p1 [layout := floatCentre $ column 0 [
			row 1 [row 0 [widget p_1_1, widget p_1_2, widget p_1_3, widget p_1_4, widget p_1_5]],
			row 2 [row 0 [widget p_2_1, widget p_2_2, widget p_2_3, widget p_2_4, widget p_2_5]],
			row 3 [row 0 [widget p_3_1, widget p_3_2, widget p_3_3, widget p_3_4, widget p_3_5]],
			row 4 [row 0 [widget p_4_1, widget p_4_2, widget p_4_3, widget p_4_4, widget p_4_5]],
			row 5 [row 0 [widget p_5_1, widget p_5_2, widget p_5_3, widget p_5_4, widget p_5_5]]],
			clientSize := sz 320 384]
	
	-- Aplica a skin Padrão
	aplicaSkin amb "padrao"