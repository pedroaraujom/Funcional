module Mensagens where

-- Funções para atualização do tabuleiro

-- Título da janela
tituloJanela :: String
tituloJanela = "Bagha Chall"

-- Textos do Menu

-- Jogo
menuJogo :: String
menuJogo = "&Jogo"

menuNovoJogoCPU :: String
menuNovoJogoCPU = "Novo jogo - contra CPU"
ajudaNovoJogoCPU :: String
ajudaNovoJogoCPU = "Inicia um novo jogo contra a CPU"

menuNovoJogo2P :: String
menuNovoJogo2P = "Novo jogo - 2 jogadores"
ajudaNovoJogo2P :: String
ajudaNovoJogo2P = "Inicia um novo jogo para 2 jogadores"

menuFecha :: String
menuFecha = "Encerrar partida"
ajudaFecha :: String
ajudaFecha = "Encerra a partida atual"

menuSair :: String
menuSair = "Sair"
ajudaSair :: String
ajudaSair = "Sai do Bagha Chall"

-- Opções
menuOpcoes :: String
menuOpcoes = "&Opções"

menuAvisar :: String
menuAvisar = "Avisar jogada inválida"
ajudaAvisar :: String
ajudaAvisar = "Exibição de uma mensagem quando a jogada for inválida"

menuSons :: String
menuSons = "Ativar/Desativar sons"
ajudaSons :: String
ajudaSons = "Ativa ou desativa os sons do jogo"

menuSkins :: String
menuSkins = "Selecionar skin"

menuSkin1 :: String
menuSkin1 = "1. Grama"
ajudaSkin1 :: String
ajudaSkin1 = "Muda o skin para a versão padrão"

menuSkin2 :: String
menuSkin2 = "2. Madeira"
ajudaSkin2 :: String
ajudaSkin2 = "Muda o skin para o versão clássica"

menuSkin3 :: String
menuSkin3 = "2. Gelo"
ajudaSkin3 :: String
ajudaSkin3 = "Muda o skin para a versão gelo"

-- Ajuda
menuAjuda :: String
menuAjuda = "&Ajuda"

menuRegras :: String
menuRegras = "Regras do Jogo"
ajudaRegras :: String
ajudaRegras = "Mostra as regras do jogo"

menuSobre :: String
menuSobre = "Sobre..."
ajudaSobre :: String
ajudaSobre = "Sobre o Bagha Chall"

ajudaResultado :: String
ajudaResultado = "Exibe os ultimos resultados"

-- Mensagens de Ajuda

-- Regras do Jogo
msgRegrasTitulo :: String
msgRegrasTitulo = "Regras do Jogo"

msgRegras :: String
msgRegras =
	"REGRAS\n" ++
	"Os tigres devem se movimentar seguindo as seguintes regras:\n" ++
	"Podem capturar as cabras em qualquer momento após o início da partida;\n" ++
	"Devem capturar uma cabra sempre que for possível;\n" ++
	"Pode comer mais de uma cabra por rodada, uma de cada vez;\n" ++
	"Devem pular sobre uma cabra em qualquer direção desde que, o pulo seja para " ++
	"uma intersecção adjacente seguindo qualquer uma das linhas que partem da " ++
	"posição em que estavam para capturá-las;\n" ++
	"Não podem pular sobre outros tigres.\n" ++
	"\n" ++
	"As cabras devem se movimentar seguindo as seguintes regras:\n" ++
	"Devem sair do jogo quando capturadas;\n" ++
	"Não podem pular sobre os tigres ou sobre outras cabras;\n" ++
	"Só poderão movimentar-se após as vinte cabras terem entrado no tabuleiro.\n" ++
	"\n" ++
	"Veja os detalhes do jogo em um vídeo no youtube em http://tuit.in/dSl \n\n"
	
--Mostra resultado
msgResultadoTitulo :: String
msgResultadoTitulo = "Resultados"

-- Sobre o jogo Bagha Chall
msgSobreTitulo :: String
msgSobreTitulo = "Sobre o Bagha Chall"

msgSobre :: String
msgSobre =
	"Bagha Chall 1.0\n" ++
    "\n" ++
    "Desenvolvido por:\n" ++
    "Igor Rafael Guimarães Medeiros\n" ++
    "José Victor de Macedo Araújo\n" ++
    "Pedro de Araújo Melo\n" ++
    "Pedro Victor Arcuri Costa\n" ++
	"Victor Hugo de Carvalho Amorim\n"

-- Nome das jogadas

strT:: String
strT = "vezT"

strC :: String
strC = "vezC"

strVazio :: String
strVazio = "vazio"

-- Mensagens dos diálogos

-- Jogada inválida
dlgInvalidaT :: String
dlgInvalidaT = "Jogada Inválida!"

dlgInvalida :: String
dlgInvalida = "Movimentação inválida. Jogue novamente."

dlgInvalidaT1 :: String
dlgInvalidaT1 = "Movimentação inválida, existe uma cabra a ser comida. Jogue novamente."

dlgInvalidaT2 :: String
dlgInvalidaT2 = "Tigre inválido, existe uma cabra a ser comida. Jogue novamente."

dlgInvalidaT3 :: String
dlgInvalidaT3 = "Movimentação inválida, existe uma cabra a ser comida. Escolha o tigre correto."

dlgInvalidaT4 :: String
dlgInvalidaT4 = "Este tigre não pode se mover, está cercado. Escolha outro tigre correto."

dlgInvalidaC :: String
dlgInvalidaC = "Você não pode inserir nessa posição. Jogue novamente."

-- Novo jogo
dlgNovoJogoT :: String
dlgNovoJogoT = "Novo Jogo?"

dlgNovoJogo :: String
dlgNovoJogo = "Deseja iniciar um novo jogo?"

-- Encerrar jogo
dlgFecharT :: String
dlgFecharT = "Encerrar Partida?"

dlgFechar :: String
dlgFechar = "Deseja encerrar a partida atual?"

-- Jogo concluído
dlgConcluidoT :: String
dlgConcluidoT = "Jogo Concluído!"

dlgVT:: String
dlgVT = "Tigre, você venceu! \n Parabéns!!!"

dlgVC :: String
dlgVC = "Cabra, você venceu! \n Parabéns!!!"

