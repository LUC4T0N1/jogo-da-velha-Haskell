module PvC.InterfacePvC where

import Compartilhado.Compartilhado
import Compartilhado.Interface
import Graphics.Gloss
import PvC.JogoPvC
import PvC.LogicaPvC (alguemVenceu)

corIA :: Color
corIA = makeColorI 255 165 0 255

corJogador :: Color
corJogador = makeColorI 128 0 128 255

tabuleiroJogoRolando :: Tabuleiro -> Picture
tabuleiroJogoRolando board =
  pictures
    [ color corIA $ figuras board (X) x,
      color corJogador $ figuras board (O) (thickCircle (min larguraQuadrado alturaQuadrado * 0.25) 10.0),
      color corTabuleiro $ divisaoQuadrados
    ]

corFimDeJogo :: Maybe JogadorPvC -> Color
corFimDeJogo (Just J) = corIA
corFimDeJogo (Just IA) = corJogador
corFimDeJogo Nothing = corEmpate

tabuleiroFimDeJogo :: Maybe JogadorPvC -> Tabuleiro -> Picture
tabuleiroFimDeJogo resultado tabuleiro = color (corFimDeJogo resultado) (imagemTabuleiro tabuleiro)

controladorInterfacePvC :: JogoPvC -> Picture
controladorInterfacePvC jogo =
  translate
    (fromIntegral larguraTela * (-0.5))
    (fromIntegral alturaTela * (-0.5))
    frame
  where
    frame = case estado jogo of
      JogadorJogando -> tabuleiroJogoRolando (tabuleiro jogo)
      IAFacilJogando -> tabuleiroJogoRolando (tabuleiro jogo)
      IADificilJogando -> tabuleiroJogoRolando (tabuleiro jogo)
      Fim winner -> tabuleiroFimDeJogo (alguemVenceu (tabuleiro jogo)) (tabuleiro jogo)