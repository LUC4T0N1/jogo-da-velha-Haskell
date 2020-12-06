module PvP.InterfacePvP where

import Compartilhado.Compartilhado
import Compartilhado.Interface
import Graphics.Gloss
import PvP.JogoPvP
import PvP.LogicaPvP

corX :: Color
corX = makeColorI 255 255 0 255

corO :: Color
corO = makeColorI 0 128 0 255

tabuleiroJogoRolando :: Tabuleiro -> Picture
tabuleiroJogoRolando board =
  pictures
    [ color corX $ figuras board (X) x,
      color corO $ figuras board (O) (thickCircle (min larguraQuadrado alturaQuadrado * 0.25) 10.0),
      color corTabuleiro $ divisaoQuadrados
    ]

corFimDeJogo :: Maybe JogadorPvP -> Color
corFimDeJogo (Just JX) = corX
corFimDeJogo (Just JO) = corO
corFimDeJogo Nothing = corEmpate

tabuleiroFimDeJogo :: Maybe JogadorPvP -> Tabuleiro -> Picture
tabuleiroFimDeJogo resultado tabuleiro = color (corFimDeJogo resultado) (imagemTabuleiro tabuleiro)

controladorInterfacePvP :: JogoPvP -> Picture
controladorInterfacePvP jogo =
  translate
    (fromIntegral larguraTela * (-0.5))
    (fromIntegral alturaTela * (-0.5))
    frame
  where
    frame = case estado jogo of
      Jogando -> tabuleiroJogoRolando (tabuleiro jogo)
      Fim winner -> tabuleiroFimDeJogo (alguemVenceu (tabuleiro jogo)) (tabuleiro jogo)