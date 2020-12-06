module PvC.JogoPvC where

import Compartilhado.Compartilhado (Quadrado (Vazio), Tabuleiro)

data JogadorPvC = J | IA deriving (Eq, Show)

data EstadoPvC = JogadorJogando | IAFacilJogando | IADificilJogando | Fim (Maybe JogadorPvC) deriving (Eq, Show)

data JogoPvC = Jogo
  { tabuleiro :: Tabuleiro,
    jogador :: JogadorPvC,
    estado :: EstadoPvC
  }
  deriving (Eq, Show)

comecoPvC :: JogoPvC
comecoPvC =
  Jogo
    { tabuleiro = replicate 3 (replicate 3 Vazio),
      jogador = J,
      estado = JogadorJogando
    }