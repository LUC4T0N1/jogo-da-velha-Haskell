module PvP.JogoPvP where

import Compartilhado.Compartilhado (Quadrado (Vazio), Tabuleiro)

data JogadorPvP = JX | JO deriving (Eq, Show)

data EstadoPvP = Jogando | Fim (Maybe JogadorPvP) deriving (Eq, Show)

data JogoPvP = Jogo
  { tabuleiro :: Tabuleiro,
    jogador :: JogadorPvP,
    estado :: EstadoPvP
  }
  deriving (Eq, Show)

comecoPvP :: JogoPvP
comecoPvP =
  Jogo
    { tabuleiro = replicate 3 (replicate 3 Vazio),
      jogador = JX,
      estado = Jogando
    }