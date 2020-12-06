module PvP.LogicaPvP where

import Compartilhado.Compartilhado
import Compartilhado.Interface
import Compartilhado.Logica
import Data.Array
import Data.Char (isDigit)
import Data.Foldable (asum)
import Data.List (transpose)
import Graphics.Gloss.Interface.Pure.Game
import PvP.JogoPvP

mudarJogador :: JogoPvP -> JogoPvP
mudarJogador game =
  if (jogador game == JX)
    then game {jogador = JO}
    else game {jogador = JX}

alguemVenceu :: Tabuleiro -> Maybe JogadorPvP
alguemVenceu tabuleiro
  | vencer O tabuleiro == True = Just JO
  | vencer X tabuleiro == True = Just JX
  | otherwise = Nothing

verificaFim :: JogoPvP -> JogoPvP
verificaFim jogo
  | Just p <- alguemVenceu (tabuleiro jogo) =
    jogo {estado = Fim $ Just p}
  | cheio (tabuleiro jogo) == True =
    jogo {estado = Fim Nothing}
  | otherwise = jogo

jogadorVsJogador :: JogoPvP -> (Int, Int) -> JogoPvP
jogadorVsJogador jogo coordenada =
  if ((validarCoordenada coordenada) && (quadradoVazio (tabuleiro jogo) (manipulaCoord coordenada) == True))
    then
      verificaFim $
        mudarJogador $
          jogo {tabuleiro = jogada (tabuleiro jogo) (manipulaCoord coordenada) quadrado}
    else jogo
  where
    quadrado =
      if (jogador jogo == JO)
        then O
        else X

controlaJogadas :: Event -> JogoPvP -> JogoPvP
controlaJogadas (EventKey (MouseButton LeftButton) Up _ clique) jogo =
  case estado jogo of
    Jogando -> jogadorVsJogador jogo $ coordenadaClicada clique
    Fim _ -> comecoPvP
controlaJogadas _ jogo = jogo