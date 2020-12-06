module PvC.LogicaPvC where

import Compartilhado.Compartilhado
import Compartilhado.Interface
import Compartilhado.Logica
import Data.Array
import Data.Char (isDigit)
import Data.Foldable (asum)
import Graphics.Gloss.Interface.Pure.Game
import PvC.JogoPvC

mudarJogador :: JogoPvC -> JogoPvC
mudarJogador game =
  if (jogador game == J)
    then game {jogador = IA}
    else game {jogador = J}

alguemVenceu :: Tabuleiro -> Maybe JogadorPvC
alguemVenceu tabuleiro
  | vencer O tabuleiro == True = Just IA
  | vencer X tabuleiro == True = Just J
  | otherwise = Nothing

verificaFim :: JogoPvC -> JogoPvC
verificaFim jogo
  | Just p <- alguemVenceu (tabuleiro jogo) =
    jogo {estado = Fim $ Just p}
  | cheio (tabuleiro jogo) == True =
    jogo {estado = Fim Nothing}
  | otherwise = jogo

verificaQuadrado :: JogoPvC -> Quadrado
verificaQuadrado jogo =
  if (jogador jogo == J)
    then X
    else O

jogadorVsDificil :: JogoPvC -> (Int, Int) -> JogoPvC
jogadorVsDificil jogo coordenada =
  if ((validarCoordenada coordenada) && (quadradoVazio (tabuleiro jogo) (manipulaCoord coordenada) == True))
    then
      verificaFim $
        mudarJogador $
          jogo {tabuleiro = jogada (tabuleiro jogo) (manipulaCoord coordenada) (verificaQuadrado jogo), estado = IADificilJogando}
    else jogo

jogadorVsFacil :: JogoPvC -> (Int, Int) -> JogoPvC
jogadorVsFacil jogo coordenada =
  if ((validarCoordenada coordenada) && (quadradoVazio (tabuleiro jogo) (manipulaCoord coordenada) == True))
    then
      verificaFim $
        mudarJogador $
          jogo {tabuleiro = jogada (tabuleiro jogo) (manipulaCoord coordenada) (verificaQuadrado jogo), estado = IAFacilJogando}
    else jogo

iaFacil :: JogoPvC -> JogoPvC
iaFacil jogo =
  verificaFim $
    mudarJogador $
      jogo {tabuleiro = escolheJogadaPerder (tabuleiro jogo) (verificaQuadrado jogo), estado = JogadorJogando}

iaDificil :: JogoPvC -> JogoPvC
iaDificil jogo =
  verificaFim $
    mudarJogador $
      jogo {tabuleiro = escolheJogadaNaoPerder (tabuleiro jogo) (verificaQuadrado jogo), estado = JogadorJogando}

controlaJogadasFacil :: Event -> JogoPvC -> JogoPvC
controlaJogadasFacil (EventKey (MouseButton LeftButton) Up _ clique) jogo =
  case estado jogo of
    JogadorJogando -> jogadorVsFacil jogo $ coordenadaClicada clique
    IAFacilJogando -> iaFacil jogo
    Fim _ -> comecoPvC
controlaJogadasFacil _ jogo = jogo

controlaJogadasDificil :: Event -> JogoPvC -> JogoPvC
controlaJogadasDificil (EventKey (MouseButton LeftButton) Up _ clique) jogo =
  case estado jogo of
    JogadorJogando -> jogadorVsDificil jogo $ coordenadaClicada clique
    IADificilJogando -> iaDificil jogo
    Fim _ -> comecoPvC
controlaJogadasDificil _ jogo = jogo

data Arvore a = No a [Arvore a]

arvoreDeJogos :: Tabuleiro -> Quadrado -> Arvore Tabuleiro
arvoreDeJogos tabuleiro quadrado = No tabuleiro [arvoreDeJogos g' (outro quadrado) | g' <- andar tabuleiro quadrado]
  where
    outro p
      | p == X = O
      | otherwise = X

mover :: Tabuleiro -> Int -> Quadrado -> [Tabuleiro]
mover t i q =
  if quadradoVazio t i
    then [cortar 3 (xs ++ [q] ++ ys)]
    else []
  where
    (xs, Vazio : ys) = splitAt i (concat t)

andar :: Tabuleiro -> Quadrado -> [Tabuleiro]
andar tabuleiro quadrado
  | vencer O tabuleiro || vencer X tabuleiro = []
  | cheio tabuleiro = []
  | otherwise = concat [mover tabuleiro i quadrado | i <- [0 .. ((9) -1)]]

arrancar :: Int -> Arvore a -> Arvore a
arrancar 0 (No x _) = No x []
arrancar n (No x ts) = No x [arrancar (n -1) t | t <- ts]

minimaxPerder :: Arvore Tabuleiro -> Arvore (Tabuleiro, Quadrado)
minimaxPerder (No t [])
  | vencer O t = No (t, O) []
  | vencer X t = No (t, X) []
  | otherwise = No (t, Vazio) []
minimaxPerder (No t ts)
  | vezDeQuem t == O = No (t, minimum ps) ts'
  | vezDeQuem t == X = No (t, maximum ps) ts'
  where
    ts' = map minimaxPerder ts
    ps = [p | No (_, p) _ <- ts']

escolheJogadaPerder :: Tabuleiro -> Quadrado -> Tabuleiro
escolheJogadaPerder tabuleiro quadrado = head [t' | No (t', q') _ <- ts, q' == best]
  where
    tree = arrancar 9 (arvoreDeJogos tabuleiro quadrado)
    No (_, best) ts = minimaxPerder tree

minimaxNaoPerder :: Arvore Tabuleiro -> Arvore (Tabuleiro, Quadrado)
minimaxNaoPerder (No t [])
  | vencer O t = No (t, O) []
  | vencer X t = No (t, X) []
  | otherwise = No (t, Vazio) []
minimaxNaoPerder (No t ts)
  | vezDeQuem t == O = No (t, maximum ps) ts'
  | vezDeQuem t == X = No (t, minimum ps) ts'
  where
    ts' = map minimaxNaoPerder ts
    ps = [p | No (_, p) _ <- ts']

escolheJogadaNaoPerder :: Tabuleiro -> Quadrado -> Tabuleiro
escolheJogadaNaoPerder tabuleiro quadrado = head [t' | No (t', q') _ <- ts, q' == best]
  where
    tree = arrancar 9 (arvoreDeJogos tabuleiro quadrado)
    No (_, best) ts = minimaxNaoPerder tree

vezDeQuem :: Tabuleiro -> Quadrado
vezDeQuem t =
  if length (filter (== O) (concat t)) < length (filter (== X) (concat t))
    then O
    else X
