module Compartilhado.Interface where

import Compartilhado.Compartilhado
import Graphics.Gloss

larguraTela :: Int
larguraTela = 900

alturaTela :: Int
alturaTela = 900

larguraQuadrado :: Float
larguraQuadrado = fromIntegral larguraTela / fromIntegral 3

alturaQuadrado :: Float
alturaQuadrado = fromIntegral alturaTela / fromIntegral 3

corTabuleiro :: Color
corTabuleiro = makeColorI 99 99 99 255

corEmpate :: Color
corEmpate = makeColorI 99 99 99 255

adicionarImagemQuadrado :: (Integral a, Integral b) => Picture -> (a, b) -> Picture
adicionarImagemQuadrado imagem (linha, coluna) = translate x y imagem
  where
    x = fromIntegral coluna * larguraQuadrado + larguraQuadrado * 0.5
    y = fromIntegral linha * alturaQuadrado + alturaQuadrado * 0.5

x :: Picture
x =
  pictures
    [ rotate 45.0 $ rectangleSolid (min larguraQuadrado alturaQuadrado * 0.75) 10.0,
      rotate (-45.0) $ rectangleSolid (min larguraQuadrado alturaQuadrado * 0.75) 10.0
    ]

figuras :: Tabuleiro -> Quadrado -> Picture -> Picture
figuras tabuleiro quadrado imagem =
  pictures $
    map (adicionarImagemQuadrado imagem . fst) $
      filter (\(_, e) -> e == quadrado) $
        (manipularTabuleiro tabuleiro 0)

intParaCoord :: Int -> (Integer, Integer)
intParaCoord i
  | i == 0 = (0, 0)
  | i == 1 = (0, 1)
  | i == 2 = (0, 2)
  | i == 3 = (1, 0)
  | i == 4 = (1, 1)
  | i == 5 = (1, 2)
  | i == 6 = (2, 0)
  | i == 7 = (2, 1)
  | i == 8 = (2, 2)

manipularTabuleiro :: Tabuleiro -> Int -> [((Integer, Integer), Quadrado)]
manipularTabuleiro t i
  | i > 8 = []
  | otherwise = ((intParaCoord i), concat t !! i) : (manipularTabuleiro t (i + 1))

divisaoQuadrados :: Picture
divisaoQuadrados =
  pictures $
    concatMap
      ( \i ->
          [ line
              [ (i * larguraQuadrado, 0.0),
                (i * larguraQuadrado, fromIntegral alturaTela)
              ],
            line
              [ (0.0, i * alturaQuadrado),
                (fromIntegral larguraTela, i * alturaQuadrado)
              ]
          ]
      )
      [0.0 .. fromIntegral 3]

imagemTabuleiro :: Tabuleiro -> Picture
imagemTabuleiro tabuleiro =
  pictures
    [ figuras tabuleiro (X) x,
      figuras tabuleiro (O) (thickCircle (min larguraQuadrado alturaQuadrado * 0.25) 10.0),
      divisaoQuadrados
    ]
