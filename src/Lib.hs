module Lib
    ( tetris
    ) where

import qualified Data.List as L

tetris :: IO ()
tetris = do
  putStrLn "Welcome to tetris"
  pieces <- return [Piece I 0, Piece S 0, Piece Z 0, Piece L 0, Piece J 0, Piece O 0, Piece T 0]
  rots <- return [0..3]
  putStrLn $ concat $ L.intersperse "\n" $ show <$> (rotate <$> rots <*> (matrixify <$> pieces))

data Segment = Block | Blank

data Row t = Row t t t t

data Matrix t = Matrix (Row t) (Row t) (Row t) (Row t)

instance Show Segment where
  show Block = "#"
  show Blank = "-"

instance (Show t) => Show (Row t) where
  show (Row a b c d) = show a ++ show b ++ show c ++ show d

instance (Show t) => Show (Matrix t) where
  show (Matrix a b c d) = show a ++ "\n" ++ show b ++ "\n" ++ show c ++ "\n" ++ show d ++ "\n"

type Rot = Int
data Letter = I | S | Z | L | J | O | T
data Piece = Piece Letter Rot

rotate 0 m = m
rotate turns (Matrix
  (Row a b c d)
  (Row e f g h)
  (Row i j k l)
  (Row m n o p)) = rotate (turns - 1) (Matrix
  (Row m i e a)
  (Row n j f b)
  (Row o k g c)
  (Row p l h d))

basematrix (Piece I _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Block Block Block Block)
  (Row Blank Blank Blank Blank)
  (Row Blank Blank Blank Blank)
basematrix (Piece S _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Blank Block Block Blank)
  (Row Block Block Blank Blank)
  (Row Blank Blank Blank Blank)
basematrix (Piece Z _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Block Block Blank Blank)
  (Row Blank Block Block Blank)
  (Row Blank Blank Blank Blank)
basematrix (Piece L _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Blank Blank Blank Block)
  (Row Blank Block Block Block)
  (Row Blank Blank Blank Blank)
basematrix (Piece J _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Block Blank Blank Blank)
  (Row Block Block Block Blank)
  (Row Blank Blank Blank Blank)
basematrix (Piece O _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Blank Block Block Blank)
  (Row Blank Block Block Blank)
  (Row Blank Blank Blank Blank)
basematrix (Piece T _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Block Block Block Blank)
  (Row Blank Block Blank Blank)
  (Row Blank Blank Blank Blank)

matrixify piece@(Piece _ rot) = rotate rot $ basematrix piece

instance Show Piece where
  show piece = show $ matrixify piece
