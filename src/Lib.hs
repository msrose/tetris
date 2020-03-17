module Lib
    ( tetris
    ) where

import qualified Data.List as L

tetris :: IO ()
tetris = do
  putStrLn "Welcome to tetris"
  pieces <- return [IPiece 0, SPiece 0, ZPiece 0, LPiece 0, JPiece 0, OPiece 0, TPiece 0]
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

data Piece = IPiece Rot | SPiece Rot | ZPiece Rot | LPiece Rot | JPiece Rot | OPiece Rot | TPiece Rot

rotate 0 m = m
rotate turns (Matrix (Row a b c d) (Row e f g h) (Row i j k l) (Row m n o p)) = rotate (turns - 1) (Matrix
  (Row m i e a)
  (Row n j f b)
  (Row o k g c)
  (Row p l h d))

basematrix (IPiece _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Block Block Block Block)
  (Row Blank Blank Blank Blank)
  (Row Blank Blank Blank Blank)
basematrix (SPiece _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Blank Block Block Blank)
  (Row Block Block Blank Blank)
  (Row Blank Blank Blank Blank)
basematrix (ZPiece _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Block Block Blank Blank)
  (Row Blank Block Block Blank)
  (Row Blank Blank Blank Blank)
basematrix (LPiece _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Blank Blank Blank Block)
  (Row Blank Block Block Block)
  (Row Blank Blank Blank Blank)
basematrix (JPiece _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Block Blank Blank Blank)
  (Row Block Block Block Blank)
  (Row Blank Blank Blank Blank)
basematrix (OPiece _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Blank Block Block Blank)
  (Row Blank Block Block Blank)
  (Row Blank Blank Blank Blank)
basematrix (TPiece _) = Matrix
  (Row Blank Blank Blank Blank)
  (Row Block Block Block Blank)
  (Row Blank Block Blank Blank)
  (Row Blank Blank Blank Blank)

matrixify piece@(IPiece rot) = rotate rot $ basematrix piece
matrixify piece@(SPiece rot) = rotate rot $ basematrix piece
matrixify piece@(ZPiece rot) = rotate rot $ basematrix piece
matrixify piece@(LPiece rot) = rotate rot $ basematrix piece
matrixify piece@(JPiece rot) = rotate rot $ basematrix piece
matrixify piece@(OPiece rot) = rotate rot $ basematrix piece
matrixify piece@(TPiece rot) = rotate rot $ basematrix piece

instance Show Piece where
  show piece = show $ matrixify piece
