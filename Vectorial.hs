module Vectorial
( Vectorial2D (..)
, X, Y, Z, Scalar
) where


type Scalar = Double
type X = Scalar
type Y = Scalar
type Z = Scalar


class Vectorial2D a where

  toCartesian :: a -> (X, Y)
  fromCartesian :: (X, Y) -> a

  mag :: a -> Scalar
  mag v = sqrt $ x ^ 2 + y ^ 2
    where (x, y) = toCartesian v

  unit :: a -> a
  unit v = fromCartesian (x / m, y / m)
    where 
      (x, y) = toCartesian v
      m = mag v

  (<->) :: a -> a -> a
  v1 <-> v2 = fromCartesian (x1 - x2, y1 - y2)
    where
      (x1, y1) = toCartesian v1
      (x2, y2) = toCartesian v2

  (<+>) :: a -> a -> a
  v1 <+> v2 = fromCartesian (x1 + x2, y1 + y2)
    where
      (x1, y1) = toCartesian v1
      (x2, y2) = toCartesian v2

  (<*>) :: Scalar -> a -> a
  k <*> v = fromCartesian (k * x, k * y)
    where (x, y) = toCartesian v

  (<.>) :: a -> a -> Scalar
  v1 <.> v2 = x1 * x2 + y1 * y2
    where
      (x1, y1) = toCartesian v1
      (x2, y2) = toCartesian v2


  


