module Lib.Geometry
( XSliceable, xmin, xmax, setXmin, setXmax
, YSliceable, ymin, ymax, setYmin, setYmax
, ZSliceable, zmin, zmax, setZmin, setZmax
, Overlappable, Sliceable
, overlaps, inside, overlap
, slice
, Cuboid, cuboid
, Rectangle, rectangle
, Line, line
) where

import Data.Function ((&))

-- slicing line segments, squares, cuboids:
-- given cuboids A and B, return a set whose union equals A, split along any overlaps with B
-- we'll use this by considering a pair of cuboids and splitting each in turn, along each axis
-- This works for integers, expecting both minimum and maximum to be included
-- (This is basically because all AoC puzzles use integers)
dslice :: ()
       => ( s -> Integer     -- get min
          , s -> Integer     -- get max
          , s -> Integer -> s  -- set min
          , s -> Integer -> s  -- set max
          )  -- lenses
       -> s     -- the shape to slice
       -> s     -- the template shape
       -> [s]   -- list of resulting shapes
dslice (getmin, getmax, setmin, setmax) a b
  {- split A over the 'd' axis. Possibilities:
     A  ----     | ------   | -------- |   ------ |     ----- |   -----
     B      ---- |     ---- |   ----   | ----     | ----      | ---------
  -}
  | getmax a < getmin b = [a]
  | getmin a > getmax b = [a]
  | getmin b <= getmin a && getmax a <= getmax b = [a]
  | getmin a < getmin b && getmin b <= getmax a && getmax a <= getmax b =
    [ setmax a (getmin b - 1)
    , setmin a (getmin b)]
  | getmax a > getmax b && getmin b <= getmin a && getmin a <= getmax b =
    [ setmin a (getmax b + 1)
    , setmax a (getmax b)]
  | getmin a < getmin b && getmax b < getmax a =    -- feels safer to spell it out rather than "otherwise"
    [ setmax a (getmin b - 1)
    , setmin a (getmax b + 1)
    , setmax (setmin a (getmin b)) (getmax b)]

data Cuboid = Cuboid { cx0 :: Integer
                     , cx1 :: Integer
                     , cy0 :: Integer
                     , cy1 :: Integer
                     , cz0 :: Integer
                     , cz1 :: Integer
                     }
  deriving (Show, Eq, Ord)

cuboid :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Cuboid
cuboid x0 x1 y0 y1 z0 z1 = Cuboid { cx0=x0, cx1=x1, cy0=y0, cy1=y1, cz0=z0, cz1=z1 }

class XSliceable x where
  xmin :: x -> Integer
  xmax :: x -> Integer
  setXmin :: x -> Integer -> x
  setXmax :: x -> Integer -> x

instance XSliceable Cuboid where
  xmin = cx0
  xmax = cx1
  setXmin c x = c { cx0=x }
  setXmax c x = c { cx1=x }

xslice :: (XSliceable c) => c -> c -> [c]
xslice = dslice (xmin, xmax, setXmin, setXmax)

class YSliceable y where
  ymin :: y -> Integer
  ymax :: y -> Integer
  setYmin :: y -> Integer -> y
  setYmax :: y -> Integer -> y

instance YSliceable Cuboid where
  ymin = cy0
  ymax = cy1
  setYmin c y = c { cy0=y }
  setYmax c y = c { cy1=y }

yslice :: (YSliceable c) => c -> c -> [c]
yslice = dslice (ymin, ymax, setYmin, setYmax)

class ZSliceable z where
  zmin :: z -> Integer
  zmax :: z -> Integer
  setZmin :: z -> Integer -> z
  setZmax :: z -> Integer -> z

instance ZSliceable Cuboid where
  zmin = cz0
  zmax = cz1
  setZmin c z = c { cz0=z }
  setZmax c z = c { cz1=z }

zslice :: (ZSliceable c) => c -> c -> [c]
zslice = dslice (zmin, zmax, setZmin, setZmax)

class Overlappable s where
  -- do the two shapes overlap?
  overlaps :: s -> s -> Bool
  -- is A completely inside B?
  inside :: s -> s -> Bool
  -- produce the overlap, if there is one
  overlap :: s -> s -> Maybe s

class Sliceable s where
  slice :: s -> s -> [s]

instance Overlappable Cuboid where
  overlaps c0 c1 =
    -- we don't overlap if there's a separation along some axis
    let
      xsep = xmin c0 > xmax c1 || xmax c0 < xmin c1
      ysep = ymin c0 > ymax c1 || ymax c0 < ymin c1
      zsep = zmin c0 > zmax c1 || zmax c0 < zmin c1
    in not (xsep || ysep || zsep)
  -- is A completely inside B?
  inside a b =
    xmin b <= xmin a && xmax a <= xmax b &&
    ymin b <= ymin a && ymax a <= ymax b &&
    zmin b <= zmin a && zmax a <= zmax b
  -- produce the overlap
  overlap a b
    | overlaps a b = Just $ cuboid (xmin a `max` xmin b) (xmax a `min` xmax b)
                                   (ymin a `max` ymin b) (ymax a `min` ymax b)
                                   (zmin a `max` zmin b) (zmax a `min` zmax b)
    | otherwise = Nothing

instance Sliceable Cuboid where    
  slice a b
    | a `overlaps` b = [a]
                     & concatMap (`xslice` b)
                     & concatMap (`yslice` b) 
                     & concatMap (`zslice` b)
    | otherwise = [a]

data Rectangle = Rectangle { rx0 :: Integer
                           , rx1 :: Integer
                           , ry0 :: Integer
                           , ry1 :: Integer
                           }
  deriving (Show, Eq, Ord)

rectangle :: Integer -> Integer -> Integer -> Integer -> Rectangle
rectangle x0 x1 y0 y1 = Rectangle { rx0=x0, rx1=x1, ry0=y0, ry1=y1 }

instance XSliceable Rectangle where
  xmin = rx0
  xmax = rx1
  setXmin r x = r { rx0=x }
  setXmax r x = r { rx1=x }

instance YSliceable Rectangle where
  ymin = ry0
  ymax = ry1
  setYmin r y = r { ry0=y }
  setYmax r y = r { ry1=y }

instance Overlappable Rectangle where
  overlaps r0 r1 =
    -- we don't overlap if there's a separation along some axis
    let
      xsep = xmin r0 > xmax r1 || xmax r0 < xmin r1
      ysep = ymin r0 > ymax r1 || ymax r0 < ymin r1
    in not (xsep || ysep)
  -- is A completely inside B?
  inside a b =
    xmin b <= xmin a && xmax a <= xmax b &&
    ymin b <= ymin a && ymax a <= ymax b
  -- produce the overlap
  overlap a b
    | overlaps a b = Just $ rectangle (xmin a `max` xmin b) (xmax a `min` xmax b)
                                      (ymin a `max` ymin b) (ymax a `min` ymax b)
    | otherwise = Nothing

instance Sliceable Rectangle where    
  slice a b
    | a `overlaps` b = [a]
                     & concatMap (`xslice` b)
                     & concatMap (`yslice` b)
    | otherwise = [a]

data Line = Line { lx0 :: Integer
                 , lx1 :: Integer
                 }
  deriving (Show, Eq, Ord)

line :: Integer -> Integer -> Line
line x0 x1 = Line { lx0=x0, lx1=x1 }

instance XSliceable Line where
  xmin = lx0
  xmax = lx1
  setXmin r x = r { lx0=x }
  setXmax r x = r { lx1=x }

instance Overlappable Line where
  overlaps l0 l1 =
    -- we don't overlap if there's a separation along some axis
    let
      xsep = xmin l0 > xmax l1 || xmax l0 < xmin l1
    in not (xsep)
  -- is A completely inside B?
  inside a b =
    xmin b <= xmin a && xmax a <= xmax b
  -- produce the overlap
  overlap a b
    | overlaps a b = Just $ line (xmin a `max` xmin b) (xmax a `min` xmax b)
    | otherwise = Nothing

instance Sliceable Line where    
  slice a b
    | a `overlaps` b = [a]
                     & concatMap (`xslice` b)
    | otherwise = [a]
