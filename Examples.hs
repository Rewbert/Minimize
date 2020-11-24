module Main where

import NelderMead       as O0
import NelderMeadRandom as O1
import LineOptimization as O2
import LineFalsification
import System.Random( mkStdGen )
import Data.List( transpose, sort, nub, tails )
import Data.Char( ord, chr )
import qualified Data.Map as M

--------------------------------------------------------------------------------

main :: IO ()
main =
  do sequence_
       [ putStrLn (show i ++ ": " ++ show y ++ " " ++ show (sh x))
       | (i,(x,y)) <- progress ([1..] `zip` xys')
       ]
 where
-- {-
  k = 10
  n = k+1
  f = sat' k
  (aL,aR) = (0,1)
  sh xs = map (\x -> fst (index x k)) xs
  p (xs,y) = y <= 0
-- -}
{-
  n = 20
  f = parse 0 9999 9999 [(0,0.0)]
  (aL,aR) = (0,255)
  sh xs = map (chr . round) xs
  p (xs,y) = y <= 0.1
-}
{-
  k = 5
  n = k*k
  f = magic2 k
  (aL,aR) = (0,100) -- (0.5001,fromIntegral n + 0.4999)
  sh xs = map snd (sort (xs `zip` [1..]))
  p (xs,y) = y <= 0.1
-}
{-
  k = 4
  n = k*k
  f = magic k
  (aL,aR) = (0.5001,fromIntegral n + 0.4999)
  sh xs = map round xs
  p (xs,y) = y <= 0.1
-}
{-
  n = 15
  f = koen
  (aL,aR) = (-10,10)
  sh = map (\x -> fromIntegral (round (x*10)) / 10)
  p (xs,y) = y <= 0.1
-}
{-
  n = 1
  f = sines -- alpine1
  (aL,aR) = (0,300)
  sh x = fromIntegral (round (x*1000)) / 1000
  p (xs,y) = y <= 0.01
-}
{-
  n = 5
  f = alpine1
  (aL,aR) = (-5,5)
  sh = map $ \x -> fromIntegral (round (x*10)) / 10
  p (xs,y) = y <= 0.01
-}
{-
  n = 3
  f = wolfe
  (aL,aR) = (-3,10)
  sh = map $ \x -> fromIntegral (round (x*10)) / 10
  p (xs,y) = y <= 0.001
-}
{-
  n = 2
  f = happycat
  (aL,aR) = (-2,2)
  sh = map $ \x -> fromIntegral (round (x*100)) / 100
  p (xs,y) = y <= 0.01
-}
  xys' = takeUntil p xys

  xys :: [([Double],Double)]
  xys = falsifyBox (mkStdGen 1113111) f [(aL,aR) | i <- [1..n]]
  --xys = O2.minimizeBox (mkStdGen 1113111) f [(aL,aR) | i <- [1..n]]

progress ((i,(xs,a)):xsas) = (i,(xs,a)) : go a xsas
 where
  go a ((i,(ys,b)):xsas)
    | b < a     = (i,(ys,b)) : go b xsas
    | otherwise = go a xsas
  go _ _ = []

takeUntil p [] = []
takeUntil p (x:xs)
  | p x        = [x]
  | otherwise  = x : takeUntil p xs

--------------------------------------------------------------------------------
-- some numeric functions

alpine1 xs =
  sum [ abs (x * sin x + 0.1*x + 0.1) | x <- xs ]

booth [x,y] = (x + 2*y - 7)^2 + (2*x - y - 5)^2

vetters [x,y,z] = (1 / (1 + (x - y)^2)) + sin ((pi * y + z) / 2) + exp ((((x+y)/y)-2)^2)

wolfe [x,y,z] =
  (4/3)*(x*x + y*y - x*y)**0.75 + z

happycat xs =
  ((lxs2 - n)^2)**a + (lxs2/2 + sum xs)/n + 0.5
 where
  n = fromIntegral (length xs)
  a = 1/8
  lxs2 = sum [ x*x | x <- xs ]

koen xs =
  sum [ abs $ if even (round (x+y)) then x-5*cos y else x-4*sin y
      | (x,y) <- xs `zip` tail xs
      ]

sines [x] =
  (sin (x/(5+cos x)) + 2.0 - 1 / (2.935 + 1.2*cos (2.5*x) + abs (0.01 - (x/10000))) + 0.5*cos x)

--------------------------------------------------------------------------------
--  magic squares using numeric function

magic :: Int -> [Double] -> Double
magic k xs =
  sum $ map abs $
-- {-
  [ fromIntegral k * ((unstab x `min` unstab y) + 0.1)
  | (i,x) <- [0..] `zip` xs
  , j <- [0..i-1]
  , let y = xs!!j
  , round x == round y
  ] ++
-- -}
{-
  [ (2*minimum [ abs (fromIntegral i - x) | x <- xs ])^2
  | i <- [1..k*k]
  ] ++
-}
  concat
  [ [ sum r - tot, sum (map (fromIntegral . round) r) - tot ]
  | r <- mat ++ transpose mat ++ [diag1, diag2]
  ]
 where
  mat = takeWhile (not . null) . map (take k) . iterate (drop k) $ xs
  tot = fromIntegral (sum [ 1 .. k*k ] `div` k)

  diag1 = [ (mat!!i)!!i | i <- [0..k-1] ]
  diag2 = [ (mat!!i)!!(k-1-i) | i <- [0..k-1] ]

  unstab x = abs (x - (a + 0.5)) `min` abs (x - (a - 0.5))
   where
    a = fromIntegral (round x)
    
magic2 :: Int -> [Double] -> Double
magic2 k xs =
  sum $ map abs $
  [ fromIntegral (abs (tot-s)) +
    minimum
    [ abs (x-y)
    | a <- r
    , (x,a') <- xas
    , a' == a
    , (y,b) <- xas
    , if tot > s then b == a+1 else a == b+1
    ]
  | r <- mat ++ transpose mat ++ [diag1, diag2]
  , let s = sum r
  , s /= tot
  ]
 where
  norm xs
    | a /= 0    = [ (x - mn)/a | x <- xs ]
    | otherwise = xs
  mn  = minimum xs
  mx  = maximum xs
  a   = (mx-mn)/100
  xas = xs `zip` [1..]
  ads = [ (a,x) | (x,a) <- sort xas ]

  mat = group k $ map fst ads
  tot = sum [ 1 .. k*k ] `div` k

  diag1 = [ (mat!!i)!!i | i <- [0..k-1] ]
  diag2 = [ (mat!!i)!!(k-1-i) | i <- [0..k-1] ]

--------------------------------------------------------------------------------

parse q0 q1 q2 ds (c:s) =
  parse q0' q1' q2' ds' s
 where
  cOpen  = (c ==. '(') &&. q0
  cPlus  = (c ==. '+') &&. (q1 ||. q2)
  cDigit = foldr1 (||.) [ c ==. w | w <- ['0'..'9'] ] &&. (q0 ||. q1)
  cClose = (c ==. ')') &&. (q1 ||. q2) &&. foldr (||.) 0 [ q | (i,q) <- ds, i <= 0 ]
 
  q0' = cOpen ||. cPlus
  q1' = cDigit
  q2' = cClose
  ds' = combine $
        [ (i,  q &&. (cDigit ||. cPlus)) | (i,q) <- ds] ++
        [ (i+1,q &&. cOpen)              | (i,q) <- ds] ++
        [ (i-1,q &&. cClose)             | (i,q) <- ds]
   where
    combine ds =
      [ (i, foldr1 (||.) [ q | (j,q) <- ds, j == i ])
      | i <- nub [ i | (i,_) <- ds ]
      ]

parse q0 q1 q2 ds [] =
  (ds =. 0) &&. (q1 ||. q2)

(==.) :: Double -> Char -> Double
c ==. w
  | round c == ord w = 0.0
  | otherwise        = abs (c - w')
 where
  w' = fromIntegral (ord w)

(&&.), (||.) :: Double -> Double -> Double
x ||. y = x `min` y -- 1/((1/x) + (1/y))
x &&. y = x + y

(=.) :: Eq a => [(a,Double)] -> a -> Double
xs =. x = head [ q | (y,q) <- xs, y == x ]

--------------------------------------------------------------------------------

sat :: Int -> [Double] -> Double
sat k xs =
  problem $ pos ++ neg
 where
  mat = group k xs

  pos =
    [ disj ps
    | ps <- mat
    ]
    
  neg =
    drop 1 $
    [ disj [-r,-s]
    | qs <- transpose mat
    , r:rs <- tails qs
    , s <- rs
    ]

  problem cs = sum cs
  
  disj xs
    | any (> 0) xs = 0
    | otherwise    = 1 + sum [ -x | x <- xs, x <= 0 ]
    
sat' :: Int -> [Double] -> Double
sat' k ys = sum problem
 where
  mat = group k $ take (k*(k+1)) $ [1..]

  problem =
    [ 1 + sum [ d | Just d <- mds ]
    | let tab = M.fromListWith min
                [ (l, d)
                | (ls,y) <- cpos `zip` ys
                , let (i,d) = index y (length ls)
                , let l = ls !! i
                ]
    , ls <- cneg
    , let mds = [ M.lookup l tab | l <- ls ]
    , null [ () | Nothing <- mds ]
    ]

  cpos =
    [ ps
    | ps <- mat
    ]
    
  cneg =
    init
    [ [r,s]
    | qs <- transpose mat
    , r:rs <- tails qs
    , s <- rs
    ]
    
--------------------------------------------------------------------------------

group k = takeWhile (not . null) . map (take k) . iterate (drop k)

index :: Double -> Int -> (Int, Double)
index x n = (i, d)
 where
  z = x * fromIntegral n
  i = floor z `min` (n-1)
  d = minimum $
      [ fromIntegral (i+1) - z | i+1 < n ] ++
      [ z - fromIntegral i     | i-1 >= 0 ]



