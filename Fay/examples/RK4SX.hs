module RK4SX where

{-
main :: IO ()
main = do
  let r = take 11 xs_dec
  print r
  return ()
-}
-- author : [stochastix](http://stochastix.wordpress.com/2012/06/29/runge-kutta-in-haskell/)

xs_dec :: [Double]
xs_dec = [xs_tilde !! k | k <- [0,1..], rem k 10000 == 0]
xs_tilde :: [Double]
xs_tilde = iterate (rk4 f h) x0
xs = [exp (-t) | t <- [0.0,0.1..]]

f x = (-x*(tan x))
h = 1/100000
x0 = 1.0
  

-- define 4th order Runge-Kutta map (RK4) 
rk4 :: Floating a => (a -> a) -> a -> a -> a
rk4 f h x = x + (1/6) * (k1 + 2*k2 + 2*k3 + k4)
            where k1 = h * f (x)
                  k2 = h * f (x + 0.5*k1)
                  k3 = h * f (x + 0.5*k2)
                  k4 = h * f (x + k3)