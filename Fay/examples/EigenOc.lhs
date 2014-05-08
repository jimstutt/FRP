> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Main where
> import ParamsOc
> import Utils
> import RK4SX
> import Data.Complex

3.2.1 Decay of Anomalies

(Eq. 3.13)

> temp1 z time= temp z * exp(-lambda*time) where
>   lambda = 0.1

>-- eq313 z t = loop step : loop step' where
>--   loop z time = do
>--     step' = temp1 z time
>--     z' = z + 100 
>--     time' = time

Perturbation function is:

> ir time = gammaOc*stepTime  where
>   gammaOc = 0.3
>   stepTime = tanh time



3.2.1 [p48]

Immediate solution (Eq. 3.15)

>-- temp z = a*exp(-i*c*h) - i*c*b*exp(-i*c*h) where h = z

>-- tempDepth :: Double -> Complex Double
>-- tempDepth depth = a * exp(-i2*c*h) - i2*c*b*exp(-i2*c*h) where
>--   i2 =  1 -- sqrt (-1) 
>--   h = depth
>--   a = heatCapacityOc
>--   b = heatCapacityPerMSqKOc:
>--   c = 1


Lower insulating boundary condition (Eq. 3.16)[p58]

>-- dTemp_dz (-h) = i*a*A*exp(-i*a*h) + i*a*b*exp(i*a*h) == 0

(Eq. 3.18)

> a = sqrt(lambda/k0) where -- (Eq. 3.15)
>   lambda = 0.9
>   k0 = 1.0
> temp z = a'*cos a*(z-h) where
>   h = 4
>   a' = 1 -- a new constant to be determined. damn! TBD.

(Eq. 3.19)
RHS is the ration of the depth of the ocean to the (radiation damped( mixing length
due to diffusion.

> diffLenObs = 140 -- m?

The first 3 roots h == 4 and x = a*h are:
1.43
4.289
7.206

(Fig. 3.2) ah tan ah

> y a h= a*h*tanh (a*h) : y a (h-1)

These correspond to (1/lambda) of:
329
36.24
6.44
years

(Eqs. 3.20-3.3) [p49] The first 3 vertical structure functions are:

> f1 z = 0.673371 * cos (0.33632*(4 + z))
> f2 z = 0.678128 * cos (1.07231*(4 + z))
> f3 z = 0.688426 * cos (1.80156*(4 + z))


Take 
(Fig. 3.5)

> wot1 depth1 = a*depth1*tan (a*depth1)
> wot2 depth1 = heatLossPerMSqKOc*depth1/k0*c0 where
>   c0 = 60*10**7/2 -- blx
> 
> test4 = fmap wot1 [1..50]
> test5 = fmap wot2 [1..50]


> upperbound_prop =
>   let h = 4
>       b = 1
>       k0 = 0.003 -- km^2/yr
>       c0 = 60*10**7/2 -- blx
>       err = 1000
>     in if ((a * h * tan (a * h) <= b * h/k0*c0 + err)&&(a * h * tan (a * h) >= b * h/k0*c0 - err))
>           then True
>           else False


(Eq. 3.25)

There are several lambdas. Fox one for now. TBD.

> depth = 4 -- kmte
> tempOc0 = 0.0

> temp0 :: Fractional a => a
> temp0 = 0.0

> main = fmap (tempOc1 

>{- tempOc1 depth time = rk4 temp0 (f_depth * exp(-lambda*time)) where
>     temp0 = 0
>     f_depth = tanh time
>--   lambdaL = 0.9 -- fmap lambda [0.0,0.1..1.0]
>     lambda = 0.01-}

3.2.2
Section 3 Steady State [p57]

(Eq. 3.49)

Spectral density (Eq 3.50)

(Eq 3.52)

> tMixF = do
>   let nf = 1 -- ?
>--       heatLossPerMSqKOc = 1.485                           -- (B)
>       i  = sqrt(-1)
>       f = 0.9
>       tMix = 1 
>       c0 = 1 -- ?
>       lambdaF = 1 + sqrt(abs((1 - 8*pi*i*f)/2)) 
>       k0 = 2/1000
>       r = (nf/heatLossPerMSqKOc) / ((-2)*pi*i*f* tMix + 1 + c0 * lambdaF - k0) where
>   return r -- needs NoMono..
