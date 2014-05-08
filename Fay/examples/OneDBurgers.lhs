> module OneDBurgers where -- in Haskell module names have to start with an uppercase alpha character. 

>-- import DiffAlg where

- ID diffusion

The conservation form

>  dA + AA == 0

is equivalent to the soliton equation:

A guage transformation with an invertible function phi is:

>-- data A 
>--   | a' a = phi * a * unphi - Df phi unphi 

This induces the transformation:

>-- F' = phi * F * unphi

of the 2-curvature. Holds for any G-connection where G is a matrix group and phi has values.

Simply (ha) deform the differential calculus (DC) by applying the zero-curvature condition to the Cole-Hopf transformation to produce a pure guage. Help.

> class Phi A  where-- the Cinfty algebra on R2.
>   t = undefined      -- canonical coordinate functions of R2.
>   x = undefined

>-- Test 1
> test1 = if((Df t,t) == (Df x,x) == (Df t,x) == 0) then True else False

>-- Test 2
> test2 = if((Df x,x) = eta * Df t) then True else False -- eta is a constant.

> data Df

More generally:

>-- Df t f = f Df t
>-- Df x f = f Df x + eta * fx * Df t -- for f in A. How do I get this fx? TBD.  

fx is the partial derivative of f! That was quick.

>-- differential of a function
> Df f = Df (ft + eta * fxx/2) dt + Df fx dx

> type T = True
> type F = False 
>-- Test 3
> test3 = if (Df x * Df x == Df t * Df t== 0) then True else False 
>-- Anticommutative Test 4
> test4 = if (Df x * Df t == -(Df t * Df x)) then T else F







Burgers, KdV, Kadomshev-Padiashvilli (KP) and  Boussinquet equations can be expressed as a zero-curvature condition for a GL(1,R)-connection wrt. a *non-commutative* calculus; an analogue of a differential calculus of forms where 1-forms and functions in general do not commute.

In ordinary differential calculus the product of a 1-form with itself vanishes?
This means that non-trivial relations emerge even from F = 0 for a single 1-form (and not just a matrix of 1-forms).










  (Dimakis)

-- Inspired by http://www.bestinclass.dk/index.php/2010/03/functional-fluid-dynamics-in-clojure/
-- http://github.com/LauJensen/Fluid-Dynamics/raw/master/fluids.clj
-- Navier Stokes (http://en.wikipedia.org/wiki/Navier–Stokes_equations)



-- | Tim van Beek

a) create a necessary part of the software infrastructure, including the implementation of numerical algorithms, that are needed for the Navier-Stokes equations,

b) gain some insights into spectral methods, like: How do I handle millions of coupled ordinary differential equations? What happens when the solution develops singular behaviour like shock waves?

Of course we won't need millions of ODE to get a good approximation, I think, but you get millions of coupled ODE when you use spectral methods for the globe, if you'd like to get a decent approximation of the synoptic scale.

I'm sorry but I don't think there is anything of interest for a pure mathematician here.



-- | Financial Time Series, Multifractals and Hidden Markov Models http://wiki.wstein.org/2008/simuw


-- | Eric Forgy

First, we start with a connection 1-form  written in terms of right components, i.e.



Burgers equation is a zero curvature condition, so we need to compute the curvature of this connection given by



The first step is easy. we simply need to compute the exterior derivative



In this step, we used the fact that



and



Next we compute



The basis  commutes with 0-forms and  so the first term above vanishes. The third term can be rewritten



so we have



The goal is to now move all basis 1-forms to the left of all 0-forms (something like normal ordering). To do this, we can start by writing the product rule as



and expanding so that



Carrying out all the differentiations results in



Returning to  we can now write



or simply



We can now write down the curvature



This vanishes when



which, aside from the term involving , is Burgers equation (with correct signs).

Having corrected the connection 1-form by writing the 0-form components to the right of the basis 1-forms, it is a simple matter to extend this analysis to higher dimensions. When extending to (3+1)-dimensions, we get the Navier-Stokes equation where the meaning of becomes clear. See the post

Navier-Stokes from the zero curvature condition in noncommutative geometry
where it becomes obvious that  is the negative of the pressure, so Burgers equation can be expressed with a pressure term as



I don't know about you, but this boggles my mind.

Of course the next step will be to correct the discrete Burgers equation and finally to write down the discrete Navier-Stokes equation (with maybe a detour or two along the way).


-- | Cole-Hopf

I wanted to work out the Cole-Hopf transformation using the correct form of Burgers equation. It turned out to be a little tricky (not too bad) so I'll leave this note.

The Cole-Hopf transformation comes about simply by expressing the connection 1-form



as a pure gauge connection, i.e.



It is just a little tricky because we have a  on the left side of the differential  and we want to compare to the original connection written in right-component form with all 0-forms to the right of the basis 1-forms.

The most straightforward way I found is to observe that



so the pure gauge connection can be written in two equivalent ways



where . Expanding this, we have



so that

Cole-Hopf Transformation

-- | Eric Forgy Discrete Calculus



 a discrete left martingale with zero curvature as the discrete Burgers equation.

Since the discrete calculus used to derive the discrete Burgers equation converges to the continuum calculus used to derive the continuum Burgers equation, then any solution to the discrete Burgers equation is guaranteed to converge to a solution of the continuum Burgers equation by construction.

Now that we know what the discrete Burgers equation is, it is now just a matter of turning the crank to determine the corresponding numerical algorithm.


> import qualified Data.Vector.Unboxed.Mutable as M
> import qualified Data.Vector.Generic.Mutable as GM
> import Debug.Hood.Observe
>-- import Criterion.Main
> import Control.Monad

{-

Gareth Sobcziuk and David Hestenes, 

"Covariant tensor analysis makes it difficult to distinguish the behaviours"

"we don't want indices". 

Jerzy Karczmarczuk Scientific Computing 

* deprecate tensor notation as unprogrammable
* laziness allows a direct bridge from initial conditions to infinity

I want to functions to be pure so we get composability.

-}

> instance Floating a => Floating (Series a) Where
>   Zs = exp sone
>   exp @ u (u0:> uq) = w Where
>     w = sint (exp u0) (u * w SDIF)
>   log u @ (u0:> uq) = sint (log u0) (SDIF u / u)
>   u @ sqrt (u0:> uq) = w Where 
>     w = sint (sqrt u0) (0.5 *> (SDIF u / w))

> type DVector = M.IOVector Double

> data Grid = Grid Int DVector

>-- |Create an empty vector
> emptyGrid :: Int -> IO Grid
> emptyGrid sz = do
>   d <- GM.unsafeNewWith (vectorLength sz) 0
>   return (Grid sz d)

>-- |Translate from 2D to 1D co-ordinates
> ix :: Int -> (Int,Int) -> Int
> ix n (i,j) = i + (n+2) * j

>-- |Write a single value at the given co-ordinates
> writeVal :: Grid -> (Int,Int) -> Double -> IO ()
> writeVal (Grid sz d) p = GM.unsafeWrite d (ix sz p) 

>-- |Write multiple values
> setVals :: Grid -> [((Int,Int),Double)] -> IO ()
> setVals g vals = forM_ vals (uncurry (writeVal g))

>-- |Read the value at the given point
> readVal :: Grid -> (Int,Int) -> IO Double
> readVal (Grid sz d) p = GM.unsafeRead d (ix sz p)

>-- |Add the sources together, writing the content out to x
> addSource :: Grid -> Grid -> Double -> IO ()
> addSource (Grid sz x) (Grid _ s) dt = forM_ [0..(vectorLength sz - 1)] $ \i -> do
>                         xa <- GM.unsafeRead x i
>                         sa <- GM.unsafeRead s i 
>                         GM.unsafeWrite x i (xa + sa*dt)

>-- |This code is vomit inducing, but handles the edge cases..
> setBnd :: Int -> Grid -> IO()
> setBnd b g@(Grid sz _) = forM_ [1..sz] 
>                           (\i -> 
>                                do
>                                  a1 <- readVal g (1,i)
>                                  a2 <- readVal g (sz,i)
>                                  a3 <- readVal g (i,1)
>                                  a4 <- readVal g (i,sz)
>                                 let mx | b == 1 = -1
>                                        | otherwise = 1
>                                 let my | b==2 = -1
>                                        | otherwise = 1
>                                 setVals g [((0,i)   ,mx * a1)
>                                           ,((sz+1,i),mx * a2)
>                                           ,((i,0)   ,my * a3)
>                                           ,((i,sz+1),my * a4)])
>                         >> do
>                           x10 <- readVal g (1,0)
>                           x01 <- readVal g (0,1)
>                           x1n1 <- readVal g (1,sz+1)
>                           x0n <- readVal g (0,sz)
>                           xn0 <- readVal g (sz,0)
>                           xn11 <- readVal g (sz+1,1)
>                           xnn1 <- readVal g (sz,sz+1)
>                           x1nn <- readVal g (sz+1,sz)
>                           setVals g [((0,0)      ,0.5 * (x10  + x01))
>                                     ,((0,sz+1)   ,0.5 * (x1n1 + x0n))
>                                     ,((sz+1,0)   ,0.5 * (xn0  + xn11))
>                                     ,((sz+1,sz+1),0.5 * (xnn1 + x1nn))]

>-- |A simple loop over each pixel
> forEachPixel :: Grid -> ((Int,Int) -> IO()) -> IO()
> forEachPixel (Grid n _) = forM_ [(u,v) | u<-[1..n], v <- [1..n]]

>-- |For simplicity, just consider up,down,left,right to be the neighbours
> neighbours :: Grid -> (Int,Int) -> IO (Double,Double,Double,Double)
> neighbours g (x,y) = do 
>   up <- readVal g (x-1,y)
>   down <- readVal g (x+1,y)
>   left <- readVal g (x,y-1)
>   right <- readVal g (x,y+1)
>   return (up,down,left,right)

> linSolveStep :: Int -> Grid -> Grid -> Double -> Double -> IO ()
> linSolveStep b x x0 a c = forEachPixel x
>                             (\(i,j) -> 
>                              do
>                                (up,down,left,right) <- neighbours x (i,j)
>                                x0v <- readVal x0 (i,j)
>                                writeVal x (i,j) ((x0v + a*(up + down + left + right)) / c))
>                           >> setBnd b x

> linSolve :: Int -> Grid -> Grid -> Double -> Double -> IO()
> linSolve b x x0 a c = forM_ [1..20] (\_ -> linSolveStep b x x0 a c)

> diffuse :: Int -> Grid -> Grid -> Double -> Double -> IO()
> diffuse b x@(Grid n _) x0 diff dt = linSolve b x x0 a (1 + 4*a) where
>    a = dt * diff * (fromIntegral n * fromIntegral n)

> advect :: Int -> Grid -> Grid -> Grid -> Grid -> Double -> IO ()
> advect b d@(Grid n _) d0 u v dt = forEachPixel d
>                                    (\(i,j) ->
>                                     do
>                                       uVal <- readVal u (i,j)
>                                       vVal <- readVal v (i,j)
>                                       let n5 = fromIntegral n + 0.5
>                                           x = min n5 (max 0.5 (fromIntegral i - dt0 * uVal))
>                                           y = min n5 (max 0.5 (fromIntegral j - dt0 * vVal))
>                                           i0 = truncate x
>                                           i1 = i0 + 1
>                                           j0 = truncate y
>                                           j1 = j0 + 1
>                                           s1 = x - fromIntegral i0
>                                          s0 = 1 - s1
>                                          t1 = y - fromIntegral j0
>                                          t0 = 1 - t1
>                                      xd0 <- readVal d0 (i0,j0)
>                                      xd1 <- readVal d0 (i0,j1)
>                                      xd2 <- readVal d0 (i1,j0)
>                                      xd3 <- readVal d0 (i1,j1)
>                                      writeVal d (i,j) (s0*(t0*xd0 + t1*xd1) + s1*(t0*xd2+ t0*xd3)))
>                                  >> setBnd b d
>                                      where
>                                        dt0 = dt * fromIntegral n

> project :: Grid -> Grid -> Grid -> Grid -> IO ()
> project u@(Grid n _) v p d = forEachPixel u
>                                 (\(i,j) ->
>                                      do
>                                        u0 <- readVal u (i+1,j)
>                                        u1 <- readVal u (i-1,j)
>                                        v0 <- readVal v (i,j+1)
>                                        v1 <- readVal v (i,j-1)
>                                        writeVal d (i,j) (-0.5 * ((u0-u1+v0-v1) / fromIntegral n))
>                                        writeVal p (i,j) 0)
>                                >> setBnd 0 d 
>                                >> setBnd 0 p 
>                                >> linSolve 0 p d 1 4 
>                                >> forEachPixel p
>                                    (\(i,j) ->
>                                     do
>                                       (up,down,left,right) <- neighbours p (i,j)
>                                       u0 <- readVal u (i,j)
>                                       v0 <- readVal v (i,j)
>                                       writeVal u (i,j) (u0 - 0.5*fromIntegral n*(down - up))
>                                       writeVal v (i,j) (v0 - 0.5*fromIntegral n*(right - left)))
>                                >> setBnd 1 u
>                                >> setBnd 2 v

> densStep :: Grid -> Grid -> Grid -> Grid -> Double -> Double -> IO ()
> densStep x x0 u v diff dt = do
>              addSource x x0 dt
>              swap x0 x
>              diffuse 0 x x0 diff dt
>              swap x0 x
>              advect 0 x x0 u v dt

> velStep :: Grid -> Grid -> Grid -> Grid -> Double -> Double -> IO ()
> velStep u v u0 v0 visc dt = do
>              addSource u u0 dt
>              addSource v v0 dt
>              swap u0 u
>              diffuse 1 u u0 visc dt
>              swap v0 v
>              diffuse 2 v v0 visc dt
>              project u v u0 v0
>              swap u0 u
>              swap v0 v
>              advect 1 u u0 u0 v0 dt
>              advect 2 v v0 u0 v0 dt
>              project u v u0 v0


> vecToList :: DVector -> IO [Double]
> vecToList d = mapM (M.read d) [0..n] where
>    n = M.length d - 1

> absDifference :: [Double] -> [Double] -> Double
> absDifference v1 v2 = sqrt (sum (map (\y -> y*y) (zipWith (-) v1 v2)))

> nearlyEqual :: [Double] -> [Double] -> Bool
> nearlyEqual x y = absDifference x y < 0.0001

> gridToList :: Grid -> IO [Double]
> gridToList (Grid _ d) = vecToList d

> vectorLength :: Int -> Int
> vectorLength sz = (sz+2)*(sz+2)

> listToVec :: [Double] -> IO DVector
> listToVec d = do
>    let n = length d
>    v <- GM.unsafeNewWith n 0.0
>    mapM_ (\(x,p) -> M.write v p x) (zip d [0..])
>    return v

> zeroGrid :: Grid -> IO ()
> zeroGrid (Grid _ ns) = M.set ns 0

>-- |Hideously inefficient way of swapping two vectors
> swap :: Grid -> Grid -> IO()
> swap (Grid n xs) (Grid _ ys) = forM_ [0..(vectorLength n - 1)] $ \i -> do
>                                  xtmp <- GM.unsafeRead xs i
>                                  ytmp <- GM.unsafeRead ys i
>                                  GM.unsafeWrite xs i ytmp
>                                  GM.unsafeWrite ys i xtmp

> burgers1 n = do
>  x <- mkGrid n 
>

> testSetBnd = do
>   putStrLn "Testing setBnd"
>   a <- listToVec [0..15]
>   let expected = [5,5,6,6,5,5,6,6,9,9,10,10,9,9,10,10]
>   let example = Grid 2 a
>   setBnd 3 example
>   b <- vecToList a
>   print (b == expected)  

> testLinSolveStep = do
>   putStrLn "Testing LinSolveStep"
>   x <- listToVec [0..15]
>   x0 <- listToVec [0..15]
>   let expectedLinStep = [0,-16.25,-27.9375,0,16.25,16.25,27.9375,27.9375,37.6875,37.6875,70.468750,70.468750,0,-37.6875,-70.46875,0]
>   linSolveStep 2 (Grid 2 x) (Grid 2 x0) 3 4 
>   c <- vecToList x
>   print (c == expectedLinStep)

> testLinSolve = do
>   putStrLn "Testing LinSolve"
>   x <- listToVec [0..15]
>   x0 <- listToVec [0..15]
>   let expected = [54.999996,54.999996,56.749998,56.749998,54.999996,54.999996,56.749998,56.749998,58.250002,58.250002,60.000002,60.000002,58.250002,58.250002,60.000002,60.000002]
>   linSolve 0 (Grid 2 x) (Grid 2 x0) 1 4
>   c <- vecToList x
>   print (nearlyEqual c expected)

> testAdvect = do
>   putStrLn "Testing advect"
>   let expected = [2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5]
>   a <- listToVec [0..15]
>   b <- listToVec [0..15]
>   c <- listToVec [0..15]
>   d <- listToVec [0..15]
>   advect 3 (Grid 2 a) (Grid 2 b) (Grid 2 c) (Grid 2 d) 9
>   result <- vecToList a
>   print (nearlyEqual result expected)

> testProject = do
>   putStrLn "Testing project"
>   u <- listToVec [0..15]
>   v <- listToVec [0..15]
>   p <- listToVec [0..15]
>   div <- listToVec [0..15]
>   project (Grid 2 u) (Grid 2 v) (Grid 2 p) (Grid 2 div)
>   uResult <- vecToList u
>   vResult <- vecToList v
>   pResult <- vecToList p
>   divResult <- vecToList div
>   let expectedU = [0.000000,5.416666,6.416666,0.000000,-5.416666,5.416666,6.416666,-6.416666,-9.416666,9.416666,10.416666,-10.416666,0.000000,9.416666,10.416666,0.000000]
>       expectedV = [0.000000,-5.416666,-6.416666,0.000000,5.416666,5.416666,6.416666,6.416666,9.416666,9.416666,10.416666,10.416666,0.000000,-9.416666,-10.416666,0.000000]
>       expectedP = [-16.180556,-16.180556,-16.597222,-16.597222,-16.180556,-16.180556,-16.597222,-16.597222,-16.597222,-16.597222,-17.013889,-17.013889,-16.597222,-16.597222,-17.013889,-17.013889]
>       expectedDiv = [-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000,-2.500000]
>   print (nearlyEqual uResult expectedU && nearlyEqual vResult expectedV &&
>          nearlyEqual pResult expectedP && nearlyEqual divResult expectedDiv)

> testVelStep = do
>   putStrLn "VelStep testing"
>   let expectedX = [0.000000,0.011987,0.041284,0.000000,-0.011987,0.011987,0.041284,-0.041284,-0.016870,0.016870,0.016870,-0.016870,0.000000,0.016870,0.016870,0.000000]
>       expectedY = [0.000000,-0.016870,-0.011987,0.000000,0.016870,0.016870,0.011987,0.011987,0.016870,0.016870,0.021753,0.021753,0.000000,-0.016870,-0.021753,0.000000]
>       expectedU = [-0.023750,-0.023750,0.000444,0.000444,-0.023750,-0.023750,0.000444,0.000444,-0.004439,-0.004439,0.014872,0.014872,-0.004439,-0.004439,0.014872,0.014872]
>       expectedV = [-0.043505,-0.043505,0.009765,0.009765,-0.043505,-0.043505,0.009765,0.009765,-0.000000,-0.000000,0.033740,0.033740,-0.000000,-0.000000,0.033740,0.033740]
>   x <- listToVec [0..15]
>   y <- listToVec [0..15]
>   u <- listToVec [0..15]
>   v <- listToVec [0..15]
>   velStep (Grid 2 x) (Grid 2 y) (Grid 2 u) (Grid 2 v) 3 4
>   xResult <- vecToList x
>   yResult <- vecToList y
>   uResult <- vecToList u
>   vResult <- vecToList v
>   print (nearlyEqual xResult expectedX && nearlyEqual yResult expectedY &&
>         nearlyEqual uResult expectedU && nearlyEqual vResult expectedV)

> mainLoop = do
>   x <- emptyGrid 80
>   y <- emptyGrid 80
>--   u <- emptyGrid 80
>--   v <- emptyGrid 80

>--  defaultMain [
>--        bgroup "Mutable Fluids" [ 
>--                    bench "Project" $ nfIO (project x y u v)
>--                   ,bench "SetBnds" $ nfIO (setBnd 2 x)
>--                   ]]

>   project x y z d
>   setBnd 2 x
  
>   return Nothing

>  tests = do

    testSetBnd

  
>  testLinSolveStep

  testLinSolve
  testAdvect
  testProject
  testVelStep

>   return ()



