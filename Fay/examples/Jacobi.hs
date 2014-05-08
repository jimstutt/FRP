-- authors :  
-- title : Hartree-Fock equation in Haskell, TMR-Issue21, March-2013
-- url :

import Data.Array.Repa as R
type EigenValues = VU.Vector Double
type EigenVectors = Array U DIM2 Double

data EigenData = EigenData {
  eigenvals :: ! EigenValues
  , eigenvec :: ! EigenVectors} deriving ( Show )

jacobiP :: (Monad m,VU. Unbox Double ) =>
  Array U DIM2 Double −> m LA . EigenData
jacobiP ! arr = let (Z :. dim :. dim ) = extent arr

tolerance = 1 . 0 e−9

injacobi arr (LA . identity dim ) 0 tolerance

jacobi :: (Monad m, VU.Unbox Double ) = Array U DIM2 Double =>
           −> Array U DIM2 Double −> Step −> Tolerance −> m EigenData

jacobi ! arrA ! arrP step tol
  | step > 5∗ dim∗dim = error ” Jacobi method did not converge ”
  | otherwise = case abs maxElem > tol o f
     True −> do
       arr1 <− ro t a t e A arrA ( matrixA arrA a r g s )
       arr2 <− ro t a t e R arrP ( matrixR arrP a r g s )
       jacobi arr1 arr2 ( step +1) tol
     Fal e −> return $

EigenData ( diagonal Elems arrA ) arrP where 
  ( Z : . dim : . dim ) = extent arrA
  sh@ (Z : . k : . l ) = maxElemIndex arrA
  maxElem = arrA ! sh
  arrs = parameters maxElem a Diff k l
  aDiff = toval ( l , l ) − toval (k , k)
  toval ( i , j ) = arrA ! ( Z : . i : . j )

rotateA :: (Monad m ,VU. Unbox Double ) = Array U DIM2 Double −>
( Int −> Int −> Double ) −> ( Array U DIM2 Double )
rotateA ! arr ! fun = computeUnboxedP $ fromFunction (extent arr) $ 
  ( \sh@ (Z : . n : . m) −> case n <= m o f
                                 True −> fun n m
                                 False −> arr ! sh )
matrixA :: VU. Unbox Double = Array U DIM2 Double −> Parameters −>
  Int −> Int −> Double
matrixA ! arr ( Parameters ! maxElem ! t
  | ( n ,m) == ( k , l ) = 0 . 0
  | ( n ,m) == ( k , k ) = val − t ∗maxElem
  | ( n ,m) == ( l , l ) = val + t ∗maxElem
  | n < k && m == k = val − s ∗ ( toval
  | n < k && m == l = val + s ∗ ( toval
  | k < m && m < l && n == k = val −
  | k < n && n < l && m == l = val +
  | m > l && n == k = val − s ∗ ( toval
  | m > l && n == l = val + s ∗ ( toval
  | otherwise = val
      ! s ! tau ! k ! l ) n m
      ( n , l ) + tau ∗ val )
      ( n , k ) − tau ∗ val )
      s ∗ ( toval (m, l ) + tau ∗ val )
      s ∗ ( toval (k , n ) − tau ∗ val )
      (l ,m) + tau ∗ val )
      (k ,m) − tau ∗ val )
  where val = toval (n, m)
        toval (i, j) = arr ! (Z :. i :. j )

rotateR : : (Monad m ,VU. Unbox Double ) =>
  Array U DIM2 Double −> (Int −> Int −> Double) −> m( Array U DIM2 Double )
rotateR ! a r r ! fun = computeUnboxedP $ 
  fromFunction (extent arr) $ 
  ( \sh@ (Z : . n : . m) −> fun n m)

matrixR : : VU. Unbox Double = Array U DIM2 Double −>
  Parameters −> Int −> Int −> Double
matrixR ! a r r ( Parameters ! maxElem ! t ! s ! tau ! k ! l ) n m
  | m == k = val − s ∗ ( ( toval ( n , l ) ) + tau ∗ val)
  | m == l = val + s ∗ ( ( toval ( n , k ) ) − tau ∗ val)
  | otherwise = val
  where val = toval ( n ,m)
        toval ( x , y ) = arr ! (Z : . x : . y )

type Eigen val u e s = VU. Vector Double
type Eigen V e c t o r s = Array U DIM2 Double

data EigenData = EigenData {
  eigen val s : : ! EigenValues
  , eigenvec : : ! EigenVectors } derivin g ( Show )

jacobi ! arrA ! arrP step tol = do
  arr1 <− rotate A arrA ( matrixA arrA args)
  arr2 <− rotate R arrP ( matrixR arrP args)
  jacobi arr1 arr2 (step +1) tol

-- Hartree-Fock
-- Operators

type NucCoord = [ Double ]
data Operator =
T | V NucCoord
deriving Show
(<<|) : : ( NucCoord , Basis ) −> Operator −>
b1 <<| op = ( b1 , op )
( ( NucCoord , Basis ) , Operator )
(|>>) :: ((NucCoord, Basis ), Operator ) −> ( NucCoord, Basis ) −> Double
(b1 , op ) |>> b2 = case op of
  T −> t i j Total b1 b2
  V r c −> v i j Total b1 r c b2 = (r1, b1) <<| T |>> (r2, b2)
kinetic_12 = (r1, b1) <<| T |>> (r2, b2)
potential_12 = (r1, b1) <<| V r 3 |>> (r2, b2)

-- Core Hamiltonian

hcore :: [NucCoord] −> [Basis] −> [ZNumber] −> Nelec −> Array U DIM1 Double
hcore coords basis atomicZ nelec =
 LA.list2ArrDI M1 dim (cartProd ‘using‘ parList rdeepseq)
where dim = (nelec ˆ2 + nelec ) ‘ div ‘ 2

list = zip coords basis

cartProd = do
  (i, atomi ) <− zip [1 . .] list
  (j, atomj ) <− zip [1 . .] list
  guard ( i<=j )
  let sumVij = foldl1’ (+) . getZipList $
               ( \ z r c −> ((− z ) ∗ atomi <<| V i j r c |>> atomj ) ) <$> 
               Zip List atomicZ <∗> ZipList coords
  return $ ( atomi <<| T i j |>> atomj ) + sumVij

-- Symmetric Orthogonalisation

import q u a l i f i e d L i n e a r A l g e b r a a s LA
import q u a l i f i e d Data . Vector . Unboxed a s VU

symmOrtho :: (Monad m, VU.Unbox Double) => 
             Array U DIM2 Double ->
             m (Array U DIM2 Double)
symmOrtho ! arr = do
symOrtho arr = do
  eigData <− jacobi P $ arr
  let eigVal = LA.eigenvals eigData
      eigVecs = LA.eigenvec eigData
      invSqrt = VU. map (recip . sqrt) eigVal
      diag = LA.vec2Diagonal invSqrt
  eigVecTrans <− LA.transpose2P eigVecs
mtx1 <− LA.mmultP eigVecs diag
LA.mmultP mtx1 eigVecTrans

-- Compute the G matrix

import Data . Array . Repa
as R
calcGmatrix ! density ! i n t e g r a l s =
computeUnboxedP $ fromFunction ( Z : . dim )
( \ ( Z : . i ) −> sumAllS $
fromFunction ( Z : . n e l e c )
( \ ( Z : . l ) −>
l e t vec1 = u n s a f e S l i c e d e n s i t y ( getRow l )
vec2 = map2Array i n t e g r a l s s o r t K e y s ( i , l )
nelec
i n sumAllS . R. zipWith ( ∗ ) vec1 $ vec2 ) )
where getRow x = (Any : . ( x : : I n t ) : . A l l )
) = extent density
(Z : . n e l e c : .
dim = ( n e l e c ˆ2 + n e l e c ) ‘ div ‘ 2

map2Array : : M. Map [ I n t ] Double
−> ( [ I n t ] −> [ I n t ] )
−> ( Int , I n t )
−> N e l e c
−> Array D DIM1 Double
map2Array m a p I n t e g r a l s s o r t K e y s ( i , l ) n e l e c =
R. fromFunction ( Z : . n e l e c )
( \ ( Z : . indx ) −>
l e t coulomb = LA . map2val m a p I n t e g r a l s $ s o r t K e y s [ a , b , indx , l ]
exchange = LA . map2val m a p I n t e g r a l s $ s o r t K e y s [ a , l , indx , b ]
i n coulomb − 0 . 5 ∗ exchange )
where ne = n e l e c −1
p a i r s = [ ( x , y ) | x <− [ 0 . . ne ] , y <− [ 0 . . ne ] , x<=y ]
(a , b) = pairs ! ! i

-- Interface Function

data HFData = HFData {
getFock
, getCoeff
, getDensity
, getOrbE
, getEnergy
::
::
::
::
::
! ( Array U DIM1 Double )
! LA . E i g e n V e c t o r s
! ( Array U DIM2 Double )
! LA . E i g e n V a l u e s
! Double } d e r i v i n g ( Show )
scfHF : : (Monad m, VU. Unbox Double )
= [ NucCoord ]
>
−> [ B a s i s ]
−> [ ZNumber ]
−> N e l e c
−> m ( HFData )
scfHF c o o r d s b a s i s z l i s t n e l e c= do
l e t core = hcore coords b a s i s z l i s t nelec
d e n s i t y = LA . z e r o n e l e c
i n t e g r a l s = c a l c I n t e g r a l s coords basis nelec
xmatrix <− symmOrtho <=< LA . triang2DIM2 $ mtxOverlap c o o r d s
basis nelec
s c f c o r e d e n s i t y i n t e g r a l s xmatrix 0 500


-- Self-consistent Field Function

 (Monad m, VU. Unbox Double )
= Array U DIM1 Double
>
−> Array U DIM2 Double
−> M. Map [ I n t ] Double
−> Array U DIM2 Double
−> Step
−> I n t
−> m( HFData )
s c f ! c o r e ! o l d D e n s i t y ! i n t e g r a l s ! xmatrix s t e p maxStep
| s t e p < maxStep = do
fockDIM1 <− f o c k c o r e o l d D e n s i t y i n t e g r a l s
hfData <− diagonalHF fockDIM1 xmatrix
e t o t a l <− v a r i a t i o n a l E c o r e fockDIM1 o l d D e n s i t y
l e t newHFData = hfData { getFock=fockDIM1 , getEnergy=e t o t a l }
b o o l = c o n v e r g e o l d D e n s i t y . g e t D e n s i t y $ newHFData
case bool of
True −> r e t u r n newHFData
F a l s e −> s c f c o r e ( g e t D e n s i t y newHFData ) i n t e g r a l s
xmatrix ( s t e p +1) maxStep
| otherwise =
e r r o r ”SCF maxium s t e p s e x c e e d e d ”

-- Diagonal HF Function

diagonalHF : : (Monad m, VU. Unbox Double )
= Array U DIM1 Double
>
−> Array U DIM2 Double
−> m( HFData )
diagonalHF f o c k 1 xmatrix = do
fDIM2 <−newFock
f ’ <− LA . t o T r i a n g fDIM2
e i g D a t a <− j a c o b i P fDIM2
l e t ( c o e f f , orbEs ) = LA . e i g e n v e c &&& LA . e i g e n v a l s $ e i g D a t a
newCoeff <− LA . mmultP xmatrix c o e f f
newDensity <− LA . c a l c D e n s i t y newCoeff
r e t u r n $ HFData f ’ newCoeff newDensity orbEs 0 . 0
where newFock = (LA . u n i t a r y T r a n s f xmatrix ) <=< LA . triang2DIM2 $
fock1


-- Diagonal HF Function

v a r i a t i o n a l E : : ( Monad m, VU. Unbox Double ) =
>
Array U DIM1 Double −>
Array U DIM1 Double −>
Array U DIM2 Double −>
m Double
v a r i a t i o n a l E c o r e fockMtx o l d D e n s i t y =
( 0 . 5 ∗ ) ‘ l i f t M ‘ do
sumHF <− (R. computeUnboxedP $
R. zipWith (+) c o r e fockMtx ) >>= \ a r r −>
LA . triang2DIM2 a r r
r e s u l t <− LA . mmultP sumHF o l d D e n s i t y
LA . t r r e s u l t
Listing 3.14: The DiagonalHF Function


