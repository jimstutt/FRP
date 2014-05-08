> module Eigen where
>-- import Data.Applicative
>-- import Data.Array.Repa


Eigensystem : www.cc.nce.edu.tw/~wttsai/Math

A is an n x n  square matrix with n eigenvalues lambda[i] and eigenvectors
u[i] where:

  A u = lambda u

Zeros of polynomials of degree n where

  p n lambda = abs (A - lambda I)

  A - lambda I == 0 is the characteristic equation of A.
  
> p n lambda i a | (a - lambda id) == 0 = lambda i

If the matrix is Hermitian then the lambda i are real and the eigenvectors u i are
orthogonal.

> a = Array 2 2  
> tr a = Sum i lambda i
> abs a = powerSet lambda i

If S is a symmetric matric, Gamma is the diagonal matrix whose diagonal
elements are the eigenvalues of S and U is the matrix whose columns are
the normalised eigenvectors of A

  U^T SU = Gamma
   S = U Gamma U^T

If x is an approximation to an eigenvector of A the

  x^T Ax/(x^T x) Rayleigh's quotient is an approximation to the
  corresponding eigenvalue.
  
  
  
beast-mcmc

  /**
     * This function rescales the eigen values; this is more stable than
     * rescaling the original Q matrix, also O(stateCount) instead of O(stateCount^2)
     */
    public void normalizeEigenValues(double scale) {
        int dim = Eval.length;
        for (int i = 0; i < dim; i++)

            Eval[i] /= scale;
    }

SequenceL Haskell DSL

matmul(x(2), y(2))[i, j] := sum( x[i, all] * y[all, j]);

> multMat :: [[Int]] -> [[Int]] -> [[Int]] 
> multMat m1 m2 = multMatT m1 (transpose m2)

> multMatT :: [[Int]] -> [[Int]] -> [[Int]] 
> multMatT m1 m2T = [[multVec row col]| col <- m2T, row <- m1]

[multVec row coljcol]

> multVec :: [Int] -> [Int] -> Int
> multVec v1 v2 =

> multMatPar :: Int: -> [[Int]] -> [[Int]] 
> multMatPar z m1 m2 = multMat m1 m2 `using` strat z
 
> strat = blockStrat
> lineStrat c matrix = -- best?
>   let blocks = concat
>     splitIntoClusters numB matrix -- result split
>                                                  -- in numB * numB blocks

> numB = round (sqrt(fromIntegral(length matric)/fromIntegral c))

> type Vector = [It]
> type Matrix ] [Vector]
 
> splitIntoClusters :: Int -> Matrix -> [[Matrix]]
> splitIntoClusters c m | c < 1 = splitIntoClusters 1 m
> splitIntoClusters c m1 = mss where
>   bh = kPartition (length m1) c

> bhsplit [][] = []
> bhsplit [] _ = error "some elements left over."
> bhsplit (t:ts) xs = hs : (bhsplit ts rest) where
>   (hs,rest) = splitAt t xs
>   ms = bhsplit bh m1 == blocks of rows
>   mss = map (colsplit bh) ms
>   colsplit (t:ts) rss
>     |head rs == [] = []
>     | otherwise =
>        (cab:colsplit ts resto) where
>           (cab,resto) = unzip
>        map (splitAt t) rs

> kPartition :: Int  -> It  -> [Int]
> kPartition n k = zipWith (+) ((replicate (n `mod` k)1) ++ repeat 0)
>   replicate k (n `div` k)  
 
 
>-- type Matrix = RealSymmetric | Hermitian 

Kagstrom and Ruhe [25]

Jordan Normal Form

> jnf = undefined

Schur Decomposition

QR Decomposition

Sturm Sequence Evaluation

Parallel Jacobi Algorithms (Sameh [30])
"Simultaneous elimination of several off-diagonal elements by a given 
orthogonal orthogonal matrix rather than only one rotation as is done
in the serial version." [http://bit.ly/IkFgal]

Parallel Block Jacobi Agorithm (Ginemez et al. [19])

Denolle et al., Solving the Surface-Wave Eigenproblem with 
Chebyshev Spectral Collocation (2011).

JAMA Eigendecomposition

Jama
Class EigenvalueDecomposition

java.lang.Object
  extended by Jama.EigenvalueDecomposition

All Implemented Interfaces:
    Serializable

public class EigenvalueDecomposition
extends Object
implements Serializable

Eigenvalues and eigenvectors of a real matrix.

If A is symmetric, then A = V*D*V' where the eigenvalue matrix D is diagonal and the eigenvector matrix V is orthogonal. I.e. A = V.times(D.times(V.transpose())) and V.times(V.transpose()) equals the identity matrix.

<pIf A is not symmetric, then the eigenvalue matrix D is block diagonal with the real eigenvalues in 1-by-1 blocks and any complex eigenvalues, lambda + i*mu, in 2-by-2 blocks, [lambda, mu; -mu, lambda].</p>

<p>The columns of V represent the eigenvectors in the sense that 

<pre>A*V = V*D, i.e. A.times(V) equals V.times(D).</pre> 

The matrix V may be badly conditioned, or even singular, so the validity of the equation A = V*D*inverse(V) depends upon V.cond().


McIntyre

This comment was made on RealClimate:

"McIntyre has extensive expertise and experience in statistical analysis. In what ways do his recent posts on the subject of the post here depend on subject areas other than those for which he has expertise and experience?
Lacking any errors, or, heaven forbid, straying outside his primary areas of extensive expertise, in what ways is he contributing to the destruction of science?
Pointers to specific examples in McIntyre’s recent posts relating to this post would be very useful."

McIntyre's peer-reviewed publication list [GRL05]

http://www.geo.lsa.umich.edu/climate/approach.html

http://home.badc.rl.ac.uk/mjuckes/mitrie_files/docs/mitrie_borehole.pdf

http://www.agu.org/journals/gl/gl0520/2005GL023586/2005GL023586.pdf
http://www.agu.org/journals/gl/gl0520/2005GL023586/2005GL023586.pdf
[NASA 1998 population Projections](http://sedac.ciesin.columbia.edu/tg/guide_glue.jsp?rd=pp&ds=7.1.3)


