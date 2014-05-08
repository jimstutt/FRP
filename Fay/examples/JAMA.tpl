<html>
<h1><a href src="http://math.nist.gov/javanumerics/jama/#Package">JAMA : Java Matrix Package</a></h1>

"JAMA is comprised of six Java classes:
<ul>
<li>Matrix</li>
<li>Cholesky Decomposition/li>
<li>LU Decomposition</li>
<li>QR Decomposition</li>
<li>Singular Value Decomposition</li>
<li>Eigenvalue Decomposition</li>
</ul>

<p>The Matrix class provides the fundamental operations of numerical linear algebra.</p>

<p>Various constructors create Matrices from two dimensional arrays of double precision floating point numbers. Various gets and sets provide access to submatrices and matrix elements.</p>

<p>The basic arithmetic operations include matrix addition and multiplication, matrix norms and selected element-by-element array operations.</p>

<p>A convenient matrix print method is also included.</p>

<p>Five fundamental matrix decompositions, which consist of pairs or triples of matrices, permutation vectors, and the like, produce results in five decomposition classes.</p>

<p>These decompositions are accessed by the Matrix class to compute solutions of simultaneous linear equations, determinants, inverses and other matrix functions.</p>

<p> The five decompositions are:</p>

<ul>
<li>Cholesky Decomposition of symmetric, positive definite matrices</li>
<li>LU Decomposition (Gaussian elimination) of rectangular matrices</li>
<li>QR Decomposition of rectangular matrices</li>
<li>Eigenvalue Decomposition of both symmetric and nonsymmetric square matrices</li>
<li>Singular Value Decomposition of rectangular matrices</li>
</ul>

The current JAMA deals only with real matrices. We expect that future versions will also address complex matrices. This has been deferred since crucial design decisions cannot be made until certain issues regarding the implementation of complex in the Java language are resolved.

The design of JAMA represents a compromise between the need for pure and elegant object-oriented design and the need to enable high performance implementations."

<h2>Object Manipulation</h2>
<ul>
<li>constructors</li>
<li>set elements</li>
<li>get elements</li>
<li>copy</li>
<li>clone</li>

<h2>Elementary Operations</h2>
<li>addition</li>
<li>subtraction</li>
<li>multiplication</li>
<li>scalar multiplication</li>
<li>element-wise multiplication</li>
<li>element-wise division</li>
<li>unary minus</li>
<li>transpose</li>
<li>norm</li>
<h2>Decompositions</h2>
<li>Cholesky</li>
<li>LU</li>
<li>QR</li>
<li>SVD</li>
<li>symmetric eigenvalue</li>
<li>nonsymmetric eigenvalue</li>

<h2>Equation Solution</h2>
<li>nonsingular systems</li>
<li>least squares</li>
<h2>Derived Quantities</h2>
<li>condition number</li>
<li>determinant</li>
<li>rank</li>
<li>inverse</li>
<li>pseudoinverse</li>

<h2>Example of Use</h2>

The following simple example solves a 3x3 linear system Ax=b and computes the norm of the residual.

<pre>
double[][] array = {{1.,2.,3},{4.,5.,6.},{7.,8.,10.}};
Matrix A = new Matrix(array);
Matrix b = Matrix.random(3,1);
Matrix x = A.solve(b);
Matrix Residual = A.times(x).minus(b);
double rnorm = Residual.normInf();
</pre>

<h2><a href src="http://math.nist.gov/javanumerics/jama/doc/">Documentation</a></h2>


</html>
