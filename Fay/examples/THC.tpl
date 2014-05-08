# Thermo-Haline Circulation

"paleoclimatic evidences suggest(s) that the large scale circulation of the Atlantic Ocean presents at least two, qualitatively different, stable modes
of operation".

* Boyle et al. (1987)
* Rahmstorf (2002)

## Very simple THC model
Stommmel (1961)
Rooth (1982)

### Robust 2D convective equation model
Cessi and Young (1992) 
Vellinga (1996) 
Lucarini et al. (2005, 2007)

### Simplified climate model
Stocker and Wright, 1991
Rahmstorf, 1995
Stocker and Schmittner (1997). 

"Whereas climate models of intermediate complexity now consistently
represent the THC as a multistable system (Rahmstorf et
al., 2005), results are not conclusive when full 3-D climate
models are considered (Stouffer and Manabe, 2003; Scott
et al., 2008). Nonetheless, recent simulations performed by
Hawkins et al. (2011) with a full 3-D climate model have suc-
cessfully reproduced the kind of bistability properties shown
by Rahmstorf et al. (2005). In the case of THC, the strength
of the hydrological cycle plays the role of dominant parame-
ter, whose variation can lead the system through bifurcations
(Sijp and England, 2006, 2011; Sijp et al., 2011). A detailed
account of these analyses can be found in Rahmstorf (1995),"

## [Dijkstra, H.: Nonlinear Physical Oceanography, Springer, Berlin,
(2005)](http://books.google.co.uk/books?hl=en&lr=&id=xcotccYpDXgC&oi=fnd&pg=PR15&dq=Dijkstra,+H.:+Nonlinear+Physical+Oceanography,+Springer,+Berlin,+2005.&ots=AytsKT4FaM&sig=QEjQThULsu1tw1hqcVipQ2ZfYQU#v=onepage&q&f=false)

[Research Institute for Mathematics and Computing Science, U. Groningen]()

### Eigensolver experts
Fred Wubs
Arie de Niet
Auke van der Ploeg (MARIN)
Eugen Botta

The climate system has 4 spheres:
* atmosphere
* hydrosphere
* biosphere
* cryosphere
* lithosphere

## Cycles
* Heinrich cycle (1988) [6000,7000] years
* Dansgaard-Oeschger cycle (1989) [1000,2500] years
* Bond cycle (1995) Progressive cooling over several Heinrich and Dansgaard-Oeschger cycles followed by an abrupt warming.

Last glacial maximum 25000 years ago.

  dq/dt = F(q,P)

where q is intensity of the THC. 
P are the parameters.

Solve:

  F(q,P) = 0

see #Djikstra (2005).

Friedel and Wentzel (1999) : matheatical treatment of the above.

Weaker meridional circulation would greatly reduce northward
heat transport.

This would be high impact but low likelihood Ramsdorf (2006).

{Recent report of simulation result showing this is highly 
unikely? TBD Find!}
 
Hasselmann (1996) established stochastic forcing as a
necessay model component: a surrogate for high-frequency
transients.

Fraedrich (1978)
Saltzman (2002)

### Effective Onde-Dimensional Langevin equation

  dx = F(x) + eta*dW

where dW is an increment in a Wiener process.
      F(x) is a smooth function of x giving the drift function.
      eta parameterises the strength of the forcing.


This opens the way to an approach in terms of the one-dimensional
Fokker-Plank equation.

### Invariant probability density Eq. 2

  prj x = c*exp (-2*v x/eta**2)

where v x is the effective potential s.t.

  dv/dx = -F x and
  c is a normalisation constant.

### Kramer's formula

Approximates the average rate of transition (r+ -> r-) from the basin of
attraction of x+ to that of x-.

Eq. 3. [p4]

  r = sqrt(d2v x/dx2*d2v x0)*exp(-2(v x0 - v x)/eta**2)/2*pi)

where

  v x0 - v x >= eta**/2

Physically this denotes moderate noise wrt. potential well depth.

The Kramer's formula represents the detailed balance conditionwi
which applies at a stationary state.

 y = -ln (prj y)

Compatibility with Eq. 2 requires Eq. 5:

  2*v y/eta**2 = U y + const

U y contains information on both the effective potentia of the systema and on
the effective intensity of the noise.

Eq. 6

  r = eta**2*sqrt()*exp(-u y0 - u y))/4*pi

## Cohomology of the on-shell Noether complex
Linear instabilities
Obstructions arise due to simultaneous presence
of rigid cosymmetries (generalised Killing condition)
and non-trivial de Rham cohomology (spacetime topology).

The analysis relies on cohomologies of:

* an on-shell Noether complex (consistent deformations)
* an adjoint Noether complex (rigid cosymmetries)
* a variational bicomplex (conserved currents).

An intermediate result gives a criterion for identifying non-linerities
which don't lead to linearisation instabilities.




## SR models

Velez-Belchi (2001) : first SR box model.
Ganopolski and Ramsdorf (2002) : a more realistic model.

## Can tipping points be highlighted by early indicators?
Sheffer et al. (2001).





#### Logistic equation

dX/dt = sigmag(1-X/Xm)

has the solution

 X = Xm/(1+(1-X/Xm)*exp(-sigmag t))

where
 
equilibrium at X = Xm.

