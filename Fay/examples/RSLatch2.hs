{-
  author : Remco Huizinga
  url    : http://Freferaat.cs.utwente.nl/conference/paper/modeling-the-behavior-of-an-rs-latch-in-haskell.pdf
  pdf    : paper_990.pdf
  date   : 1997

  "[T]here exists an exponential relation between the minimal
  time the bistable operates in metastability (tr) and the
  time interval ($latex Delta T (tr)) in the excitation
  space within the critical input window".

  "If it then holds that the setup time and hold time...are
  equal, the bistable (sic) is said to be symmetric."

The exponential equation:

  $latex \Delta \( t r \) = \delta \sup tpn/Tau exp - tr/Tau $

Parameters

* delta

"width of the critical input window
the time between transitions of the R and S
signals where metastability can occur".

*tpn

"the normal propagation time outside the critical
input window ie. the time it takes for the output
signal to change from one stable state to the
other stable state.".

$latex tr = tpn + Tau log \( Tsetup/g\)$
-}

{-# LANGUAGE FlexibleContexts, ImplicitParams  #-}

import Prelude
import FFI

data Signal = High | Low | MS Double -- deriving (Show, Eq, Ord)
data Drop = R Double | S Double | N -- deriving (Show, Eq, Ord)

type In = (Signal,Signal)
type Out = (Signal,Signal)
type State = (Signal,Signal)
type Time = Double
type TPN = Double
type Tau = Double
type SHtime = Double

--The paper gives this signature:

--  rslatch :: In -> Drop -> SHtime -> Tau -> TPN -> State -> (Time, Out)

-- ghci gives this signature:

--rslatch3 :: (Eq t, Eq t1, Num t, Num t1, Num t3, Num t4, Num t2) =>
--  (t, t1) -> t2 -> (t3, t4) -> (t2, (t3, t4))

rslatch3 (0,0) tpn state = (tpn,(1,1))
rslatch3 (1,0) tpn state = (tpn,(1,0))
rslatch3 (0,1) tpn state = (tpn,(0,1))
rslatch3 (1,1) tpn state = (0,state)
rslatch3 (_,_) _   _     = error "Invalid input"

main :: Fay ()
main = do
  showP t31
  showP t32
  showT t62
  showT t63
  showT t64
  showT t65

showT :: (Show a, Show b) => (b,(a,a)) -> Fay ()
showT t = do
          print $ fst t
          print $ (fst . snd) t
          print $ (snd . snd) t

showP :: (Show a) => (a,(a,a)) -> Fay ()
showP p = do
          print $ fst p
          print $ (fst . snd) p
          print $ (snd . snd) p
          
--t31, t32 :: (Double,(Integer,Integer))
t31 = rslatch3 (0,1) 0.1 (1,0)
t32 = rslatch3 (1,0) 0.1 (0,1)
  
{-
This shows us that, because the latch is active low, the
state doesn’t change when the inputs are both high.

MODELING THE RS-LATCH
This section will first introduce the equation the model is
based upon. According to [9] there exists an exponential
relation between the minimal time the bistable operates
in metastability (tr) and the time interval (∆T (tr)) in the
excitation space within the critical input window. The
term bistable refers to an electronic circuit that has 2 sta-
ble states at any given time so that it is possible to switch
between them. If it then also holds that the setup time
and hold time (see the next section) are equal, the bistable
is said to be symmetric. The exponential relation is given
by:

  tpn τ \exp(−tr/τ) ∆T (tr) = δe e

In this formula, δ is the width of the critical input window,
this is the time between transitions of the R and S signals
where metastability can occur. tpn is the normal propaga-
tion time outside the critical input window, i.e. the time it
takes for the output signal to change from one stable state
to the other stable state [4]. τ is a parameter unique to
each bistable. As is also stated in [1], the value of τ is not
usually included in the data sheet but can be determined
either experimentally or by electric simulation.
-}

rslatch6 :: In -> Drop -> SHtime -> Tau -> TPN -> State -> (Time, Out)

-- ghci gives this type signature:

t61, t62 :: (Time,Out)
t61 = rslatch6 (High,Low) N 1 1 1 (Low,High)
t62 = rslatch6  (Low,High) (R 0.3) 1 1 1 (High,Low)
t63 = rslatch6 (High,Low) N 0.1 0.1 4.0 (Low,High)
t64 = rslatch6  (Low,High) (R 0.3) 1 1 0.1 (High,Low)
t65 = rslatch6 (High,Low) (S 1.0) 3.0 1 2 (Low,High)
t66 = rslatch6  (Low,High) (R 0.3) 3 1 1 (High,Low)

-- Normal behavior

rslatch6 (Low, Low) _ _ _ tpn _     = (tpn, (High, High))
rslatch6 (High, Low) _ _ _ tpn _    = (tpn, (High, Low))
rslatch6 (Low, High) _ _ _ tpn _    = (tpn, (Low, High))
rslatch6 (High, High) _ _ _ _ state = (0, state)

-- Metastable by setup time violation

rslatch6 (MS g, _) d st tau tpn _ =
  let
    a = if (g < (0.001*st)) then (0.001*st) else g
    tr = tpn + tau*log(st/a) in
      case d of
           (R t) -> (t + tpn, (Low, High))
           (S t) -> (t + tpn, (High, Low))
           (N)   -> (tr, (Low, High))
         
-- Metastable by hold time violation

--t61a = rslatch6a (0,1) 0.1 (1,0) (0,1) 0.1 (1,0)
--t61b = rslatch6a (0,1) 0.1 (1,0) (0,1) 0.1 (1,0)

rslatch6a :: (t1, Signal) -> Drop -> Double -> Double -> Double -> t
             -> (Double, (Signal, Signal))
rslatch6a (_, MS g) d ht tau tpn _ =
  let
    a = if (g < (0.001*ht)) then (0.001*ht) else g
    tr = tpn + tau*log(ht/a) in
      case d of
           (R t) -> (t + tpn, (Low, High))
           (S t) -> (t + tpn, (High, Low))
           (N)   -> (tr, (High, Low))
rslatch6a _ _ _ _ _ _  = error "Invalid input."

-- 3.2.2 Runt pulse violation

{-data Latch = Latch SHtime Tau Twfil Twmin TPN
--  deriving (Show, Eq, Ord)

type Twfil = Double
type Twmin = Double

-- ghci says:

rslatch4 :: (t1, Drop) -> Drop -> Latch -> t -> (Double, (Signal, Signal))
rslatch4 (_, R tw) d (Latch _ tau twfil twmin tpn) _
  | (tw < twfil) = (0, (Low, High))
  | (tw >= 0.5*(twfil+twmin)) =
      let tr = tpn + tau *log((0.5*(twmin-twfil)) / a)
        in case d of
               (R t) -> (t + tpn, (Low, High))
               (S t) -> (t + tpn, (High, Low))
               (N)   -> (tr, (High, Low))
  | (tw < 0.5*(twfil+twmin)) =
      let tr = tpn + tau *log((0.5*(twmin-twfil)) / b)
        in case d of
               (R t) -> (t + tpn, (Low, High))
               (S t) -> (t + tpn, (High, Low))
               (N)   -> (tr, (Low, High))
          where
            a = if (tw <= 0.5005*(twfil+twmin))
                then (0.0005*(twfil+twmin))
                else (tw - 0.5*(twfil+twmin))
            b = if (tw >= 0.4995*(twfil+twmin))
                then (0.0005*(twfil+twmin))
                else (0.5*(twfil+twmin) - tw)

hole = undefined
data Hole = Hole
data Hole1 a = Hole1
data Hole2 a b = Hole2

-- TBD "apply tag essentially lets us jump between files"

myNot a = hole --not a
myNand a b = hole -- if a then b

dlatch :: In -> Drop -> Latch -> State -> (Time, Out)
dlatch (d, c) drp latch state
  -- Normal behavior
  | ((d==High || d==Low) && (c==High || c==Low)) =
                  -- s::Drop
      let (t, o) = rslatch4 (r, s) drp latch state
       -- (r,s) :: (Drop,Drop) 
          (r, s) = (Hole,hole) -- (myNand c (myNot d), myNand c d)
      in
      case t of
           0         -> (0, o)
           otherwise -> (2 + t, o)
  -- Metastable behavior
  | otherwise =
     let (t, o) = hole --rslatch4 (d,c) drp latch state
     in (2 + t, o)

--(r, s) = (myNand c (myNot d), myNand c d)

[Jim Duggan, DES vs PetriNet](www.systemdynamics.org/conferences/2006/proceed/papers/DUGGA171.pdf)

Duggan, J. and Browne, J. (1988) 'ESPNET: Expert System Based Simulator of Petri Nets'. IEE Proceedings-D Control Theory and Applications, 135 (4). [Details]

Duggan, J. (2002) 'A Distributed Computing Approach to System Dynamics'. System Dynamics Review, 18 (1):87-98. [DOI] [Details]


Xing, Y., Madden, M., Duggan, J., and Lyons, G. (2003) 'Context-Based Distributed Regression in Virtual Organizations'. ECML-2003 Workshop on Parallel and Distributed computing for Machine Learning; The 14th European Conference on Machine Learning and the 7th European Conference on Principles and Practice of Knowledge Discovery in Databases, . [Details]
What is disributed regression?

Conneely, B., Duggan, J. and Lyons, G. (2003) 'A Distributed System Dynamics-Based Framework for Modeling Virtual Organizations'. International System Dynamics Conference, . [Details]

Duggan, J. (2004) 'Policy Diffusion in the Beer Game'. 22nd International Conference of the Systems Dynamics Society, . [Details]

Duggan, J. (2004) 'System Dynamics Simulation for Software Development'. Simulation in Manufacturing, Services and Logistics Workshop, . [Details]

Duggan, J (2008) 'Equation-based policy optimization for agent-oriented system dynamics models'. System Dynamics Review, 24 :97-118. [DOI] [Details]


J. Duggan (2010) 'Computational Modelling and Simulation for an Interconnected World'. ERCIM News, . [Details]

E. Howley and J. Duggan. (2011) 'Investing in the Commons: A Study of Openness and the Emergence of Cooperation'. Advances in Complex Systems, 14 (2):229-250. [DOI] [Details]

D. Mungovan, E. Howley & J. Duggan (2008) Modelling Antivirus Defence Strategies in Scale Free Networks Procs. of the Nineteenth Irish Conference on Artificial Intelligence and Cognitive Science University College Cork, Cork, , 27-AUG-08 - 29-AUG-08 [Details]
J. Duggan (2008) Statistical Thinking Tools for System Dynamics 26th International Conference of the Systems Dynamics Society Athens, [Details]

J. Huang, E. Howley & J. Duggan. (2010) An Eigenvector Approach for Analysing Linear Feedback Systems 27th International System Dynamics Conference Seoul, South Korea, [Details]

D. Mungovan, E. Howley & J. Duggan. (2010) A Generative Approach to Constructing Dynamic Networks with Small World Properties 5th International Workshop on Emergent Intelligence on Networked Agents (WEIN 2010) at AAMAS ?10 Toronto, Canada, [Details]
J. Huang, E. Howley & J. Duggan. (2010) An Extension of Loop Deactivation in the Behavioral Method 27th International System Dynamics Conference Seoul, South Korea, [Details]

J. Duggan (2011) iSimPlus: A Multi-Method Simulation Tool for Modelling Complex Systems Proceedings of the 29th International Conference of the System Dynamics Society Washington DC, , 24-JUL-11 - 28-JUL-11 [Details]

J. Duggan (2011) A Multi-method Equation-based Approach for Healthcare Modeling INFORMS Healthcare 2011 Conference ? Modelling and Optimization Track Montreal, Canada, , 20-JUN-11 - 22-JUN-11 [Details]
E. Howley & J. Duggan (2011) Tag-Based Cooperation in N-Player Dilemmas Proceedings of the 10th International Conference on Autonomous Agents and Multiagent Systems Taipei, Taiwan, [Details]

http://www.google.co.uk/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&ved=0CDQQFjAA&url=http%3A%2F%2F

http://www.azimuthproject.org/azimuth/show/Thermal+energy+storage
http://www.azimuthproject.org/azimuth/show/Zero+carbon+Britain+2030

ADAM , see [HBG+11]

http://www.azimuthproject.org/azimuth/show/Combined+Heat+and+Power+%28CHP%29+Plants

[K. Jensen, L.M. Kristensen, Coloured Petri Nets, DOI 10.1007/b95112, (C) Springer-Verlag Berlin Heidelberg 2009](http://www.cs.au.dk/~cpnbook/)

www.systemdynamics.org%2Fconferences%2F2006%2Fproceed%2Fpapers%2FDUGGA171.pdf&ei=HlpdUeGnO8is0QX8yIGIAQ&usg=AFQjCNHSn6sl6g1vZR2id8Ep2BQrmI3GSw&sig2=5HRIwPg9YhZzNTZ0JoQpXA&bvm=bv.44770516,d.d2k

http://frigidcode.com/
-}


