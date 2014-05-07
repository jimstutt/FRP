* [David Deutsch: Quantum Theory, the Church-Turing
principle and the universal quantum computer, 1985]
* [Proc. RSL, A 400, pp. 97-117 (1985)]

## Church-Turing hypothesis
'Every 'functionwhich would normally be regarded as computable' can be computed by the universal Turing machine' [p3].

### quasi-mathematical view:
algorithm ~ computation
# physical view [Church-Turing *Principle*]:

* [Hofstadter, 1979] : various interpretations.

### Church-Turing *Principle*

natural : computable in Nature

### Perfect Simulation
'A computing machine M is capable of perfectly simulating a physical system S, under a given labelling of their inputs and outputs, if there exists a program pi(S) for M which renders M computationally equivalent to S under that labelling. In other words, pi(S) converts M into a 'black box' functionally indistinguishable from S.

### Physical Church-Turing hypothesis
'Every finitely realizable physcial system can be perfectly simulated by a universal model computing machine operating by finite means.' [p3].

#### Advantages over Turing and other formulations:
* better defined and more physical
* refers exclusively to objective concepts such as ['measurement'], ['preparation'] and ['physical system'] which are already there in [measurement theory].
* ''Finite means' can be defined axiomatically without restrictive assumptions about the form of physical laws'.

* [Robin Gandy, 1980].

### Finite Means
A computing machine proceeds in a sequence of steps whose duration has a non-zero lower bound, then it operates by 'finite means':

* if only a finite subsystem (though not always the same one) is in motion during any step, and
* if the motion depends only on the state of a finite subsystem, and
# if the rule that specifies the motion can be given finitely in the mathematical sense (eg. as an integer).
* [Turing machines] satisfy these conditions and so does the universal quantum computer Q.
* This statement of the CT principle is much stronger than the intial formulation by CT and is not satisfied by any Turing machine in classical physics.
** The possible states of a classical system necessarily form a continuum due to the continuity of classical dynamics.
** Yet there are only finitely many ways of preparing a finite input for T.
** Thus T cannot perfectly simulate any classical
dynamical system. [p4].
* {practically treated as imperfect successive discrete approximation}

## Third law of thermodynamics
'No finite process can reduce the entropy or temperature of a finitely realizable physical system to zero'.
* not directly refutable cos no temperature measurement of finite accuracy could distinguish absolute zero from an arbitrarily small positive temperature.
* {interesting question about fluctuations into negative temperature? TBD}
# Similarly, since the number of possible programs for a universal computer is infinite, no experiment could in general verify that none of them can simulate a system that is thought to be a counter-example to (1.2).

### Verification
* The usual criterion for the empirical status of a theory is that it be refutable [Popper, 1959].
* 'Principles' are not outside the realm of empirical science.
* They are essential frameworks within which directly testable theories are formulated.
* Logic alone determines whether or not a given physical theory contradicts a principle.
* If the directly testable theory but contradicts the principle, then that principle is deemed to be refuted, albeit indirectly.
* If all known experimentally-corroborated theories satisfy a restrictive principle, then that principle is corroborated and becomes, on the one hand, a guide in the construction of new theories, and on the other, a means of understanding more deeply the content of existing theories.
# It is often claimed (uncited?) that every 'reasonable' *physical* (as opposed to mathematical) model for computation, at least for the deterministic computations of functions from Z to Z, is equivalent to Turing's. But this is not so; there is no *a priori* reason why physical laws should respect the limitations of the mathematical processes we call algorithms (i.e. the functions C(T)).' [p4].
* There is nothing paradoxical in postulating physical systems which compute functions not in C(T).
* {non-constructive proofs which might be an sign of not being permitted by physical models. TBD Redo}
# [Chaitin, 1977] has shown how the truth values of all 'interesting' non-Turing decidable propositions of a given formal system might be tabulated very efficiently in the first few digits of a single physical constant.
* 'But if they were, it might be argued, we could never know because we could not check the accuracy of the 'table' provided by Nature. This is a fallacy.'
* We believe the empirical theory in calculator design not some check by other machines.
# [Feynman, 1982] doubts that his [universal quantum simulator] can simulate fermion systems. Why? TBD.
* 'it is not a computing machine in the sense of this article' although it 'can surely simulate any system with a finite-dimensional state space. {Duhh?}.
* [Benioff, 1982]: quantum kinematic and dynamic model for computation, but still classical in the non-obedience of [CT].

## Random Numbers and discrete stochastic systems
When the program:

    phi(V_8, 2) . pi(I, 2, a)

halts, slot a contains with probability 1/2 either a zero or a one.
* {ie only one *result* bit needed for this definition of random!}
** Iterative programs can then generate other probabilities, including any probability which is a recursive real.
** Q can do more...

#### A first quantum program

    1/sqrt(2) |pi((I, 2, a)> (|cos theta>|0> + sin theta |1>)

yields in slot a, a bit which is zero with probability

    cos^2 theta

* The whole continuum of states of this form are valid programs for Q.
* NB Valid programs exist with arbitrary irrational probabilities cos^2 theta and sin^2 theta.
* Thus Q can simulate every discrete finite stochastic system.

## Perfect simulation of arbitrary finite physical systems
'A faithful description of a finitely realizable physical system with an L-dimensional state space H cannot therefore be made *via* state vectors in H but must use density matrices rho_a^b.
* 'Indeed, all density matrices are allowed in principle except (thanks to the 'entropy' hals of the third law) pure cases.' [p11].
# The multiverse has unitary dynamics
* H' : heat bath role

## Black hole entropy
'If the theory of thermodynamics of black holes is trustworthy, no system enclosed by a surface with an appropriately defined area A can have more than a finite number (Bekenstein, 1981)

    N(a) = exp(Ac^3/4hbarG)

of distinguishable accessible states. That is, in a suitable basis the system can be perfectly described by using an N(A)-dimensional state space, and hence perfectly simulated by Q.' [p13].

# [Bennett, 1983] reviewed several different measures of complexity (or 'depth', or 'knowledge') that have been proposed. Most suffer from the fatal disadvantage that they assign a high 'complexity' to a purely random state. Thus they do not distinguish true knowledge from mere information content. Bennett has overcome this problem. His 'logical depth' is roughly the running time of the shortest T-program that would compute a given state psi from a blank input. Logical depth is at a minimum for random states. Its intuitive physical justification is the 'likeliest explanation' why a physical system might be found to be in the state psi is that psi was indeed computed from that shortest T-program. In biological terminology, logical depth measures the amount of evolution that was needd to evolve psi from the simplest possible precursors.'

# 'In physical reality most random states are not generated by 'long programs' (i.e. Precursors whose complexity is near to their own), but by short programs relying on indeterministic hardware.
However there is a quantum analogue of Bennett's idea which solves this problem. Let us define the Q-logical depth of a quantum state as the running time of the shortest Q-program that would generate the state from a blank input (or, perhaps ...the harmonic mean of all such programs). Random numbers can be generated by *short* Q-programs.'

### Fundamentals
'We have seen that quantum theory obeys the strong form of the [CT principle] only on the assumption that the third law of thermodynamics is true. This relation is probably better understood by considering the [CT principle] as more fundmental and deriving the third law from it and quantum theory.

# 'The fact that classical physics does no obey (the CT principle) tempts one to go further. Some of the features that distinguish quantum theory from classical physics (for example the discreteness of observables?) can evidently be derived from (1.2) and the laws of thermodynamics alone. The new principle has therefore given us at least part of the solution to Wheeler's problem 'Why did quantum theory have to be?' [eg. Wheeler, 1985]. [p17].

# 'Various 'arrows of time' that exist in different areas of physics have by now been connected and shown to be different manifestations of the same effect. But, contrary to what is often asserted, the 'psychological' or 'epistemological' arrow of time is an exception.'

'Before Bennet (1973) it could be maintained that computation is intrinsically irreversible, and since psychological processes such as the growth of knowledge are computations, the psychological arrow of time is necessarily aligned with the direction in which entropy increases. This view is now untenable, the alleged connection fallacious.'

# DD guesses that Q-logical depth might be non-decreasing and provide a basis for deriving the second law and connecting psychological (or epistemological) and thermodynamic 'arrows of ime'.' [p17].

### Presumptive experiments
* measuring coupling constants
* determining the form of interactions because the result must be known to write the program. {How does this come about? TBD}

### EPR Q-program

    int n = 8 * random
    bool x, y = false 	// irreversible preparation
    V(8, y)			// see eq 2.15
    x eorab y 			// perfect measurement
    if V(n, y) != V( n, x) // measure y in the random direction and x in the parallel direction.

      then ("QM refuted") else ("QM corroborated").

QED.

#### Source: [deutsch85.pdf]

* Rule One: Take it only as far as you're interested!

* Classification of monsters to produce a supermarket.

## Seth Lloyd. 

* Seth Lloyd, [Chapter 7, Director's Cut - Random House, Inc.
](www.randomhouse.com/kvpa/lloyd/SethLloyd.pdf‎)

* Ng, J., Phys. Rev. Lett. 86, 2946-2947 (2001). 19. Lloyd, S., ArXiv/quant-ph/0110141. 
* Eddington, A.S., The Mathematical Theory of Relativity, Cambridge ...
* [Computational capacity of the universe](SethLlloyd.pdf)

* Bekenstein, J.D., Phys. Rev. D. 23, 287 (1981); Phys. Rev. Letters 46, 623 (1981);
Phys. Rev. D. 30, 1669-1679 (1984).
* ’t Hooft, G., gr-qc/9310026; Susskind, L., J. Math. Phys. 36, 6377 (1995).
* Zizzi, P., Entropy 2, 36-39 (2000).
* Zizzi, P., ‘The Early Universe as a Quantum Growing Network,’ in Proceedings of IQSA Fifth Conference, March 31-April 5, 2001, Cesena-Cesenatico, Italy; gr-qc/0103002.
* Ng, J., Phys. Rev. Lett. 86, 2946-2947 (2001).
* Lloyd, S., ArXiv/quant-ph/0110141.
* Eddington, A.S., The Mathematical Theory of Relativity, Cambridge University Press, Cambridge (1924).
* Fredkin, E., ‘Ultimate Limits to Computation,’ MIT Laboratory for Computer Science Seminar (2000).
* Banks, T., Fischler, W., Shenker, S.H., Susskind, L. ‘M Theory As A Matrix Model: A Conjecture,’ Phys. Rev. D55, S112-S118 (1997).

#### Hardware Random Number generator

Cannot exist.

#### Random 'oracle'
Dunno? Can't match Q.
* [Bennett, 1981].

### Restricted Signal Licenses (RSL)



## Tamil english words
cash : casser cash till
cashew nut
orange
mango
catamaran : cat = tied maran = wood
teak : wood teakmaran

## FAQ: What types of numbers are there?
Pi is irrational.
sqrt(2) is irrational.
numeral
ordinal
cardinal
natural number = postive integer
real
rational
irrational
complex
integer
sqrt(-1) : imaginary or 90 degree rotation.
0 :
oo : infinity
dx : infinitessimal
aleph-0 : sup oo TBD.
