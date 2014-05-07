*** Parser:
*** Renamer/typechecker:

src/Snap/Snaplet/Fay.hs:126:25:
    Could not deduce (Data f2) arising from a use of ‘showToFay’
    from the context (Show f2)
      bound by the type signature for
                 toFayax :: Show f2 => Handler h1 h2 f2 -> Handler h1 h2 ()
      at src/Snap/Snaplet/Fay.hs:123:12-58
    Possible fix:
      add (Data f2) to the context of
        the type signature for
          toFayax :: Show f2 => Handler h1 h2 f2 -> Handler h1 h2 ()
    In the second argument of ‘(.)’, namely ‘showToFay’
    In the second argument of ‘(.)’, namely ‘A.encode . showToFay’
    In the first argument of ‘(=<<)’, namely
      ‘writeLBS . A.encode . showToFay’
Upsweep partially successful.
*** Deleting temp files:
Deleting: /tmp/ghc3495_0/ghc3495_11.s
Warning: deleting non-existent /tmp/ghc3495_0/ghc3495_11.s
link(batch): upsweep (partially) failed OR
   Main.main not exported; not linking.
*** Deleting temp files:
Deleting: 
*** Deleting temp dirs:
Deleting: /tmp/ghc3495_0
/home/jim/Sandbox/GHC/bin/ghc returned ExitFailure 1


* http://delivery.acm.org/10.1145/2620000/2611829/p20-meijer.pdf?ip=78.145.51.9&id=2611829&acc=open&key=4D4702B0C3E38B35.4D4702B0C3E38B35.4D4702B0C3E38B35.6D218144511F3437&CFID=448395638&CFTOKEN=28083190&__acm__=1398702350_78e7895ed5c86c177c4c337559daa2a4

 The fundamental problem with side effects is that there are many uncontrollable ways to observe them, and even worse, it is often possible to simulate one effect with another. Adding recursion to pure combinatorial circuits makes it possible to build flip-flops that provide mutable state. You can prove that state and delimited continuations suffice to simulate any effect, and that unchecked exceptions can simulate continuations.
You cannot be careful enough in dealing with effects.  The fundamental problem with side effects is that there are many uncontrollable ways to observe them, and even worse, it is often possible to simulate one effect with another. Adding recursion to pure combinatorial circuits makes it possible to build flip-flops that provide mutable state. You can prove that state and delimited continuations suffice to simulate any effect, and that unchecked exceptions can simulate continuations. You cannot be careful enough in dealing with effects. 

 Every object oriented programming system on the planet in a strict language has some kind of esoteric rule about what you can do during constructors and initialization order and/or has a pile of ad hoc systems for backfilling the settings you couldn't make in the constructor as a result. Laziness makes all of that go away.

* [Ed Kmett](https://plus.google.com/116476108003497242388/posts/hERu6Ls9KA5)

