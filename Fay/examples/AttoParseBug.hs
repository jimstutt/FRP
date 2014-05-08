-- haskell-cafe : Sat, 16 Mar 2013 10:40:08 +0200 [02:40:08 AM MDT

import Parser.Attoparsec

bad = (pB *> pure (:) <*> pA <*> bad) <|> (pC *> pure [])

-- is different than,

good = do
  e <- eitherP pB pC -- (Left <$> pB) <|> (Right <$> pC)
     case e of
          Left  _ -> (:) <$> pA <*> good
          Right _ -> pure []

--
{-
I think the mistake here is to parse something and then decide if
its it valid. It should be the parser which decides whether its
valid. So rather than:


     suffix <- A.option "" ((:"") <$> A.letter_ascii)

try:
-}

best = do
     typ <- A.choice [ {- list or valid suffix parsers -} ]
     return $ Score.Typed typ num

--

I would agree that what attoparsec does for <|> of Alternative and mplus
for MonadPlus is correct since e.g. the mplus laws say that a failure
must be identity and therefore the following alternatives must be
considered. I also find it very convenient that attoparsec works this
way, and prefer it to what parsec does by default.
empty/mzero are indeed identities in Parsec.

What doesn't hold is the law

   v >> mzero = mzero

But this one is often violated:

  > flip runState 0 $ runMaybeT mzero
  (Nothing,0)

  > flip runState 0 $ runMaybeT $ lift (modify (+1)) >> mzero
  (Nothing,1)
--
{-
Nope that isn't the case either. Even if I make use of defaultSeed through create the problem still remains. The problem seems to be in the generation of a vector of (a,a) i.e in the part
-}

V.generateM ((round $ p*(fromIntegral $ l*z)) `div` 2) (\i-> R.uniformR ((0,0) , (l-1,l-1)) gen)

--in line 16.

