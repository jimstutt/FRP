forLoop :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
forLoop start cond inc f = go start (return ())
  where
   -- go !x m | cond x = go (inc x) (m >> f x)
      go !x | cond x = f x >> go (inc x)
          --  | otherwise = m
            | otherwise = return ()

{-
This is just a short notice that using

   foldl' (+) 0 [0..100000::Int]

is over 10 times slower than using

  flip execState 0 $
    forLoop (0 :: Int) (< n) (+1) $ \i -> do
      x <- get
      put $! x + i

with `loopFor` as on https://github.com/nh2/loop.

Even using an IORef is twice as fast as the pure foldl' (but still 5
times slower than strict State).

The benchmark is at
http://htmlpreview.github.io/?https://github.com/nh2/loop/blob/master/results/bench-foldl-and-iorefs-are-slow.html.

(All benchmarks are linked from https://github.com/nh2/loop.)

You can see there that the problem is gone when using Vector.foldl', but
only for Int - for Word32 it persists.

It seems that manual looping is beneficial even when dealing with prime
examples of pure code that GHC ought to optimize well.

Niklas
-}

{-
Ice-sheet growth is slow but melting is fast. It isn't the coldness of winter which causes ice-sheet growth but the coldness of summer failing to melt the ice-sheets; ice accumulates, increasing albedo and thus increased heat reflection out of the atmosphere and consequent cooling of the climate.
If linear and periodic forcings are modulated out of the model, is there a residual stochastic component in glaciation or deglaciation?

Piketti
http://theconversation.com/piketty-has-redefined-capital-after-200-years-of-confusion-25770?utm_medium=email&utm_campaign=The+Weekend+Conversation&utm_content=The+Weekend+Conversation+CID_44a992f73baca7903c1a33f4039f0528&utm_source=campaign_monitor_uk&utm_term=Piketty%20has%20redefined%20capital%20after%20200%20years%20of%20confusion

 Joseph Schumpeter, who wrote in his History of Economic Analysis:

    The concept was essentially monetary, meaning either actual money, or claims to money, or some goods evaluated in money … What a mass of confused, futile, and downright silly controversies it would have saved us, if economists had had the sense to stick to those monetary and accounting meanings of the term instead of trying to ‘deepen’ them!


Author

    Geoffrey M. Hodgson

    Research Professor, Hertfordshire Business School at University of Hertfordshire

Disclosure Statement

Geoffrey M. Hodgson does not work for, consult to, own shares in or receive funding from any company or organisation that would benefit from this article, and has no relevant affiliations.

The Conversation is funded by the following universities: Aberdeen, Birmingham, Bradford, Bristol, Cardiff, City, Durham, Glasgow Caledonian, Goldsmiths, Lancaster, Leeds, Liverpool, Nottingham, The Open University, Queen's University Belfast, Salford, Sheffield, Surrey, UCL and Warwick.

It also receives funding from: Hefce, Hefcw, SAGE, SFC, RCUK, The Nuffield Foundation, The Wellcome Trust, Esmée Fairbairn Foundation and The Alliance for Useful Evidence
Events

    Food production and food security under climate change: Results from the IPCC assessment — University of Leeds — Leeds

    FOOD FOR THOUGHT (PAUL ARCHER) — University of Bradford — Bradford, Bradford

    The Energy Crisis Revisited: the 1970s and 2010s in Comparison — Cardiff University — Cardiff [Caerdydd GB-CRD]

    Sir Michael Barber lecture: Getting every child into school and learning; why wait? — University of Warwick —

More Events
Those houses won’t buy themselves. David Muir, CC BY-NC-ND

Thomas Piketty’s bestselling book Capital in the Twenty-First Century shows that in a selection of developed countries the stock of capital is growing faster than economic output, causing disturbing increases in wealth inequality. His thesis has sparked a major global debate about capitalism, inequality and taxation policy.

His redefinition of the central concept of capital has received less attention. For Piketty, capital is a saleable asset that can receive a monetary return. This is the everyday notion of capital that is used in business circles. Hence, for Piketty, capital includes cash, bonds, and shares, collateralisable assets such as buildings, land, machinery, and intellectual property. From its origins in Italy in the 13th century, capital has meant the fund of money to be invested, or the monetary value of tangible or intangible assets owned and usable as collateral.

By contrast, since Adam Smith, economists have contrived definitions of capital that are very different from that found in everyday business. Elsewhere I outline this change of usage and argue for a return to the original definition. Shifting attention from ownership rights in assets to the physical things themselves, Smith changed the meaning of “capital” from the money-value of an investment to the physical assets that were purchased and used in production. Smith further argued that labour was a productive resource and could also be regarded as “capital”. The term “human capital” was devised later.

Since Smith, economists have long argued over the meaning and theory of capital. But generally they have treated as physical assets, powers, or resources that are involved in the production of wealth. Karl Marx tried to link “capital” to the class structure of the capitalist system, but he still referred to physical assets. The Cambridge capital controversies of the 1960s and 1970s were also mostly over problems of measuring lots of different physical resources. Issues of ownership and collateralisation have been side-lined by economists for more than 200 years.

After Nobel Laureate Gary Becker popularised the term “human capital” in the 1960s, the word “capital” has become promiscuous. Sociologists Pierre Bourdieu and James Coleman promoted the term “social capital”. Now we also have “environmental capital”, “health capital”, “symbolic capital”, “cultural capital”, “political capital” and even “erotic capital”. Economists and sociologists have stretched the meaning of the term to cover any asset.

These highly extended usages of the term are far-removed from the business meaning of capital as the money value of an investment. Take the terms “social capital”, “symbolic capital” or “cultural capital”. These refer to assets that largely are not owned as property, cannot be sold, and cannot be used as collateral. But extensions of secure property rights, and the development of financial institutions through which property can be used as collateral to secure loans for investment, have been hallmark developments in the evolution of capitalism over the past 300 years.

A few economists have objected to the perversion of meaning of the term capital. One of these was Joseph Schumpeter, who wrote in his History of Economic Analysis:

    The concept was essentially monetary, meaning either actual money, or claims to money, or some goods evaluated in money … What a mass of confused, futile, and downright silly controversies it would have saved us, if economists had had the sense to stick to those monetary and accounting meanings of the term instead of trying to ‘deepen’ them!

What about “human capital”? Humans are fully saleable and usable as collateral only when they are slaves. In the [Cambridge Journal of Economics](http://cje.oxfordjournals.org/content/early/2014/04/03/cje.beu013.abstract) I point out that the first known usage of the term – in 1842 – was in the context of slavery.

Piketty likewise restricts the description of humans as capital to when they are slaves. In his book he points out that the capital value of slaves from the founding of the United States until 1863, was about 150% of annual national income. With Abraham Lincoln’s 1863 Emancipation Proclamation, they were no longer objects of alienable property. As slaves were transformed into wage labourers, the human component of total US capital assets was reduced to zero.

Piketty had to reverse more than two centuries of abuse by economists and sociologists of the notion of capital to make his powerful empirical and theoretical case. His data are based on a commendable redefinition of the concept that ends an age of obscurantism.

Capitalism is arguably a historically specific system where capital plays a dominant role. Other forms of so-called “capital” have a much greater longevity. “Social capital” goes back to the primates. If “human capital” means any learned capacity for labour, then that too goes back millions of years. The use of stone tools (“capital goods”?) also has great longevity.

But the life of “capital” as a saleable asset valued in money is less than one-thousandth that of social capital, human capital or capital goods. Capital (as defined by business people, Schumpeter, Piketty and myself) is much more historically specific than its purported relatives, and hence is much more useful in identifying the highly dynamic system of capitalism that first emerged a few hundred years ago.

http://www.opednews.com/articles/The-Land-Cycle-and-the-Sto-by-Scott-Baker-130728-228.html

Geoffrey M. Hodgson, [What is capital? Economists and sociologists have changed its meaning: should it be changed back? (2011)](http://cje.oxfordjournals.org/content/early/2014/04/03/cje.beu013.abstract)

Abstract

This article traces the historical usages of the term capital and the explosion of different types of supposed ‘capital’ in the twentieth century, including ‘human capital’ and ‘social capital’. In medieval and early modern times, capital meant money investable or invested in business. This meaning persists in business circles today. In contrast, Adam Smith treated physical assets, machines and people as ‘capital’ and this different usage has dominated economics since. The pre-Smithian meaning referred to money or other saleable assets that could be used as collateral. This article questions the change in meaning by economists and sociologists and highlights the importance of collateralisable property for capitalism. ‘Human capital’ can only be collateral if the humans involved are slaves. ‘Social capital’ can never be used as collateral and it is not even owned. These important issues are masked by the broadened notion of ‘capital’. Given the conceptual problems involved, economists and sociologists should consider returning to the pre-Smithian and surviving business usage of the term.

---

If the heart of the problem is a rate of return on private assets that is too high, the better solution is to lower that rate of return. How? Raise minimum wages! That lowers the return on capital that relies on low-wage labor. Support unions! Tax corporate profits and personal capital gains, including dividends! Lower the interest rate actually required of businesses! Do this by creating new public and cooperative lenders to replace today’s zombie mega-banks. And if one is concerned about the monopoly rights granted by law and trade agreements to Big Pharma, Big Media, lawyers, doctors, and so forth, there is always the possibility (as Dean Baker reminds us) of introducing more competition.

Finally, there is the estate and gift tax—a jewel of the Progressive era. This Piketty rightly favors, but for the wrong reason. The main point of the estate tax is not to raise revenue, nor even to slow the creation of outsized fortunes per se; the tax does not interfere with creativity or creative destruction. The key point is to block the formation of dynasties. And the great virtue of this tax, as applied in the United States, is the culture of conspicuous philanthropy that it fosters, recycling big wealth to universities, hospitals, churches, theaters, libraries, museums, and small magazines.



* J. K. Galbraith, [Thomas Piketty,"Capital in the 21st century" (Review, 2014)](http://www.dissentmagazine.org/article/kapital-for-the-twenty-first-century)

The double-Irish-Dutch sandwich. TBD.

Scott Baker, [18-year land cycle](http://www.opednews.com/articles/The-Land-Cycle-and-the-Sto-by-Scott-Baker-130728-228.html)
Georgist.

http://www.zerohedge.com/news/2013-07-27/liquidity-update-record-high-deposits-fed-reserves-and-foreign-bank-cash-fed-owns-31

http://www.ritholtz.com/blog/2011/04/case-shiller-100-year-chart-2011-update/
http://www.nytimes.com/imagepages/2006/08/26/weekinreview/27leon_graph2.html
http://www.ritholtz.com/blog/2010/07/updating-the-case-shiller-100-chart-forecast/

-}

