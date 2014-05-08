> module Params where

These parameters have been renamed for uniqueness and comprehensibility.
Symbols used in the SCM book [1] are indicated at the end of each comment line as : (sym).
Several of these values have not yet been clearly sourced and/or verified.
I'll check this file into the googlecode repo when names have been agreed.
Feedback please to the azimuth forum or jims at stuttard dot org. 

Possible naming conventions

P1. Kelvin not Celsius?
P2. Rate is always wrt time otherwise it's Per unit?
P3. PUA suffix for per meter squared, PUV for per unit volume?
P4. t is time?
P5. temp is temperature?
P6. Both observed and calculated values should have a cited source, error bars and, for observations, measurement dates.

[1] Adapted from Gerald North A Simple Climate Model

> heatLossAt = 1.9                     --  W: This is at the top of the atmosphere (A)
> heatLossOc = 3.6                    -- : (A)
> heatLossPerKAt = 1.9                 -- W m^(-2) K(-1) : (B)
> q0 = 342.5                           -- W : (Q0)
> coalbedoIceFree = 0.7                -- : (af)
> coalbedoIce = 0.35                   --  :(ai)

Coalbedo is absorbance = (1 - albedo)

> stepFunc temp = if temp >= 0 then 1 else 0



[Tim van Beek Earth in a Box (2011)](http://bit.ly/IW6DYJ)

> geoRad = 0.08                         -- Wm^(-2) geothermal heat from earth
> wvVisLightMin = 400*10**(-9)     -- m (1 nm = 10^(-9))
> wvVisLightMax  = 700*10**(-9)   -- m 

Equilibrium insolation was discussed on [Judith Curry's blog.](http://bit.ly/ILou84):

WebHubTelescope wrote:

"One side argues for the 1370/4 flux ratio (the accepted view, and as derived by Tim here), and the other side argues for 1370/2 (the climate 
skeptic view). The skeptics are resolute in their defense of this factor, using arguments that a layman might just consider logical, largely because 
they use familiar concepts (i.e. night and day)." 

It seems there is a simple climate model described in a paper by Joseph Postma. Pekka accuses him of bad trigonometry, multiplying insolation by a factor 
of 1/3.14. Is this so?

Pekka wrote:

"Postma doesn’t even get the atmospheric radiative flux right.

The emission is not sTa4, it is fsTa4, where f is the atmospheric emissivity/absorptivity (following his notation) and Ta is the atmospheric 
temperature. The emissivity is a unitless factor between 0 and 1 descrbing how good of an absorber/emitter the object is relative to 
an ideal body."

John O'Sullivan replied:

"Pekka, the strawman is your not Postma’s. Postma paper shows you that Co2 is the opposite of a conductor – it is an insulator. To insulate your 
house, you want a material that DOESN’T absorb heat. When something absorbs heat it’s a conductor. But greenhouse theory falsely teaches us 
to think of heat-absorption as insulation. It’s twisted junk logic.
Relative to nitrogen and oxygen, one can only call CO2 a radiative conductor. It would be an insulator if it reflected IR, but it doesn’t. Instead, it 
is “black” to IR, i.e., it’s an absorber. And absorbers conduct heat from one place to another. Throw in the second law and it’s clear which way the 
heat goes.
Think of the confused greenhouse theorist on the Titanic who jumps into the frigid Atlantic and shouts: “Good thing this water is absorbing my 
body heat. Now I’ll stay warm enough to survive.”
Bottom line: If greenhouse gases trap radiation and don’t emit it, then heating by back-radiation cannot occur. If greenhouse gases emit the 
radiation they absorb, however, they cannot be said to trap it. QED."

And this trivial irrationality continues. It would be a laugh if it wasn't so depressing... etc, blah...

A certain Latour was quoted as writing:

"Conclusions: Postma affirms there is no such thing as a GHG. CO2 causes slight global cooling rather than vanishingly small global warming.
Atmospheric gases do indeed “trap” or hold more energy when CO2 increases because their heat capacity increases with CO2 exchange for O2. H2O works the same way and dominates. But if global average temperature is fixed at -18C, 5 km up, the temperature-altitude profile adjusts a bit and air becomes cooler at the surface, not warmer. This procedure works for all planets of all stars.
Whole CO2 cooling package.
Measure and analyze solar spectrum.
Calculate corresponding T avg from radiating surface = 5778K using Plank’s Law of Radiation (1905)
Calculate energy emission rate, w/sq mtr from Stephan-Boltzmann law W = kT**4 (Solar Radiance is 2.01*10**7 w/sq mtr – steridian)
Determine dispersed reception rate at Earth by geometry 150 kk km away. The solar constant is 1366 to 1370 w/sq mtr at Earth.
NASA measured 30% is reflected. Rest is absorbed and reradiated into space; constant input = constant output.
So remaining 959 w/sq mtr absorbed and readmitted at corresponding avg T from Stephan-Boltzmann Law again. T avg = -18C.
(It is important to account for geometry, stationary flat disk to rotating sphere. GHG model got this wrong.)
Actual temperature at 5 km is measured to be -18C.
Conservation of energy = KE+PE proves atmosphere KE and hence T decreases with increasing PE and altitude, determined only by gas heat capacity, Cp.
(GHG theory got this wrong because it imagined a glass greenhouse in the sky, back-radiating energy from cold high altitudes to warmer surface in violation of Second Law of Thermodynamics.)
Using Cp air, average T of air at surface is 14.5C.
Cp depends on composition. Cp of CO2 is 63% greater than Cp of N2; Cp of H2O and O2 are 23.5% greater than N2.
Temperature measures molecular motion and it takes more energy to move a heavier molecule of CO2 than O2 the same amount.
This means a cu mtr of CO2 contains 63% more energy than a cu mtr of N2 at the same temperature and pressure.
If CO2 increases from 400 to 500 ppm, and T avg remains the same where did the additional energy trapped in CO2 instead of O2 come from? Below! Cools.
T avg at surface decreases 0.675C to 13.825C. GHG theory says it goes up about +0.005C, maybe.
Whole temperature vs altitude profile pivots a bit at 5 km to accommodate higher CO2 according to these laws of physics."

[Latour](https://engineering.purdue.edu/ChE/AboutUs/News/DrsPierreLatourandDuncanMellichampReceive2007Outsta)

Some possible sense:

Nabil Swedan | August 16, 2011 at 3:39 pm | Reply

Dear Dr. Postma:
With all due respect, your model is flawed for the following:
1) Equation (12) is wrong and does not represent the energy balance of the atmosphere. Where is the latent heat of condensation of water vapor?
2) In Figure-1, how can a colder atmosphere exchange a net radiation, s TA4, with a warmer surface of the earth. This violates the law of 
thermodynamics.
3) Your model in Figure-1 suggests that the atmosphere as a whole traps fs Ts4, which must heat all of the atmosphere. Our observations shows 
that the upper atmosphere is not warming but cooling instead. How do you explain the greenhouse cooling effect in the upper atmosphere?
4) There is no greenhouse gas effect and practical experiments are a proof.


> solarRadianceLatour = 2.01*10**7     -- W/sq mtr – steridian

> integInsolAvPostma = 610 -- Wm^(-2) == 322K (49C) (elsewhere he says he uses 300== 30C)
> avRadPostma = 240   -- Wm^(-2) ~== 255 K
> netEnergyFluxPostma = 370       --Wm^(-20



[p37]

> delTime = 0.5 * tau              -- s time step : (del t)
> tau = 1                                -- s CO2 doubling time (taudoub)
> heatLossPlanetObs = 218.0              -- W m^(-2)  : (A)
> heatLossPerMSqPlanetObs = 1.9           -- W  m^(-2) K^(-1) : (B)
> qt = 342.5                           -- J
> heatCapacityPlanet = 5.0 * 10**6  -- J TBD Planet or land? TBD.
>-- ai = 0.38;  -- ice-i planet
>-- af = 0.71;  -- ice-free planet

> heatLossPlanet = 314.9             -- W calculated
> heatLossPerMSqKPlanet = 4.61  -- W/m2/K calculated


The heat capacity of the ocean is approximately 1000 times that of the landmass. 
Check both

heatCapacityLand = heatCapacityOc + err|heatCapacityOc - err

(Eq 3.1)[p45]


3.1 Slab Ocean (Eq 3.2)[p47]

> cSlab = 2.343*10**8 -- J K^(-1) m(-2).
> relaxTime = cSlab/heatLossPerM2KOc -- years == 5
> heatLossPerM2KOc = 1.485  -- W/m2/K (A)
>-- heatLossOc =  -- defined at line 21
> heatCapacityOc = 60 * 10**7/2        -- J half of what? Correlates with NU's Oc =30*Land? TBD : (Q)
> cO2x2WPerMSq = 4                           -- W/m2 per doubling of CO2
> sensitivity = 4/1.485                -- == 2.7 K.


[p58]

> vertHeatCond = (2/1000)                          -- (km^2/yr) vert heat conductivity : (vC0)
> upVel = 4/1000                             -- upwelling velocity (4/1000) km yr : (w)
> heatCapacityCol = 4.18*10**9         -- heat cap of a 1 km column (J km^(-1) m^(-2) K^(-1)) : (c0)
> heatCapacityPUAK = 0.530                            -- W/(m2 deg C) : (wC0)
> upDiffLen = k0/wot                              -- 0.5 km (lud)
> upDiffTime= k0/wot**2                         -- 125 yr (tud)
> k0 = 1                               -- wot? TBD. (k0)
> wot = 1                                -- wot? TBD. (w)
> diffLen =heatCapacity0Oc*k0/heatLossOc                           -- (ldif) 0.140 km
> heatCapacity0Oc  = 0                     -- initial heat capacity (c0) No TBD.
> alpha = (upDiffLen/diffLen) - 1                   -- m 2.571
> eta =(qAp - heatCapacityOc)/(wot*heatCapacity0Oc)               -- 34 degrees (qAp,aOc,w TBD,c0)
> c02x2 = 70                              -- years doubling time 140, 70 yr
> qAp = 0.7                                     -- (qAp) TBD.


gamma = 4/c02x2                    -- all-ice = 0.029, all-earth = 0.057 W/(m2 yr) don't like this! TBD. See line 29 above.

> muAllIce = gammaPlanet/(heatCapacityPerMSqKOc*(1 + alpha))  -- all ice = 0.0153, all earth = 0.0313 (mu,wC0,alpha)
> heatCapacityPerMSqKOc = 5.0*10**7       -- terrible! TBD
> overTurnTimeOc = 3.5                    -- years heat overturning time

[Philip Mote The annual stratospheric cycle of water vapor in a general circulation model (2010)]
  
>-- degCPerCO2x2 = 3.0  
>-- ppmCO2 = 280 -- ppm historic? TBD.
>-- startCO2 = 387.5 -- ppm CO2 2010
>-- avAnnIncCO2 = 1.87 -- ppm 2000=2010 Either 1.953 or 1.87 TBD.
>-- avTempDegC years = log ((387.5 + (1.953 * years + (ppmCO2/degCPerCO2x2)) / 280) * 9.996

The summary forcing equation for CO2 is:

>-- radForce = 5.35*exp(c2/c1) where
>--   c1 = undefined -- TBD
>--   c2 = undefined -- TBD

=> radForce = 3.71 W/sq m, 

> temperature = (3/3.71)*5.35*exp(387.5/280) 

==> temp = 1.41 deg 

Nathan Urban wrote on the Azimuth Forum

[For] an 80-meter slab ocean.  

Modifying my previous calculation, that works out to a heat capacity per unit area of:

> cSlabNU = 2.343*10**8                              -- J/K/m2 Calculated by Nathan Urban.

It also says that the time constant is:
  
> timeConst = cSlab/heatCapacityPerMSqKOc -- 5 years which is the relaxation time from the impulse to a possibly new equilibrium.

Together these imply that:

rateBOc = 1.485                                              -- W/m2/K (B)

  If you assume a forcing of about:
  
> cO2x2 = 4                                                       -- W/m2 per doubling of CO2

This works out to a reasonable climate sensitivity [3] of about 

sensitivity = 4/1.485                                 -- 2.7 K.

[p118]



(Eq 5.54)

Coalbedo for zonally-averaged surface air temperatures

> a0 = 0.679 -- Graves et al () from FFT satellite data
> a1 = -0.241

  A = 202 -- W/m2 which is defined as what here? TBD

  B = 1.9 already defined above for ocean as bOc, rateB or rateBOc? TBD

  Computed mean annual insolation

> h0 = 0.703
> h2 = 0.543
> h4 = 0.016
 
(Fig. 56) temperature vs sin latitude

[p256] (Eq. 9.57)

>-- heatDiffPerWSqMS = -- J (W m^2) s^(-1) : (QR)
>-- velH = -- m s^(-1) : (V)
>-- velV = -- vertical velocity in pressure coordinates (dp/dt) : (w)
>-- latentHeatCond =
>-- heatCapacityConstP =
>-- gasConst = -- the gas constant : (r)

 
 Extras
 
Global Warming Potential
Measured relative to one tonne of CO2. [5]
 
Biomass
 
>-- globBioC :: Floating a => a
> globBioC = 560* 10**9              -- Gt gigatonnes
> bioCOc =112*10**9  --  Gt low 56*10**9 high 

> bioCLand = globBioC - bioCOc          -- Gt
> bioProdPerYr = 100*10**9         -- Gt year^(-1)
 
[http://www.azimuthproject.org/azimuth/show/Biomass]
 
 Aerosol
 
 Permafrost methane and ocean clathrate production
 Footnotes
[1] Gerald North A Simple Climate Model (2011). Unpublished MS (April 2012).
[2] The rate of change of coalbedo per doubling time for CO2 for the next 150, double that of the previous 150 at 0.028 or 0.029. 
[3] Climate sensivity to forcing has dimensions:

   m^(-1) * time^3 * temperature difference

 and is measured in   

   K*m^2/W == difference in degrees Kelvin square meters per Watt.
  

[4] "The diffusion coefficient beneath the ocean mixed-layer is 1.2 cm s^(-1) as  required for the best fit of the model and observationsfor the period 1880 to 1978."

> diffConstOcSub = 0.012                 -- m^(2) s^(-1)

http://img85.imageshack.us/img85/6623/hansen1981gis.gif

[5] BAU projection anticipated about 410ppm of CO2 and 2250ppb of CH4 by now; the actual values are about 390ppm for CO2, and 1825ppb for CH4.

> gwpCH4 = (*72)                         -- Tonnes CO2 per tonne CH4

[6] cO2Persist80 = (30,95)             -- Persistence time for the 80% of non-millenial CO2.

[7] Nathan Urban's interpolated IPCC temp data

digitized part of the “best” BaU curve from the figure at the top of page 74 of the AR1 report. The data are:

Didn't understand

1990.113 0.986 etc.

Should it be this?

1990 0.113 0.986
1992 0.243 1.032
1994 0.566 1.094
1997 0.277 1.149
1999 0.407 1.195
2001 0.731 1.242
2003 0.667 1.296
2009 0.476 1.444
2011 0.219 1.498
2013 0.349 1.568

A linear fit indicates that their projection for the last two decades was about 0.246 °C/decade. So, not quite 0.3 °C/decade as implied by their century average. But not as low as the current (2007) AR4 estimate of ~0.2 °C/decade.

A similar exercise with their “low” curve gives:

1988 0.564 0.683
1991 0.275 0.698
1993 0.598 0.737
1996 0.309 0.768
1999 0.794 0.823
2002 0.699 0.885
2005 0.603 0.931
2008 0.508 0.993
2010 0.832 1.040
2013 0.349 1.087

which corresponds to a lower bound of 0.169 °C/decade.

Utility Functions


>-- write :: [Double] -> IO ()
> write dl outf
>   | length dl > 0 = do
>--       write dl outf
>       appendFile outf  (show dl)
>       appendFile outf "\n"
>       write (tail dl) outf
>   | otherwise = putStrLn []
