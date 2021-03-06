> module ParamsOc where

(Eq 3.1)[p45]


3.1 Slab Ocean (Eq 3.2)[p47]

> heatLossOc = 3.6                     -- W m**(-2) K(-1) : (A)
> cSlab = 2.343*10**8               -- J K**(-1) m**(-2).
> relaxTime = cSlab/heatLossPerMSqKOc -- years == 5
> heatLossPerMSqKOc = 1.485       -- W/m2/K (A)
> heatCapacityOc = 60 * 10**7/2 -- J half of what? Correlates with NU's Oc =30*Land? TBD : (Q)
> cO2x2WPerMSq = 4                  -- W/m2 per doubling of CO2
> sensitivity = 4/1.485                -- == 2.7 K.

[p58]

Slab : a 1 m^2 x 1 km parallelpiped
Coalbedo : heat retained

Table 1

> vertHeatCond = (2/1000)                       -- (km^2/yr) vertical heat conductivity : (vC0)
> upVel = 4/1000                                    -- upwelling velocity (4/1000) km yr : (w)
> heatCapacityCol = 4.18*10**9               -- heat cap of a 1 km column (J km^(-1) m^(-2) K^(-1)) : (c0)
> heatCapacityPerMSqKOc = 0.530             -- W/(m2 deg C) : (wC0)
> upDiffLen = k0/upVel   
> ldif = c0*k0/heatLossPerMSqKOc            -- C0 K0/B
> lud = 0.5
> upDiffTime= k0/upVel**2                         -- 125 yr (tud)
> tud = 125
> k0 = 2/1000                                        -- km^2 yr^(-1)
> c0 = 4.18*10**9                                  -- J/( km m^2 degC)
> diffLen =heatCapacity0Oc*k0/heatLossOc -- (ldif) 0.140 km
> heatCapacity0Oc  = c0                           -- initial heat capacity (c0) No TBD.
> alpha = (upDiffLen/diffLen) - 1               -- 2.571 m

Eq. 3.50 Spectral Density

nf
b
f
k0
c0



absorbed - radiated/upwardly diffused : J/velocity

> eta =(coalbedo*heatCapacityOc - heatCapacity0Oc)/(upVel*heatCapacity0Oc)               -- 34 degC (qAp,aOc,w TBD,c0)
> c02x2 = 70                                         -- years doubling time 140, 70 yr
> coalbedo = 1 - albedo                                         -- (Ap)
> albedo = 0.3
> gamma = 4/c02x2                    -- all-ice = 0.029, all-earth = 0.057 W/(m2 yr) don't like this! TBD. See line 29 above.

> muAllIce = gamma/(heatCapacityPerMSqKOc*(1 + alpha))  -- all ice = 0.0153, all earth = 0.0313 (mu,wC0,alpha)
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
 
(Fig. 5.6) temperature vs sin latitude

[p256] (Eq. 9.57)

> heatDiffWPerWMSqS = 1.025-- W m^2 s^(-1) : (QR)
> velH = 1 -- m s^(-1) : (V)
> velV =  1 -- vertical velocity in pressure coordinates (dp/dt) : (w)
> latentHeatCond = 2.4
> heatCapacityConstP = 5*10**6
> gasConst = 8.3144621 * 10**3 -- N m (kmol K)^(-1) : the gas constant : (r), SD75

[wikipedia]

> rhoH2O = 1000.00                           -- kg m^3 density of water 
> spHeatH2O = 41841.00                    -- J kg^(-1) K^(-1) : (c) 
> heatCapVolH2O = 0.74                 --  J m^(-3) K^(-1) volumetric heat capacity of water
> earthRadius = 6371.00                      -- km range = [6353, 6384] (diff 31)

(Eq. 3.19)[p49]

>-- l24  k0 = 0.003              -- km^2 yr^(-1)
> depth4 = 4                      -- km
> diffusionLen = 140    -- m

3.2.1 Upwelling [p57]

(Fig. 3.5) [p53] Instantaneous response
(Fig. 3.5) Heat conducting ocean respone

>-- k = 0.02
>-- gamma = 0.029                    -- W m^(-2)
>-- depth1 = 1                                    -- km

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

digitized part of the �best� BaU curve from the figure at the top of page 74 of the AR1 report. The data are:

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

A linear fit indicates that their projection for the last two decades was about 0.246 �C/decade. So, not quite 0.3 �C/decade as implied by their century average. But not as low as the current (2007) AR4 estimate of ~0.2 �C/decade.

A similar exercise with their �low� curve gives:

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

which corresponds to a lower bound of 0.169 �C/decade.


Global coefficients for comparative nonsense checking


> gammaPlanet = 0.056                        -- W m^(-2) c02x2^(-1) rate of change of coalbedo per doubling of CO2[1]
> heatLossPlanetObs = 218.0              -- W m^(-2)  : (A)
> heatLossPerMSqPlanetObs = 1.9           -- W  m^(-2) K^(-1) : (B)
> qt = 342.5                           -- W m^(-2)
> heatCapacityPlanet = 5.0 * 10**6  -- J TBD Planet or land? TBD.
 


