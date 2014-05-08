> module ParamsGlob where

[p27]

> tempSun = 5778                       -- deg K black body at surface
> au = 1.4959*10**11                 -- m earth sun distance == 1 astronomical unit (au)
> sunRadius = 6.96*10**8           -- m
> sigmaSB = 5.67*10**(-8)          -- J m^(-2) K(-4) Stephan-Boltzman constant

wikipedia

Physical characteristics
Mean radius	6,371.0 km[6]
Equatorial radius	6,378.1 km[7][8]
Polar radius	6,356.8 km[9]
Flattening	0.0033528[10]

>-- circEarth = 4.0075017*10**4                        -- km (equatorial)[8]

40,007.86 km (meridional)[11][12]

> radEarth = 6378.1                                 -- equatorial
> circumEarth = 2*pi*radEarth
> areaEarth = pi*(radEarth**2)
> volEarth = 4*pi*(radEarth**3)

>-- areaEarth = 5.10072000*10**8                 -- km2[13][14][note 5]
>-- areaLand  = 1.48940000*10**8                 --  km2 land (29.2 %)

> areaOc = 3.61132*10**8                            --  km2 water (70.8 %)

>-- volEarth = 1.08321*1012                         -- km3[3]
> massEarth = 5.9736*1024                           -- kg[3]
> meanDensityEarth = 5.515                          -- g/cm3[3]
> grav = 9.780327                                        -- m/s2[15]

0.99732 g

Escape velocity	11.186 km/s[3]
Sidereal rotation

> orbitPeriod= 0.99726968                                   -- d[16]
> dayLen = 24 -- 23h 56m 4.100s
> equatRotVel = 1674.4                                 -- km/h (465.1 m/s)[17]
>-- Axial tilt =	23°26'21".4119                     --[2]
> albedoE = 0.367                                        -- (geometric) albedo of the earth Check. TBD. [3]
>-- 0.306 (Bond)[3]

Surface temp.
   Kelvin
   Celsius	
min	mean	max

184 K[18]	287.2 K[19]	331 K[20]
-89.2 °C	14 °C	57.8 °C

Atmosphere

> surfaceP = 101.325                        --kPa (MSL)

Composition 78.08% nitrogen (N2)[3]
20.95% oxygen (O2)
0.93% argon

0.038% carbon dioxide

About 1% water vapor (varies with climate)

> gasConstDryAir = 8.314463175 -- J/mol K(75) [wikipedia](wikipedia.org)
 
Proper names

> type HeatCapacity = Double
> type HeatLoss = Double
> type Enthalpy = Double
> type Rate = Double
> type Depth = Double

> data Heat = Heat

> data LatentHeat = Evaporation|Condensation

> heatCapacityEarth = 6 * 10**7 -- megaJ
> 

> gammaPlanet = 0.056                        -- W m^(-2) c02x2^(-1) rate of change of coalbedo per doubling of CO2[1]

> tau0 = 2.5 * 10**7                    -- (C/B) == heatCapacity/heatLossPerKAt
> cLand = 10**7/2  

> tempBlackB temp = emissivity * sigmaSB*temp**4 where
>   emissivity = 0.5                               -- atmospheric emissivity (absorptivity (sic) or absorbance?) Ranges from 0 to 1. TBD.