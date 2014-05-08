<h1><a href src="http://www.climate.unibe.ch/jcm/doc/jcm/mod/climate.html">Upwelling Diffusion Energy Balance Model</a></h1>

"How it works
This is an efficient java implementation of the Wigley/Raper Upwelling-Diffusion Energy Balance (UDEB) model which was used to make many of the the smooth-curve plots in the IPCC-TAR WG1. Parameters are tuned to fit seven different GCM predictions, as described in IPCCTAR-WG1-Appx 9.1. The system of heat fluxes is resolved using an efficient eigenvector calculation method.
Features of UDEB model
(Adjustable parameters in red)
Four surface boxes: north & south, land & ocean
In each box, the "lambda" values for calculating heat flux to space are derived from the prescribed equilibrium climate sensitivity and land-ocean temperature ratio parameters.
Surface fluxes depend on land-ocean and north-south conductivities (these are fairly arbitrary, but have little impact on the global average temperature)
Radiative forcing of aerosols and short-lived gases is unevenly distributed between boxes (e.g. most of the sulphate aerosol cooling is in the north-land box)
Two 1-D Upwelling-Diffusion Oceans (north and south), connected only at surface:
Fixed vertical diffusivity between layers (unlike carbon model).
A high latitude downwelling "pipe" (rather than a separate box), for whose water temperature is a fixed fraction of the average temperature (polar sink temperature ratio parameter).
A "sea-ice" parameter adjusts the water/air temperature ratio.
The lag of the surface ocean warming also depends on the mixed layer depth.
The rate of this downwelling/upwelling is reduced with temperature to account for changing thermohaline circulation (parameter to be added).

See also:
Efficient Eigenvector Calculation Method
Global Temperature Plot, Discussion
Correspondence with IPCC predictions
/IPCC-TAR-WG1 appendix 9.1
Raper et al 2001 and references therein.
An IPCC technical paper (1997) describes an earlier version of this model.
Future development
The next step from here, might be to investigate the possibility to develop an interactive java version of intermediate complexity models. The simplest of these is the Bern zonally averaged model with the 2.5D Ocean."

<h2><q href src="http://www.climate.unibe.ch/jcm/doc/science/eigenvec.html">Efficient Eigenvector Calculation</a></h2>

"The carbon and climate models both use about 40 ocean layers (x2 oceans for climate model). If using a simple integration method, the timestep would have to be quite small (e.g. 1 month) in order that the fluxes are much smaller than the box contents, and so the calculation over hundreds of years is rather slow. Therefore in order to get an instant response in the plots as the parameters are dragged by the mouse, a more efficient matrix-eigenvector method has been applied.

eatures of this method
:
An exact analytical solution
The only approximation is that the change in non-linear fluxes is linear within one timestep.
The timestep can be any size (depending on input data and plot resolution)
The ramp function must be iterated when a non-linear flux is a function of box contents.
Only matrix cells needed for data output or non-linear input are calculated.
Usually more efficient than pulse response function especially with many timesteps.
Unlike PRF, preserves knowledge of all box contents (needed for thermal expansion or depth profiles etc.)

The eigenvectors only need to be recalculated when you change a parameter that alters the linear fluxes (such as internal mixing rates), not when the external emissions change.
Eigenvectors, inverses, etc. are calculated using the convenient Java Matrix Package ("JAMA"), which was developed at MIT, and has simply been bolted onto Java Climate Model."

<a href src="http://math.nist.gov/javanumerics/jama/">JAMA</a>

<h2>JCM References</h2>
<h3>Science models</h3>

"The two papers below (and references therein) describe the calculation methods used in IPCC-TAR, and also implemented in JCM.
Bern-CC Model
(used for carbon cycle and atmospheric chemistry in JCM)
Global Warming Feedbacks on Terrestrial Carbon Uptake under the IPCC emissions scenarios , F.Joos, I.C.Prentice, S.Sitch, R.Meyer, G.Hooss, G.K.Plattner, S. Gerber, K.Hasselmann, Global Biogeochemical Cycles v15 no4, 891-907, 2001
Wigley-Raper UDEB model
(used for temperature and sea-level in JCM)
Use of a upwelling-diffusion energy balance to simulate and diagnose A/OGCM results, S.C.B. Raper, J.M. Gregory, T.J. Osborn, Climate Dynamics v17, p601-13, 2001
See also IPCCTAR-WG1-Ch9 Appendix 9.1
Sources of Data
Carbon Dioxide Information Analysis Centre (CDIAC) (source of historical CO2 Emissions for each country, combined to make JCM regional data)
RIVM IMAGE model (source of regional socioeconomic projections under the IPCC-SRES scenarios)
IPCC-Data Distribution Centre (source of regional climate GCM data) Note also
Links into IPCC-TAR online

<h2>Load-data module</h2>

"Load-data module
The original historical and scenario data is stored in java files "histdata" and "sresdata". This is compressed by the routines in "savedata", to make the binary file data.dat. When the model starts up, "loaddata" simply loads and uncompresses this datafile into specific modules.

Note, the data for the regional climate map are loaded by the RegCliMap Panel

Java Source Code:
Histdata.java, SRESdata.java, Savedata.java, Loaddata.java
Interactions
Affects:
(Historical data) People, Regshares, Oghga, Carbon Radfor Climate,
(Future scenario data) SRES

How it works
Compression method
For each sequence of data, the maximum and minumum and range are found. Each data item is then converted to one byte, by subtracting the minimum, dividing by the range, and multiplying by 256. So the precision is 0.4% of the range, or one pixel of a typical plot."

T.J. Osborn, Upwelling Diffusion Model, Climate Dynamics 2000

Starts with an assumption of a climate sensitivity to a doubling of CO2 
concentration of 2.6 C from 1860-2100.

The simulation produces a increase in climate sensitivity from 2 C 
to 3.85 C after 900 years and a sea level rise of (~0.9 m? TBD).





Gauss-Lebesque RK.