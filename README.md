# README
[![License](https://img.shields.io/badge/license-GPLv3-blue.svg)](https://raw.githubusercontent.com/BramDr/VIC_support/master/LICENSE.txt) [![DOI](https://zenodo.org/badge/7766/BramDr/VIC_support.svg)](https://zenodo.org/badge/latestdoi/7766/BramDr/VIC_support)

----

Supporting documentation and scripts for VIC-WUR. Inputs are missing for confidentiality reasons.

  * **Data** contains all data used to generate the VIC parameters, this should be created locally.
  * **Scripts** contains all scripts used to transform the input data and generate the VIC parameters.
  * **Tools** contains programs that are used to maintain support (such as code formatting).

----

## Data
Data needed to run the scripts (including possible sources and references).

**Global country borders shapefile with UN coding**  
[TM_WORLD_BORDERS-3.0](https://thematicmapping.org/downloads/world_borders.php)  

**Water withdrawal per country per year**  
[AQUASTAT database](http://www.fao.org/nr/water/aquastat/data/query/index.html?lang=en)  
[EUROSTAT database](https://ec.europa.eu/eurostat/data/database)  
[World Water Development report 15](http://www.unesco.org/new/en/natural-sciences/environment/water/wwap/wwdr/2015-water-for-a-sustainable-world/):
WWAP (United Nations World Water Assessment Programme). 2015. The United Nations World Water Development Report 2015: Water for a Sustainable World. Paris, UNESCO.

**Country population, GDP and GVA database**  
[Maddison project database 2018](https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2018):
 Maddison Project Database, version 2018. Bolt, Jutta, Robert Inklaar, Herman de Jong and Jan Luiten van Zanden (2018), “Rebasing ‘Maddison’: new income comparisons and the shape of long-run economic development”, Maddison Project Working paper 10  
[Penn World Table version 9.1](https://www.rug.nl/ggdc/productivity/pwt/):
Feenstra, Robert C., Robert Inklaar and Marcel P. Timmer (2015), "The Next Generation of the Penn World Table" American Economic Review, 105(10), 3150-3182 doi:10.1257/aer.20130954  
[World Band development indicators](https://databank.worldbank.org/source/world-development-indicators)  

**Global gridded population maps**  
[Hyde 3.2.1 population maps](ftp://ftp.pbl.nl/../hyde/):
Klein Goldewijk, K., A. Beusen, J.Doelman and E. Stehfest (2017), Anthropogenic land-use estimates for the Holocene; HYDE 3.2, Earth System Science Data, under review.  

**Global gridded light intensity map**  
[NASA black-marble 2016 grayscale](https://earthobservatory.nasa.gov/features/NightLights/page3.php)  

**Thermo-electric database (including location, fuel-type and cooling system)**  
Van Vliet, M. T., Wiberg, D., Leduc, S., & Riahi, K. (2016). Power-generation system vulnerability and adaptation to changes in climate and water resources. Nature Climate Change, 6(4), 375. doi:10.1038/nclimate2903  
[EIA national energy data](https://www.eia.gov/)

**Global gridded livestock maps**  
[FAO Gridded Livestock of the World maps](http://www.fao.org/livestock-systems/en/):
Gilbert, M., Nicolas, G., Cinardi, G., Van Boeckel, T.P., Vanwambeke, S.O., Wint, G.R. and Robinson, T.P. (2018). Global livestock distribution data for cattle, buffaloes, horses, sheep, goats, pigs, chickens and ducks in 2010 (GLW 3). Nature Scientific Data 5:180227 doi:10.1038/sdata.2018.227.

**Dam (including location, capacity and function)**  
[Global Reservoir and Dam database v1.1](http://globaldamwatch.org/grand/):
Lehner, B., C. Reidy Liermann, C. Revenga, C. Vörösmarty, B. Fekete, P. Crouzet, P. Döll, M. Endejan, K. Frenken, J. Magome, C. Nilsson, J.C. Robertson, R. Rodel, N. Sindorf, and D. Wisser. 2011. High-resolution mapping of the world’s reservoirs and dams for sustainable river-flow management. Frontiers in Ecology and the Environment 9 (9): 494-502. doi:10.1890/100125

**Global land-use dataset**  
[MIRCA2000 v1.1](https://www.uni-frankfurt.de/45218031/data_download):
Portmann, F. T., Siebert, S. & Döll, P. (2010): MIRCA2000 – Global monthly irrigated and rainfed crop areas around the year 2000: A new high-resolution data set for agricultural and hydrological modeling, Global Biogeochemical Cycles, 24, GB 1011, doi:10.1029/2008GB003435.

**Global gridded groundwater fraction for sectors**  
Döll, P., Hoffmann-Dobrev, H., Portmann, F. T., Siebert, S., Eicker, A., Rodell, M., ... & Scanlon, B. R. (2012). Impact of water withdrawals from groundwater and surface water on continental water storage variations. Journal of Geodynamics, 59, 143-156. doi10.1016/j.jog.2011.05.001

**Global pumping capacity**  
Sutanudjaja, E. H., Van Beek, R., Wanders, N., Wada, Y., Bosmans, J. H., Drost, N., ... & Karssenberg, D. (2018). PCR-GLOBWB 2: a 5 arcmin global hydrological and water resources model. Geoscientific Model Development, 11(6), 2429-2453, doi:10.5194/gmd-11-2429-2018..

**Global irrigation efficiency**  
[FAO irrigation water use](http://www.fao.org/nr/water/aquastat/water_use_agr/index.stm):
Frenken, K., & Gillet, V. (2012). Irrigation water requirement and water withdrawal by country. FAO, Rome, Italy.

**Global gridded air temperature**  
[WATCH forcing data ERA interim v5](http://www.eu-watch.org/data_availability):
Weedon,  G.P.,  Balsamo,  G.,  Bellouin,  N.,  Gomes,  S.,  Best,  M.J.  and  Viterbo,  P.,  2014.  The  WFDEI meteorological  forcing  data  set:  WATCH  Forcing  Data  methodology  applied  to  ERA-Interim reanalysis data. Water Resources Research doi:10.1002/2014WR015638

**Global delta map**  
[Global Delta Map](http://www.globaldeltarisk.net/data.html):
Z.D. Tessler, C.J. Vörösmarty, M. Grossberg, I. Gladkova, H. Aizenman, J.P.M. Syvitski, E. Foufoula-Georgiou. Profiling Risk and Sustainability in Coastal Deltas of the World. Science 349(6248), 638-643 (2015) doi:10.1126/science.aab3574.

**Global gridded flow direction and length**  
[RVIC input data](https://rvic.readthedocs.io/en/latest/about/model-overview/):
Hamman, J., Nijssen, B., Roberts, A., Craig, A., Maslowski, W., and Osinski, R. ( 2017), The coastal streamflow flux in the Regional Arctic System Model, J. Geophys. Res. Oceans, 122, 1683– 1701, doi:10.1002/2016JC012323.  
[HydroSHEDS/HYDRO1K](http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30/hydro): Wu, H., Kimball, J. S., Li, H., Huang, M., Leung, L. R., and Adler, R. F. (2012), A new global river network database for macroscale hydrologic modeling, Water Resour. Res., 48, W09701, doi:10.1029/2012WR012313.  

**Older VIC parameter files**  
[VIC input parameters 2 degree](https://vic.readthedocs.io/en/master/Datasets/Datasets/):
Nijssen, B., O'Donnell, G.M., Hamlet, A.F. et al. Hydrologic Sensitivity of Global Rivers to Climate Change. Climatic Change 50, 143–175 (2001). https://doi.org/10.1023/A:1010616428763  
[VIC input parameters 0.5 degree](https://vic.readthedocs.io/en/master/Datasets/Datasets/):
van Vliet, M. T., Franssen, W. H., Yearsley, J. R., Ludwig, F., Haddeland, I., Lettenmaier, D. P., & Kabat, P. (2013). Global river discharge and water temperature under climate change. Global Environmental Change, 23(2), 450-464. https://doi.org/10.1016/j.gloenvcha.2012.11.002  

**Global CO2-concentration dataset**
[ISIMIP input data](https://www.isimip.org/gettingstarted/#input-data-bias-correction)  

**Global fertilizer dataset**
[GGCMI input data](http://www.rdcep.org/research-projects/ggcmi):
Mueller, N., Gerber, J., Johnston, M. et al. Closing yield gaps through nutrient and water management. Nature 490, 254–257 (2012). https://doi.org/10.1038/nature11420  
Hurtt, G. C., Chini, L., Sahajpal, R., Frolking, S., Bodirsky, B. L., Calvin, K., Doelman, J. C., Fisk, J., Fujimori, S., Klein Goldewijk, K., Hasegawa, T., Havlik, P., Heinimann, A., Humpenöder, F., Jungclaus, J., Kaplan, J. O., Kennedy, J., Krisztin, T., Lawrence, D., Lawrence, P., Ma, L., Mertz, O., Pongratz, J., Popp, A., Poulter, B., Riahi, K., Shevliakova, E., Stehfest, E., Thornton, P., Tubiello, F. N., van Vuuren, D. P., and Zhang, X.: Harmonization of global land use change and management for the period 850–2100 (LUH2) for CMIP6, Geosci. Model Dev., 13, 5425–5464, https://doi.org/10.5194/gmd-13-5425-2020, 2020.  
Zhang, B., Tian, H., Lu, C., Dangal, S. R. S., Yang, J., and Pan, S.: Global manure nitrogen production and application in cropland during 1860–2014: a 5 arcmin gridded global dataset for Earth system modeling, Earth Syst. Sci. Data, 9, 667–678, https://doi.org/10.5194/essd-9-667-2017, 2017.  

**WOFOST input files**
[crop files](https://github.com/ajwdewit/WOFOST_crop_parameters):
Allard de Wit, Hendrik Boogaard, Davide Fumagalli, Sander Janssen, Rob Knapen, Daniel van Kraalingen, Iwan Supit, Raymond van der Wijngaart, Kees van Diepen (2019), 25 years of the WOFOST cropping systems model, Agricultural Systems, 168, 154-167, doi.org/10.1016/j.agsy.2018.06.018.  

**Global soil dataset**
[ISRIC-WISE soil properties 30 seconds](https://data.isric.org/geonetwork/srv/eng/catalog.search#/home):
N.H. Batjes, Harmonized soil property values for broad-scale modelling (WISE30sec) with estimates of global soil carbon stocks, Geoderma, Volume 269, 2016, Pages 61-68, ISSN 0016-7061, https://doi.org/10.1016/j.geoderma.2016.01.034.  

**Crop yield per country per year**  
[FAOSTAT database](http://www.fao.org/faostat/en/#data)  

**Global soil depth dataset**  
[Global 1-km Gridded Thickness of Soil, Regolith, and Sedimentary Deposit Layers](https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1304):
Pelletier, J. D., Broxton, P. D., Hazenberg, P., Zeng, X., Troch, P. A., Niu, G.-Y., Williams, Z., Brunke, M. A., and Gochis, D. (2016), A gridded global data set of soil, immobile regolith, and sedimentary deposit thicknesses for regional and global land surface modeling, J. Adv. Model. Earth Syst., 8, 41– 65, doi:10.1002/2015MS000526.  

**Global 30 meter elevation dataset**  
[SRTM void filled](https://doi.org/10.5066/F7F76B1X)  

**MODIS datasets**  
[MODIS 8-day LAI - MCD15A2H v006](https://doi.org/10.5067/MODIS/MCD15A2H.006)  
[MODIS yearly landcover - MCD12Q1 v006](https://doi.org/10.5067/MODIS/MCD12Q1.006)  
[MODIS daily albedo - MCD43A4 v006](https://doi.org/10.5067/MODIS/MCD43A3.006)  
[MODIS 16-day NDVI - MOD13A1 v006](https://doi.org/10.5067/MODIS/MOD13A1.006)  

**Crop stomatal resistance response**
AINSWORTH, E.A. and ROGERS, A. (2007), The response of photosynthesis and stomatal conductance to rising '[CO2]': mechanisms and environmental interactions. Plant, Cell & Environment, 30: 258-270. https://doi.org/10.1111/j.1365-3040.2007.01641.x  

**FACE experiment datasets**
Kimball, B. A., Pinter Jr, P. J., LaMorte, R. L., Leavitt, S. W., Hunsaker, D. J., Wall, G. W., ... & White, J. W. (2017). Data from the Arizona FACE (free-air CO2 enrichment) experiments on wheat at ample and limiting levels of water and nitrogen. Open Data Journal for Agricultural Research, 3. https://doi.org/10.18174/odjar.v3i1.15826  
Yang, L., Huang, J., Yang, H., Zhu, J., Liu, H., Dong, G., ... & Wang, Y. (2006). The impact of free-air CO2 enrichment (FACE) and N supply on yield formation of rice crops with large panicle. Field Crops Research, 98(2-3), 141-150. https://doi.org/10.1016/j.fcr.2005.12.014 **personal communication**  
Hasegawa, T., Li, T., Yin, X. et al. Causes of variation among rice models in yield response to CO2 examined with Free-Air CO2 Enrichment and growth chamber experiments. Sci Rep 7, 14858 (2017). https://doi.org/10.1038/s41598-017-13582-y **personal communication**  

**Weather datasets**
[ERA5 0.25 degree historical reanalysis](https://www.ecmwf.int/en/forecasts/datasets/reanalysis-datasets/era5)  
[ISIMIP3b 0.5 degree bias-adjusted climate models](https://www.isimip.org/protocol/3/):
Lange, S.: Trend-preserving bias adjustment and statistical downscaling with ISIMIP3BASD (v1.0), Geosci. Model Dev., 12, 3055–3070, https://doi.org/10.5194/gmd-12-3055-2019, 2019.  
