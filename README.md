# VIC-WUR_support
Supporting documentation and scripts for VIC-WUR. Inputs are missing for confidentiality reasons.
- Transformation scrips are used to transform input dataset to appropriate formats for use in other scripst.
- Analysis scripts are used to analyse water-use input. This is used to generate global grid-based estimates of water-use demands.
- Input scripts are used to generate the input files required by VIC-WUR.

## Inputs
**Global country borders shapefile with UN coding**  
[TM_WORLD_BORDERS-3.0](https://thematicmapping.org/downloads/world_borders.php)  

**Global coastline borders shapefile**  
[Natural Earth coastlines version 4.0.0](https://www.naturalearthdata.com/downloads/50m-physical-vectors/50m-coastline/)

**Water withdrawal per country per year**  
[AQUASTAT database](http://www.fao.org/nr/water/aquastat/data/query/index.html?lang=en)  
[EUROSTAT database](https://ec.europa.eu/eurostat/data/database)  
[World Water Development report 15](http://www.unesco.org/new/en/natural-sciences/environment/water/wwap/wwdr/2015-water-for-a-sustainable-world/):
WWAP (United Nations World Water Assessment Programme). 2015. The United Nations World Water Development Report 2015: Water for a Sustainable World. Paris, UNESCO.

**Country population, GDP and GVA database**  
[Maddison project database 2018](https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2018):
 Maddison Project Database, version 2018. Bolt, Jutta, Robert Inklaar, Herman de Jong and Jan Luiten van Zanden (2018), “Rebasing ‘Maddison’: new income comparisons and the shape of long-run economic development”, Maddison Project Working paper 10  
[Penn World Table version 9.1](https://www.rug.nl/ggdc/productivity/pwt/):
Feenstra, Robert C., Robert Inklaar and Marcel P. Timmer (2015), "The Next Generation of the Penn World Table" American Economic Review, 105(10), 3150-3182  
[World Band development indicators](https://databank.worldbank.org/source/world-development-indicators)  

**Global gridded population maps**  
[Hyde 3.2.1 population maps](ftp://ftp.pbl.nl/../hyde/):
Klein Goldewijk, K., A. Beusen, J.Doelman and E. Stehfest (2017), Anthropogenic land-use estimates for the Holocene; HYDE 3.2, Earth System Science Data, under review.  

**Global gridded light intensity map**  
[NASA black-marble 2016 grayscale](https://earthobservatory.nasa.gov/features/NightLights/page3.php)  

**Thermo-electric database (including location, fuel-type and cooling system)**  
Van Vliet, M. T., Wiberg, D., Leduc, S., & Riahi, K. (2016). Power-generation system vulnerability and adaptation to changes in climate and water resources. Nature Climate Change, 6(4), 375.  

**Global gridded livestock maps**  
[FAO Gridded Livestock of the World maps](http://www.fao.org/livestock-systems/en/):
Gilbert, M., Nicolas, G., Cinardi, G., Van Boeckel, T.P., Vanwambeke, S.O., Wint, G.R. and Robinson, T.P. (2018). Global livestock distribution data for cattle, buffaloes, horses, sheep, goats, pigs, chickens and ducks in 2010 (GLW 3). Nature Scientific Data 5:180227 doi: 10.1038/sdata.2018.227.

**Dam (including location, capacity and function)**  
[Global Reservoir and Dam database v1.1](http://globaldamwatch.org/grand/):
Lehner, B., C. Reidy Liermann, C. Revenga, C. Vörösmarty, B. Fekete, P. Crouzet, P. Döll, M. Endejan, K. Frenken, J. Magome, C. Nilsson, J.C. Robertson, R. Rodel, N. Sindorf, and D. Wisser. 2011. High-resolution mapping of the world’s reservoirs and dams for sustainable river-flow management. Frontiers in Ecology and the Environment 9 (9): 494-502.

**Global land-use dataset**  
[MIRCA2000 v1.1](https://www.uni-frankfurt.de/45218031/data_download):
Portmann, F. T., Siebert, S. & Döll, P. (2010): MIRCA2000 – Global monthly irrigated and rainfed crop areas around the year 2000: A new high-resolution data set for agricultural and hydrological modeling, Global Biogeochemical Cycles, 24, GB 1011, doi:10.1029/2008GB003435.

**Global gridded groundwater fraction for sectors**  
Döll, P., Hoffmann-Dobrev, H., Portmann, F. T., Siebert, S., Eicker, A., Rodell, M., ... & Scanlon, B. R. (2012). Impact of water withdrawals from groundwater and surface water on continental water storage variations. Journal of Geodynamics, 59, 143-156.

**Global irrigation efficiency**  
[FAO irrigation water use](http://www.fao.org/nr/water/aquastat/water_use_agr/index.stm):
Frenken, K., & Gillet, V. (2012). Irrigation water requirement and water withdrawal by country. FAO, Rome, Italy.

**Global gridded air temperature**  
[WATCH forcing data ERA interim v5](http://www.eu-watch.org/data_availability):
Weedon,  G.P.,  Balsamo,  G.,  Bellouin,  N.,  Gomes,  S.,  Best,  M.J.  and  Viterbo,  P.,  2014.  The  WFDEI meteorological  forcing  data  set:  WATCH  Forcing  Data  methodology  applied  to  ERA-Interim reanalysis data. Water Resources Research

**Global delta map**  
[Global Delta Map](http://www.globaldeltarisk.net/data.html):
Z.D. Tessler, C.J. Vörösmarty, M. Grossberg, I. Gladkova, H. Aizenman, J.P.M. Syvitski, E. Foufoula-Georgiou. Profiling Risk and Sustainability in Coastal Deltas of the World. Science 349(6248), 638-643 (2015) doi:10.1126/science.aab3574.

**River discharge data**  
[Global Runoff Data Centre database](https://www.bafg.de/GRDC/EN/02_srvcs/21_tmsrs/riverdischarge_node.html)

**Global gridded flow direction and length**  
[RVIC input data](https://rvic.readthedocs.io/en/latest/about/model-overview/):
Hamman, J.J., Nijssen, B., Roberts, A., Osinski, R., Lettenmaier D.P., Maslowski., W. (Manuscript in preparation - 2015). Land - Ocean Coupling in the Regional Arctic System.

## Primary references
Simulating human impacts on global water resources using VIC-5 (Droppers et al., 2019)

