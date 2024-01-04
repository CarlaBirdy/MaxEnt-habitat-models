# Habitat suitability maps for Australian flora and fauna under CMIP6 climate scenarios
Companion repository for the analysis of "Habitat suitability maps for Australian flora and fauna under CMIP6 climate scenarios" which is published in the GigaScience journal as of Dec 2023. This analysis was led by Carla Archibald and assisted by David Summers, Erin Graham and Brett Bryan based on MaxEnt models (https://biodiversityinformatics.amnh.org/open_source/maxent/) fitted as a part of the CliMAS project by Erin Graham and JCU (https://www.jcu.edu.au/offline/climas). The original CliMAS analysis can be found here: https://github.com/erinmgraham/NRM.

The source data for the climate scenarios can be found on the WorldClim website here: https://www.worldclim.org/. The source data for the species data can be found on the Atlas of Living Australia website here: https://www.ala.org.au/. The species habitat models will also be hosted open open-access GigaDB, found here: 

Archibald CL; Summers DM; Graham EM; Bryan BA. Supporting data for "Habitat suitability maps for Australian flora and fauna under CMIP6 climate scenarios" GigaScience Database 2023. http://dx.doi.org/10.5524/102491

This repository contains all code and documentation which was used to produce the:
1. Code to pre-process climate data and species data
2. Code to process the MaxEnt analysis
3. Figures

The majority of this analysis was submitted to a Linux (virtual machine) cluster computer using a SLURM Queue Manager. Each Server Node has: E5-2680v3 @2.50Ghz (or slightly better), 12 Cores, 70Gb.

![Graphical-Abstract](https://github.com/CarlaBirdy/MaxEnt-habitat-models/assets/19372004/df904369-4406-455b-b55d-cd8045a3c414)
Figure: Graphical abstract

This analysis was done as a part of the @land-use-trade-offs project.
