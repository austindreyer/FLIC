# Fly Liquid-food Interaction Counter  
Files relating to the use of FLIC system, including folders for specific usages. In total, they allow for in depth analysis of *Drosophila* feeding behaviors recorded using the Fly Liquid-food Interaction Counter (FLIC) system. Designed for use in R (>v2.6.0). 

#### Generic Folder 
Contains scripts and functionality required for processing of FLIC data files, produced by Pletcher lab at University of Michigan

#### UI Folder
Contains programs that call individual functions for complete runthrough of data processing up through the production of circadian analysis-ready files

#### HB Folder
"Home brew" scripts are those that are unique to the Cavanaugh lab's approach/requirements for circadian feeding analysis

#### Anticipation Folder
Set of scripts that describe how to analyze FLIC data for changes in anticipatory behavior, focuses on shifts in behavioral timing around the time of major environmental changes (e.g. lights on/off)

#### Deprecated Folder
Old scripts that are out of the pipeline flow, but might still prove useful...

### Key Packges
* [tidyverse](https://www.tidyverse.org/) - One stop shop for data science in R, specifically:
  * [dplyr](https://www.rdocumentation.org/packages/dplyr/versions/0.7.8) - Data manipulation
  * [ggplot2](https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf) - Data visualization
* [signal](https://cran.r-project.org/web/packages/signal/signal.pdf) - Processing of rhythmic data

##### Special Thanks
Thank you to the [Pletcher lab](https://sites.google.com/a/umich.edu/pletcher-lab/) at University of Michigan! 
