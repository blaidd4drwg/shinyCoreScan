# shinyCoreScan

shinyCoreScan is a WebApp written in R using the Shiny framework that processes Avaatech XRF Corescanner files (bAXIL batch csv files) and allows the inspection, cleaning, and plotting of XRF data.

## Getting Started

You can either run a local copy of shinyCoreScan using a [point release](https://github.com/blaidd4drwg/shinyCoreScan/releases) from the github project website or use the already deployed WebApp on [shinyapps.io](https://surfsedi.shinyapps.io/shinycorescan/). The two version are not necessarily at the same stage.

### Prerequisites for local deployment

For a local deployment of the WebApp you will need a recent [GNU R installation](https://www.r-project.org/) and for your own convenience [RStudio](https://rstudio.com/). Additionally, shinyCoreScan requires the following packages to be installed (listed in `global.R`):

```
library(shiny)
library(shinyjqui)
library(shinyjs)
library(reactlog)
library(shinyalert)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(shiny.info)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(viridis)
library(purrr)
library(tibble)
library(DT)
```

## Using shinyCoreScan

shinyCoreScan is divided in different tabpanels: Import, Diagnostics, Plotting et cetera. Using the Diagnostics panel for data inspection and cleaning is not mandatory but strongly advised.

### Import page

The import page allows the user to import their XRF Corescanner files (bAXIL batch csv files). Make sure that every file only contains data of one core (or core section). In case the import fails check if there are duplicate spectrum files (spe files created by bAXIL scanning software).

![Import page of shinyCoreScan initially](README_files/shinyCoreScan_import_init.png)

During import, the data of the input files is being reshaped and cleaned:

* The offset at the beginning of each core (stemming from the step chosen in the scanning software e.g. to skip the "green stuff" used to cap the top of core sections) is removed, i.e. the depth is zeroed. However, in order to possibly concatenate core sections, a value of 1 mm is added. In practice, this unlikely poses any problems.
* In case of multiple element traces at different voltages (e.g. the same element in the models for 10 kV and 30 kV), the element trace with the weaker signal (i.e. Area) is removed. This is to maintain uniqueness of key - value pairs during data transformation.
* The Rhodium element traces are discarded as (only) they require the usage of an additional variable to cover the type of spectrum recorded (incoherend vs. coherent) and are rarely used by the author.
* Repeated measures are removed, as they are difficult to deal with, especially since they are usually only done in intervals and the bAXIL batch files already contain statistical information for each measurement (standard deviation of Area or cps during measuring time).

Import and processing of the XRF files may take a minute. After the import is finished (progress bar disappears), the data is available for further processing. You can choose to `concatenate core sections` in the import panel on the left. This only works for valid longcores. The naming scheme for longcores is as follows:

```
.+(?=\\W[[:alpha:]]+)
```

This means, that The name of the long core (e.g. MI18-L8) needs to be followed by a non-word (e.g dash or underline) and then followed by the section at the end, which has to be a letter. E.g. MI18-L8-A ... MI18-L8-E would be valid names of a longcore.

If `concatenate core sections` is chosen with either ascending or descending order of sections, an Element can be chosen in the import panel to preview the spectrum over the whole longcore length `z`. A table shows information about the length of each section, their `SectionID`, the name of the longcore and the absolute, cumulative length/depth `z`. Choosing this mode is necessary to plot longcores on the Plotting page.
