# Practical session 2

<i>Mapping deep-water fishing grounds: from fishing tracks to fishing effort maps</i>

This repository contains all the material needed for the practical session 2. Starting from pre-classified deep-water fishing tracks, R scripts used to estimate and map metrics of fishing pressure in the context of the GFCM-DWRS framework will be introduced and shared. The session will be focused on a pilot GSA. An hands-on exercise related to all the topics covered during the day is also foreseen.

[To acceed the user guide, also used as main course handbook, please click on this line](https://enriconarmelloni.github.io/index.html)

## Prerequisites

 <ul>
  <li>Participants to the GFCM-workshop (held online on 07/02/2022): the virtual machine shared has all the pre-requisites needed to run the code.</li>
  <li>Any other: the `sf` package have some compatibility issue for the actual version of the shared code. The version required is 0.9-6, available [here](https://cran.r-project.org/src/contrib/Archive/sf) </li>
</ul> 
 To install the correct `sf` version from the R console you may run the following lines of code:
```
require(devtools)
install_version("sf", version = "0.9-6", repos = "http://cran.us.r-project.org")
```
## Folder organization

The folder is organised into 3 sub-folders:

 <ul>
  <li>R: contains all the R code needed for the session</li>
  <li>data: contains all the data on fishing activity </li>
  <li>maps: contains the shapefiles used to make the background of the maps </li>
</ul> 

A detailed user guide can be found [here](https://enriconarmelloni.github.io/index.html)

