# Practical sessions

<i>Processing and mapping AIS data </i>

This repository contains all the material needed for the practical sessions. The first session will focus on the analysis of raw AIS data for generic purposes such as identification of trips, classification of fishing gear and exctaction of singnals relative to fishing activity. The second session will be focused on the estimation and mapping of fishing effort, and relative pressure indicators, in the context of the GFCM-DWRS framework with specific focus on a pilot GSA.

[To acceed the user guide, also used as main course handbook, please click here](https://irbimmaps.github.io/dwrs_workshop_2022/index.html)

## Prerequisites

 <ul>
  <li>Participants to the GFCM-workshop (held online on 07/02/2022): the virtual machine shared has all the pre-requisites needed to run the code.</li>
  <li>Any other: the `sf` package have some compatibility issue for the actual version of the shared code. The version required is 0.9-6, available at https://cran.r-project.org/src/contrib/Archive/sf </li>
</ul> 
 To install the correct `sf` version from the R console you may run the following lines of code:
 
```
require(devtools)
install_version("sf", version = "0.9-6", repos = "http://cran.us.r-project.org")
```

## Description of the repository

There are 2 practical session folders:

 <ul>
  <li>Practical session #1 - Processing and classification: from AIS raw data to fishing points</li>
  <li>Practical session #2 - Mapping deep-water fishing grounds: from fishing tracks to fishing effort maps</li>
</ul> 
