# gefcom2017data

Data for eight zones in New England as used in the direct track of the 2017 Global Energy Forecasting Competition (GEFCom2017). Zones are included in the table below.

Zone                          | Abbreviation | Type
------------------------------|-------------|-----------
Total                         | Total       | Aggregated
Maine                         | ME          | Bottom
New Hampshire                 | NH          | Bottom
Vermont                       | VT          | Bottom
Connecticut                   | CT          | Bottom
Rhode Island                  | RI          | Bottom
Massachusetts                 | MASS        | Aggregated
Southeast Massachusetts       | SEMASS      | Bottom
Western-Central Massachusetts | WCMASS      | Bottom
Northeast Massachusetts       | NEMASSBOST  | Bottom


As this data is for the defined data track only dew point and dry bulb temperature variables are included in the `gefcom` data frame.


## Installation

From your R console, simply run:

```{r}
install.packages("devtools")
library(devtools)
devtools::install_github("camroach87/gefcom2017data")
```
