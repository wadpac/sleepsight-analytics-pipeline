# sleepsight-analytics-pipeline
Set of function to process data from the sleepsight study (King's College London). This code is currently under development. If you have feedback or questions then feel free to post them as an issue inhttps://github.com/wadpac/sleepsight-analytics-pipeline.

## Installation:

The following code will install Sleepsight from GitHub and other packages you will need.

```
list.of.packages <- c("devtools", "data.table","roxygen2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(data.table)
library(devtools)
install_github("wadpac/sleepsight-analytics-pipeline")
```

## Expected data input:

- Withings wearable data.
- Mobile phone data as collected and preprocessed with the Passive data kit (PDK).

## Using the code

See [exploredata](explorate.R) for an example of how to use the key functions in the package.

