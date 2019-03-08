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

- The data is assumed to be stored as follows:
  - studyfolder
    - folder with data participant 1
      - multiple folders with data from the various channels
    - folder with data participant 2
      - multiple folders with data from the various channels
- For the moment, the data channels can include:
  - Passive data kit (pdk) data from phone.
  - pdk data from Withings wearable.
- Not facilitated yet, but scheduled for implementation:
  - pdk survey data
  - direct downloads from Withings.

## Expected output:

- Multiple .RData files with raw preprocessed data per channels
- One csv file with merged data from all channels
- png file with histograms of event distribution over 24-hour rhythm for each channel per participant asa quick check of data availablity and plausibility.

Variable names:
- lighton - whether phone captured light above 10 lux.
- sreenon	- whether phone screen was on.
- PSGmove	- whether phone speed or PSG indicated movement.
- AppAct - whether phone apps were actively used.
- batinteract	- whether phone battery was put on charge or taken off charge (only the movement of plugging it in or out is captured).
- phoneacc - whether phone accelerates by more than 0.03 times gravitational acceleration.
- withingsMove - whether withings wearable indicates movement.
- deepsleep	- whether withings wearable indicates deep sleep.
- lightsleep - whether withings wearable indicates light sleep.
- awake	- whether withings wearable indicates awake (as part of subclasses of sleep).
- AppHalted	- whether phone was restarting (runtime dropping to zero).
- SunSetRise - indicates when there was sunset or sunrise.
- hour - hour in the day.
-	min - minute of an hour.
- min_inday - minute in the day relative to midnight.

Extensions _dd and _pdk used in combination with Whithings wearable refers to direct download and pdk origin, respectively.


## Using the code

See [exploredata](explorate.R) for an example of how to use the key functions in the package.

