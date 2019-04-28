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

## Expected data input and folder structure:

- The data is assumed to be stored as follows:
  - studyfolder
    - folder with raw data participant 1 (note: calling it SS01 might be practical, but is not necessary)
      - pdk_pdk-app-event.txt
      - Phone_sensors (folder)
        - pdk-device-battery (folder)
        - pdk-foreground-application (folder)
        - ... and all other pdk output folders with txt files from the various channels
      - Sleep_diary (folder)
        - pdk_sleep-survey.txt
      - Withings-data (folder)
        - txt-files for pdk download (optionally stored subfolders)
        - csv-files for direct download (optionally stored subfolders)
    - folder with raw data participant 2
      - same structure as participant 1
- For the moment, the data channels can include:
  - Passive data kit (pdk) data from phone.
  - Folder with data from Withings wearable, identified as any string with 'Withings-' in it.
    Inside this folder there will be subfolders for:
    - Direct downloads from Withings.
    - pdk data for Withings.

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

See [exploredata](exploredata.R) for an example of how to use the key functions in the package.

