## Source the Code and Login to Aquarius
We've create a set of functions to greatly ease the import of R data series into R. To use those functions in your R code, first source the functions:  
`source(“/epscorfs/AquariusR/aquariusHelperFunctions.R”, chdir = T)`  
Make sure to include the `chdir = T`, it's important.

Then, login to the Aquarius server using:  
`aquariusLoginWithFile("/epscorfs/AquariusR/aquariusLogin")`

If you want to use the functions on your own desktop or laptop, you need to copy the entire directory `/epscorfs/AquariusR/` to your local machine and then change the path in the `source` and `aquariusLoginWithFile` commands above, remembering again to include the `chdir = T` in the `source` function. I should also note that the Aquarius server is only accessible within the UVM network, so you'll need to VPN in to UVM if you want to pull Aquarius data to your local machine.

## Examples of Use
The file `/epscorfs/AquariusR/aquariusExample.R` has a number of examples of how to use the functions you just sourced in the last section. Make sure you include the first two `source()` and aquariusLoginWithFile() statements -- **and adjust their paths if using them locally**. 

## Timezones
Each Aquarius dataseries has a timezone context, as well as your current R session. Note that the format of the `startTime` and `endTime` parameters in the functions below include a timezone. In addition, the `Timestamp` column of the returned data.frame is printed in the local timezone. To adjust that local timezone context, use:  
`attr(df$Timestamp, "tzone") <- "Etc/GMT+5"`  
and  
`attr(df$Timestamp, 'tzone') <- ""`  
to change it back to the default as defined by your operating system.

## Available Functions for Use
Note that the two functions that fetch the Aquarius data series return R data frames:

### getSingleCorrectedAquariusDataSeries(dsName, startTime, endTime, *minInterval=0*)  
Returns a [data.frame](https://stat.ethz.ch/R-manual/R-devel/library/base/html/data.frame.html) containing the observations in the Aquarius data series `dsName` between `startTime` and `endTime`. The dataframe will have two columns: `Timestamp` which is the time of the observation and `dsName` which is the observation data requested.  
The optional minInterval parameter is used round the timestamps to the nearest minute interval specified. The default, 0, denotes no rounding of the time stamps. As an example, minInterval = 15 would round all of the timestamps to the nearest quarter-hour (:00, :15, :30, or :45) and minInterval = 5 would round all of the timestamps to the nearest 5 minutes interval (:00, :05, :10, :15, etc.).

Example:  
```
df <- getCorrectedAquariusDataSeries(
  "Temperature.SoilTemp_15cm@HD_pit1",
  startTime = "2018-06-01T00:00:00-05:00",
  endTime   = "2018-08-01T00:00:00-05:00")
```  

### getCorrectedAquariusDataSeries(dsNames, startTime, endTime, *minInterval=0*)
Returns a [data.frame](https://stat.ethz.ch/R-manual/R-devel/library/base/html/data.frame.html) containing the observations of all of the Aquarius data series in the list `dsName` between `startTime` and `endTime`. The dataframe will have `n+1` columns where `n` is the length of `dsName`: `Timestamp` which is the time of the observation and then `n` columns, 1 for each element in `dsName` containing the observation data requested. The name of each observation data column be exactly the same as the corresponding element in `dsName`.

The optional minInterval parameter is used round the timestamps to the nearest minute interval specified. The default, 0, denotes no rounding of the time stamps. As an example, minInterval = 15 would round all of the timestamps to the nearest quarter-hour (:00, :15, :30, or :45) and minInterval = 5 would round all of the timestamps to the nearest 5 minutes interval (:00, :05, :10, :15, etc.). This feature is particularly useful when trying to "line-up" observations between multiple data series that might vary by a few minutes or seconds.

Example:
```
df_multiple_ds <- getCorrectedAquariusDataSeries(
  c("Nitrate N.NO3N@Hungerford Brook",
    "N (Dis).TDN_Lab_Results@Hungerford Brook"),
  startTime = "2015-12-01T00:00:00-05:00",
  endTime   = "2016-12-01T00:00:00-05:00",
  minInterval = 5)
```
