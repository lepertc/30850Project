# 30850Project

seed - For anything we want to be reproduceable lets use 06022018 as a seed. Note for the 5% and 10% sample the seed 20160602 was used.

## Data Acquisition

u_getdata_bikeshare.R - downloads data from 2010-17, cleans up the date variable, takes a long time to run. I recommend creating a data file with this so you don't have to run it many times. Make sure to install the data.table package.

u_smaller_df.R - functions to make the data smaller and more manageable. sample.df samples the data.table. There are four aggregate functions which aggregate at various levels.

### Data sets

u_financials.csv - time series for key financial indicators

u_stations.csv - census information for each station

u_Weather/u_NOAA_weather_data.csv - daily weather for the DC area (measured at Reagan airport)

## Datasplitting - in folder called u_data_splitting
setup_and_sample - samples test sets for analysis of daily aggregated counts. RUN THIS FIRST
5percent - analysis of daily counts based on 5% training set
5percent - analysis of daily counts based on 10% training set
5percent - analysis of daily counts based on 25% training set

setup_STATIONS - constructs and preprocesse data frame for stations aggregated counts. RUN THIS FIRST
stations_total - analysis of total station counts
stations_prop - analysis of proportion of rides at station that are end rides

## Accumulation testing - in folder called u_Ordered

u_Ordered.Rproj - environment in which to run accumulation testing

u_make_df.R - processes the data for accumulation testing. Requires an RData file stored outside of the repo called df.RData. df.RData is the result of running u_getdata_bikeshare.R

u_addition.R - Calculates the amount added to the quantity for all three accumulation tests at different p values.  Saved after running into a file called u_useThis.RData

u_useThis.RData - processed data for accumulation testing

u_process_this.R - further processing for accumulation testing

u_order_hypotheses_clean.R - Runs accumulation testing and produces figure 7. Order on intuition or 5%; test on 95%

u_order_small.R - Runs accumulation testing and produces figure 8. Order on intuition or 5%; test on 5%
