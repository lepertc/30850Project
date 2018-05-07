# 30850Project

seed - For anything we want to be reproduceable lets use 06022018 as a seed. Note for the 5% and 10% sample the seed 20160602 was used.

getdata_bikeshare.R - downloads data from 2010-17, cleans up the date variable, takes a long time to run. I recommend creating a data file with this so you don't have to run it many times. Make sure to install the data.table package.

smaller_df.R - functions to make the data smaller and more manageable. sample.df samples the data.table. There are four aggregate functions which aggregate at various levels.

get_stocks.R - still in progress
