Test script for applying a custom function across a rolling window. I searched for hours to try to find this answer. Very useful for analyzing continuous monitoring data. 

Here I have applied a function for upstream/downstream temperature comparisons from paired USGS gages (script also shows a little of pulling NWIS data). The custom function is a simple one but great for demo purposes. 

Credit for genius function tutorial goes to Matt Dancho http://www.business-science.io/timeseries-analysis/2017/07/23/tidy-timeseries-analysis-pt-2.html . I simply applied to wqm data.

- Emma Jones (emma.jones@deq.virginia.gov)

R version 3.4.1 (2017-06-30) -- "Single Candle"
dataRetrieval v 2.7.3
tidyquant v 0.5.3
