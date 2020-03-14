# Covid19-Austria

Script which scrapes Austrian infection data from wikipedia and produces figures on infections and infection to test ratios.
Automatically posts figures to twitter. (needs a twitter app configured).

## Dependencies (R-packages)
rvest, tidyverse, xml2, feather

## Dependencies with automatic tweeting feature
twitteR

Please see [here](https://www.r-bloggers.com/send-tweets-from-r-a-very-short-walkthrough/) for how to configure twitteR in R.

## How to run
R covid19_at.R

### Creates these figures
![infections](figures/covid19_infektionen.png)
![infections](figures/covid19_testungen.png)
![infections](figures/covid19_testungen_absolut.png)


