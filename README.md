![MIT license](https://img.shields.io/github/license/inwe-boku/lecture-scientific-computing)

# Covid19-Austria

Package which scrapes Austrian Covid-19 infection data from wikipedia and downloads international Covid-19 data from [Johns Hopkins CSSE](https://github.com/CSSEGISandData/COVID-19/) and produces figures on infections and infection to test ratios.

## Installation
Works with package devtools (install before usage!).
<pre><code>
devtools::install_github("joph/covid19at")
</code></pre>

## Example script
See run_analysis.R for Austria
See run_analysis_brazil.R for Brazil

### Creates among others, these figures

<img src = "figures/covid19_growth_rate.png" width="600">
<img src = "figures/covid19_doubling.png" width="600">
<img src = "figures/covid19_infektionen.png" width="600">
<img src = "figures/covid19_predictions_comparison.png" width="600">
<img src = "figures/covid19_vergleich_laender.png" width="600">
<img src = "figures/covid19_infektionen_tests_ratio.png" width="600">
<img src = "figures/covid19_anzahl_tests.png" width="600">
<img src = "figures/covid19_predictions.png" width="600">
<img src = "figures/covid19_prediction_quality.png" width="600">
<img src = "figures/covid19_log.png" width="600">



