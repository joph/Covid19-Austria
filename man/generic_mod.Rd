% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covid19_at.R
\name{generic_mod}
\alias{generic_mod}
\title{Fits a generic linear model to input data and projects future infections from the model}
\usage{
generic_mod(wikipedia_table_in, minus_n, week_ahead, model_name)
}
\arguments{
\item{wikipedia_table_in}{Table to be fitted. Has to contain columns Datum and Infektionen_trans}

\item{minus_n}{How many days should be removed from dataset (starting from the latest date)}

\item{week_ahead}{Period for which should be predicted. Has to be a vector of POSIXct dates.}

\item{model_name}{A string indicating the name of the model}
}
\value{
A data.frame with Dates,
}
\description{
The function fits a generic linear model between Datum and Infektionen_trans.
It allows to reduce the number of input data points
}
