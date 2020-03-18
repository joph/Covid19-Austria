library(covid19at)
library(ggplot2)
library(tidyverse)

wikipedia_tables <- covid19at::scrape_wikipedia_at()


it<-scrape_wikipedia_it()

wikipedia_tables_new <- wikipedia_tables[[1]]
wikipedia_tables_old <- wikipedia_tables[[2]]

wikipedia_tables_new <-wikipedia_tables_new %>%
  bind_rows(tibble(Datum = c(as.POSIXct("2020-03-18 08:00:00 ")),
                                          NOE = c(0),
                                          W = c(0),
                                          St = c(0),
                                          `T` = c(0),
                                          `O?` = c(0),
                                          S = c(0),
                                          B = c(0),
                                          V = c(0),
                                          K = c(0),
                                          `Infektionen kumuliert` = c(1471),
                                          `Genesen kumuliert` = c(9),
                                          `Aktuell Infizierte` = c(1471-9-3),
                                          `Todesf?lle kumuliert` = c(3),
                                          Neuinfektionen = c(1471-1332),
                                          `Testungen kumuliert` = c(11977),
                                          Zuwachs = c(0)

                                          ))



theme_set(theme_classic(base_size = 14))

plot_overview_at(wikipedia_tables_new)


s<-function(x){
  return(x^(1/14))
}

p<-function(x){
  return(x^14)
}

plot_prediction(wikipedia_tables_new,
                Sys.Date() + 3,
                3,
                s,
                p)

plot_prediction(wikipedia_tables_new,
                Sys.Date() + 3,
                3,
                log,
                exp)

plot_infected_tests_ratio(wikipedia_tables_new)

plot_number_tests(wikipedia_tables_new)

plot_compare_at_it()

international_cases<-download_international_cases()

plot_country_comparison(international_cases,
                   c("Austria",
                     "China",
                     "United States",
                     "Italy",
                     "Brazil"))

tweet_results(wikipedia_tables_new,
                     wikipedia_tables_old,
              TRUE)


