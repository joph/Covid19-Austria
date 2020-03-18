library(covid19at)
library(ggplot2)

wikipedia_tables <- covid19at::scrape_wikipedia_at()


it<-scrape_wikipedia_it()

wikipedia_tables_new <- wikipedia_tables[[1]]
wikipedia_tables_old <- wikipedia_tables[[2]]

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

country_comparison(international_cases,
                   c("Austria",
                     "China",
                     "United States",
                     "Italy",
                     "Brazil"))

tweet_results(wikipedia_tables_new,
                     wikipedia_tables_old,
              TRUE)


