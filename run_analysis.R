library(covid19at)
library(ggplot2)

wikipedia_tables <- covid19at::scrape_wikipedia()

wikipedia_tables_new <- wikipedia_tables[[1]]
wikipedia_tables_old <- wikipedia_tables[[2]]

theme_set(theme_classic(base_size = 14))

plot_overview(wikipedia_tables_new)

plot_prediction(wikipedia_tables_new,
                Sys.Date() + 3,
                3,
                log,
                exp)

plot_test_statistics(wikipedia_tables_new)

plot_number_tests(wikipedia_tables_new)

tweet_results_if_new(wikipedia_tables_new,
                     wikipedia_tables_old)


wikipedia_table<-wikipedia_tables_new %>%
  mutate(Infektionen = `Infektionen kumuliert`) %>%
  mutate(log_Infektionen = log(Infektionen))


wikipedia_table1<-wikipedia_table[1:(nrow(wikipedia_table)-1),]
wikipedia_table2
wikipedia_table3

mod1<-lm(log_Infektionen[1:] ~ Datum[1:n()], data=wikipedia_table)

