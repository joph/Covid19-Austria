library(covid19at)
library(ggplot2)

wikipedia_tables <- covid19at::scrape_wikipedia_at()

it<-scrape_wikipedia_it()

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

plot_compare_at_it()

tweet_results_if_new(wikipedia_tables_new,
                     wikipedia_tables_old)


