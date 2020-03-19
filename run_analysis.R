library(covid19at)
library(ggplot2)
library(tidyverse)

wikipedia_tables <- covid19at::scrape_wikipedia_at()
it<-scrape_wikipedia_it()

wikipedia_tables_new <- wikipedia_tables[[1]]
wikipedia_tables_old <- wikipedia_tables[[2]]


wikipedia_tables_new %>%
  mutate(log_inf = log(`Infektionen kumuliert`)) %>%
  ggplot(aes(x = Datum, y =log_inf)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1000) +
  ylim(c(0,8))

ggsave("figures/logplot.png")

international_cases<-download_international_cases()

# Difference in route between years

wikipedia_tables_new <-wikipedia_tables_new %>%
  bind_rows(tibble(Datum = c(as.POSIXct("2020-03-19 14:30:00 ")),
                                          NOE = c(0),
                                          W = c(0),
                                          St = c(0),
                                          `T` = c(0),
                                          `O?` = c(0),
                                          S = c(0),
                                          B = c(0),
                                          V = c(0),
                                          K = c(0),
                                          `Infektionen kumuliert` = c(2013),
                                          `Genesen kumuliert` = c(9),
                                          `Aktuell Infizierte` = c(2013-9-6),
                                          `Todesf?lle kumuliert` = c(6),
                                          Neuinfektionen = c(2013-1646),
                                          `Testungen kumuliert` = c(13724),
                                          Zuwachs = c(0)

                                          ))



theme_set(theme_classic(base_size = 14))

plot_prediction_combined(wikipedia_tables_new,
                         Sys.Date() + 4,
                         polynom = 9,
                         exp = FALSE)

plot_country_comparison(international_cases,
                        c("Austria",
                          "China",
                          "United States",
                          "Italy",
                          "Brazil"))

plot_growth_data(wikipedia_tables_new)

plot_doubling_time(wikipedia_tables_new)

plot_infected_tests_ratio(wikipedia_tables_new)

plot_number_tests(wikipedia_tables_new)


authentification <- feather("authentification")

setup_twitter_oauth(consumer_key = authentification$consumer_key[1],
                    access_token = authentification$access_token[1],
                    consumer_secret = authentification$consumer_secret[1],
                    access_secret = authentification$access_secret[1])


tweet1<-updateStatus(text = "#COVID_19 Vorhersage mit exponentiellem, polynomiellem und Italien Modell.",
                     mediaPath = PREDICTIONS_FILENAME
)

tweet2<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Wachstumsrate: ",
                     mediaPath = GROWTH_RATE_FILENAME,
                     inReplyTo=tweet2$id
)

tweet3<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Dauer Verdopplungen der Infektionen in Tagen:  ",
                     mediaPath = DOUBLING_FILENAME,
                     inReplyTo=tweet3$id)


tweet4<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Gesamtanzahl Tests",
                     mediaPath = NMB_TESTS_FILENAME,
                     inReplyTo=tweet4$id)

tweet5<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Verhaeltnis Infektionen : Tests:",
                     mediaPath = INFECTED_TEST_RATIO_FILENAME,
                     inReplyTo=tweet5$id)


tweet6<-updateStatus(text = "#COVID_19 Data Update. Vergleich Laender. Datenpunkte bis zum Vortag.",
                     mediaPath = COUNTRY_COMPARISON_FILENAME,
                     inReplyTo=tweet5$id)


tweet7<-updateStatus(text = "#COVID_19 #R-Stats Code for analysis of Austrian infection data here: https://github.com/joph/Covid19-Austria",
                     inReplyTo=tweet6$id)




tweet_results(wikipedia_tables_new,
              wikipedia_tables_old,
              TRUE)

##### testing stuff
plot_overview_at(wikipedia_tables_new)



countries <- c("Italy",
               "Brazil",
               "Austria",
               "United States",
               "China")

p <- international_cases %>%
  filter(Region %in% countries) %>%
  filter(Type == "Infected") %>%
  ggplot(aes(x = Date, y = log(Cases_proportional*100))) +
  geom_line(aes(col = Region), size = 1) +
  ylab("log_Infektionen (% der Bevölkerung)") +
  scale_color_manual(values = COLORS) +
  labs(caption = paste("Quelle: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/", ""))

p

ggsave(
       "figures/coutnry_comparsion.png",
       p)

s<-function(x){
  return(x^(1/9))
}

p<-function(x){
  return(x^9)
}

plot_prediction(wikipedia_tables_new,
                Sys.Date() + 31,
                3,
                s,
                p,
                " (polynomielles Modell M1): ")

plot_prediction(wikipedia_tables_new,
                Sys.Date() + 31,
                2,
                log,
                exp,
                " (exponentielles Modell M0): ")

