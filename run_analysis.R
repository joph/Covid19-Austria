library(covid19at)
library(ggplot2)
library(tidyverse)
library(twitteR)

theme_set(theme_classic(base_size = 14))

db_at <- covid19at::scrape_wikipedia_at()
db_international <- download_international_cases()

db_at <- manual_data_entry(db_at,
                           date = as.POSIXct("2020-03-21 08:00:00"),
                          cases_infected_cum = 2664,
                          cases_dead_cum = 7,
                          cases_recovered_cum = 9,
                          tests = 18545)

plot_growth_data(db_international,
                 "United Kingdom")


plot_growth_data(db_at)

plot_doubling_time(db_international,
                   "United Kingdom")

plot_doubling_time(db_at)

plot_overview(db_international,
              "United Kingdom")

plot_overview(db_at)

plot_prediction_combined(db_at,
                         db_international,
                         Sys.Date() + 4,
                         region1 = "Austria",
                         region2 = "Italy",
                         delay = 8,
                         polynom = 6,
                         exp = FALSE)

plot_country_comparison(db_international,
                        c("Austria",
                          "China",
                          "United States",
                          "United Kingdom",
                          "Italy",
                          "Brazil",
                          "South Africa"))


plot_infected_tests_ratio(db_at)

plot_number_tests(db_at)

plot_prediction(db_international,
                Sys.Date() + 4,
                region = "Italy")


authentification <- feather("authentification")

setup_twitter_oauth(consumer_key = authentification$consumer_key[1],
                    access_token = authentification$access_token[1],
                    consumer_secret = authentification$consumer_secret[1],
                    access_secret = authentification$access_secret[1])


tweet1<-updateStatus(text = "#COVID_19 Vorhersage mit polynomiellem und Italien Modell.",
                     mediaPath = PREDICTIONS_FILENAME
)

tweet2<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Wachstumsrate: ",
                     mediaPath = GROWTH_RATE_FILENAME,
                     inReplyTo=tweet1$id
)

tweet3<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Dauer Verdopplungen der Infektionen in Tagen:  ",
                     mediaPath = DOUBLING_FILENAME,
                     inReplyTo=tweet2$id)


tweet4<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Gesamtanzahl Tests",
                     mediaPath = NMB_TESTS_FILENAME,
                     inReplyTo=tweet3$id)

tweet5<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Verhaeltnis Infektionen : Tests:",
                     mediaPath = INFECTED_TEST_RATIO_FILENAME,
                     inReplyTo=tweet4$id)


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


it<-scrape_wikipedia_it() %>%
  mutate(Datum = Datum + 8*3600*24)


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


growth<-wikipedia_tables_new %>%
  # first sort by year
  arrange(Datum) %>%
  mutate(selector = ((n()-1):0)%%5) %>%
  filter(selector == 0) %>%
  mutate(Diff_datum = as.numeric(Datum - lag(Datum)),  # Difference in time (just in case there are gaps)
         Diff_growth = `Infektionen kumuliert` - lag(`Infektionen kumuliert`),
         Lag_infektionen = lag(`Infektionen kumuliert`),
         Rate_percent = 100 * ((`Infektionen kumuliert` /Lag_infektionen)^(1/Diff_datum) - 1))   %>%
  dplyr::select(Datum,
                Diff_datum,
                Diff_growth,
                Rate_percent,
                Lag_infektionen,
              `Infektionen kumuliert`)


