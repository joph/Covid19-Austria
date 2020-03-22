library(covid19at)
library(ggplot2)
library(tidyverse)
library(twitteR)
library(feather)

theme_set(theme_classic(base_size = 16))

update_geom_defaults("line", list(size = 1.2))


db_at <- scrape_wikipedia_at()
db_international <- download_international_cases()

db_at <- db_at %>%
  filter(Date < as.POSIXct("2020-03-21 23:59:00"))

db_at <- manual_data_entry(db_at,
                           date = as.POSIXct("2020-03-22 15:00:00"),
                          cases_infected_cum = 3244,
                          cases_dead_cum = 16,
                          cases_recovered_cum = 9,
                          tests = 21368)




countries<-c("Austria",
  "China",
  "United States",
  #"United Kingdom",
  "Italy",
  "Brazil",
  #"Korea, Rep.",
  #"South Africa",
  "Japan")

log_plot(db_international,
         "Infected",
         countries,
         lab = "Positiv getestete Individuen \n(% der Bevölkerung, Logarithmische Skala)")

log_plot(db_international,
         "Dead",
         countries,
         lab = "Verstorbene Individuen \n(% der Bevölkerung, Logarithmische Skala)")


plot_growth_data(db_international,
                 "Italy")


plot_growth_data(db_at, avg = 4)

plot_doubling_time(db_international,
                   "Italy")

plot_doubling_time(db_at, avg = 4)

plot_overview(db_international,
              "United Kingdom")

plot_overview(db_at)

plot_prediction_combined(db_at,
                         db_international,
                         Sys.Date() + 7,
                         region1 = "Austria",
                         region2 = "Italy",
                         delay = 8,
                         polynom = 6,
                         exp = FALSE,
                         limDate = Sys.Date() - 16,
                         colors = COLORS[c(1, 7, 9)],
                         log_scale = FALSE)

plot_infected_tests_ratio(db_at)

plot_number_tests(db_at)

plot_prediction(db_international,
                Sys.Date() + 4,
                region = "Austria")


authentification <- feather("authentification")

setup_twitter_oauth(consumer_key = authentification$consumer_key[1],
                    access_token = authentification$access_token[1],
                    consumer_secret = authentification$consumer_secret[1],
                    access_secret = authentification$access_secret[1])


tweet1<-updateStatus(text = "#COVID_19 #coronavirusat Datenupdate Österreich. Vorhersage  mit polynomiellem und Italien-Modell. 1/7",
                     mediaPath = PREDICTIONS_FILENAME
)

tweet2<-updateStatus(text = "#COVID_19 #coronavirusat Datenupdate Österreich. Wachstumsrate. 2/7",
                     mediaPath = GROWTH_RATE_FILENAME,
                     inReplyTo=tweet1$id
)

tweet3<-updateStatus(text = "#COVID_19 #coronavirusat Datenupdate Österreich. Dauer Verdopplungen der Infektionen in Tagen. 3/7",
                     mediaPath = DOUBLING_FILENAME,
                     inReplyTo=tweet2$id)


tweet4<-updateStatus(text = "#COVID_19 #coronavirusat Datenupdate Österreich. Gesamtanzahl Tests. 4/7",
                     mediaPath = NMB_TESTS_FILENAME,
                     inReplyTo=tweet3$id)

tweet5<-updateStatus(text = "#COVID_19 #coronavirusat Datenupdate Österreich. Verhältnis Infektionen zu Tests. 5/7",
                     mediaPath = INFECTED_TEST_RATIO_FILENAME,
                     inReplyTo=tweet4$id)


tweet6<-updateStatus(text = "#COVID_19 #coronavirusat Datenupdate Österreich. Ländervergleich Sterbefälle Datenpunkte bis zum Vortag.",
                     mediaPath = FILE_NAME_LOG_PLOT,
                     inReplyTo=tweet5$id)

tweet7<-updateStatus(text = "#COVID_19 #coronavirusat Datenupdate Österreich. Überblick (logarithmische Skala)",
                     mediaPath = OVERVIEW_FILENAME,
                     inReplyTo=tweet6$id)

tweet8<-updateStatus(text = "#COVID_19 #coronavirusat #RStats Code for analysis of Austrian infection data here. https://github.com/joph/Covid19-Austria",
                     inReplyTo=tweet7$id)

