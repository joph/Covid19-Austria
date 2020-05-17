library(covid19at)
library(ggplot2)
library(tidyverse)
library(twitteR)
library(feather)
library(directlabels)
library(drc)



theme_set(theme_classic(base_size = 16))

update_geom_defaults("line", list(size = 1.2))

Sys.setlocale("LC_ALL", "German")

db_at <- scrape_wikipedia_at()

db_international <- download_international_cases()

db_at <- db_at %>%
  filter(!(Date > as.POSIXct("2020-05-01 01:00:00")))

db_at <- manual_data_entry(db_at,
                           date = as.POSIXct("2020-05-01 15:00:00"),
                           #8958
                           cases_infected_cum = 15470,
                           cases_dead_cum = 589,
                           cases_recovered_cum = 13110,
                           tests = 264079,
                           hospital = 472,
                           intensive = 124)



####try logistic curve
infected <- get_infected(db_at,
                         "Austria")
mL <- drm(Cases ~ Date, data = infected, fct = L.3(), type = "continuous")
summary(mL)
plot(mL)


###bis 29.3., 10:00

#db_at %>% write_feather("data/database.feather")


###write files to disk -> scientific computing
db_at %>%
  spread(Type, Cases) %>%
  dplyr::select(Date, Infected) %>%
  filter(Date > as.POSIXct("2020-03-04")) %>%
  na.omit() %>%
  write_csv("g:/meine ablage/lva/scientific computing/lecture-scientific-computing/lecture03-python-introduction/austria_covid_19_data.csv")





countries<-c("Austria",
  "China",
  "United States",
  "United Kingdom",
  #"Italy",
  "Brazil",
  #"Korea, Rep.",
  #"South Africa",
#  "Japan",
  "Spain",
  "Sweden",
#  "United Kingdom",
  "Belgium",
  "Italy")
#"Japan")


plot_test_share(db_at)

log_plot(db_international,
         "Infected",
         countries,
         lab = "Positiv getestete Individuen \n(% der Bevölkerung)")

log_plot(db_international,
         "Dead",
         countries,
         lab = "Verstorbene Individuen \n(% der Bevölkerung)",
         filename_add = "DEAD")



results<-plot_growth_data(db_at %>% filter(Date > Sys.Date() - 21),
                 avg = 7)


results[[1]]

results[[2]] %>%
  filter(Date > Sys.Date()) %>%
  tail(10)


plot_overview_short(db_at,
              region = "Austria",
              diff = FALSE,
              log_scale = FALSE,
              roll_mean_length = 7)


plot_infected_tests_ratio(db_at)

plot_number_tests(db_at)

authentification <- feather("authentification")

setup_twitter_oauth(consumer_key = authentification$consumer_key[1],
                    access_token = authentification$access_token[1],
                    consumer_secret = authentification$consumer_secret[1],
                    access_secret = authentification$access_secret[1])


#tweet1<-updateStatus(text = " #COVIDー19  #CoronaVirusAt Datenupdate Österreich. Vorhersage  mit polynomiellem und Italien-Modell. 1/9",
#                     mediaPath = get_filename(PREDICTIONS_FILENAME, "")
#)

tweet1<-updateStatus(text = " #COVID #CoronaVirusAt #Corona. Erkrankte, Hospitalisierte, Intensiv Hospitalisierte. 1/7",
                     mediaPath = get_filename(OVERVIEW_FILENAME, ""))

#tweet1<-updateStatus(text = " #COVID #CoronaVirusAt Corona. Anteil neuer positiver Tests an aktuellen Erkrankungen. 1/7",
#                     mediaPath = get_filename(TEST_SHARE_FILENAME, ""))

tweet2<-updateStatus(text = " #COVID  #CoronaVirusAt Datenupdate Österreich. Ländervergleich positive Tests. 2/7",
                     mediaPath = get_filename(FILE_NAME_LOG_PLOT, ""),
                     inReplyTo=tweet1$id)

tweet3<-updateStatus(text = " #COVID  #CoronaVirusAt Datenupdate Österreich. Ländervergleich. Covid19-Tote.3/7",
                     mediaPath = get_filename(FILE_NAME_LOG_PLOT, "DEAD"),
                     inReplyTo=tweet2$id)

tweet4<-updateStatus(text = " #COVID  #CoronaVirusAt Datenupdate Österreich. Wachstumsraten. 4/7",
                     mediaPath = get_filename(GROWTH_RATE_FILENAME, "")
                     ,
                     inReplyTo=tweet3$id
)

tweet5<-updateStatus(text = "#COVID  #CoronaVirusAt Datenupdate Österreich. Gesamtanzahl Tests. 5/7",
                     mediaPath = get_filename(NMB_TESTS_FILENAME, ""),
                     inReplyTo=tweet4$id)

tweet6<-updateStatus(text = "#COVID #CoronaVirusAt Datenupdate Österreich. Verhältnis Infektionen zu Tests. 6/7",
                     mediaPath = get_filename(INFECTED_TEST_RATIO_FILENAME, ""),
                     inReplyTo=tweet5$id)

tweet7<-updateStatus(text = "#COVID  #CoronaVirusAt #RStats Code for analysis of Austrian infection data here. https://github.com/joph/Covid19-Austria 7/7",
                      inReplyTo=tweet6$id)






stweet1<-updateStatus(text = " #COVID #CoronaVirusAt Corona. Fälle/Tag, im Durchschnitt der letzten 7 Tage (logarithmische Skala). 1/7",
                     mediaPath = get_filename(OVERVIEW_FILENAME, ""))



tweet3<-updateStatus(text = " #COVID  #CoronaVirusAt Datenupdate Österreich. Wachstumsrate. 2/7",
                     mediaPath = get_filename(GROWTH_RATE_FILENAME, "")
                     ,
                     inReplyTo=tweet1$id
)

tweet4<-updateStatus(text = " #COVID  #CoronaVirusAt Datenupdate Österreich. Dauer Verdopplungen der Infektionen in Tagen. 3/7",
                     mediaPath = get_filename(DOUBLING_FILENAME, "")
                     ,
                     inReplyTo=tweet3$id
                     )


tweet5<-updateStatus(text = "#COVID  #CoronaVirusAt Datenupdate Österreich. Gesamtanzahl Tests. 4/7",
                     mediaPath = get_filename(NMB_TESTS_FILENAME, ""),
                     inReplyTo=tweet4$id)

tweet6<-updateStatus(text = "#COVID #CoronaVirusAt Datenupdate Österreich. Verhältnis Infektionen zu Tests. 5/7",
                     mediaPath = get_filename(INFECTED_TEST_RATIO_FILENAME, ""),
                     inReplyTo=tweet5$id)


tweet7<-updateStatus(text = " #COVID  #CoronaVirusAt Datenupdate Österreich. Ländervergleich Sterbefälle. Datenpunkte bis zum Vortag. 6/7",
                     mediaPath = get_filename(FILE_NAME_LOG_PLOT, ""),
                     inReplyTo=tweet6$id)

#tweet8<-updateStatus(text = "#COVIDー19  #CoronaVirusAt Datenupdate Österreich. Überblick (logarithmische Skala). 7/9",
#                     mediaPath = get_filename(OVERVIEW_FILENAME, ""),
#                     inReplyTo=tweet7$id)


#tweet9<-updateStatus(text = "#COVIDー19  #CoronaVirusAt Datenupdate Österreich. Modellqualität. 8/9",
#                     mediaPath = get_filename(PREDICTION_QUALITY_FILENAME, ""),
#                     inReplyTo = tweet8$id
#)

tweet10<-updateStatus(text = "#COVID  #CoronaVirusAt #RStats Code for analysis of Austrian infection data here. https://github.com/joph/Covid19-Austria 7/7",
                     inReplyTo=tweet7$id)


