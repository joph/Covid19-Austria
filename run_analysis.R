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
  filter(Date < as.POSIXct("2020-04-01 01:00:00"))

db_at <- manual_data_entry(db_at,
                           date = as.POSIXct("2020-04-01 07:00:00"),
                           #8958
                           cases_infected_cum = 10182,
                           cases_dead_cum = 128,
                           cases_recovered_cum = 1095,
                           tests = 52344,
                           hospital = 1110,
                           intensive = 198)


outputdir<-"../Covid19-Austria-data/"

db_at %>%
  write_csv(paste0(outputdir,
                           "Austrian-data-long.csv"))

db_at %>%
  dplyr::select(-Cases_proportional) %>%
  spread(Type, Cases) %>%
  write_csv(paste0(outputdir,
                   "Austrian-data-wide.csv"))




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
  #"United Kingdom",
  "Italy",
  "Brazil",
  #"Korea, Rep.",
  #"South Africa",
  "Japan",
  "Spain",
  "Sweden")

log_plot(db_international,
         "Infected",
         countries,
         lab = "Positiv getestete Individuen \n(% der Bevölkerung, Logarithmische Skala)")

log_plot(db_international,
         "Dead",
         countries,
         lab = "Verstorbene Individuen \n(% der Bevölkerung, Logarithmische Skala)")


plot_growth_data(db_international,
                 "Italy",
                 avg= 7)


plot_growth_data(db_at,
                 avg = 2)

plot_doubling_time(db_international,
                   "Italy",
                   avg = 7)

plot_doubling_time(db_at, avg = 7)

plot_overview(db_international,
              "Italy")

plot_overview(db_at,
              diff = TRUE,
              log_scale = FALSE,
              roll_mean_length = 7)

tweet8<-updateStatus(text = " #COVIDー19 #CoronaVirusAt Corona Neue positiv Tests / Tag, im Durchschnitt der letzten 7 Tage. Die Maßnahmen scheinen zu wirken.",
                     mediaPath = get_filename(OVERVIEW_FILENAME, ""))

tweet10<-updateStatus(text = "#COVIDー19  #CoronaVirusAt Corona #RStats Code for analysis of Austrian infection data here. https://github.com/joph/Covid19-Austria",
                      inReplyTo=tweet8$id)


plot_prediction_combined(db_at,
                         db_international,
                         Sys.Date() + 6,
                         region1 = "Austria",
                         region2 = "Italy",
                         delay = 8,
                         polynom = 4,
                         exp = FALSE,
                         limDate = Sys.Date() - 16,
                         colors = COLORS[c(1, 7, 9)],
                         log_scale = FALSE)

plot_prediction_combined(db_at,
                         db_international,
                         Sys.Date() + 8,
                         region1 = "Austria",
                         region2 = "Italy",
                         delay = 16,
                         polynom = 6,
                         exp = FALSE,
                         limDate = Sys.Date() - 16,
                         colors = COLORS[c(1, 7, 9)],
                         log_scale = FALSE,
                         type = "Dead")

plot_infected_tests_ratio(db_at)

plot_number_tests(db_at)

plot_prediction(db_at,
                Sys.Date() + 2,
                region = "Austria")



prediction_quality(db_at,
                   0,
                   5)


authentification <- feather("authentification")

setup_twitter_oauth(consumer_key = authentification$consumer_key[1],
                    access_token = authentification$access_token[1],
                    consumer_secret = authentification$consumer_secret[1],
                    access_secret = authentification$access_secret[1])


tweet1<-updateStatus(text = " #COVIDー19  #CoronaVirusAt Datenupdate Österreich. Vorhersage  mit polynomiellem und Italien-Modell. 1/9",
                     mediaPath = get_filename(PREDICTIONS_FILENAME, "")
)




tweet3<-updateStatus(text = " #COVIDー19  #CoronaVirusAt Datenupdate Österreich. Wachstumsrate. 2/9",
                     mediaPath = get_filename(GROWTH_RATE_FILENAME, "")
                     ,
                     inReplyTo=tweet1$id
)

tweet4<-updateStatus(text = " #COVIDー19  #CoronaVirusAt Datenupdate Österreich. Dauer Verdopplungen der Infektionen in Tagen.",
                     mediaPath = get_filename(DOUBLING_FILENAME, "")
                     ,
                     inReplyTo=tweet3$id
                     )


tweet5<-updateStatus(text = "#COVIDー19  #CoronaVirusAt Datenupdate Österreich. Gesamtanzahl Tests. 4/9",
                     mediaPath = get_filename(NMB_TESTS_FILENAME, ""),
                     inReplyTo=tweet4$id)

tweet6<-updateStatus(text = "#COVID19 #CoronaVirusAt Datenupdate Österreich. Verhältnis Infektionen zu Tests. 5/9",
                     mediaPath = get_filename(INFECTED_TEST_RATIO_FILENAME, ""),
                     inReplyTo=tweet5$id)


tweet7<-updateStatus(text = " #COVIDー19  #CoronaVirusAt Datenupdate Österreich. Ländervergleich Sterbefälle. Datenpunkte bis zum Vortag. 6/9",
                     mediaPath = get_filename(FILE_NAME_LOG_PLOT, ""),
                     inReplyTo=tweet6$id)

tweet8<-updateStatus(text = "#COVIDー19  #CoronaVirusAt Datenupdate Österreich. Überblick (logarithmische Skala). 7/9",
                     mediaPath = get_filename(OVERVIEW_FILENAME, ""),
                     inReplyTo=tweet7$id)


tweet9<-updateStatus(text = "#COVIDー19  #CoronaVirusAt Datenupdate Österreich. Modellqualität. 8/9",
                     mediaPath = get_filename(PREDICTION_QUALITY_FILENAME, ""),
                     inReplyTo = tweet8$id
)

tweet10<-updateStatus(text = "#COVIDー19  #CoronaVirusAt #RStats Code for analysis of Austrian infection data here. https://github.com/joph/Covid19-Austria 9/9",
                     inReplyTo=tweet9$id)


