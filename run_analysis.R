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

db_at <- get_data_at_corin.at()

db_international <- download_international_cases()

db_at %>% tail()

#db_at <- db_at %>%
#  filter(!(Date > as.POSIXct("2020-05-01 01:00:00")))

db_at <- db_at %>% filter(Date<Sys.Date())

#db_at <- manual_data_entry(db_at,
 #                          date = as.POSIXct("2020-12-23 15:00:00"),
#                           cases_infected_cum = 		280743,
#                           cases_dead_cum = 3184,
#                           cases_recovered_cum = 221692,
#                           tests = 	3098318,
#                           hospital = 	4340,
#                           intensive = 	701)

db_at <- manual_data_entry(db_at,
                           date = as.POSIXct("2021-01-01 15:00:00"),
                           cases_infected_cum = 				361623,
                           cases_dead_cum = 6261,
                           cases_recovered_cum = 334901,

                           hospital = 	2243,
                           intensive = 	385,
                           tests = 		3857382)

db_at <- manual_data_entry(db_at,
                           date = as.POSIXct("2021-01-02 15:00:00"),
                           cases_infected_cum = 				362043,
                           cases_dead_cum = 6261,
                           cases_recovered_cum = 334901,

                           hospital = 	2243,
                           intensive = 	385,
                           tests = 		3857382)

countries<-c("Austria",
  #"China",
  "United States",
  #"United Kingdom",
  #"Italy",
  "Brazil",
  #"Korea, Rep.",
  #"South Africa",
#  "Japan",
  #"Spain",
  "Sweden",
  "Italy",
"Switzerland",
  "Germany",
"Netherlands",
#  "United Kingdom",
  "Belgium")
#,
#"India")
#  "Italy")
#"Japan")



#plot_test_share(db_at)



log_plot(db_international,
         "Infected",
         countries,
         lab = "Positiv getestete Individuen \n(% der Bevölkerung)")

log_plot(db_international,
         "Dead",
         countries,
         lab = "Verstorbene Individuen \n(% der Bevölkerung)",
         filename_add = "DEAD")

results<-plot_growth_data(db_at %>% filter(Date > Sys.Date() - 14),
                 avg = 7)



results[[1]]

results[[2]]

results<-plot_growth_data(db_at %>% filter(Type %in% c("Currently_Ill")) %>% filter(Date>Sys.Date()-90),
                          avg = 7)
results[[1]]


ggsave("figures/growth-new-cases.png")

db_at %>%
  filter(Type == "New_Cases") %>%
  ggplot(aes(x=(Date), y=Cases))+
  geom_line()

results<-plot_growth_data(db_at %>% filter(Type %in% c("Currently_Ill")),
                          avg = 7)
results[[1]]


ggsave("figures/growth.png")
how_long_until_daily_infections_below(results[[2]],
                                                db_at,
                                                1050)

results[[2]] %>% tail()


plot_overview_short(db_at,
              region = "Austria",
              diff = FALSE,
              log_scale = TRUE,
              roll_mean_length = 7)


plot_overview(db_at,
                    region = "Austria",
                    diff = FALSE,
                    log_scale = TRUE,
                    roll_mean_length = 7)


plot_overview_short(db_at,
                    region = "Austria",
                    diff = FALSE,
                    log_scale = FALSE,
                    roll_mean_length = 7)

ggsave("figures/overview-facet-wrap.png")

plot_infected_tests_ratio(db_at)

plot_number_tests(db_at)

plot_number_tests_aggregate(db_at)

plot_relative_values(db_at %>% filter(Date > Sys.Date() - 200))

ggsave("figures/relative-facet-wrap.png")


ggsave("figures/tests_per_day.png")



library(lubridate)

db_at %>% filter(Type == "Nmb_Tested") %>% mutate(c=c(0,diff(Cases))) %>%
  group_by(day=wday(Date)) %>%
  summarize(tests_per_day=mean(c)) %>%
  ggplot(aes(x=day,y=tests_per_day)) +
  geom_line() +
  scale_x_continuous(breaks=seq(1,7),
                     labels=c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa")) +
  xlab("Tag") +
  ylab("Für Vortag gemeldete Covid-19 Tests \n (Durcschnitt pro Wochentag in Österreich)")


ggsave("figures/tests-per-day.png")

db_at %>% filter(Type == "Infected") %>% mutate(c=c(0,diff(Cases))) %>%
  group_by(day=wday(Date)) %>%
  summarize(tests_per_day=mean(c)) %>%
  ggplot(aes(x=day,y=tests_per_day)) +
  geom_line() +
  scale_x_continuous(breaks=seq(1,7),
                     labels=c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa")) +
  xlab("Tag") +
  ylab("Für Vortag gemeldete positive Covid-19 Tests \n (Durcschnitt pro Wochentag in Österreich)")


ggsave("figures/infected-per-day.png")

authentification <- feather("authentification")

setup_twitter_oauth(consumer_key = authentification$consumer_key[1],
                    access_token = authentification$access_token[1],
                    consumer_secret = authentification$consumer_secret[1],
                    access_secret = authentification$access_secret[1])


#tweet1<-updateStatus(text = " #COVIDー19  #CoronaVirusAt Datenupdate Österreich. Vorhersage  mit polynomiellem und Italien-Modell. 1/9",
#                     mediaPath = get_filename(PREDICTIONS_FILENAME, "")
#)

tweet1<-updateStatus(text = " #COVID #CoronaVirusAt #Corona. Erkrankte, Hospitalisierte, Intensiv Hospitalisierte. Verstorbene. 1/8",
                     mediaPath = get_filename(OVERVIEW_FILENAME, ""))

#tweet1<-updateStatus(text = " #COVID #CoronaVirusAt Corona. Anteil neuer positiver Tests an aktuellen Erkrankungen. 1/7",
#                     mediaPath = get_filename(TEST_SHARE_FILENAME, ""))

tweet2<-updateStatus(text = " #COVID  #CoronaVirusAt Datenupdate Österreich. Ländervergleich positive Tests. 2/8",
                     mediaPath = get_filename(FILE_NAME_LOG_PLOT, ""),
                     inReplyTo=tweet1$id)

tweet3<-updateStatus(text = " #COVID  #CoronaVirusAt Datenupdate Österreich. Ländervergleich. Covid19-Tote. 3/8",
                     mediaPath = get_filename(FILE_NAME_LOG_PLOT, "DEAD"),
                     inReplyTo=tweet2$id)

tweet4<-updateStatus(text = " #COVID  #CoronaVirusAt Datenupdate Österreich. Wachstumsraten. 4/8",
                     mediaPath = get_filename(GROWTH_RATE_FILENAME, "")
                     ,
                     inReplyTo=tweet3$id
)

tweet5<-updateStatus(text = "#COVID  #CoronaVirusAt Datenupdate Österreich. Gesamtanzahl Tests. 5/8",
                     mediaPath = get_filename(NMB_TESTS_FILENAME, ""),
                     inReplyTo=tweet4$id)

tweet6<-updateStatus(text = "#COVID #CoronaVirusAt Datenupdate Österreich. Verhältnis Infektionen zu Tests. 6/8",
                     mediaPath = get_filename(INFECTED_TEST_RATIO_FILENAME, ""),
                     inReplyTo=tweet5$id)


tweet7<-updateStatus(text = "#COVID #CoronaVirusAt Verhältnis Hospitalisierte, intensive Hospitalisierte & Verstorbene zu derzeit positiv Getesteten 7/8",
                     mediaPath = get_filename(RELATIVE_FILENAME, ""),
                     inReplyTo=tweet6$id)


tweet8<-updateStatus(text = "#COVID  #CoronaVirusAt #RStats Code for analysis of Austrian infection data here. https://github.com/joph/Covid19-Austria 8/8",
                      inReplyTo=tweet7$id)






tweet1<-updateStatus(text = " #COVID #CoronaVirusAt Corona. Fälle/Tag, im Durchschnitt der letzten 7 Tage (logarithmische Skala). 1/7",
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


