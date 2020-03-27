library(covid19at)
library(ggplot2)
library(tidyverse)
library(twitteR)
library(feather)
library(directlabels)

theme_set(theme_classic(base_size = 16))

update_geom_defaults("line", list(size = 1.2))

country <- "Brazil"
OVERVIEW_FILENAME <- paste0("figures/",country,"_covid19_infektionen.png")
MODEL_FILENAME <- paste0("figures/",country,"_covid19_predictions.png")
COMPARISON_AT_IT_FILENAME <- paste0("figures/",country,"_vergleich_at_it.png")
INFECTED_TEST_RATIO_FILENAME <- paste0("figures/",country,"_covid19_infektionen_tests_ratio.png")
NMB_TESTS_FILENAME <- paste0("figures/",country,"_covid19_anzahl_tests.png")
COUNTRY_COMPARISON_FILENAME<-paste0("figures/",country,"_covid19_vergleich_laender.png")
PREDICTIONS_FILENAME<-paste0("figures/",country,"_covid19_predictions_comparison.png")
GROWTH_RATE_FILENAME<-paste0("figures/",country,"_covid19_growth_rate.png")
DOUBLING_FILENAME<-paste0("figures/",country,"_covid19_doubling.png")
FILE_NAME_LOG_PLOT <-paste0("figures/",country,"_covid19_log.png")
PREDICTION_QUALITY_FILENAME <- paste0("figures/",country,"_covid19_prediction_quality.png")

#x<-Sys.getlocale()
Sys.setlocale("LC_ALL", "Portuguese")

db_international <- download_international_cases()

countries<-c("China",
  "United States",
  #"United Kingdom",
  "Italy",
  "Brazil",
  #"Korea, Rep.",
  #"South Africa",
  "Japan",
  "Spain")

authentification <- feather("authentification")


plot_prediction_combined(db_international,
                         db_international,
                         Sys.Date() + 8,
                         region1 = "Brazil",
                         region2 = "United States",
                         delay = 8,
                         polynom = 17,
                         exp = FALSE,
                         limDate = Sys.Date() - 16,
                         colors = COLORS[c(1, 7, 9)],
                         log_scale = FALSE,
                         "_Brazil_",
                         "br")

tweet1<-updateStatus(text = "#COVID19 #Covid19brasil Atualição de dados Brasil. Projeção com modelo polinomial e com dados dos Estados Unidos 1/7",
                     mediaPath = PREDICTIONS_FILENAME
)


plot_growth_data(db_international,
                 "Brazil",
                 avg= 7,
                 "br")

tweet2<-updateStatus(text = "#COVID19 #Covid19brasil Atualição de dados Brasil. Taxa de crescimento de indivíduos com teste positivo 2/7",
                     mediaPath = GROWTH_RATE_FILENAME,
                     inReplyTo=tweet1$id
)


plot_doubling_time(db_international,
                   "Brazil",
                   avg = 7,
                   "br")


tweet3<-updateStatus(text = "#COVID19 #Covid19brasil Atualição de dados Brasil. Tempo até duplicação dos indivíduos com teste positivo. 3/7",
                     mediaPath = DOUBLING_FILENAME,
                     inReplyTo=tweet2$id)


log_plot(db_international,
         "Infected",
         countries,
         lab = "Numéro de indivíduos com teste positivo \n(% da população, escala logarítimico)",
         "br")

tweet4<-updateStatus(text = "#COVID19 #Covid19brasil Atualição de dados Brasil.  Comparação de países, número de indivíduos com teste positivo.",
                     mediaPath = FILE_NAME_LOG_PLOT,
                     inReplyTo=tweet3$id)

log_plot(db_international,
         "Dead",
         countries,
         lab = "Numéro de indivíduos mortos \n(% da população, escala logarítimico)",
         "br")

tweet5<-updateStatus(text = "#COVID19 #Covid19brasil Atualição de dados Brasil. Comparação de países, número de indivíduos mortos.",
                     mediaPath = FILE_NAME_LOG_PLOT,
                     inReplyTo=tweet4$id)
tweet6<-updateStatus(text = "#COVID19 #RStats Code for analysis of Austrian and Brazilian infection data here. https://github.com/joph/Covid19-Austria",
                      inReplyTo=tweet5$id)










plot_overview(db_international,
              "Brazil",
              "br")


plot_prediction(db_international,
                Sys.Date() + 2,
                region = "Brazil")



prediction_quality(db_international,
                   0,
                   0,
                   "_Brazil_")



setup_twitter_oauth(consumer_key = authentification$consumer_key[1],
                    access_token = authentification$access_token[1],
                    consumer_secret = authentification$consumer_secret[1],
                    access_secret = authentification$access_secret[1])

