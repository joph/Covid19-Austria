COLORS <- c("#c72321",
           "#861719",
           "#fbd7a9",
           "#ba9f7c",
           "#7a6952",
           "#6e9b9e",
           "#0d8085",
           "#19484c",
           "#f0c320",
           "#af8f19")



### wikipedia url
WIKIPEDIA_URL_AT <- "https://de.wikipedia.org/wiki/COVID-19-F%C3%A4lle_in_%C3%96sterreich"

WIKIPEDIA_URL_IT <- "https://de.wikipedia.org/wiki/COVID-19-Epidemie_in_Italien"

OVERVIEW_FILENAME <- "figures/covid19_infektionen.png"
MODEL_FILENAME <- "figures/covid19_predictions.png"
COMPARISON_AT_IT_FILENAME <- "figures/vergleich_at_it.png"
INFECTED_TEST_RATIO_FILENAME <- "figures/covid19_infektionen_tests_ratio.png"
NMB_TESTS_FILENAME <- "figures/covid19_anzahl_tests.png"
COUNTRY_COMPARISON_FILENAME<-"figures/covid19_vergleich_laender.png"

### source eurostat
INHABITANTS_ITALY <- 60.48*10^6

### source eurostat
INHABITANTS_AUSTRIA <- 8.82*10^6

download_international_cases<-function(){

  confirmed<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  dead<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
  recovered<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

  download.file(confirmed,
                "data/global_confirmed.csv")

  download.file(dead,
                "data/global_dead.csv")

  download.file(recovered,
                "data/global_recovered.csv")

  c<-read_csv("data/global_confirmed.csv") %>%
    gather(Date,
           Cases,
           -`Province/State`,
           -`Country/Region`,
           -Lat,
           -Long) %>%
    mutate(Type = "Infected")

  d<-read_csv("data/global_dead.csv") %>%
    gather(Date,
           Cases,
           -`Province/State`,
           -`Country/Region`,
           -Lat,
           -Long) %>%
    mutate(Type = "Dead")

  r<-read_csv("data/global_recovered.csv") %>%
    gather(Date,
           Cases,
           -`Province/State`,
           -`Country/Region`,
           -Lat,
           -Long) %>%
    mutate(Type = "Recovered")

  all_data <- bind_rows(c, d, r) %>%
    mutate(`Province/State` = ifelse(is.na(`Province/State`), "", `Province/State`)) %>%
    mutate(Date = mdy(Date)) %>%
    mutate(Region = paste(`Country/Region`)) %>%
    group_by(Region, Date, Type) %>%
    summarize(Cases = sum(Cases)) %>%
    dplyr::select(Region, Date, Type, Cases) %>%
    ungroup() %>%
    mutate(Region = ifelse(Region == "US", "United States", Region))

  population<-wb(indicator = "SP.POP.TOTL") %>%
    filter(date == 2018) %>%
    dplyr::select(country,
                  value)

  all_data<-full_join(all_data,
            population,
            by=c("Region" = "country")) %>%
    na.omit() %>%
    mutate(Population = value) %>%
    dplyr::select(-value) %>%
    mutate(Cases_proportional = Cases/Population)

    return(all_data)
}

plot_country_comparison<-function(db,
                             countries){

  p <- db %>%
    filter(Region %in% countries) %>%
    filter(Type == "Infected") %>%
    ggplot(aes(x = Date, y = Cases_proportional * 100)) +
    geom_line(aes(col = Region), size = 1) +
    ylab("Infektionen (% der Bevölkerung)") +
    scale_color_manual(values = COLORS)

  ggsave(COUNTRY_COMPARISON_FILENAME,
         p)

  p

}

#' Download, clean and save data on infections in Austria from wikipedia
#'
#' Downloads the data and saves a feather file with the data in data/data.feather
#'
#' @param url Url to the wikipedia page. Is preconfigured and may only be changed if wikipedia URL changed.
#' @return A list of two tibbles: the first elment contains a tibble with up-to-date data, the second element contains a tibble with the last data set loaded from disk.
#'                                Both tibbles have the same format and include all information from the wikipedia table.
#' @examples
#' scrape_wikipedia_at()
scrape_wikipedia_at<-function(wikipedia_url = WIKIPEDIA_URL_AT){

  if(!dir.exists("figures")){
    dir.create("figures")
  }

  if(!dir.exists("data")){
    dir.create("data")
  }


  webpage <- xml2::read_html(wikipedia_url)

  wikipedia_table <- rvest::html_table(webpage, fill = TRUE)[[2]]

  wikipedia_table_clean <- wikipedia_table[-1,]

  names(wikipedia_table_clean)<-c("Datum",
                                "NOE",	"W",	"St",	"T",	"O?",	"S",	"B",	"V",	"K",
                                "Infektionen kumuliert",
                                "Genesen kumuliert",
                                "Aktuell Infizierte",
                                "Todesf?lle kumuliert",
                                "Neuinfektionen",
                                "Testungen kumuliert",
                                "Zuwachs")

  wikipedia_table_clean <- wikipedia_table_clean %>%
    mutate(Datum = str_replace(Datum, "[\\(]","")) %>%
    mutate(Datum = str_replace(Datum, "[\\)]","")) %>%
    mutate(Datum = str_replace(Datum, "\\.02\\.", ".02. 08:00")) %>%
    mutate(Datum = str_replace(Datum, "01\\.03\\.", "01.03. 08:00")) %>%
    mutate(Datum = str_replace(Datum, "02\\.03\\.", "02.03. 08:00")) %>%
    mutate(Datum = str_replace(Datum, "\\. ", ".2020 ")) %>%
    mutate(Datum = as.POSIXct(Datum, format = "%d.%m.%Y %H:%M"))

  wikipedia_table_clean <- wikipedia_table_clean %>%
    mutate(`Testungen kumuliert` = str_replace(`Testungen kumuliert`, "\\.", "")) %>%
    mutate(`Testungen kumuliert` = str_replace(`Testungen kumuliert`, "\\[.*\\]", "")) %>%
    mutate() %>%
    as_tibble()


  wikipedia_table_conv <- wikipedia_table_clean

  wikipedia_table_conv[ , -1] <-
    lapply(wikipedia_table_conv[, -1],
         as.numeric)


  wikipedia_table_conv_old <- NULL

  if(file.exists("data/data.feather")){
    wikipedia_table_conv_old <- feather("data/data.feather")
  }

  write_feather(wikipedia_table_conv, "data/data.feather")


  return(list(wikipedia_table_conv,
              wikipedia_table_conv_old))

}

#' Download, clean and save data on infections in Italy from wikipedia
#'
#' @param url Url to the wikipedia page. Is preconfigured and may only be changed if wikipedia URL changed.
#' @return A tibble with infection data for Italy
#'
#' @examples
#' scrape_wikipedia_it()
scrape_wikipedia_it<-function(wikipedia_url = WIKIPEDIA_URL_IT){

  if(!dir.exists("figures")){
    dir.create("figures")
  }

  if(!dir.exists("data")){
    dir.create("data")
  }


  webpage <- xml2::read_html(wikipedia_url)

  wikipedia_table <- rvest::html_table(webpage, fill = TRUE)[[2]]

  wikipedia_table_clean <- wikipedia_table %>%
    mutate(Datum = str_replace(Datum, "[\\(]","")) %>%
    mutate(Datum = str_replace(Datum, "[\\)]","")) %>%
    mutate(Datum = str_replace(Datum, "\\.20", ".2020 "))   %>%
    mutate(Datum = as.POSIXct(Datum, format = "%d.%m.%Y %H:%M")) %>%
    filter(!is.na(Datum)) %>%
    mutate(Infektionen = `Infektionen (kumuliert)`)

  #wikipedia_table_clean <- wikipedia_table_clean %>%
  #  mutate(`Testungen kumuliert` = str_replace(`Testungen kumuliert`, "\\.", "")) %>%
  #  mutate(`Testungen kumuliert` = str_replace(`Testungen kumuliert`, "\\[.*\\]", "")) %>%
  #  mutate() %>%
  #  as_tibble()

  return(wikipedia_table_clean)

}



#' Compares Austrian and Italian infection data
#'
#' The function creates a plot comparing Austrian and Italian infection data.
#' It saves the plot to COMPARISON_AT_IT_FILENAME
#'
#' @param days_shift How many days the Austrian data is shifted
#' @return
#'
#' @examples
#' plot_compare_at_it()
plot_compare_at_it<-function(days_shift = 8){


  wiki_at <- covid19at::scrape_wikipedia_at()[[1]]
  wiki_it <- covid19at::scrape_wikipedia_it()

  wiki_at <- wiki_at %>%
    mutate(Infektionen = `Infektionen kumuliert` / INHABITANTS_AUSTRIA) %>%
    mutate(Country = paste0("Österreich \n (", days_shift, " Tage nach vorne versetzt)")) %>%
    dplyr::select(Datum, Country, Infektionen) %>%
    mutate(Datum = Datum - 24*3600*days_shift)

  wiki_it <- wiki_it %>%
    mutate(Infektionen = Infektionen / INHABITANTS_ITALY) %>%
    mutate(Country = "Italien") %>%
    dplyr::select(Datum, Country, Infektionen)

  #wiki_it <- wiki_it[-1,]

  merged <- bind_rows(wiki_at,
                      wiki_it)

  p<-merged %>%
    ggplot(aes(x = Datum, y = Infektionen * 100)) +
    geom_line(aes(col = Country), size = 1) +
    geom_point(aes(col = Country)) +
    scale_color_manual(values = COLORS[c(1,6)]) +
    ylab("Infektionen (% Gesamtpopulation)")

  ggsave(COMPARISON_AT_IT_FILENAME,
         p,
         width = 10,
         height = 5)

  p

}


#' Get date of last available data point in db
#'
#'
#' @param wikipedia_table Data to be checked
#' @return The date of the last data point
#'
#' @examples
#' get_last_date(scrape_wikipedia_at()[[1]])
get_last_date<-function(wikipedia_table){

  return(wikipedia_table$Datum[nrow(wikipedia_table)])

}

#' Get date of first available data point in db
#'
#'
#' @param wikipedia_table Data to be checked
#' @return The date of the first data point
#'
#' @examples
#' get_first_date(scrape_wikipedia_at()[[1]])
get_first_date<-function(wikipedia_table){

  return(wikipedia_table$Datum[1])

}





#' Plots an overview of Austrian infection data
#'
#' Plots figures and saves it to OVERVIEW_FILENAME
#'
#'
#' @param wikipedia_table Austrian infection data to be used
#' @return
#'
#' @examples
#' plot_overiew_at(scrape_wikipedia_at()[[1]])
plot_overview_at<-function(wikipedia_table){
  #### plot overview data
  p <- wikipedia_table %>%
    gather(Variable, Value, -Datum) %>%
    filter(Variable %in% c("Infektionen kumuliert",
                         "Genesen kumuliert",
                         "Aktuell Infizierte")) %>%
    ggplot(aes(x = Datum, y = Value)) +
    geom_point(aes(col = Variable)) +
    geom_line(aes(col = Variable),
            size=1) +
    scale_color_manual(values = COLORS[c(1,5, 10)]) +
    ylab("Wert (Individuen)") +
    labs(caption = paste("Quelle: Wikipedia. Letzter Datenpunkt:", get_last_date(wikipedia_table)))

  ggsave(OVERVIEW_FILENAME,
         p,
         width = 10,
         height = 5)

  p

}

#' Fits a generic linear model to input data and projects future infections from the model
#'
#' The function fits a generic linear model between Datum and Infektionen_trans.
#' It allows to reduce the number of input data points
#'
#' @param wikipedia_table_in Table to be fitted. Has to contain columns Datum and Infektionen_trans
#' @param minus_n How many days should be removed from dataset (starting from the latest date)
#' @param week_ahead Period for which should be predicted. Has to be a vector of POSIXct dates.
#' @param model_name A string indicating the name of the model
#' @return A data.frame with Dates,
#'
generic_mod<-function(wikipedia_table_in,
              minus_n,
              week_ahead,
              model_name){

  wikipedia_table_short<-wikipedia_table_in[1:(nrow(wikipedia_table_in) - minus_n),]
  mod<-lm(Infektionen_trans ~ Datum, data = wikipedia_table_short)

  predictions<-predict(mod, week_ahead)

  df<-data.frame(Datum = week_ahead,
                 fitted = predictions, Model_Name = model_name)


  return(df)

}



INFECTED_TEST_RATIO_FILENAME
NMB_TESTS_FILENAME


#' Creates a plot showing a model fit to the data. The model can be chosen arbitrarily
#'
#' The function fits a generic linear model between Datum and Infektionen
#' Before fitting, Infektionen are transformed according to the given transformation functions.
#' The model is fitted for different amounts of data points.
#' The models are plotted together with data points and the resulting plot is saved to MODEL_FILENAME
#'
#' @param wikipedia_table Data to be fitted. Has to contain columns Datum and Infektionen
#' @param final_date Up to which date should be predicted (POSIXct date)
#' @param steps How many models should be fitted by reducing the amount of data points.
#'              I.e. 3 indicates that one model with all data points, one model with all
#'              data points minus the last one and one model with all data points minus
#'              the two last ones is fitted.
#' @param transformation_f Function that transforms input data
#' @param transformation_f_inverse Inverse of trnasformation_f. This function indicates the kind of model. I.e. if using exp here, we estimate an exponential model.
#' @return
#'
#' @example
#' plot_prediction(scrape_wikipedia_at()[[1]], Sys.Date() + 3, 3)
#'
plot_prediction<-function(wikipedia_table,
                          final_date,
                          steps = 3,
                          transformation_f = log,
                          transformation_f_inverse = exp){


  wikipedia_table_transform <- wikipedia_table %>%
    mutate(Infektionen_trans = transformation_f(`Infektionen kumuliert`))

  week_ahead<-data.frame(Datum = c(wikipedia_table_transform$Datum,
                                   seq(as.POSIXct(get_last_date(wikipedia_table) + 3600*24),
                                       as.POSIXct(final_date) + 3600 * 16,
                                       3600*24)))

  results <- mapply(generic_mod,
         list(wikipedia_table_transform),
         1:steps,
         list(week_ahead),
         paste0("M",1:steps),
         SIMPLIFY = FALSE)

  res<-bind_rows(results)

  forecast<-bind_cols(data.frame(Datum = wikipedia_table_transform$Datum,
                                 Infektionen_trans = (wikipedia_table_transform$Infektionen_trans)),
          data.frame(fitted=rep(NA, nrow(wikipedia_table_transform)), Model_Name = "M")) %>%
          bind_rows(res) %>%
        mutate(Infektionen = transformation_f_inverse(Infektionen_trans),
         Prediction = transformation_f_inverse(fitted)) %>%
    dplyr::select(Datum, Infektionen, Prediction, Model_Name) %>%
    gather(Variable, Value, -Datum, -Model_Name)

  prediction_week_ahead <- forecast %>%
    filter(Variable == "Prediction" & Model_Name == "M1") %>%
    dplyr::select(Value) %>%
    tail(1) %>%
    unlist()

  forecast %>%
    write_feather(path = paste0("data/prediction", Sys.Date()))

  p <- forecast %>%
    ggplot(aes(x = Datum, y = Value)) +
    geom_point() +
    geom_line(aes(col = Model_Name, linetype = Variable), size = 1.1) +
    scale_color_manual(values = COLORS[c(1, 3, 5, 6, 8, 10)]) +
    ylab("Infektionen (Faelle)") +
    labs(caption = paste("Quelle: Wikipedia. Letzer Datenpunkt:",
                         get_last_date(wikipedia_table),
                       "\nVorhersage Infektionen am ", final_date," (exponentielles Modell M1): ",
                       round(prediction_week_ahead)))

  ggsave(MODEL_FILENAME,
         p,
         width = 10,
         height = 5)

  p
}


#' Plots timeseries of the infected:number of tests ratio.
#'
#' Plots figures and saves it to INFECTED_TEST_RATIO_FILENAME
#'
#'
#' @param wikipedia_table Austrian infection data to be used
#' @return
#'
#' @examples
#' plot_infected_tests_ratio(scrape_wikipedia_at()[[1]])

plot_infected_tests_ratio<-function(wikipedia_table){

  first_value_testungen <- wikipedia_table$`Testungen kumuliert`[1]


  p <- wikipedia_table %>%
    mutate(Testungen = c(first_value_testungen, diff(`Testungen kumuliert`))) %>%
    dplyr::select(Datum, `Testungen kumuliert`, Testungen, Neuinfektionen) %>%
      mutate(`Verh?ltnis Infektionen zu Tests` = 100 * Neuinfektionen/Testungen) %>%
    ggplot(aes(x = Datum, `Verh?ltnis Infektionen zu Tests`)) +
    geom_point(col = COLORS[1]) +
    geom_line(col = COLORS[1], size = 1) +
    ylab("Verhaeltnis Covid-19 Infektionen zu Tests (%)") +
    labs(caption = paste("Quelle: Wikipedia. Letzter Datenpunkt:", get_last_date(wikipedia_table)))


  ggsave(INFECTED_TEST_RATIO_FILENAME,
         p,
         width = 10,
         height = 5)

  p

}



#' Plots timeseries of number of tests
#'
#' Plots figures and saves it to NMB_TESTS_FILENAME
#'
#'
#' @param wikipedia_table Austrian infection data to be used
#' @return
#'
#' @examples
#' plot_number_tests(scrape_wikipedia_at()[[1]])

plot_number_tests<-function(wikipedia_table){

  first_value_testungen <- wikipedia_table$`Testungen kumuliert`[1]

  p <- wikipedia_table %>%
    mutate(Testungen = c(first_value_testungen, diff(`Testungen kumuliert`))) %>%
    dplyr::select(Datum, `Testungen kumuliert`, Testungen, Neuinfektionen) %>%
    ggplot(aes(x = Datum, `Testungen`)) +
    geom_point(col = COLORS[1]) +
    geom_line(col = COLORS[1], size = 1) +
    ylab("Testungen") +
    labs(caption = paste("Quelle: Wikipedia. Letzter Datenpunkt:", get_last_date(wikipedia_table)))

  ggsave(NMB_TESTS_FILENAME,
         p,
         width = 10,
         height = 5)

  p
}


#### tweet results

tweet_results <- function(wikipedia_table_conv, wikipedia_table_conv_old, tweet_always = FALSE){

  tweet<-tweet_always

  if(is.null(wikipedia_table_conv_old)){
    tweet<-TRUE
  }else{

    last_date<-get_last_date(wikipedia_table_conv)
    last_date_old<-get_last_date(wikipedia_table_conv_old)
    if(last_date>last_date_old){

      tweet<-TRUE

      }
  }

  if(tweet){
    authentification <- feather("authentification")

    setup_twitter_oauth(consumer_key = authentification$consumer_key[1],
                    access_token = authentification$access_token[1],
                    consumer_secret = authentification$consumer_secret[1],
                    access_secret = authentification$access_secret[1])


    tweet1<-updateStatus(text = "#COVID_19 Vorhersage mit exponentiellem Modell: M1: alle Datenpunkte, M2: alle Datenpunkte bis gestern. M3: alle Datenpunkte bis vorgestern.",
                       mediaPath = MODEL_FILENAME
                        )

    tweet2<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Vergleich Infektionen Italien und Österreich (8 Tage nach vorne versetzt): ",
                         mediaPath = COMPARISON_AT_IT_FILENAME,
                         inReplyTo=tweet1$id
    )


    tweet3<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Neuinfektionen: ",
                         mediaPath = OVERVIEW_FILENAME,
                         inReplyTo=tweet2$id
    )

    tweet4<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Verhaeltnis Infektionen:Tests ",
                     mediaPath = INFECTED_TEST_RATIO_FILENAME,
                     inReplyTo=tweet3$id)


    tweet5<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Gesamtanzahl Tests",
                     mediaPath = NMB_TESTS_FILENAME,
                     inReplyTo=tweet4$id)

    tweet6<-updateStatus(text = "#COVID_19 Data Update. Vergleich Laender. Datenpunkte bis zum Vortag.",
                         mediaPath = COUNTRY_COMPARISON_FILENAME,
                         inReplyTo=tweet5$id)


    tweet7<-updateStatus(text = "#COVID_19 #R-Stats Code for analysis of Austrian infection data here: https://github.com/joph/Covid19-Austria",
                         inReplyTo=tweet6$id)



  }


}
