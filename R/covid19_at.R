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
WIKIPEDIA_URL_AT <- "https://de.wikipedia.org/wiki/COVID-19-Pandemie_in_%C3%96sterreich"

WIKIPEDIA_URL_IT <- "https://de.wikipedia.org/wiki/COVID-19-Epidemie_in_Italien"

OVERVIEW_FILENAME <- "figures/covid19_infektionen.png"
MODEL_FILENAME <- "figures/covid19_predictions.png"
COMPARISON_AT_IT_FILENAME <- "figures/vergleich_at_it.png"
INFECTED_TEST_RATIO_FILENAME <- "figures/covid19_infektionen_tests_ratio.png"
NMB_TESTS_FILENAME <- "figures/covid19_anzahl_tests.png"
COUNTRY_COMPARISON_FILENAME<-"figures/covid19_vergleich_laender.png"
PREDICTIONS_FILENAME<-"figures/covid19_predictions_comparison.png"
GROWTH_RATE_FILENAME<-"figures/covid19_growth_rate.png"
DOUBLING_FILENAME<-"figures/covid19_doubling.png"

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
    mutate(Cases_proportional = Cases/Population) %>%

    mutate(Date = as.POSIXct(paste0(Date, " 00:00:00"))) %>%
    dplyr::select(Region,
                  Date,
                  Population,
                  Type,
                  Cases,
                  Cases_proportional)

    return(all_data)
}

plot_country_comparison<-function(db_,
                             countries){

  max_date <- db_ %>%
    filter(Region %in% countries) %>%
    filter(Type == "Infected") %>%
    summarize(Max_Date = max(Date)) %>%
    unlist()

  p <- db_ %>%
    filter(Region %in% countries) %>%
    filter(Type == "Infected") %>%
    ggplot(aes(x = Date, y = Cases_proportional * 100)) +
    geom_line(aes(col = Region), size = 1) +
    ylab("Positiv getestete Individuen (% der Bevölkerung)") +
    scale_color_manual(values = COLORS) +
    labs(caption = paste("Quelle: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/", ""))


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
                                "Zuwachs",
                                "Zuwachs1")


  wikipedia_table_1 <- rvest::html_table(webpage, dec = ",", fill = TRUE)[[3]]

  wikipedia_table_clean$`Testungen kumuliert`<-tibble(t=(c((wikipedia_table_1$`Testungen aufsummiert`))))$t


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


  wikipedia_table_conv <- wikipedia_table_conv %>%
    mutate(Region = "Austria") %>%
    mutate(Population = INHABITANTS_AUSTRIA) %>%
    dplyr::select(Region,
                  Date = Datum,
                  Dead = `Todesf?lle kumuliert`,
                  Infected = `Infektionen kumuliert`,
                  Recovered = `Genesen kumuliert`,
                  Nmb_Tested = `Testungen kumuliert`,
                  Population) %>%
    gather(Type,
           Cases,
           -Region,
           -Date,
           -Population) %>%
    mutate(Cases_proportional = Cases / INHABITANTS_AUSTRIA)



  return(wikipedia_table_conv)

}


get_infected<-function(db,
               region){
  db <- db %>%
    filter(Region == region) %>%
    filter(Type == "Infected")

  return(db)
}

get_growth<-function(db){
  growth<-db %>%
    # first sort by year
    arrange(Date) %>%
    mutate(Selector = ((n()-1):0)%%5) %>%
    filter(Selector == 0) %>%
    mutate(Diff_date = as.numeric(Date - lag(Date)),  # Difference in time (just in case there are gaps)
           Diff_growth = Cases - lag(Cases),
           Lag_cases = lag(Cases),
           Rate_percent = 100 * ((Cases /Lag_cases)^(1/Diff_date) - 1))   %>%
    dplyr::select(Date,
                  Diff_date,
                  Diff_growth,
                  Rate_percent,
                  Lag_cases)

  return(growth)
}

plot_growth_data<-function(db,
                           region = "Austria"){

  db<-get_infected(db,
                  region)

  growth<-get_growth(db)

  p <- growth %>%
    ggplot(aes(x = Date, y = Rate_percent)) +
    geom_bar(stat = "identity", fill = COLORS[1]) +
    ylab("Tägliche Wachstumsrate der\n positiv getesteten Individuen\n
         (5-Tagesdurchschnitt in %)") +
    labs(caption = paste("Quelle: Wikipedia. Letzter Datenpunkt:",
                         get_last_date(db)))




  ggsave(GROWTH_RATE_FILENAME,
         p,
         width = 10,
         height = 5)
  p

}

plot_doubling_time<-function(db,
                             region = "Austria"){

  db<-get_infected(db,
                  region)

  growth <- get_growth(db) %>%
  mutate(Doubling = log(2) /log(1+Rate_percent/100))


p<-growth %>%
  ggplot(aes(x = Date, y = Doubling)) +
  geom_bar(stat = "identity", fill = COLORS[1]) +
  ylab("Verdopplungszeit der positiv getesteten Individuen\n(5-Tagesdurchschnitt in Tagen)") +
  labs(caption = paste("Quelle: Wikipedia. Letzter Datenpunkt:", get_last_date(db)))


ggsave(DOUBLING_FILENAME,
       p,
       width = 10,
       height = 5)

p

}

manual_data_entry<-function(db,
                  date,
                  cases_infected_cum,
                  cases_recovered_cum,
                  cases_dead_cum,
                  tests,
                  region = "Austria") {


  population<-wb(indicator = "SP.POP.TOTL") %>%
    filter(date == 2018) %>%
    dplyr::select(country,
                  value) %>%
    filter(country == region)


  new_entry <- tibble(
    Region = rep(region, 4),
    Date = rep(date, 4),
    Population = rep(population$value, 4),
    Type = c("Infected",
             "Recovered",
             "Dead",
             "Nmb_Tested"),
    Cases = c(cases_infected_cum,
              cases_recovered_cum,
              cases_dead_cum,
              tests)
  ) %>%
    mutate(Cases_proportional = Cases / Population)

  db<-db %>%
    bind_rows(new_entry)

  return(db)

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
    ylab("Positiv getestete Individuen (% Gesamtpopulation)")

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
get_last_date<-function(db){

  return(db$Date[nrow(db)])

}

#' Get date of first available data point in db
#'
#'
#' @param wikipedia_table Data to be checked
#' @return The date of the first data point
#'
#' @examples
#' get_first_date(scrape_wikipedia_at()[[1]])
get_first_date<-function(db){

  return(db$Date[1])

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
plot_overview<-function(db,
                           region = "Austria") {

  db<-db %>%
      filter(Region == region) %>%
      filter(Type %in% c("Infected",
                         "Recovered",
                         "Dead"))

  #### plot overview data
  p <- db %>%
    ggplot(aes(x = Date, y = Cases)) +
    geom_point(aes(col = Type)) +
    geom_line(aes(col = Type),
            size=1) +
    scale_color_manual(values = COLORS[c(1,5, 10)]) +
    ylab("Wert (Individuen)") +
    labs(caption = paste("Quelle: Wikipedia. Letzter Datenpunkt:", get_last_date(db)))

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
  mod<-lm(Infektionen_trans ~ Date, data = wikipedia_table_short)

  predictions<-predict(mod, week_ahead)

  df<-data.frame(Date = week_ahead,
                 fitted = predictions,
                 Model_Name = model_name)


  return(df)

}



plot_prediction_combined<-function(db,
                                   db_international,
                                   final_date,
                                   polynom,
                                   region1 = "Austria",
                                   region2 = "Italy",
                                   delay = 8,
                                   exp = TRUE){

  week_ahead <- data.frame(Date = c(db$Date %>% unique(),
                                     seq(as.POSIXct(get_last_date(db) + 3600*24),
                                         as.POSIXct(final_date) + 3600 * 16,
                                         3600*24)))
  inhabitants1 <- db %>%
    filter(Region == region1) %>%
    summarize(p = max(Population)) %>%
    unlist()

  inhabitants2 <- db_international %>%
    filter(Region == region2) %>%
    summarize(p = max(Population)) %>%
    unlist()

  db <- get_infected(db,
                     region1)

  db2 <- get_infected(db_international,
                      region2)

  base_data <- db %>%
    dplyr::select(Date,
                  fitted = Cases
                  ) %>%
    mutate(Model_Name = "Beobachtungen",
           Size = 2)

  db_exp <- db %>%
    filter(Cases > 0) %>%
    mutate(Infektionen_trans = log(Cases))

  db_poly <- db %>%
    mutate(Infektionen_trans = (Cases)^(1/polynom))


  model_name_region <- paste0(region2, " (",delay," Tage nach hinten versetzt)")

  db2<-db2 %>%
    mutate(Date = Date + delay*3600*24) %>%
    mutate(fitted = (Cases * inhabitants1 / inhabitants2)) %>%
    mutate(Model_Name = model_name_region,
           Size = 1) %>%
    dplyr::select(Date,
                  fitted,
                  Model_Name,
                  Size)



  results <- generic_mod(db_exp,
                         0,
                         week_ahead,
                         "Exponentielles Modell") %>%
    mutate(fitted = exp(fitted),
           Size = 1) %>%
    bind_rows(generic_mod(
      db_poly,
      0,
      week_ahead,
      paste0("Polynomielles Modell\n Grad = ", polynom)
    ) %>%
      mutate(Date = as.POSIXct(Date)) %>%
      mutate(fitted = fitted^polynom,
             Size = 1)) %>%
    bind_rows(db2) %>%
    bind_rows(base_data)

  results$Model_Name <- factor(results$Model_Name)

  results$Model_Name <- factor(results$Model_Name, levels = rev(levels(results$Model_Name)))

  estimates_ahead <- results %>% group_by(Model_Name) %>%
    summarize(res = max(fitted))

  estimate_2 <- results %>%
    filter(Model_Name == model_name_region) %>%
    dplyr::select(fitted) %>% unlist()
  estimate_2 <- estimate_2[nrow(db2)]

  results %>%
    dplyr::select(-Size) %>%
    spread(Model_Name,fitted) %>%
      write_delim(paste0("data/prediction", Sys.Date(), ".csv"), delim=";")

  if(!exp){
    results <- results %>%
      filter(Model_Name != "Exponentielles Modell")
  }

  p <- results %>%
         ggplot(aes(x = Date, y = fitted)) +
         geom_line(aes(col = Model_Name), size = 1) +
         geom_point(aes(col = Model_Name), size = 2) +
         scale_color_manual(values = COLORS) +
         ylab("Anzahl positiv getesteter Individuen") +
    labs(caption = paste("Quelle: Wikipedia. Letzer Datenpunkt:",
                         get_last_date(db_at),
                         "\nVorhersage positiv getesteter Individuen am ", final_date, "\nPolynomielles Modell: ",
                         round(estimates_ahead$res[1]),
                         "\n",
                         model_name_region,
                         round(estimate_2),
                         "\nExponentielles Modell: ",
                         round(estimates_ahead$res[3])
                         ))

  ggsave(PREDICTIONS_FILENAME,
         p,
         width = 10,
         height = 5)

  p




}

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
plot_prediction<-function(db,
                          final_date,
                          steps = 3,
                          transformation_f = log,
                          transformation_f_inverse = exp,
                          add_string =  " (exponentielles Modell M1): ",
                          region = "Austria"){

  db<-get_infected(db,
                   region)

  db_transform <- db %>%
    filter(Cases > 0) %>%
    mutate(Infektionen_trans = transformation_f(Cases))

  week_ahead<-data.frame(Date = c(db$Date %>% unique(),
                                   seq(as.POSIXct(get_last_date(db) + 3600*24),
                                       as.POSIXct(final_date) + 3600 * 16,
                                       3600*24)))

  results <- mapply(generic_mod,
         list(db_transform),
         0:steps,
         list(week_ahead),
         paste0("M",0:steps),
         SIMPLIFY = FALSE)

  res<-bind_rows(results)

  forecast<-bind_cols(data.frame(Date = db_transform$Date,
                                 Infektionen_trans = (db_transform$Infektionen_trans)),
          data.frame(fitted=rep(NA, nrow(db_transform)), Model_Name = "M")) %>%
          bind_rows(res) %>%
        mutate(Infected = transformation_f_inverse(Infektionen_trans),
         Prediction = transformation_f_inverse(fitted)) %>%
    dplyr::select(Date, Infected, Prediction, Model_Name) %>%
    gather(Variable, Value, -Date, -Model_Name)

  prediction_week_ahead <- forecast %>%
    filter(Variable == "Prediction" & Model_Name == "M0") %>%
    dplyr::select(Value) %>%
    tail(1) %>%
    unlist()

  forecast %>%
    write_feather(path = paste0("data/prediction", Sys.Date()))

  p <- forecast %>%
    ggplot(aes(x = Date, y = Value)) +
    geom_point() +
    geom_line(aes(col = Model_Name, linetype = Variable), size = 1.1) +
    scale_color_manual(values = COLORS) +
    ylab("Infektionen (Faelle)") +
    labs(caption = paste("Quelle: Wikipedia. Letzer Datenpunkt:",
                         get_last_date(db),
                       "\nVorhersage positiv getestete Individuen am ", final_date, add_string,
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

plot_infected_tests_ratio<-function(db,
                                    region = "Austria"){

  db<-db %>%
    filter(Region == region & Type %in% c("Nmb_Tested", "Infected")) %>%
    arrange(Date) %>%
    dplyr::select(Date, Type, Cases) %>%
    spread(Type, Cases)


  first_value_tests <- db$Nmb_Tested[1]
  first_value_infected <- db$Infected[1]


  p <- db %>%
    mutate(Tests_Ind = c(first_value_tests, diff(Nmb_Tested))) %>%
    mutate(Infected_Ind = c(first_value_infected, diff(Infected))) %>%
    dplyr::select(Date, Nmb_Tested, Tests_Ind, Infected_Ind) %>%
      mutate(Prop_Infected_Tests = 100 * Infected_Ind/Tests_Ind) %>%
    ggplot(aes(x = Date, y = Prop_Infected_Tests)) +
    geom_point(col = COLORS[1]) +
    geom_line(col = COLORS[1], size = 1) +
    ylab("Verhaeltnis Covid-19 Infektionen zu Tests (%)") +
    labs(caption = paste("Quelle: Wikipedia. Letzter Datenpunkt:", get_last_date(db)))


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

plot_number_tests<-function(db,
                            region = "Austria"){

  db<-db %>%
    filter(Region == region & Type %in% c("Nmb_Tested", "Infected")) %>%
    arrange(Date) %>%
    dplyr::select(Date,
                  Type,
                  Cases) %>%
    spread(Type,
           Cases)


  first_value_tests <- db$Nmb_Tested[1]

  p <- db %>%
    mutate(Tests_Ind = c(first_value_tests, diff(Nmb_Tested))) %>%
    ggplot(aes(x = Date, y = Tests_Ind)) +
    geom_point(col = COLORS[1]) +
    geom_line(col = COLORS[1], size = 1) +
    ylab("Verhaeltnis Covid-19 Infektionen zu Tests (%)") +
    labs(caption = paste("Quelle: Wikipedia. Letzter Datenpunkt:", get_last_date(db)))


  ggsave(NMB_TESTS_FILENAME,
         p,
         width = 10,
         height = 5)

  p
}
