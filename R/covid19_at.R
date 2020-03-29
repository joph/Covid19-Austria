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

OVERVIEW_FILENAME <- "covid19_infektionen.png"
MODEL_FILENAME <- "covid19_predictions.png"
COMPARISON_AT_IT_FILENAME <- "vergleich_at_it.png"
INFECTED_TEST_RATIO_FILENAME <- "covid19_infektionen_tests_ratio.png"
NMB_TESTS_FILENAME <- "covid19_anzahl_tests.png"
COUNTRY_COMPARISON_FILENAME<-"covid19_vergleich_laender.png"
PREDICTIONS_FILENAME<-"covid19_predictions_comparison.png"
GROWTH_RATE_FILENAME<-"covid19_growth_rate.png"
DOUBLING_FILENAME<-"covid19_doubling.png"
FILE_NAME_LOG_PLOT <-"covid19_log.png"
PREDICTION_QUALITY_FILENAME <- "covid19_prediction_quality.png"

overwrite_filenames<-function(country){
  covid19at::OVERVIEW_FILENAME <- paste0("figures/",country,"_covid19_infektionen.png")
  covid19at::MODEL_FILENAME <- paste0("figures/",country,"_covid19_predictions.png")
  covid19at::COMPARISON_AT_IT_FILENAME <- paste0("figures/",country,"_vergleich_at_it.png")
  covid19at::INFECTED_TEST_RATIO_FILENAME <- paste0("figures/",country,"_covid19_infektionen_tests_ratio.png")
  covid19at::NMB_TESTS_FILENAME <- paste0("figures/",country,"_covid19_anzahl_tests.png")
  covid19at::COUNTRY_COMPARISON_FILENAME<-paste0("figures/",country,"_covid19_vergleich_laender.png")
  covid19at::PREDICTIONS_FILENAME<-paste0("figures/",country,"_covid19_predictions_comparison.png")
  covid19at::GROWTH_RATE_FILENAME<-paste0("figures/",country,"_covid19_growth_rate.png")
  covid19at::DOUBLING_FILENAME<-paste0("figures/",country,"_covid19_doubling.png")
  covid19at::FILE_NAME_LOG_PLOT <-paste0("figures/",country,"_covid19_log.png")
  covid19at::PREDICTION_QUALITY_FILENAME <- paste0("figures/",country,"_figures/covid19_prediction_quality.png")
}

### source eurostat
INHABITANTS_ITALY <- 60.48*10^6

### source eurostat
INHABITANTS_AUSTRIA <- 8.82*10^6

download_international_cases<-function(){
  confirmed<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  dead<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  recovered<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

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
    mutate(Region = ifelse(Region == "US", "United States", Region)) %>%
    mutate(Region = ifelse(Region == "Korea, South", "Korea, Rep.", Region))

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
                             countries,
                             filename_add = ""){

  max_date <- db_ %>%
    filter(Region %in% countries) %>%
    filter(Type == "Infected") %>%
    summarize(Max_Date = max(Date)) %>%
    unlist()

  p <- db_ %>%
    filter(Region %in% countries) %>%
    filter(Type == "Infected") %>%
    ggplot(aes(x = Date, y = Cases_proportional * 100)) +
    geom_line(aes(col = Region)) +
    ylab("Positiv getestete Individuen (% der Bevölkerung)") +
    scale_color_manual(values = COLORS) +
    labs(caption = paste("Quelle: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/", ""))


  ggsave(get_filename(COUNTRY_COMPARISON_FILENAME,
                      filename_add),
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
                                #"Aktuell Infizierte",
                                #"Genesen kumuliert",
                                "Todesf?lle kumuliert",
                                "Neuinfektionen",
                                "Zuwachs")


  wikipedia_table_1 <- rvest::html_table(webpage, dec = ",", fill = TRUE)[[3]]

  wikipedia_table_clean$`Testungen kumuliert`<-tibble(t=c(wikipedia_table_1$`Tests aufsummiert`))[1:nrow(wikipedia_table_clean),]$t

  wikipedia_table_2 <- rvest::html_table(webpage, dec = ",", fill = TRUE)[[4]]

  wikipedia_table_2 <- wikipedia_table_2[-1, ]
  names(wikipedia_table_2)[10]<-"Bundesweit"

  wikipedia_table_clean$`Genesen kumuliert`<-tibble(t=c(wikipedia_table_2$Bundesweit))[1:nrow(wikipedia_table_clean),]$t

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

scrape_wikipedia_at_states<-function(wikipedia_url = WIKIPEDIA_URL_AT){

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
                                  "B",	"K",	"NOE",	"OOE",	"S",	"St",	"T_",	"V",	"W",
                                  "Infektionen kumuliert",
                                  #"Aktuell Infizierte",
                                  #"Genesen kumuliert",
                                  "Todesf?lle kumuliert",
                                  "Neuinfektionen",
                                  "Zuwachs")

  wikipedia_table_clean <- wikipedia_table_clean %>%
    mutate(Datum = str_replace(Datum, "[\\(]","")) %>%
    mutate(Datum = str_replace(Datum, "[\\)]","")) %>%
    mutate(Datum = str_replace(Datum, "\\.02\\.", ".02. 08:00")) %>%
    mutate(Datum = str_replace(Datum, "01\\.03\\.", "01.03. 08:00")) %>%
    mutate(Datum = str_replace(Datum, "02\\.03\\.", "02.03. 08:00")) %>%
    mutate(Datum = str_replace(Datum, "\\. ", ".2020 ")) %>%
    mutate(Datum = as.POSIXct(Datum, format = "%d.%m.%Y %H:%M"))

  state_data <- wikipedia_table_clean %>%
    dplyr::select(Date = Datum,
                  Niederösterreich = NOE,
                  Wien = W,
                  Steiermark = St,
                  Tirol = T_,
                  Oberösterreich = OOE,
                  Salzburg = S,
                  Burgenland = B,
                  Vorarlberg = V,
                  Kärnten = K)

  state_data <- state_data %>%
    as_tibble() %>%
    gather(State, Infected, -Date) %>%
    mutate(Infected = as.numeric(Infected))

  return(state_data)

}

get_infected<-function(db,
                       region,
                       type = "Infected"){
  db <- db %>%
    filter(Region == region) %>%
    filter(Type == type)

  return(db)
}

get_growth<-function(db,
                     avg){
  growth<-db %>%
    # first sort by year
    arrange(Date) %>%
    mutate(Diff_date = as.numeric(Date - lag(Date, avg)),  # Difference in time (just in case there are gaps)
           Diff_growth = Cases - lag(Cases, avg),
           Lag_cases = lag(Cases, avg),
           Rate_percent = 100 * ((Cases /Lag_cases)^(1/Diff_date) - 1))   %>%
    dplyr::select(Date,
                  Diff_date,
                  Diff_growth,
                  Rate_percent,
                  Lag_cases)

  return(growth)
}

plot_growth_data<-function(db,
                           region = "Austria",
                           avg = 5,
                           language = "at",
                           filename_add = ""){

  db<-db %>% filter(Cases > 0)

  type_lng <- "Wachstumsrate Positiv \ngetestete Individuen"

  ylab_lng <- paste0("Tägliche Wachstumsrate\n",
         "(Geometrisches Mittel über ",
         avg,
         " Tage in %)")

  caption_lng <- "Quelle: Wikipedia. Letzter Datenpunkt:"

  date_lng <- "Datum"

  if(language == "br"){

    type_lng <- "Taxa de crescimento \ndos indivíduos com teste positivo"

    ylab_lng <- paste0("Taxa de crescimento diário\n",
                       "(Média geométrico de ",
                       avg,
                       " dias em %)")

    caption_lng <- "Fonte: Wikipedia. Último ponto de dados:"
    date_lng <- "Data"
  }

  db1<-get_infected(db,
                  region)

  growth1<-get_growth(db1,
                     avg) %>%
    mutate(Type = type_lng)

  db2<-db %>%
    filter(Region == region) %>%
    filter(Type == "Nmb_Tested")

  growth2<-NULL

  if(nrow(db2) > 0){
  growth2<-get_growth(db2,
                      avg) %>%
    mutate(Type = "Wachstumsrate Anzahl Tests")
  }

  growth<-bind_rows(growth1,
                    growth2)

  p <- growth %>%
    na.omit() %>%
    ggplot(aes(x = Date, y = Rate_percent)) +
    geom_line(aes(col = Type)) +
    scale_color_manual(values = COLORS) +
    ylab(ylab_lng) +
    labs(caption = paste(caption_lng,
                         get_last_date(db))) +
    ggtitle(region) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab(date_lng)




  ggsave(get_filename(GROWTH_RATE_FILENAME,
                       filename_add),
         p,
         width = 10,
         height = 5)
  p

}

log_plot<-function(db,
                   variable = "Infected",
                   regions = c("Austria"),
                   lab = "Positiv getestete Individuen \n(% der Bevölkerung, Logarithmische Skala)",
                   language = "at",
                   filename_add = "")  {

  db <- db %>%
    filter(Type == variable) %>%
    filter(Region %in% regions)

  source <- paste("Quelle: Wikipedia. Letzter Datenpunkt: " ,
                  get_last_date(db))

  xlab_caption <- "Datum"

  if(language == "br"){
    source <- paste("Fonte: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/ Último ponto de dados: " ,
                    get_last_date(db))
    xlab_caption <- "Data"
  }

  p <- db %>%
    ggplot(aes(x = Date, y = Cases_proportional)) +
    geom_line(aes(col = Region)) +
    labs(caption = source) +
    ylab(lab) +
    scale_y_log10() +
    scale_color_manual(values = COLORS) +
    scale_x_discrete(expand=c(0, 1000000)) +
    geom_dl(aes(label = Region, col = Region),
            method = list(dl.combine("last.points"),
                          cex = 0.8)) +
    xlab(xlab_caption)

  ggsave(get_filename(FILE_NAME_LOG_PLOT, filename_add),
         p)

  p




}

plot_doubling_time<-function(db,
                             region = "Austria",
                             avg = 5,
                             language = "at",
                             filename_add = ""){


  db<-db %>% filter(Cases > 0)

  ylab_lng <- paste0("Verdopplungszeit \n(Geometrisches Mittel über ",
                     avg,
                     " Tage in Tagen)")

  caption_lng <- "Quelle: Wikipedia. Letzter Datenpunkt:"

  date_lng <- "Datum"

  type_lng <- "Positiv getestete Individuen"

  if(language == "br"){

    ylab_lng <- paste0("Dias até indivíduos com \nteste positivo dobram\n",
                       "(Média geométrica de  ",
                       avg,
                       " dias em %)")

    caption_lng <- "Fonte: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/. Último ponto de dados:"
    date_lng <- "Data"

    type_lng <- "Indivíduos com teste positivo"

  }

  db1<-get_infected(db,
                  region)

  growth1<-get_growth(db1,
                      avg) %>%
    mutate(Type = type_lng) %>%
    mutate(Doubling = log(2) /log(1+Rate_percent/100))

  db2<-db %>%
    filter(Region == region) %>%
    filter(Type == "Nmb_Tested")

  growth2<-NULL

  if(nrow(db2) > 0){
    growth2<-get_growth(db2,
                        avg) %>%
      mutate(Type = "Anzahl Tests") %>%
      mutate(Doubling = log(2) /log(1+Rate_percent/100))
  }

  growth<-bind_rows(growth1,
                    growth2)




p<-growth %>%
  na.omit() %>%
  ggplot(aes(x = Date, y = Doubling)) +
  geom_line(aes(color = Type)) +
  scale_color_manual(values = COLORS) +
  ylab(ylab_lng) +
  labs(caption = paste(caption_lng, get_last_date(db))) +
  ggtitle(region) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab(date_lng)


ggsave(get_filename(DOUBLING_FILENAME,
                     filename_add),
       p,
       width = 10,
       height = 5)

p

}

manual_data_entry<-function(db,
                  date,
                  cases_infected_cum = NA,
                  cases_recovered_cum = NA,
                  cases_dead_cum =NA,
                  tests = NA,
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

#' Get date of last available data point in db
#'
#'
#' @param wikipedia_table Data to be checked
#' @return The date of the last data point
#'
#' @examples
#' get_last_date(scrape_wikipedia_at()[[1]])
get_last_date<-function(db){
  return(max(db$Date))

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
                        region = "Austria",
                        language = "at",
                        filename_add = "") {


  caption_lng <- "Quelle: Wikipedia. Letzter Datenpunkt:"

  ylab_lng <- "Individuen \n(logarithmische Skala)"

  date_lng <- "Datum"

  if(language == "br"){

    ylab_lng <- "Indíviduos\n(Escala logarítimica)"

    caption_lng <- "Fonte: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/. Último ponto de dados:"
    date_lng <- "Data"
  }

  db<-db %>%
      filter(Region == region) %>%
      filter(Type %in% c("Infected",
                         "Recovered",
                         "Dead"))

  #### plot overview data
  p <- db %>%
    ggplot(aes(x = Date, y = Cases)) +
    geom_point(aes(col = Type)) +
    geom_line(aes(col = Type)) +
    scale_color_manual(values = COLORS[c(1,5, 10)]) +
    ylab(ylab_lng) +
    labs(caption = paste(caption_lng, get_last_date(db))) +
    ggtitle(region) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_log10() +
    xlab(date_lng)

  ggsave(get_filename(OVERVIEW_FILENAME,
                       filename_add),
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
                                   exp = TRUE,
                                   limDate = Sys.Date() - 30,
                                   colors_in = COLORS,
                                   log_scale = FALSE,
                                   filename_add = "",
                                   language = "at",
                                   type = "Infected"){


  type_lng <- "Wachstumsrate Positiv \ngetestete Individuen"

  ylab_lng <- paste0("Anzahl positiv getesteter Individuen")

  caption_lng <- "Quelle: Wikipedia. Letzter Datenpunkt:"

  date_lng <- "Datum"

  model_name_region <- paste0(region2, " (",delay," Tage nach hinten versetzt)")

  polynom_lng <- "Polynomielles Modell\n Grad = "

  observations_lng <- "Beobachtungen"

  mod_exp_lng <- "Exponentielles Modell"

  if(language == "br"){

    type_lng <- "Número de indivíduos mortos"
    model_us <- "Estados Unidos "

    ylab_lng <- paste0("Número de indivíduos mortos")

    model_name_region <- paste0(region2, " (com atraso de ",delay," dias)")

    polynom_lng <- "Modelo polinomial \n Grau  = "

    caption_lng <- "Fonte: Wikipedia. Último ponto de dados:"
    date_lng <- "Data"

    observations_lng <- "Observações"

    mod_exp_lng <- "Modelo exponencial"
  }

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
                     region1,
                     type)

  db2 <- get_infected(db_international,
                      region2,
                      type)

  base_data <- db %>%
    dplyr::select(Date,
                  fitted = Cases
                  ) %>%
    mutate(Model_Name = observations_lng)

  db_exp <- db %>%
    filter(Cases > 0) %>%
    mutate(Infektionen_trans = log(Cases))

  db_poly <- db %>%
    mutate(Infektionen_trans = (Cases)^(1/polynom))




  db2<-db2 %>%
    mutate(Date = Date + delay*3600*24) %>%
    mutate(fitted = (Cases * inhabitants1 / inhabitants2)) %>%
    mutate(Model_Name = model_name_region) %>%
    mutate(Size = 1) %>%
    dplyr::select(Date,
                  fitted,
                  Model_Name,
                  Size)



  results <- generic_mod(db_exp,
                         0,
                         week_ahead,
                         mod_exp_lng) %>%
    mutate(fitted = exp(fitted),
           Size = 1) %>%
    bind_rows(generic_mod(
      db_poly,
      0,
      week_ahead,
      paste0(polynom_lng, polynom)
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
      write_delim(paste0("data/prediction", filename_add, Sys.Date(), ".csv"), delim=";")

  if(!exp){
    results <- results %>%
      filter(Model_Name != "Exponentielles Modell")
  }

  caption_lng <- paste("Quelle: Wikipedia. Letzer Datenpunkt:",
        get_last_date(db),
        "\nVorhersage positiv getesteter Individuen am ", final_date, "\nPolynomielles Modell: ",
        round(estimates_ahead$res[1]),
        "\n",
        model_name_region,
        round(estimate_2)
        #,
        #"\nExponentielles Modell: ",
        #round(estimates_ahead$res[3])
  )

  if(exp){
    caption_lng <- paste0(caption_lng,
           "\nExponentielles Modell: ",
           round(estimates_ahead$res[3]))

  }

  if(language == "br"){
   caption_lng <- paste("Fonte: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/ Último ponto de dados:",
          get_last_date(db),
          "\nProjeção de indivíduos mortos no ", final_date, "\nModelo polinomial: ",
          round(estimates_ahead$res[3]),
          "\n",
          model_name_region,
          round(estimate_2)
          #,
          #"\nExponentielles Modell: ",
          #round(estimates_ahead$res[3])
    )

   if(exp){
     caption_lng <- paste0(caption_lng,
            "\nModelo exponencial: ",
            round(estimates_ahead$res[4]))

   }
  }

  p <- results %>%
         filter(Date > limDate) %>%
         ggplot(aes(x = Date, y = fitted)) +
         geom_line(aes(col = Model_Name)) +
         geom_point(aes(col = Model_Name), size = 2) +
         scale_color_manual(values = colors_in) +
         ylab(ylab_lng) +
    labs(caption = caption_lng) +
    ggtitle(region1) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab(date_lng)

  if(log_scale){

    p<-p + scale_y_log10()

  }

  print(paste0(filename_add, PREDICTIONS_FILENAME))

  ggsave(get_filename(PREDICTIONS_FILENAME, filename_add),
         p,
         width = 10,
         height = 5)

  p




}

get_filename<-function(filename, add = ""){

  paste0("figures/", add, filename) %>% return()

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
                          region = "Austria",
                          filename_add = ""){

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
    geom_line(aes(col = Model_Name, linetype = Variable)) +
    scale_color_manual(values = COLORS) +
    ylab("Infektionen (Faelle)") +
    labs(caption = paste("Quelle: Wikipedia. Letzer Datenpunkt:",
                         get_last_date(db),
                       "\nVorhersage positiv getestete Individuen am ", final_date, add_string,
                       round(prediction_week_ahead))) +
    ggtitle(region) +
    theme(plot.title = element_text(hjust = 0.5))


  ggsave(get_filename(MODEL_FILENAME,
                      filename_add),
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
    geom_line(col = COLORS[1]) +
    ylab("Verhaeltnis Covid-19 Infektionen zu Tests (%)") +
    labs(caption = paste("Quelle: Wikipedia. Letzter Datenpunkt:", get_last_date(db))) +
    ggtitle(region) +
    theme(plot.title = element_text(hjust = 0.5))


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
    geom_line(col = COLORS[1]) +
    ylab("Anzahl Tests (Tests / Tag)") +
    labs(caption = paste("Quelle: Wikipedia. Letzter Datenpunkt:", get_last_date(db))) +
    ggtitle(region) +
    theme(plot.title = element_text(hjust = 0.5))


  ggsave(NMB_TESTS_FILENAME,
         p,
         width = 10,
         height = 5)

  p
}


read_prediction<-function(date, filename_add = ""){

    file<-paste0("data/prediction",
                 filename_add,
                 date,
                 ".csv")

    res<-read_delim(file,
                    delim = ";") %>%
      mutate(prediction_date = date)

   names(res) <- c("Date",
                       "Polynomielles Modell",
                       "Italienmodell",
                       "Exponential",
                       "Observations",
                   "Prediction_Date")




    return(res)



}

prediction_quality<-function(db,
                             shift = 1,
                             length = 6,
                             filename_add = ""){


  date <- Sys.Date() - shift

  last_cases<-get_infected(db,
                           "Austria") %>%
    mutate(Date = as.Date(Date)) %>%
    filter(Date == date)

  cases <- last_cases$Cases[1]


  dates <- seq(Sys.Date() - length - shift,
               Sys.Date() - shift, 1)

  data_real<-tibble(Date_Forecast=date,
                    Prediction_Date = dates,
                    Variable="Tatsächliche Fälle",
                    Value = cases)


  predictions<-mapply(read_prediction,
         dates,
         list(filename_add),
         SIMPLIFY = FALSE) %>%
    bind_rows() %>%
    mutate(Date_Forecast = as.Date(Date))  %>%
    filter(Date_Forecast == date) %>%
    dplyr::select(-Date) %>%
    gather(Variable,
           Value,
           -Date_Forecast,
           -Prediction_Date) %>%
    na.omit() %>%
    bind_rows(data_real) %>%
    filter(Variable != "Observations" & Variable != "Exponential")

  p <- predictions %>%
    spread(Variable, Value) %>%
    mutate(`Polynomielles Modell` = na.approx(`Polynomielles Modell`)) %>%
    ggplot(aes(x = Prediction_Date)) +
    geom_line(aes(x = Prediction_Date,
                  y = `Polynomielles Modell`,
                  color = "Polynomielles Modell")) +
    geom_line(aes(x = Prediction_Date,
                  y = Italienmodell,
                  color = "Italienmodell")) +
    geom_line(aes(x = Prediction_Date,
                  y = `Tatsächliche Fälle`,
                  color = "Tatsächlicher Wert"),
              size = 1.5) +
    geom_ribbon(aes(ymax=`Polynomielles Modell`, ymin=Italienmodell),
                alpha = 0.1)+
    scale_color_manual(values = COLORS[c(7, 1, 9)]) +
    xlab("Datum der Vorhersage") +
    ylab("Getestete Individuen") +
    ggtitle(paste0("Vorhersagequalität für Österreich für ", date)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.title = element_blank())


  ggsave(get_filename(PREDICTION_QUALITY_FILENAME,
                      filename_add),
         p,
         width = 10,
         height = 5)

  p






}


