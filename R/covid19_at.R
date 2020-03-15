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
WIKIPEDIA_URL <- "https://de.wikipedia.org/wiki/COVID-19-F%C3%A4lle_in_%C3%96sterreich"

scrape_wikipedia<-function(wikipedia_url = WIKIPEDIA_URL){

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


  return(list(wikipedia_table_conv, wikipedia_table_conv_old))

}


get_last_date<-function(wikipedia_table){

  return(wikipedia_table$Datum[nrow(wikipedia_table)])

}


get_first_date<-function(wikipedia_table){

  return(wikipedia_table$Datum[1])

}




plot_overview<-function(wikipedia_table){
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

  ggsave("figures/covid19_infektionen.png",
         p,
         width = 10,
         height = 5)

  p

}


exp_mod<-function(wikipedia_table,
              minus_n,
              week_ahead,
              type){

  wikipedia_table<-wikipedia_table %>%
    mutate(Infektionen = `Infektionen kumuliert`) %>%
    mutate(log_Infektionen = log(Infektionen))

  wikipedia_table<-wikipedia_table[1:(nrow(wikipedia_table)-minus_n),]
  mod<-lm(log_Infektionen ~ Datum, data = wikipedia_table)

  predictions<-predict(mod, week_ahead)

  df<-data.frame(Datum = week_ahead,
                 fitted = predictions, Type = type)


  return(df)

}

#### plot prediction
plot_prediction<-function(wikipedia_table,
                          final_date,
                          steps = 3){




  week_ahead<-data.frame(Datum = seq(as.POSIXct(get_first_date(wikipedia_table) + 3600*24), as.POSIXct(final_date), 3600*24))

  results <- mapply(exp_mod,
         list(wikipedia_table),
         1:steps,
         list(week_ahead),
         paste0("M",1:steps),
         SIMPLIFY = FALSE)

  res<-bind_rows(results)

  forecast<-bind_cols(mod1$model,
          data.frame(fitted=mod1$fitted.values, Type = "M")) %>%
          bind_rows(res) %>%
        mutate(Infektionen = exp(log_Infektionen),
         Prediction = exp(fitted)) %>%
    dplyr::select(Datum, Infektionen, Prediction, Type) %>%
    gather(Variable, Value, -Datum, -Type)

  prediction_week_ahead <- forecast %>%
    filter(Variable == "Prediction" & Type == "M1") %>%
    dplyr::select(Value) %>%
    tail(1) %>%
    unlist()

  forecast %>%
    write_feather(path = paste0("data/prediction", Sys.Date()))

  p <- forecast %>%
    ggplot(aes(x = Datum, y = Value)) +
    geom_point() +
    geom_line(aes(col = Type, linetype = Variable), size = 1.1) +
    scale_color_manual(values = COLORS[c(1, 3, 6, 10)]) +
    ylab("Infektionen (Faelle)") +
    labs(caption = paste("Quelle: Wikipedia. Letzer Datenpunkt:",
                         get_last_date(wikipedia_table),
                       "\nVorhersage Infektionen am ", final_date," (exponentielles Modell M1): ",
                       round(prediction_week_ahead)))

  ggsave("figures/covid19_predictions.png",
         p,
         width = 10,
         height = 5)

  p
}

#### plot test statistics

plot_test_statistics<-function(wikipedia_table){

  first_value_testungen <- wikipedia_table$`Testungen kumuliert`[1]


  p <- wikipedia_table %>%
    mutate(Testungen = c(first_value_testungen, diff(`Testungen kumuliert`))) %>%
    dplyr::select(Datum, `Testungen kumuliert`, Testungen, Neuinfektionen) %>%
      mutate(`Verh?ltnis Infektionen zu Tests` = 100 * Neuinfektionen/Testungen) %>%
    ggplot(aes(x = Datum, `Verh?ltnis Infektionen zu Tests`)) +
    geom_point(col = COLORS[1]) +
    geom_line(col = COLORS[1], size = 1) +
    ylab("Verh?ltnis Covid-19 Infektionen zu Tests (%)") +
    labs(caption = paste("Quelle: Wikipedia. Letzter Datenpunkt:", get_last_date(wikipedia_table)))


  ggsave("figures/covid19_testungen.png",
         p,
         width = 10,
         height = 5)

  p

}

#### plot number of tests

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

  ggsave("figures/covid19_testungen_absolut.png",
         p,
         width = 10,
         height = 5)

  p
}


#### tweet results, if new

tweet_results_if_new <- function(wikipedia_table_conv, wikipedia_table_conv_old){

  tweet<-FALSE

  if(is.null(wikipedia_table_conv_old)){
    tweet<-TRUE
  }else{

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

    tweet1<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Neuinfektionen: ",
             mediaPath = "figures/covid19_infektionen.png"
             )

    tweet2<-updateStatus(text = "#COVID_19 Vorhersage mit exponentiellem Modell: ",
                       mediaPath = "figures/covid19_predictions.png",
                       inReplyTo=tweet1$id
                        )

    tweet3<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Verhaeltnis Infektionen:Tests ",
                     mediaPath = "figures/covid19_testungen.png",
                     inReplyTo=tweet2$id)


    tweet4<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Gesamtanzahl Tests",
                     mediaPath = "figures/covid19_testungen_absolut.png",
                     inReplyTo=tweet3$id)
  }


}
