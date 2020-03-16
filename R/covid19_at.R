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


### source eurostat
INHABITANTS_ITALY <- 60.48*10^6

### source eurostat
INHABITANTS_AUSTRIA <- 8.82*10^6

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

plot_compare_at_it<-function(){


  wiki_at <- covid19at::scrape_wikipedia_at()[[1]]
  wiki_it <- covid19at::scrape_wikipedia_it()

  wiki_at <- wiki_at %>%
    mutate(Infektionen = `Infektionen kumuliert` / INHABITANTS_AUSTRIA) %>%
    mutate(Country = "Österreich \n (8 Tage nach vorne versetzt)") %>%
    dplyr::select(Datum, Country, Infektionen) %>%
    mutate(Datum = Datum - 24*3600*8)

  wiki_it <- wiki_it %>%
    mutate(Infektionen = Infektionen / INHABITANTS_ITALY) %>%
    mutate(Country = "Italien") %>%
    dplyr::select(Datum, Country, Infektionen)

  #wiki_it <- wiki_it[-1,]

  p<-merged %>%
    ggplot(aes(x = Datum, y = Infektionen * 100)) +
    geom_line(aes(col = Country), size = 1) +
    geom_point(aes(col = Country)) +
    scale_color_manual(values = COLORS[c(1,6)]) +
    ylab("Infektionen (% Gesamtpopulation)")

  ggsave("figures/vergleich_at_it.png",
         p,
         width = 10,
         height = 5)

  p

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


generic_mod<-function(wikipedia_table_in,
              minus_n,
              week_ahead,
              type){

  wikipedia_table_short<-wikipedia_table_in[1:(nrow(wikipedia_table_in)-minus_n),]
  mod<-lm(Infektionen_trans ~ Datum, data = wikipedia_table_short)

  predictions<-predict(mod, week_ahead)

  df<-data.frame(Datum = week_ahead,
                 fitted = predictions, Type = type)


  return(df)

}


#### plot prediction
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
          data.frame(fitted=rep(NA, nrow(wikipedia_table_transform)), Type = "M")) %>%
          bind_rows(res) %>%
        mutate(Infektionen = transformation_f_inverse(Infektionen_trans),
         Prediction = transformation_f_inverse(fitted)) %>%
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
    ylab("Verhaeltnis Covid-19 Infektionen zu Tests (%)") +
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
                       mediaPath = "figures/covid19_predictions.png"
                        )

    tweet2<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Vergleich Infektionen Italien und Österreich (8 Tage nach vorne versetzt): ",
                         mediaPath = "figures/vergleich_at_it.png.png",
                         inReplyTo=tweet1$id
    )


    tweet3<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Neuinfektionen: ",
                         mediaPath = "figures/covid19_infektionen.png",
                         inReplyTo=tweet2$id
    )




    tweet4<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Verhaeltnis Infektionen:Tests ",
                     mediaPath = "figures/covid19_testungen.png",
                     inReplyTo=tweet3$id)


    tweet5<-updateStatus(text = "#COVID_19 Data Update fuer Oesterreich. Gesamtanzahl Tests",
                     mediaPath = "figures/covid19_testungen_absolut.png",
                     inReplyTo=tweet4$id)
  }


}
