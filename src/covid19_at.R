library(rvest)
library(tidyverse)
library(xml2)
library(feather)


colors<- c("#c72321", 
           "#861719", 
           "#fbd7a9",
           "#ba9f7c", 
           "#7a6952", 
           "#6e9b9e",
           "#0d8085", 
           "#19484c",
           "#f0c320",
           "#af8f19")

theme_set(theme_classic(base_size = 14))

### wikipedia url
wikipedia_url_ <- "https://de.wikipedia.org/wiki/COVID-19-F%C3%A4lle_in_%C3%96sterreich"

figures_dir<-"../figures/"

scrape_wikipedia<-function(wikipedia_url = wikipedia_url_){

  webpage <- xml2::read_html(wikipedia_url)

  wikipedia_table <- rvest::html_table(webpage, fill = TRUE)[[3]] 

  wikipedia_table_clean <- wikipedia_table[-1,]

  names(wikipedia_table_clean)<-c("Datum",
                                "Nö",	"W",	"St",	"T",	"Oö",	"S",	"B",	"V",	"K", 
                                "Infektionen kumuliert",
                                "Genesen kumuliert",
                                "Aktuell Infizierte",
                                "Todesfälle kumuliert",
                                "Neuinfektionen",	
                                "Testungen kumuliert")

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


  return(wikipedia_table_conv)
  
}


wikipedia_table_conv_old <- NULL
if(file.exists("../data/data.feather")){
  wikipedia_table_conv_old <- feather("../data/data.feather")
}

wikipedia_table_conv <- scrape_wikipedia()

write_feather(wikipedia_table_conv, "../data/data.feather")


last_date<-wikipedia_table_conv$Datum[nrow(wikipedia_table_conv)]
  
wikipedia_table_conv %>% 
  gather(Variable, Value, -Datum) %>% 
  filter(Variable %in% c("Infektionen kumuliert",
                         "Genesen kumuliert",
                         "Aktuell Infizierte")) %>% 
  ggplot(aes(x = Datum, y = Value)) + 
  geom_point(aes(col = Variable)) + 
  geom_line(aes(col = Variable),
            size=1) +
  scale_color_manual(values = colors[c(1,5, 10)]) +
  ylab("Wert (Individuen)") +
  labs(caption = paste("Source: Wikipedia. Latest data point: ", last_date))
  

ggsave("../figures/covid19_infektionen.png")


first_value_testungen <- wikipedia_table_conv$`Testungen kumuliert`[1]

wikipedia_table_conv %>% 
  mutate(Testungen = c(first_value_testungen, diff(`Testungen kumuliert`))) %>% 
  dplyr::select(Datum, `Testungen kumuliert`, Testungen, Neuinfektionen) %>% 
    mutate(`Verhältnis Infektionen zu Tests` = 100 * Neuinfektionen/Testungen) %>% 
  ggplot(aes(x = Datum, `Verhältnis Infektionen zu Tests`)) +
  geom_point(col = colors[1]) +
  geom_line(col = colors[1], size = 1) +
  ylab("Verhältnis Covid-19 Infektionen zu Tests (%)") +
  labs(caption = paste("Source: Wikipedia. Latest data point: ", last_date))


ggsave("../figures/covid19_testungen.png")
  
  
wikipedia_table_conv %>% 
  mutate(Testungen = c(first_value_testungen, diff(`Testungen kumuliert`))) %>% 
  dplyr::select(Datum, `Testungen kumuliert`, Testungen, Neuinfektionen) %>% 
  ggplot(aes(x = Datum, `Testungen`)) +
  geom_point(col = colors[1]) +
  geom_line(col = colors[1], size = 1) +
  ylab("Testungen") +
  labs(caption = paste("Source: Wikipedia. Latest data point: ", last_date))


ggsave("../figures/covid19_testungen_absolut.png")




tweet<-FALSE

if(is.null(wikipedia_table_conv_old)){
  tweet<-TRUE  
}else{
  
    last_date_old<-wikipedia_table_conv_old$Datum[nrow(wikipedia_table_conv)]
    if(last_date>last_date_old){
      
      tweet<-TRUE
    
      }
}

if(tweet){
  authentification <- feather("../authentification")

  library(twitteR)
  setup_twitter_oauth(consumer_key = authentification$consumer_key[1],
                    access_token = authentification$access_token[1],
                    consumer_secret = authentification$consumer_secret[1],
                    access_secret = authentification$access_secret[1])

  tweet1<-updateStatus(text = "#COVID_19 Data Update für Österreich. Neuinfektionen: ", 
             mediaPath = "../figures/covid19_infektionen.png"
             )

  tweet2<-updateStatus(text = "#COVID_19 Data Update für Österreich. Verhältnis Infektionen:Tests ", 
                     mediaPath = "../../figures/covid19_testungen.png",
                     inReplyTo=tweet1$id)


  tweet3<-updateStatus(text = "#COVID_19 Data Update für Österreich. Gesamtanzahl Tests", 
                     mediaPath = "../../figures/covid19_testungen_absolut.png",
                     inReplyTo=tweet2$id)
}

