
library(pdftools)
library(tidytext)
library(udpipe)
library(tidyverse)

path = "/Users/chenjunzhuo/Documents/GitHub/final-project-xiaoqi-ellie-lee/Annual Energy outlook/"

## pdf_text and udpipe are slow to run, here is the reference code for functions and loops
## please refer to code below to operate each .pdf file separately
list <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2021", "2022")
for (i in list) {
  filename <- paste0("AEO_", i)
  wd <- paste0("AEO_", i, ".pdf")
  assign(filename, pdf_text(paste0(path, wd)))
 }


# simplification <- function(data, i){
#  output <- 
#    udpipe(data, "English") %>%
#    filter(!upos  %in% c("PUNCT", "CCONJ")) %>%
#    anti_join(stop_words, by = c("lemma" = "word")) %>%
#    select(lemma) %>%
#    mutate(year = i)
#  return(output)
#}

#for (i in list) {
#  filename <- paste0("AEO_", i, "_parsed")
#  assign(filename, simplification(data, i))
#}
##################################################

AEO_2022_parsed <- 
  udpipe(AEO_2022, "english") %>%
  filter(!upos  %in% c("PUNCT", "CCONJ")) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  select(lemma) %>%
  mutate(year = "2022")
AEO_2021_parsed <- 
  udpipe(AEO_2021, "english") %>%
  filter(!upos  %in% c("PUNCT", "CCONJ")) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  select(lemma) %>%
  mutate(year = "2021")
AEO_2019_parsed <- 
  udpipe(AEO_2019, "english") %>%
  filter(!upos  %in% c("PUNCT", "CCONJ")) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  select(lemma) %>%
  mutate(year = "2019")
AEO_2018_parsed <- 
  udpipe(AEO_2018, "english") %>%
  filter(!upos  %in% c("PUNCT", "CCONJ")) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  select(lemma) %>%
  mutate(year = "2018")
AEO_2017_parsed <- 
  udpipe(AEO_2017, "english") %>%
  filter(!upos  %in% c("PUNCT", "CCONJ")) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  select(lemma) %>%
  mutate(year = "2017")
AEO_2016_parsed <- 
  udpipe(AEO_2016, "english") %>%
  filter(!upos  %in% c("PUNCT", "CCONJ")) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  select(lemma) %>%
  mutate(year = "2016")
AEO_2015_parsed <- 
  udpipe(AEO_2015, "english") %>%
  filter(!upos  %in% c("PUNCT", "CCONJ")) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  select(lemma) %>%
  mutate(year = "2015")
AEO_2014_parsed <- 
  udpipe(AEO_2014, "english") %>%
  filter(!upos  %in% c("PUNCT", "CCONJ")) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  select(lemma) %>%
  mutate(year = "2014")
AEO_2013_parsed <- 
  udpipe(AEO_2013, "english") %>%
  filter(!upos  %in% c("PUNCT", "CCONJ")) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  select(lemma) %>%
  mutate(year = "2013")
AEO_2012_parsed <- 
  udpipe(AEO_2012, "english") %>%
  filter(!upos  %in% c("PUNCT", "CCONJ")) %>%
  anti_join(stop_words, by = c("lemma" = "word")) %>%
  select(lemma) %>%
  mutate(year = "2012")
#  group_by(lemma) %>%
#  count(sort = TRUE)

#sentiment analysis
sentiment_nrc   <- get_sentiments("nrc")   %>% dplyr::rename(nrc   = sentiment)
sentiment_afinn <- get_sentiments("afinn") %>% dplyr::rename(affin =     value)
sentiment_bing  <- get_sentiments("bing")  %>% dplyr::rename(bing  = sentiment)

ten_years <- bind_rows(AEO_2022_parsed, AEO_2021_parsed, AEO_2019_parsed,
                        AEO_2018_parsed, AEO_2017_parsed, AEO_2016_parsed, 
                        AEO_2015_parsed, AEO_2014_parsed, AEO_2013_parsed
                        )

ten_years_senti <- ten_years %>%
  left_join(sentiment_nrc,   by = c("lemma" = "word")) %>%
  left_join(sentiment_afinn, by = c("lemma" = "word")) %>%
  left_join(sentiment_bing,  by = c("lemma" = "word"))

# Standardize word count
nrc_demo <- ten_years_senti %>% 
  group_by(year, nrc) %>%
  summarise(n = n()) %>%
  filter(!is.na(nrc))

nrc_demo$sum_year <- ave(nrc_demo$n, nrc_demo$year, FUN=sum)
nrc_demo <- nrc_demo %>% mutate(prop = n/sum_year)
  
affin_demo <- ten_years_senti %>% 
  group_by(year, affin) %>%
  summarise(n = n()) %>%
  filter(!is.na(affin))

affin_demo$sum_year <- ave(affin_demo$n, affin_demo$year, FUN=sum)
affin_demo <- affin_demo %>% mutate(prop = n/sum_year)

# Plot two sentiment bar chart

# 1. NRC
ggplot(data = filter(nrc_demo, !is.na(nrc))) +
  geom_col(aes(x = nrc, y = prop, fill = year), position = "dodge", stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Annual Energy Report Sentiment: NRC",
       x = element_blank(), y = "Count") +
  scale_fill_brewer(palette = "Reds") +
  theme_bw() +
  theme(legend.title = element_blank())

# 2. AFFIN
ggplot(data = filter(affin_demo, !is.na(affin))) +
  geom_col(aes(x = affin, y = prop, fill = year), position = "dodge") +
  labs(title = "Annual Energy Report Sentiment: AFFIN",
       x = element_blank(), y = "Count") +
  scale_fill_brewer(palette = "Blues") +
  theme_bw() +
  theme(legend.title = element_blank())


#############################End#################################


