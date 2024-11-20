library(tuber) 
library(httr) 
library(purrr) 
library(tidyverse) 
library(httpuv) 
library(ROAuth) 
library(writexl) 
library(xlsx) 
library(readxl)
library(tm)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(stopwords)



# Youtube idlerinin tanimlanmasi 
client_id <- "..."
client_secret_id <- "..." 

#Youtube'a baglanmak icin kullanilan kod asagidadi.
yt_oauth(client_id, client_secret_id, token = '')  

#Yorumlari cekip excell formatinda yazdirma 
get_all_comments(video_id = "...")
comments <- get_all_comments(video_id = "...")
write_xlsx(comments, "dosya\\yolu")

yorumlar <- read_xlsx(file.choose())

yorumlar$textOriginal <- str_replace_all(yorumlar$textOriginal, "http[^[:space:]]*", " ") 
yorumlar$textOriginal <- str_replace_all(yorumlar$textOriginal, "#//S+", " ") 
yorumlar$textOriginal <- str_replace_all(yorumlar$textOriginal, "@//S+", " ") 
yorumlar$textOriginal <- str_replace_all(yorumlar$textOriginal, "[[:punct:][:blank:]]+"," ") 
yorumlar$textOriginal <- str_to_lower(yorumlar$textOriginal, "tr")
yorumlar$textOriginal <- removeNumbers(yorumlar$textOriginal)
yorumlar$textOriginal <- str_replace_all(yorumlar$textOriginal, "[<].*[>]", " ") 
yorumlar$textOriginal <- gsub("\uFFFD", "", yorumlar$textOriginal, fixed = TRUE)
yorumlar$textOriginal <- gsub("\n", "", yorumlar$textOriginal, fixed = TRUE)
yorumlar$textOriginal <- str_replace_all(yorumlar$textOriginal, "[^[:alnum:]]", " ")

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

veri <- yorumlar %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, textOriginal)

stopwords::stopwords("tr",source = "stopwords-iso")
liste <- stopwords::stopwords("tr",source = "stopwords-iso")

liste <- c()
yorumlar$textOriginal <- removeWords(yorumlar$textOriginal, liste)

kelimeler <- yorumlar %>% select(textOriginal)  %>%   mutate(linenumber = row_number()) %>% unnest_tokens(word, textOriginal)
head(kelimeler) # Ilk 6 kelimenin konsola getirilmesi

kelimeler %>% # 30'dan fazla tekrar eden kelimeler
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  filter(!is.na(word)) %>%
  filter(n > 0) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "#F28500", color = "black") +
  xlab("Kelimeler") +
  ylab("Frekans") +
  coord_flip() +
  theme_classic() +
  ggtitle("Frekans Analizi", subtitle = "En sýk kullanýlan kelimeler") +
  labs(caption = "Veri kaynaðý: Yorumlar")+
  theme(
    axis.text.y = element_text(size = 12, color = "#65350F"),
    axis.text.x = element_text(size = 10, color= '#65350F'),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#B2560D"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.background = element_rect(fill = "#FFFFA7", color = NA), 
    plot.background = element_rect(fill = "#FFFDD0", color = "black") 
  )

wordcloud(kelimeler$word, min.freq = 1, max.words = 30, scale = c(2,1,10), 
          colors = brewer.pal(8, "Dark2"),random.color = T, random.order = F)

#Duygu analizi

library(dplyr)
library(readr)
library(sentimentr)
library(ggeasy)
library(ggthemes)
library(pander)
library(pastecs)

#yorumlar dosyasini kutuphane olarak tekrar kaydetme.
kutuphane <- yorumlar

#Turkce kelimelerin polarite karsiligi -1 ve 1 olan lexicon verisetini cagirma
lexicon <- read.table(file.choose(),
                      header = TRUE,
                      sep = ';',
                      stringsAsFactors = FALSE)

str(lexicon)

#lexicon verisetindeki WORD ve POLARITY sutunlarini alip
#word ve value olarak lexicon2 veriseti olusturur
lexicon2 <- lexicon %>%
  select(c("ï..WORD","POLARITY")) %>%
  rename('word'="ï..WORD", 'value'="POLARITY")

kutuphane %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, textOriginal) %>%
  inner_join(lexicon2, by = c("word" = "word")) %>%
  group_by(linenumber) %>%
  summarise(sentiment = sum(value)) %>%
  left_join(kutuphane %>%
              mutate(linenumber = row_number()), by = "linenumber") %>%
  write_csv("sentiment_output.csv")

#sonuclari analiz etme ve gorsellestirme
yeni <- read_csv("sentiment_output.csv", show_col_types = FALSE)
yeni <- read_csv("sentiment_output.csv", col_types = cols(textOriginal = "c", linenumber = "n", sentiment = "n"))



frekans_tablosu <- table(yeni$sentiment)
# Duyarlilik Puani Frekans Tablosu
print(frekans_tablosu)

#Duyarlilik puanlarinin dagiliminin grafik seklinde gosterilmesi 
ggplot(data = yeni, aes(x = sentiment)) +
  geom_histogram(binwidth = 1, fill = "#fc4c4e", color = 'black') +
  xlab("Duygu Skoru") +
  ylab("Frekans") +
  ggtitle("Duyarlilik Puanlarinin Dagilimi") +
  ggeasy::easy_center_title()+
  theme(
    panel.background = element_rect(fill = "#fc9483"), 
    plot.background = element_rect(fill = "#fdab9f"),  
    plot.title = element_text(hjust = 0.5),  
    plot.subtitle = element_text(hjust = 0.5)
  )


#duygu analizi sonuclarini degerlendirme
neutral <- length(which(yeni$sentiment == 0))
positive <- length(which(yeni$sentiment > 0))
negative <- length(which(yeni$sentiment < 0))


#toplam duygu sayisini hesaplama
toplam = positive + neutral + negative

#duygu kategorileri ve oranlari icin veri cercevesi olusturma
Duygular <- c("Pozitif", "Notr", "Negatif")
Degerler <- c((positive/toplam)*100, (neutral/toplam)*100, (negative/toplam)*100)
output <- data.frame(Duygular, Degerler)

#duygu kategorilerini factore donusturme
output$Duygular <- factor(output$Duygular, levels=Duygular)

#bar plot ile duygu analizi oranlarini gorsellestirme
ggplot(output, aes(x=Duygular, y=Degerler))+
  geom_bar(stat = "identity", aes(fill = Duygular))+
  ggtitle(" Duygu Analizinin Oranlari")+
  theme(
    panel.background = element_rect(fill = "#f5f5dc"), 
    plot.background = element_rect(fill = "white"),  
    plot.title = element_text(hjust = 0.5),  
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

#oranlarin head degerleri
head((positive/toplam)*100)
head((neutral/toplam)*100)
head((negative/toplam)*100)

# Polarite skoru 
polarite <- sentiment(yorumlar$textOriginal)
tablo <- cbind(yorumlar$textOriginal, polarite[,c(3,4)])
ggplot(tablo, aes(word_count, sentiment))+
  geom_point(color="blue")+
  geom_hline(yintercept = mean(tablo$sentiment), color= "red", size=1)+
  labs(y= "Skor", x= "Kelimelerin Frekansi")+
  theme_igray()+
  labs()+
  theme(plot.caption = element_text(hjust = 0, face = "italic"))

stat.desc(yeni$sentiment, basic = T) %>% pander()



