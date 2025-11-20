install.packages ("quanteda")
install.packages ("readtext")
install.packages ("stringi")
install.packages("ggplot2")
install.packages("tm")

install.packages("quanteda.textplots")
install.packages("quanteda.textstats")

install.packages("text2vec")
install.packages("uwot")
install.packages("syuzhet")
install.packages("stm")
install.packages("udpipe")
install.packages("reshape2")
install.packages("topicmodels")

install.packages("dplyr")
install.packages("tidyverse")
install.packages("readr")

library(quanteda)
library(readtext)
library(stringi)
library(ggplot2)
library(quanteda.textplots)
library(quanteda.textstats)
library(text2vec)
library(uwot)
library(syuzhet)
library(stm)
library(udpipe)
library(reshape2)
library(topicmodels)
library(tm)
library(dplyr)
library(tidyverse)
library(readr)

#1
UWE <- texts(readtext("Under Western Eyes.txt", encoding = "UTF-8"))
uwe_clean <- tokens(UWE, remove_punct = TRUE) %>% 
  tokens_tolower()
uwe_token_count <- as.character(uwe_clean)
length (uwe_token_count)
uwe_remove <- tokens_remove(uwe_clean, stopwords("english"))
#2
TWOO <- texts(readtext("The Wizard of Oz.txt", encoding = "UTF-8"))
twoo_clean <- tokens(TWOO, remove_punct = TRUE) %>% 
  tokens_tolower()
twoo_token_count <- as.character(twoo_clean)
length (twoo_token_count)
twoo_remove <- tokens_remove(twoo_clean, stopwords("english"))
#3
TSG <- texts(readtext("The secret garden.txt", encoding = "UTF-8"))
tsg_clean <- tokens(TSG, remove_punct = TRUE) %>% 
  tokens_tolower()
tsg_token_count <- as.character(tsg_clean)
length (tsg_token_count)
tsg_remove <- tokens_remove(tsg_clean, stopwords("english"))
#4
TLP <- texts(readtext("The little prince.txt", encoding = "UTF-8"))
tlp_clean <- tokens(TLP, remove_punct = TRUE) %>%
  tokens_tolower()
tlp_token_count <- as.character(tlp_clean)
length(tlp_token_count)
tlp_remove <- tokens_remove(tlp_clean, stopwords("english"))
#5
PP <- texts(readtext("Pippi.txt", encoding = "UTF-8"))
pp_clean <- tokens(PP, remove_punct = TRUE) %>%
  tokens_tolower()
pp_token_count <- as.character(pp_clean)
length(pp_token_count)
pp_remove <- tokens_remove(pp_clean, stopwords("english"))
#6
PAW <- texts(readtext("PETER AND WENDY.txt", encoding = "UTF-8"))
paw_clean <- tokens(PAW, remove_punct = TRUE) %>%
  tokens_tolower()
paw_token_count <- as.character(paw_clean)
length(paw_token_count)
paw_remove <- tokens_remove(paw_clean, stopwords("english"))
#7
NOS <- texts(readtext("Nostromo.txt", encoding = "UTF-8"))
nos_clean <- tokens(NOS, remove_punct = TRUE) %>%
  tokens_tolower()
nos_token_count <- as.character(nos_clean)
length(nos_token_count)
nos_remove <- tokens_remove(nos_clean, stopwords("english"))
#8
LJ <- texts(readtext("Lord Jim.txt", encoding = "UTF-8"))
lj_clean <- tokens(LJ, remove_punct = TRUE) %>%
  tokens_tolower()
lj_token_count <- as.character(lj_clean)
length(lj_token_count)
lj_remove <- tokens_remove(lj_clean, stopwords("english"))
#9
HOD <- texts(readtext("Heart of Darkness.txt", encoding = "UTF-8"))
hod_clean <- tokens(HOD, remove_punct = TRUE) %>%
  tokens_tolower()
hod_token_count <- as.character(hod_clean)
length(hod_token_count)
hod_remove <- tokens_remove(hod_clean, stopwords("english"))
#10
ANN <- texts(readtext("Anne.txt", encoding = "UTF-8"))
ann_clean <- tokens(ANN, remove_punct = TRUE) %>%
  tokens_tolower()
ann_token_count <- as.character(ann_clean)
length(ann_token_count)
ann_remove <- tokens_remove(ann_clean, stopwords("english"))
#total tokens <- 744118(70万字的儿童文学语料库)
# 创建数据框
novel_data <- data.frame(
  novel = c("Under Western Eyes", "The Wizard of Oz", "The secret garden", 
            "The little prince", "Pippi", "PETER AND WENDY", 
            "Nostromo", "Lord Jim", "Heart of Darkness", "Anne"),
  token_count = c(
    length(uwe_token_count),
    length(twoo_token_count),
    length(tsg_token_count),
    length(tlp_token_count),
    length(pp_token_count),
    length(paw_token_count),
    length(nos_token_count),
    length(lj_token_count),
    length(hod_token_count),
    length(ann_token_count)
  )
)

ggplot(novel_data, aes(x = reorder(novel, token_count), y = token_count)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = token_count), hjust = -0.2, size = 3) +
  labs(
    title = "Comparison of Token Quantities in Various Novels",
    x = "Title of the novel",
    y = "Tokens"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

#画情感分析的图
UWE_syuzhet <- get_text_as_string("Under Western Eyes.txt")
uwe_sentences <- get_sentences(UWE_syuzhet)
uwe_sentiment <- get_sentiment(uwe_sentences, method = "syuzhet")

TWOO_syuzhet <- get_text_as_string("The Wizard of Oz.txt")
twoo_sentences <- get_sentences(TWOO_syuzhet)
twoo_sentiment <- get_sentiment(twoo_sentences, method = "syuzhet")

TSG_syuzhet <- get_text_as_string("The secret garden.txt")
tsg_sentences <- get_sentences(TSG_syuzhet)
tsg_sentiment <- get_sentiment(tsg_sentences, method = "syuzhet")

TLP_syuzhet <- get_text_as_string("The little prince.txt")
tlp_sentences <- get_sentences(TLP_syuzhet)
tlp_sentiment <- get_sentiment(tlp_sentences, method = "syuzhet")

PP_syuzhet <- get_text_as_string("Pippi.txt")
pp_sentences <- get_sentences(PP_syuzhet)
pp_sentiment <- get_sentiment(pp_sentences, method = "syuzhet")

PAW_syuzhet <- get_text_as_string("PETER AND WENDY.txt")
text_char <- as.character(PAW)
text_char <- iconv(text_char, to = "UTF-8", sub = " ") 
paw_sentences <- get_sentences(text_char)
paw_sentiment <- get_sentiment(paw_sentences, method = "syuzhet")

NOS_syuzhet <- get_text_as_string("Nostromo.txt")
nos_sentences <- get_sentences(NOS_syuzhet)
nos_sentiment <- get_sentiment(nos_sentences, method = "syuzhet")

LJ_syuzhet <- get_text_as_string("Lord Jim.txt")
lj_sentences <- get_sentences(LJ_syuzhet)
lj_sentiment <- get_sentiment(lj_sentences, method = "syuzhet")

HOD_syuzhet <- get_text_as_string("Heart of Darkness.txt")
hod_sentences <- get_sentences(HOD_syuzhet)
hod_sentiment <- get_sentiment(hod_sentences, method = "syuzhet")

ANN_syuzhet <- get_text_as_string("Anne.txt")
ann_sentences <- get_sentences(ANN_syuzhet)
ann_sentiment <- get_sentiment(ann_sentences, method = "syuzhet")

all_books <- data.frame(
  time = c(seq_along(cumsum(uwe_sentiment)), 
           seq_along(cumsum(twoo_sentiment)),
           seq_along(cumsum(tsg_sentiment)),
           seq_along(cumsum(tlp_sentiment)),
           seq_along(cumsum(pp_sentiment)),
           seq_along(cumsum(paw_sentiment)),
           seq_along(cumsum(nos_sentiment)), 
           seq_along(cumsum(lj_sentiment)),
           seq_along(cumsum(hod_sentiment)),
           seq_along(cumsum(ann_sentiment))),
  sentiment = c(cumsum(uwe_sentiment), 
                cumsum(twoo_sentiment),
                cumsum(tsg_sentiment),
                cumsum(tlp_sentiment),
                cumsum(pp_sentiment),
                cumsum(paw_sentiment),
                cumsum(nos_sentiment),
                cumsum(lj_sentiment),
                cumsum(hod_sentiment),
                cumsum(ann_sentiment)),
  book = rep(c("Under Western Eyes", "The Wizard of Oz", "The Secret Garden",
               "The Little Prince", "Pippi Longstocking", "Peter and Wendy", 
               "Nostromo", "Lord Jim", "Heart of Darkness", "Anne of Green Gables"),
             times = c(length(cumsum(uwe_sentiment)), 
                       length(cumsum(twoo_sentiment)),
                       length(cumsum(tsg_sentiment)),
                       length(cumsum(tlp_sentiment)),
                       length(cumsum(pp_sentiment)),
                       length(cumsum(paw_sentiment)),
                       length(cumsum(nos_sentiment)),
                       length(cumsum(lj_sentiment)),
                       length(cumsum(hod_sentiment)),
                       length(cumsum(ann_sentiment))))
)

ggplot(all_books, aes(x = time, y = sentiment, color = book)) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  labs(x = "Narrative time", 
       y = "Emotional Valence", 
       title = "Emotion Trajectories in Children's Literature",
       color = "Book Title") +
  theme_minimal() +
  scale_color_manual(values = c("Under Western Eyes" = "#376795", 
                                "The Wizard of Oz" = "#528fad",
                                "The Secret Garden" = "#72bcd5",
                                "The Little Prince" = "#aadce0",
                                "Pippi Longstocking" = "#ffe6b7",
                                "Perter and Wendy" = "#ffd06f",
                                "Nostromo" = "#f7aa58",
                                "Lord Jim" = "#ef8a47", 
                                "Heart of Darkness" = "#e76254", 
                                "Anne of Green Gables" = "#67000D")) +
  theme(legend.position = "bottom")

#找出高潮和低谷的文本部分
# 收集所有情感数据和句子
all_sentiments <- list(
  uwe_sentiment, twoo_sentiment, tsg_sentiment, tlp_sentiment, 
  pp_sentiment, paw_sentiment, nos_sentiment, lj_sentiment, 
  hod_sentiment, ann_sentiment
)

all_sentences <- list(
  uwe_sentences, twoo_sentences, tsg_sentences, tlp_sentences,
  pp_sentences, paw_sentences, nos_sentences, lj_sentences,
  hod_sentences, ann_sentences
)

titles <- c("Under Western Eyes", "The Wizard of Oz", "The secret garden",
            "The little prince", "Pippi", "PETER AND WENDY", 
            "Nostromo", "Lord Jim", "Heart of Darkness", "Anne")

# 创建包含区块信息的主数据框
corpus_df <- map2_dfr(1:length(titles), titles, ~{
  book_index <- .x
  book_title <- .y
  sentences_vector <- all_sentences[[book_index]]
  sentiment_vector <- all_sentiments[[book_index]]
  
  chunk_size <- 100
  n_chunks <- ceiling(length(sentiment_vector) / chunk_size)
  
  map_dfr(1:n_chunks, function(i) {
    start <- (i-1) * chunk_size + 1
    end <- min(i * chunk_size, length(sentiment_vector))
    
    # 确保有文本内容
    if (start <= length(sentences_vector) && end <= length(sentences_vector)) {
      chunk_text <- sentences_vector[start:end]
    } else {
      chunk_text <- NA
    }
    
    data.frame(
      title = book_title,
      chunk = i,
      line_number = start:end,
      text = chunk_text,
      stringsAsFactors = FALSE
    )
  })
}) %>% filter(!is.na(text))

# 创建情感分析结果数据框
corpus_processed <- map2_dfr(1:length(titles), titles, ~{
  book_index <- .x
  book_title <- .y
  sentiment_vector <- all_sentiments[[book_index]]
  
  chunk_size <- 100
  n_chunks <- ceiling(length(sentiment_vector) / chunk_size)
  
  data.frame(
    title = book_title,
    chunk = 1:n_chunks,
    ave_sentiment = sapply(1:n_chunks, function(i) {
      start <- (i-1) * chunk_size + 1
      end <- min(i * chunk_size, length(sentiment_vector))
      mean(sentiment_vector[start:end], na.rm = TRUE)
    }),
    stringsAsFactors = FALSE
  )
})
#文件导出
extreme_sections <- corpus_processed %>%
  group_by(title) %>%
  filter(ave_sentiment == max(ave_sentiment) | ave_sentiment == min(ave_sentiment)) %>%
  mutate(section_type = ifelse(ave_sentiment == max(ave_sentiment), "high", "low")) %>%
  ungroup()

climax_texts <- corpus_df %>%
  inner_join(extreme_sections, by = c("title", "chunk")) %>%
  group_by(title, chunk, section_type) %>%
  summarise(
    full_text = paste(text, collapse = " "),
    .groups = "drop"
  ) %>%
  mutate(header = paste0("\n【", title, " - ", section_type, " - part", chunk, "】\n"))

all_climaxes <- paste0(climax_texts$header, climax_texts$full_text, collapse = "\n\n")
write_file(all_climaxes, "all_climax_sections.txt")

full_corpus <- paste(corpus_df$text, collapse = " ")
write_file(full_corpus, "full_corpus_cleaned.txt")
