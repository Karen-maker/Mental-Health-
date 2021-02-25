# Install packages
install.packages("tm")  # for text mining
install.packages("NLP")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis
install.packages("ggplot2") # for plotting graphs
install.packages("pdftools") #for pdf_text

# Load
library("NLP")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library("pdftools") # we need this library to use pdf_text
library(dplyr)
library(tidytext)


setwd("/Users/karenlarios/Desktop/Social Media")
files <- list.files(path="/Users/karenlarios/Desktop/Social Media")

my_pdf_text <- lapply(files, pdf_text)


# creating the corpus 
Rpdf <- readPDF(control = list(text = "-layout"))
data_corpus <- Corpus(URISource(files), 
                      readerControl = list(reader = Rpdf))


# creating the DTM 
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
data_dtm <- DocumentTermMatrix(data_corpus, control = list(tokenize="words", 
                                                           removePunctuation = TRUE, 
                                                           stopwords = stopwords("english"), 
                                                           stemming = TRUE))
# Convert to tidy
data_td <- tidy(data_dtm)
data_td

# Perform sentiment analysis, getting the positive and negative for each word
data_sentiments <- data_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

data_sentiments

#Visualize which words most often contributed to positive or negative sentiment type of histogram
library(ggplot2)
data_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 3) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

# creating a tf-idf format 
data_tf_idf <- data_td%>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))
data_tf_idf

#creating a plot from the tf-idf format
data_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(term, levels=rev(unique(term)))) %>%
  group_by(document) %>%
  top_n(20) %>%
  ungroup %>%
  ggplot(aes(term, tf_idf, fill=document))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~document, ncol=2, scales="free")+
  coord_flip()

# creating a wordcloud
#install.packages("reshape2")
library(reshape2)
data_sentiments%>%
  inner_join(get_sentiments("bing")) %>%
  count(term, sentiment, sort=TRUE) %>%
  acast(term ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("lightblue", "pink"),
                   max.words=200, scale=c(1, 0.1), random.order = TRUE)



