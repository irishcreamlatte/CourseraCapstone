library(tidyverse)
library(quanteda)
library(qdap)

library(data.table)
library(stringi)

setwd("C:/Users/Gemelee/Desktop/CourseraCapstone")
load("myData.RData")

saveRDS(uni_df, file = "uni_df.RDS")
saveRDS(bi_df, file = "bi_df.RDS")
saveRDS(tri_df, file = "tri_df.RDS")

blogs_size <- file.info("./final/en_US/en_US.blogs.txt")$size
news_size <- file.info("./final/en_US/en_US.news.txt")$size
twitter_size <- file.info("./final/en_US/en_US.twitter.txt")$size

blogs_words <- stri_count_words(blogs)
news_words <- stri_count_words(news)
twitter_words <- stri_count_words(twitter)


data.frame("File" = c("Blogs", "News", "Tweets"),
           "Size" = c(blogs_size, news_size, twitter_size),
            "Rows" = c(length(blogs), length(news), length(twitter)), 
            "Words" = c(sum(blogs_words), sum(news_words), sum(twitter_words)))

blogs <- read_lines("final/en_US/en_US.blogs.txt")
news <- read_lines("final/en_US/en_US.news.txt")
twitter <- read_lines("final/en_US/en_US.twitter.txt")

## Create corpus

set.seed(1234)
blogs_sample <- sample(blogs, length(blogs) * 0.05)
news_sample <- sample(news, length(news) * 0.05)
twitter_sample <- sample(twitter, length(twitter) * 0.05)

freq_blogs <- freq_terms(blogs_sample, 10)
plot(freq_blogs)
freq_news <- freq_terms(news_sample, 10)
plot(freq_news)
freq_twitter <- freq_terms(twitter_sample, 10)
plot(freq_twitter)

freq_blogs_stop <- freq_terms(blogs_sample, at.least = 3, stopwords=qdapDictionaries::Top25Words, 10)
freq_news_stop <- freq_terms(news_sample, at.least = 3, stopwords=qdapDictionaries::Top25Words, 10)
freq_twitter_stop <- freq_terms(twitter_sample, at.least = 3, stopwords=qdapDictionaries::Top25Words, 10)

freq_blogs_stop %>% 
    ggplot(aes(x = reorder(WORD, -FREQ), y = FREQ)) +
    geom_bar(stat = "identity", color = "skyblue", fill = "steelblue") + 
    coord_flip() + 
    labs(title = "Frequent Blog Words", subtitle = "Without Stopwords", x = "Count", y = "Word")

freq_news_stop %>% 
    ggplot(aes(x = reorder(WORD, -FREQ), y = FREQ)) + 
    geom_bar(stat = "identity", color = "skyblue", fill = "steelblue") + 
    coord_flip() + 
    labs(title = "Frequent News Words", subtitle = "Without Stopwords", x = "Count", y = "Word")

freq_twitter_stop %>% 
    ggplot(aes(x = reorder(WORD, -FREQ), y = FREQ)) + 
    geom_bar(stat = "identity", color = "skyblue", fill = "steelblue") + 
    coord_flip() +
    labs(title = "Frequent Words in Tweets", subtitle = "Without Stopwords", x = "Count", y = "Word")


datasample <- c(blogs_sample, news_sample, twitter_sample)

mycorpus <- corpus(datasample)
print(mycorpus)
summary(mycorpus)

## tokenizing 
## changed 
tokens_mycorpus <- tokens(mycorpus, 
                          remove_punct = TRUE, 
                          remove_symbols = TRUE,
                          remove_numbers = TRUE, 
                          remove_url = TRUE, 
                          split_hyphens = TRUE, 
                          remove_separators = TRUE)
##changed
tokens_mycorpus <- tokens_tolower(tokens_mycorpus)
tokens_mycorpus <- tokens_remove(tokens_mycorpus,
                                 stopwords("english"), 
                                 verbose = TRUE)
## changed 
profanity <- read_lines("./swearWords.txt")
profanity <- gsub(" ", "", profanity)
profanity <- tolower(profanity)
tokens_mycorpus <- tokens_remove(tokens_mycorpus, 
                                 pattern = profanity, 
                                 verbose = TRUE)

mycorpus_unigrams <- tokens_ngrams(tokens_mycorpus, n = 1)
mycorpus_bigrams <- tokens_ngrams(tokens_mycorpus, n = 2)
mycorpus_trigrams <- tokens_ngrams(tokens_mycorpus, n = 3)


## DFM

dfm_unicorpus <- dfm(mycorpus_unigrams, 
                     stem = TRUE)
dfm_bigrams <- dfm(mycorpus_bigrams,  
                   stem = TRUE)
dfm_trigrams <- dfm(mycorpus_trigrams,  
                    stem = TRUE)

#changed unicorpus
unicorpus <- textstat_frequency(dfm_unicorpus)
##
unicorpus %>% 
    data.frame() %>%  
    arrange(desc(frequency)) %>% 
    top_n(15, frequency) %>% 
    ggplot(aes(x = reorder(feature, -frequency), y = frequency)) + 
    geom_bar(stat = "identity", color = "skyblue", fill = "steelblue") + 
    theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
    labs(title = "Top 15 Words", x = "Words", y = "Count")

textplot_wordcloud(dfm_unicorpus, max_words = 100, 
                   rotation = 0.25,
                   color = rev(RColorBrewer::brewer.pal(10, "RdBu")))

#changed bicorpus
bicorpus <- textstat_frequency(dfm_bigrams)
##
bicorpus %>% 
    data.frame() %>% 
    arrange(desc(frequency)) %>% 
    top_n(15, frequency) %>% 
    ggplot(aes(x = reorder(feature, -frequency), y = frequency)) + 
    geom_bar(stat = "identity", color = "skyblue", fill = "steelblue") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Top 15 Bigrams", x = "Words", y = "Count")

textplot_wordcloud(dfm_bigrams, max_words = 100, 
                   rotation = 0.25, 
                   color = rev(RColorBrewer::brewer.pal(10, "RdBu")))

#changed tricorpus
tricorpus <- textstat_frequency(dfm_trigrams)
##
tricorpus %>% 
    data.frame() %>% 
    arrange(desc(frequency)) %>% 
    top_n(15, frequency) %>% 
    ggplot(aes(x = reorder(feature, -frequency), y = frequency)) + 
    geom_bar(stat = "identity", color = "skyblue", fill = "steelblue") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Top 15 Trigrams", x = "Words", y = "Count")

textplot_wordcloud(dfm_trigrams, max_words = 50, 
                   rotation = 0.25, 
                   color = rev(RColorBrewer::brewer.pal(10, "RdBu")))

## 


cumulative_uni <- topfeatures(dfm_unicorpus, length(dfm_unicorpus))
cumulative_uni <- data.frame(cumulative_uni)
cumulative_uni$n <- c(1:nrow(cumulative_uni))
cumulative_uni$total <- cumsum(cumulative_uni$cumulative_uni)
ggplot(cumulative_uni, aes(x =n, y =total)) + geom_line() + xlab('Number of Unique Words') + ylab('Coverage') + ggtitle('Coverage per Number of Unigrams')

cumulative_bi <- topfeatures(dfm_bigrams, length(dfm_bigrams))
cumulative_bi <- data.frame(cumulative_bi)
cumulative_bi$n <- c(1:nrow(cumulative_bi))
cumulative_bi$total <- cumsum(cumulative_bi$cumulative_bi)
ggplot(cumulative_bi, aes(x =n, y =total)) + geom_line() + xlab('Number of Bigrams') + ylab('Coverage') + ggtitle('Coverage per Number of Bigrams')

cumulative_tri <- topfeatures(dfm_trigrams, length(dfm_trigrams))
cumulative_tri <- data.frame(cumulative_tri)
cumulative_tri$n <- c(1:nrow(cumulative_tri))
cumulative_tri$total <- cumsum(cumulative_tri$cumulative_tri)
ggplot(cumulative_tri, aes(x =n, y =total)) + geom_line() + xlab('Number of Trigrams') + ylab('Coverage') + ggtitle('Coverage per Number of Trigrams')

## trimming and smoothing the DFMs 

unicorpus %>% 
    data.frame() %>%  
    filter(frequency < 50) %>% 
    group_by(frequency) %>% 
    summarize(n_distinct(feature)) %>% 
    view()

bicorpus %>% 
    data.frame() %>% 
    filter(frequency < 50) %>% 
    group_by(frequency) %>% 
    summarize(n_distinct(feature)) %>% 
    view()

tricorpus %>% 
    data.frame() %>% 
    filter(frequency < 50) %>% 
    group_by(frequency) %>% 
    summarize(n_distinct(feature)) %>% 
    view()

trimmed_uni <- dfm_trim(dfm_unicorpus, min_termfreq = 5)
trimmed_bi <- dfm_trim(dfm_bigrams, min_termfreq = 5)
trimmed_tri <- dfm_trim(dfm_trigrams, min_termfreq = 3)

totals_uni <- colSums(trimmed_uni)
totals_bi <- colSums(trimmed_bi)
totals_tri <- colSums(trimmed_tri)

uni_df <- data.table(word = names(totals_uni), count = totals_uni)
bi_df <- data.table(
    word1 = sapply(strsplit(names(totals_bi), "_", fixed = TRUE), '[[', 1), 
    word2 = sapply(strsplit(names(totals_bi), "_", fixed = TRUE), '[[', 2),
    count = totals_bi
)
tri_df <- data.table(
    word1 = sapply(strsplit(names(totals_tri), "_", fixed = TRUE), '[[', 1), 
    word2 = sapply(strsplit(names(totals_tri), "_", fixed = TRUE), '[[', 2),
    word3 = sapply(strsplit(names(totals_tri), "_", fixed = TRUE), '[[', 3),
    count = totals_tri
)


## TRIGRAMS KNESER-NEY 
## discount = 0.75
tri_df <- tri_df[, D := 0.75]
## nominator of first term
tri_df <- tri_df[, Nom := pmax(count - D, 0)]
## denominator (count of string without final word)
tri_df <- tri_df[, Den := sum(count), 
                 by = list(word2, word1)]
## lambda 
tri_df <- tri_df[, LFinTerm := n_distinct(word3), 
                 by = list(word2, word1)]
## Pcont 
tri_df <- tri_df[, PrevString := n_distinct(word1, word2), 
                 by = list(word3)]
tri_df <- tri_df[, Pcont := PrevString / nrow(tri_df)]
##  kneser-ney 
tri_df <- tri_df[, Probability := (Nom/Den) + 
                     ((D/Den) * LFinTerm) * Pcont]

## BIGRAMS KNESER-NEY 
## discount = 0.75 
bi_df <- bi_df[, D := 0.75]
## nominator of first term (continuation count - how many
## different words word2 can follow)
#test <- test[, CC := n_distinct(word1), by = word2]
bi_df <- bi_df[, Nom := pmax(count - D, 0)]
## denominator (count of word1) 
bi_df <- bi_df[, Den := sum(count), by = word1]
## lambda 
bi_df <- bi_df[, LFinTerm := n_distinct(word2), 
               by = word1]
## Pcont 
bi_df <- bi_df[, PrevString := n_distinct(word1), 
               by = word2]
bi_df <- bi_df[, Pcont := PrevString / nrow(bi_df)]
##  kneser-ney 
bi_df <- bi_df[, Probability := (Nom/Den) + 
                   ((D/Den) * LFinTerm) * Pcont]

## UNIGRAMS

uni_df <- uni_df[, totalcount := sum(count)]
uni_df <- uni_df[, Probability := count/totalcount]

### FUNCTIONS 


prediction <- function(x, y){
    ## no words given, select random word
    if(x == "" & y == ""){
        predict = uni_df[sample(nrow(uni_df), 1), ] %>% 
            select(word)
    ## use trigrams 
    } else if(x %in% tri_df$word1 & 
       y %in% tri_df$word2){
        predict = tri_df %>% 
            filter(word1 %in% x &
                    word2 %in% y) %>% 
            arrange(desc(Probability)) %>% 
            top_n(1, Probability) %>% 
            select(word3)
        ## using bigrams
    }  else if(y %in% bi_df$word1){
        predict = bi_df %>% 
            filter(bi_df$word1 %in% y) %>% 
            arrange(desc(Probability)) %>% 
            top_n(1, Probability) %>% 
            select(word2)
    }  else if(x %in% tri_df$word2 & y == ""){
        predict = tri_df %>% 
            filter(tri_df$word2 %in% x) %>% 
            arrange(desc(Probability)) %>% 
            top_n(1, Probability) %>% 
            select(word3)
    }  else if(x %in% bi_df$word1 & y == ""){
        predict = bi_df %>% 
            filter(bi_df$word1 %in% x) %>% 
            arrange(desc(Probability)) %>% 
            top_n(1, Probability) %>% 
            select(word2)
        ## use probs from unigrams
    } else {
        predict = uni_df %>% 
            arrange(desc(Probability)) %>% 
            top_n(50, Probability) %>% 
            sample_n(size = 1, replace = TRUE) %>% 
            select(word)
    }
    return(predict)
}


save.image(file = "myData.RData")

save(uni_df, bi_df, tri_df, file = "data.Rdata")

load("data.Rdata")