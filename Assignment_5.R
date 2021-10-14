# Task 1:Hierarchical Clustering
library(factoextra)
library(ggsci)
library(ggthemes)
library(dplyr)


# 1
data <- read.csv("D:/bostonuniversity/ad699_data_mining/assignment_5/country-data.csv")

View(data)

# 2a
row.names(data) <- data[,1]
data <- select(data,c(-1))
View(data)
dist <- dist(data, method = "euclidean")
dendrogram_1 <- hclust(dist,method = "average")
fviz_dend(dendrogram_1, cex = 0.3, lwd = 0.7,
         ggtheme = theme_wsj())

fviz_dend(dendrogram_1, cex = 0.3, lwd = 0.7, k=3,
          rect=TRUE, k_colors="jco",rect_border="jco",
          rect_fill=TRUE, ggtheme = theme_wsj())

# 2c
fviz_nbclust(data, FUNcluster = hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
fviz_dend(dendrogram_1, cex = 0.3, lwd = 0.7, k=4,
          rect=TRUE, k_colors="jco",rect_border="jco",
          rect_fill=TRUE, ggtheme = theme_wsj())

clusters_1 <- cutree(dendrogram_1, k=4)
clusters_1
View(clusters_1)

# 2d
data_1 <- data
data_1$cluster <- clusters_1
summary_stats_1 <- data_1 %>% group_by(cluster) %>% summarise_all(c("mean"))
View(summary_stats_1)
impact_sort_1 <- sapply(summary_stats_1[,c(-1)],sd) %>% sort(decreasing=TRUE)
impact_sort_1

# 4
data_norm <- sapply(data, scale) %>% data.frame
row.names(data_norm) <- row.names(data)
data_norm

# 5
fviz_nbclust(data_norm, FUNcluster = hcut, method = "wss") +
  geom_vline(xintercept = 9, linetype = 2)+
  labs(subtitle = "Elbow method")


dist_norm <- dist(data_norm,method = "euclidean")
dendrogram_2 <- hclust(dist_norm,method = "average")

fviz_dend(dendrogram_2, cex = 0.3, lwd = 0.7, k=9,
          rect=TRUE, k_colors="jco",rect_border="jco",
          rect_fill=TRUE, ggtheme = theme_wsj())

View(data_norm)
clusters_2 <- cutree(dendrogram_2,k=9)
View(clusters_2)
data_2 <- data_norm
data_2$cluster <- clusters_2
summary_stats_2 <- data_2 %>% group_by(cluster) %>% summarise_all(c("mean"))
View(summary_stats_2)
impact_sort_2 <- sapply(summary_stats_2[,c(-1)],sd) %>% sort(decreasing=TRUE)
impact_sort_2


# 7

data_3 <- data_norm
data_3$gdpp <- data_3$gdpp * 0.24
data_3$income <- data_3$income * 0.16
data_3$inflation <- data_3$inflation * 0.16
data_3$exports <- data_3$exports * 0.04
data_3$imports <- data_3$imports * 0.04
data_3$health <- data_3$health* 0.24
data_3$total_fer <- data_3$child_mort * 0.04
data_3$child_mort <- data_3$child_mort * 0.04
data_3$life_expec <- data_3$life_expec * 0.04
data_3

# 8a
View(data_3)
fviz_nbclust(data_3, FUNcluster = hcut, method = "wss") +
  geom_vline(xintercept = 9, linetype = 2)+
  labs(subtitle = "Elbow method")

dist_3 <- dist(data_3, method = "euclidean")
dendrogram_3 <- hclust(dist_3,method = "average")

fviz_dend(dendrogram_3, cex = 0.3, lwd = 0.7, k=9,
          rect=TRUE, k_colors="jco",rect_border="jco",
          rect_fill=TRUE, ggtheme = theme_wsj())

clusters_3 <- cutree(dendrogram_3, k=9)
View(clusters_3)

# 8b
data_3$cluster <- clusters_3
summary_stats_3 <- data_3 %>% group_by(cluster) %>% summarise_all(c("mean"))
View(summary_stats_3)
impact_sort_3 <- sapply(summary_stats_3[,c(-1)],sd) %>% sort(decreasing=TRUE)
impact_sort_2
impact_sort_3

# 8c
View(data_3)

# Task2: Text mining
library(gutenbergr)
library(tidytext)
library(tidyverse)
library(textdata)
# 1,2
seed <- 30
book <- gutenberg_download(seed*2)

# 3
class(book)
View(book)

# 4
words <- book %>% unnest_tokens(word, text)
View(words)

# 5a
meaningful_words <- words %>% anti_join(stop_words) 
sorted_words <- meaningful_words %>% count(word, sort = TRUE)
sorted_words[1:10,1:2]

# 5b
word_pairs <- book %>% unnest_tokens(bigram, text, token='ngrams',n=2) %>% drop_na()
word_pairs
top_pairs <- word_pairs %>% count(bigram, sort = TRUE)
top_pairs


# 6a
words_sentiment_count <- sorted_words %>% inner_join(get_sentiments("bing"))
top_sentiment <- words_sentiment_count %>% top_n(10,n)
top_sentiment

# 7

negative_words <- words_sentiment_count %>% filter(sentiment=="negative")
negative_10_words <- negative_words[1:10,1:3]
positive_words <- words_sentiment_count %>% filter(sentiment=="positive")
positive_10_words <- positive_words[1:10,1:3]
sentiment_20_words <- rbind(negative_10_words,positive_10_words)
ggplot(sentiment_20_words, aes(x=reorder(word,n), y=n, fill = sentiment)) + geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  coord_flip() + ggtitle("Positive and Negative Sentiments")




# 8
sentiment_score <- meaningful_words %>% inner_join(get_sentiments("afinn"))
sum(sentiment_score$value)


