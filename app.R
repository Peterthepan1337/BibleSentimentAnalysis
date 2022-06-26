install.packages("tidyverse")
install.packages("wordcloud2")
install.packages("textdata")

library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
library(widyr) #Use for pairwise correlation
library(textdata)

library(ggplot2) #Visualizations (also included in the tidyverse package)
library(ggrepel) #`geom_label_repel`
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(circlize) #Visualizations - chord diagram

library(yarrr)
library(radarchart) 
library(ggraph) 
library(wordcloud2) #Wordclouds 

#Color Palette
my_colors <- c("#e0d4ad", "#573329", "#aa9450", "#fdc723", "#a77d02", "#D65E00") #Holy Color Palette

#Custom Theme
theme_bible <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #Put title in the middle
        axis.ticks = aticks, 
        panel.grid.minor = pgminor, 
        legend.title = lt, 
        legend.position = lp) 
}


my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

#### Read in the original data ####
# Downloaded from
# https://www.kaggle.com/datasets/oswinrh/bible?datasetId=1366

bibleOrig <- read.csv("t_web.csv", stringsAsFactors = FALSE)
bibleOrig <- rename(bibleOrig, book = b, chapter = c, text = t)
names(bibleOrig)

glimpse(bibleOrig)

#Combine Chapter Verses into Chapters
bibleChapters <- bibleOrig %>% group_by(book,chapter) %>% summarise(text = paste(text, collapse=" ")) 
glimpse(bibleChapters)#Check if structure is correct
dim(bibleChapters)#Check df dimensions

#### Clean Text ####
# contractions
fix.contractions <- function(doc) {
  
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  doc <- gsub("'s", "", doc)
  return(doc)
}

bibleChapters$text <- sapply(bibleChapters$text, fix.contractions)

# remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
bibleChapters$text <- sapply(bibleChapters$text, removeSpecialChars)

# convert to lowercase
bibleChapters$text <- sapply(bibleChapters$text, tolower)

# Take a look
str(bibleChapters[100, ]$text, nchar.max = 300)
summary(bibleChapters)

# Take out undesired words
undesirable_words <- c("")

bibleWordsFiltered <- bibleChapters %>%
  ungroup() %>%
  unnest_tokens(word, text) %>% #break the text into individual tokens
  anti_join(stop_words) %>% #Take out stop words
  distinct() %>% #remove duplicate words
  filter(!word %in% undesirable_words) %>% #filter out undesired words
  filter(nchar(word) > 3) #remove words smaller than 3 characters

#Word frequency
full_word_count <- bibleChapters %>%
  ungroup() %>% #ungroup bibleChapters
  unnest_tokens(word, text) %>%
  group_by(book, chapter) %>% 
  summarise(num_words = n()) %>%
  arrange(desc(num_words)) 

full_word_count[1:10,] %>%
  ungroup(num_words, book) %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(book = color_tile("lightpink","lightpink")(book)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Books With Highest Word Count") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)

#Look for the top Words
bibleWordsFiltered %>%
  ungroup %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Book Count") +
  ggtitle("Most Frequently Used Words in Bible Books") +
  coord_flip()

head(bibleWordsFiltered)

#Word Cloud Top 50 Words
bibleWordCounts <- bibleWordsFiltered %>%
  ungroup() %>%
  count(word, sort = TRUE) 

wordcloud2(bibleWordCounts[1:50, ], size = .5)

#### TF-IDF ####
popular_tfidf_words <- bibleChapters %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3) %>%
  count(book, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, book, n)

head(popular_tfidf_words)

#Arrange
top_popular_tfidf_words <- popular_tfidf_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>% 
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(book, tf_idf) %>%
  mutate(row = row_number())

#Top 50 TF-IDF Words
wc_top_tfidf <- popular_tfidf_words %>%
  arrange(desc(tf_idf)) %>%
  select(word, tf_idf)

wordcloud2(wc_top_tfidf[1:50, ], size=.5)

#### LEXICON ####

#Overall Positive/Negative/Sentiment Plot 
bible_nrc <- bibleWordsFiltered %>%
  inner_join(get_sentiments("nrc"))

bible_nrc_sub <- bibleWordsFiltered %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))

nrc_plot <- bible_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 20000)) + #Hard code the axis limit
  ggtitle("Bible NRC Sentiment") +
  coord_flip()

plot(nrc_plot)

#Mood Ring by first 30 books
grid.col = c("1" = my_colors[1], "2" = my_colors[2], "anger" = "grey", "anticipation" = "grey", "disgust" = "grey", "fear" = "grey", "joy" = "grey", "sadness" = "grey", "surprise" = "grey", "trust" = "grey")

book_mood <-  bible_nrc %>%
  filter(book>=0 & book<=30 & book != "NA" & !sentiment %in% c("positive", "negative")) %>%
  count(sentiment, book) %>%
  group_by(book, sentiment) %>%
  summarise(sentiment_sum = sum(n)) %>%
  ungroup()

circos.clear()
#Set the gap size
circos.par(gap.after = c(rep(2, length(unique(book_mood[[1]])) - 1), 15,
                         rep(2, length(unique(book_mood[[2]])) - 1), 15))
chordDiagram(book_mood, grid.col = grid.col, transparency = .2)
title("Relationship Between Mood and Book")

#Book number Sentiment analysis
plot_words_bookNr <- bible_nrc %>%
  filter(book == "2") %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(10)) %>%
  ungroup()

plot_words_bookNr %>%
  ggplot(aes(word, 1, label = word, fill = sentiment )) +
  geom_point(color = "transparent") +
  geom_label_repel(force = 1,nudge_y = .5,  
                   direction = "y",
                   box.padding = 0.05,
                   segment.color = "transparent",
                   size = 3) +
  facet_grid(~sentiment) +
  theme_bible() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Book NRC Sentiment") +
  coord_flip()


#Radar Chart

book_sentiment_nrc <- bible_nrc_sub %>%
  group_by(book, sentiment) %>%
  count(book, sentiment) %>%
  select(book, sentiment, sentiment_book_count = n)

total_sentiment_book <- bible_nrc_sub %>%
  count(book) %>%
  select(book, book_total = n)

glimpse(book_sentiment_nrc)

#Join the two and create a percent field
book_radar_chart <- book_sentiment_nrc %>%
  inner_join(total_sentiment_book, by = "book") %>%
  mutate(percent = (sentiment_book_count / book_total) * 100 ) %>%
  filter(book %in% c("40","66","50")) %>%
  select(-sentiment_book_count, -book_total) %>%
  spread(book, percent)
  
chartJSRadar(book_radar_chart, main="NRC Book Radar Chart")
