# a simple sentiment analysis of MSFT Q2 2025 earnings call transcript
# ref: 1) https://www.spglobal.com/marketintelligence/en/news-insights/blog/analyzing-sentiment-in-quarterly-earnings-calls-q2-2022
# 2) https://jagg19.github.io/2019/04/sentiment-analysis-conf-call/
# 3) https://rpubs.com/eR_ic/transfoRmers


# load libraries
library(tidyverse)
library(tidytext)
library(textdata)
library(rvest)
library(ggplot2)
library(scales)
library(wordcloud2)

# 1) "import" data
# scrape Microsoft's q2 2025 earnings call transcript from the motley fool
msft_q2_2025_url <- "https://www.fool.com/earnings/call-transcripts/2025/01/29/microsoft-msft-q2-2025-earnings-call-transcript/"
raw_html <- read_html(msft_q2_2025_url)

# 2) transform data
# turn raw html into a character vector, each element is a paragraph (a html p tag)
transcript <- raw_html %>%
  html_elements(".article-body") %>%
  html_elements("p") %>%
  html_text2()

# 3) build a simple sentiment analysis "model" and plot results
# 3.1) check most frequent words
df_freq <- tibble(paragraph = 1:length(transcript), text = transcript) %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
  
df_freq %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    ggtitle("Top Word Count")


df_freq %>%
  count(word, sort = TRUE, name = "freq") %>%
  top_n(50) %>%
  wordcloud2()

# 3.2) simple sentiment analysis using Loughran-McDonald dictionary
# ref: https://sraf.nd.edu/loughranmcdonald-master-dictionary/
loughran <- get_sentiments("loughran")
df_sentiment <- tibble(paragraph = 1:length(transcript), text = transcript) %>%
  unnest_tokens(word, text) %>%
  inner_join(loughran, by = "word")

df_pn <- df_sentiment %>%
  filter(sentiment == "positive" | sentiment == "negative") %>%
  filter(!(word %in% c("question", "closing")))

# plot top positive word count
df_pn %>% 
  count(word, sentiment) %>%
  filter(sentiment == "positive") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    ggtitle("Top Positive Word Count")

# plot top negative word count
df_pn %>% 
  count(word, sentiment) %>%
  filter(sentiment == "negative") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    ggtitle("Top Negative Word Count")

# Plot percentage of total positive vs negative word count
df_pn %>% 
  count(sentiment) %>%
  mutate(perc = percent(n / sum(n))) %>%
  ggplot(aes(x="", y=n, fill=sentiment)) + 
    geom_col() +
    geom_text(aes(label = perc), position = position_stack(vjust = 0.5)) + 
    coord_polar(theta = "y") +
    ggtitle("Positive vs Negative Word Count") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))

# 3.3) simple sentiment analysis using a pre-trained and fine-tuned transformer model
library(stringr)
library(reticulate) # the reticulate library is an R interface to Python

# modify the below line of code depending on how you
# 1) installed Python; 2) setup your Python
# virtual environment for the keras, tensorflow & transformers package 
# here, the use_condaenv() function specifies which Conda managed Python virtual 
# environment (created on my computer) to use
# more details: https://rstudio.github.io/reticulate/articles/versions.html
use_condaenv("py312", required = TRUE)

# load the transformer library from Python
# note that the Python keras3 library isn't compatible with the transformers library;
# you need the Python tf-keras library (together with tensorflow library)
transformers <- reticulate::import("transformers")

# use a pre-trained model to classify the sentiment of the transcript
# in the addition, the model is further fine-tuned on financial news
# https://huggingface.co/mrm8488/distilroberta-finetuned-financial-news-sentiment-analysis
classifier <- transformers$pipeline(task = "text-classification", model = "mrm8488/distilroberta-finetuned-financial-news-sentiment-analysis")

# only classify those paragraph that's longer than 20 words
transcript_long_sentence <- transcript[str_count(transcript) > 20]

# classify the sentiment of the long paragraphs
output <- classifier(transcript_long_sentence)

# turn the output into a tibble/dataframe
df_output <- tibble(tlong = transcript_long_sentence,
                    pnn = map_chr(output, 1),
                    score = map_dbl(output, 2))

# count positive, negative, and neutral paragraphs
# plot the percentage of positive, negative, and neutral paragraphs
df_output %>% 
  count(pnn) %>%
  mutate(perc = percent(n / sum(n))) %>%
  ggplot(aes(x="", y=n, fill=pnn)) + 
  geom_col() +
  geom_text(aes(label = perc), position = position_stack(vjust = 0.5)) + 
  coord_polar(theta = "y") +
  ggtitle("Positive, Neutral, and Negative Long Paragraphs (>60 words)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


