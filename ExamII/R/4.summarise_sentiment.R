#Function 3 - summarise_sentiment
## Goal: Count positive and negative words and calculate a sentiment ratio.

summarise_sentiment <- function(file_path) {

#Step 1: Preprocess text
  words <- preprocess_text(file_path)

#Step 2:Count positive and negative words in the text
  pos_count <- length(match_words_simple(words, positive_words))
  neg_count <- length(match_words_simple(words, negative_words))

#Step 3: Calculate sentiment ratio
  ratio <- pos_count / (neg_count + 1)

#Step 4: Return results as a list
  result <- list(
    positive = pos_count,
    negative = neg_count,
    sentiment_ratio = ratio,
    positive_words = pos_matches,
    negative_words = neg_matches
  )

  return(result)
}
