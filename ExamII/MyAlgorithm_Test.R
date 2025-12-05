## 1. Function1 (Preprocess text)
preprocess_text <- function(file_path) {
text_data <- read.delim(file_path, header = FALSE, stringsAsFactors = FALSE)
text_combined <- apply(text_data, 1, paste, collapse = " ")
text_lower <- tolower(text_combined)
words_list <- unlist(strsplit(text_lower, split = " "))
words_clean <- gsub("[.,;!?]", "", words_list)
return(words_clean)}

# Run function1
words <- preprocess_text("Example_negative.txt")
head(words,20)

## 2.Function2 (Match Words)
match_words_simple <- function(words, sentiment_words) {
  matched_words <- c()
  for (s_word in sentiment_words) {
    if (grepl("\\*$", s_word)) {
      prefix <- gsub("\\*", "", s_word)
      matches <- words[startsWith(words, prefix)]
    } else {
      matches <- words[words == s_word]
    }
    matched_words <- c(matched_words, matches)
  }
  return(matched_words)}

#Positive/negative words
positive_words <- c("hope","help")
negative_words <- c("fatigue", "heavy", "barely", "nause*",

                                      "frustrat*", "trap*", "restless", "sick","overwhelm*")
#Run function2
pos_matches <- match_words_simple(words,positive_words)
neg_matches <- match_words_simple(words, negative_words)

cat("Positive words found:", paste(pos_matches, collapse = ", "), "\n")
cat("Negative words found:", paste(neg_matches, collapse = ", "), "\n")


## 3.Function 3 (Summary Sentiment)
summarise_sentiment <- function(file_path) {
words <- preprocess_text(file_path)

pos_count <- length(match_words_simple(words, positive_words))
neg_count <- length(match_words_simple(words, negative_words))

ratio <- pos_count / (neg_count + 1)

result <- list(
    positive = pos_count,
    negative = neg_count,
    sentiment_ratio = ratio,
    positive_words = pos_matches,
    negative_words = neg_matches
)
  return(result)
}

#Run function3
summary_result <- summarise_sentiment("Example_negative.txt")
summary_result

