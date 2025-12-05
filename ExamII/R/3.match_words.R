#Function 2 - match_words
## Goal:Compare the words from the text to a list of positive or negative words

  match_words_simple <- function(words, sentiment_words) {

#Step1:Create an empty vector to store all words that match the list
    matched_words <- c()

#Step2: Check each word in sentiment list
    for (s_word in sentiment_words) {

#Step 3: Check if the word ends with * and match any word with that prefix
      if (grepl("\\*$", s_word)) {
    # Remove the * to get the prefix
        prefix <- gsub("\\*", "", s_word)
    # Find all words in the text that start with this prefix
        matches <- words[startsWith(words, prefix)]
      } else {
    #Otherwise, only match exact words
        matches <- words[words == s_word]
      }
#Step 4: Add all matches found
      matched_words <- c(matched_words, matches)
    }

    return(matched_words)
  }
