#Function1 - preprocess_text
## Goal: Reads a text file, converts to lowercase, removes punctuation, and splits into words

preprocess_text <- function(file_path) {

#Step 1: Read the text as a data frame
    text_data <- read.delim(file_path, header = FALSE, stringsAsFactors = FALSE)

#Step 2: Combines all columns of each row into a single string
    text_combined <- apply(text_data, 1, paste, collapse = " ")

#Step 3: Converts all letters to lowercase
    text_lower <- tolower(text_combined)

#Step 4: Splits each sentence into individual words
    words_list <- unlist(strsplit(text_lower, split = " "))

#Step 5: Removes punctuation
    words_clean <- gsub("[.,;!?]", "", words_list)

    return(words_clean)
  }
