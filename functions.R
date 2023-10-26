#Functions

#Functions
custom_remove_words<-function(text, pattern) {
  text <- unlist(strsplit(text, " ")) # Split text into words
  text <- text[!str_detect(text, pattern)] # Remove words matching the pattern
  text <- paste(text, collapse = " ") # Reconstruct the text
  return(text)
}


custom_remove_punctuation_everything_else <- function(text) {
  # Define a regular expression to match and remove periods (.) unless they are next to numbers
  pattern <- "(?<![0-9])\\.(?![0-9])|([^A-Za-z0-9.-])"
  # Use gsub to replace the matched characters with spaces
  cleaned_text <- gsub(pattern, " ", text, perl = TRUE)
  return(cleaned_text)
}

custom_remove_punctuation_preseve_number<- function(text) {
  # Define a regular expression to match and remove punctuation but preserve hyphens and numbers
  pattern <- "([^A-Za-z0-9.-])"
  # Use gsub to replace the matched characters with spaces
  cleaned_text <- gsub(pattern, " ", text)
  return(cleaned_text)
}

## create custom function to remove other misc characters
text_preprocessing<- function(x)
{gsub('http\\S+\\s*','',x) # remove URLs
  gsub('wwww\\S+\\s*','',x) # remove URLs
  gsub('#\\S+','',x) # remove hashtags
  gsub('[[:cntrl:]]','',x) # remove controls and special characters
  gsub("^[[:space:]]*","",x) # remove leading whitespaces
  gsub("[[:space:]]*$","",x) # remove trailing whitespaces
  gsub(' +', ' ', x) # remove extra whitespaces
}

stop_words<-stopwords("english")

# Custom function to remove stopwords unless they are part of hyphenated words
custom_remove_stopwords <- function(text, stopwords) {
  words <- unlist(str_split(text, "\\s+"))
  cleaned_words <- character(0)
  
  for (word in words) {
    if (str_detect(word, "-")) {
      # If the word contains a hyphen, keep it
      cleaned_words <- c(cleaned_words, word)
    } else if (!(word %in% stopwords)) {
      # If the word is not a stop word, keep it
      cleaned_words <- c(cleaned_words, word)
    }
  }
  
  # Reconstruct the text
  cleaned_text <- paste(cleaned_words, collapse = " ")
  return(cleaned_text)
}

remove_standalone_numbers <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  words <- Filter(function(word) !grepl("^\\d+$", word), words)
  cleaned_text <- paste(words, collapse = " ")
  return(cleaned_text)
}

