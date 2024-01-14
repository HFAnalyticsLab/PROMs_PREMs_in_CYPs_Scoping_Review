#Functions

library(stopwords)

#Remove custom words 
custom_remove_words<-function(text, pattern) {
  text <- unlist(strsplit(text, " ")) # Split text into words
  text <- text[!str_detect(text, pattern)] # Remove words matching the pattern
  text <- paste(text, collapse = " ") # Reconstruct the text
  return(text)
}


#remove punctuation unless the period is next to a number 
custom_remove_punctuation_everything_else <- function(text) {
  # Define a regular expression to match and remove periods (.) unless they are next to numbers
  pattern <- "(?<![0-9])\\.(?![0-9])|([^A-Za-z0-9.-])"
  # Use gsub to replace the matched characters with spaces
  cleaned_text <- gsub(pattern, " ", text, perl = TRUE)
  return(cleaned_text)
}


#remove punctuation unless it's next to a hyphen or a number 
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


#Specifying stop words 
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


#Removing numbers without hyphens 
remove_standalone_numbers <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  words <- Filter(function(word) !grepl("^\\d+$", word), words)
  cleaned_text <- paste(words, collapse = " ")
  return(cleaned_text)
}


#make text lower case and change _ into a space
clean_up<-function(text=type){
  new_text<-str_replace_all(tolower(text), "_", " ")
}


#Make the first letter capital 
title<-function(text=type){
  new_text<-StrCap(text)
}

# Define a function to remove duplicates within a cell
remove_duplicates_within_cell <- function(cell_text) {
  words <- unlist(strsplit(cell_text, ","))
  unique_words <- unique(words)
  return(paste(unique_words, collapse = ", "))
}



#Extract cleaning from mural data for those with only one level 
extract_cleaning_one_level<-function (raw=data_applied){
  
  df<-raw %>% 
    clean_names()
  
  colnames <-df[1,] %>% 
    t() %>% 
    as.data.frame()
  
    colnames<-colnames %>% 
      mutate(cat=row.names(colnames)) %>% 
      rename(group=V1) %>% 
      mutate(cat=gsub("[0-9]","",cat)) 
    
    rownames(colnames)<-NULL
    
    tab_df<-df %>% 
      row_to_names(1) %>% 
      pivot_longer(everything(),names_to="group", values_to="desc") %>% 
      separate(desc, into=c("study_id", "desc"), sep="]-") %>% 
      mutate(study_id=(gsub("\\[|\\]", "", study_id))) %>%
      mutate(study_id=(gsub("['\"]", "", study_id))) %>% 
      filter(!is.na(study_id)) %>% 
      left_join(colnames)
    
    
    print(tab_df)
    
    table_findings<-tab_df %>% 
      left_join(colnames) %>% 
      group_by(study_id) %>% 
      # mutate(desc=paste(desc,collapse=".")) %>% 
      distinct(group, cat, desc, .keep_all = TRUE) %>% 
      mutate_at(vars(!starts_with(c("a", "d"))), list(clean_up)) %>% 
      mutate_at(vars(!starts_with(c("a","d"))), list(title)) %>% 
      group_by(study_id) %>% 
      mutate(group=paste(group, collapse= ","), 
             cat=paste(cat, collapse=",")) %>% 
      distinct(study_id, .keep_all = TRUE) %>% 
      left_join(demog,   by=c("study_id"="covidence_number"))  %>% 
      mutate_if(is.character, funs(remove_duplicates_within_cell)) %>% 
      ungroup() %>% 
      select(author, type, collection, country, speciality, cat, group, desc) 
    print(table_findings)
    
    # Save the result into the global environment
    assign("table_findings", table_findings, envir = .GlobalEnv)
    
    
}

#extract cleaning from mural data for those with two levels (those with barriers and facilitators)
extract_cleaning_twolevels<-function(raw=barriers) {
  df<-raw %>% 
    clean_names()
  
  colnames <-df[1:2,] %>% 
    t() %>% 
    as.data.frame()
  
  colnames<-colnames %>% 
    mutate(cat=row.names(colnames)) %>% 
    rename(group=V1, lowergroup=V2) %>% 
    mutate(cat=gsub("[0-9]","",cat)) 
  
  rownames(colnames)<-NULL
  
  tab_df<-df %>% 
    row_to_names(2) %>% 
    pivot_longer(everything(),names_to="lowergroup", values_to="desc") %>% 
    separate(desc, into=c("study_id", "desc"), sep="]-") %>% 
    mutate(study_id=(gsub("\\[|\\]", "", study_id))) %>%
    mutate(study_id=(gsub("['\"]", "", study_id))) %>% 
    filter(!is.na(study_id))
  
  
  print(tab_df)
  
  table_findings<-tab_df %>% 
    left_join(colnames) %>% 
    group_by(study_id) %>% 
    mutate(desc=paste(desc,collapse=".")) %>% 
    distinct(group, cat, desc, .keep_all = TRUE) %>% 
    mutate_at(vars(!starts_with(c("a", "d"))), list(clean_up)) %>% 
    mutate_at(vars(!starts_with(c("a","d"))), list(title)) %>% 
    group_by(study_id) %>% 
    mutate(lowergroup=paste(lowergroup, collapse = ","),
           group=paste(group, collapse= ","), 
           cat=paste(cat, collapse=",")) %>% 
    distinct(study_id, .keep_all = TRUE) %>% 
    left_join(demog,   by=c("study_id"="covidence_number"))  %>% 
    mutate_if(is.character, funs(remove_duplicates_within_cell)) %>% 
    ungroup() %>% 
    select(author, type, collection, country, speciality, cat, group, lowergroup, desc) 
  
  print(table_findings)
  
  # Save the result into the global environment
  assign("table_findings", table_findings, envir = .GlobalEnv)
  
  
}

# start landscape
start.landscape=function(doc){
  doc=body_end_section_continuous(doc)
  return("landscape orientation started")
}

# end landscape
end.landscape=function(doc){
  doc=body_end_section_landscape(doc)
  return("landscape orientation ended")
}

split_into_sentences <- function(text) {
  # Define a regular expression pattern for sentence splitting
  sentence_pattern <- "\\s*[.!?]\\s*"
  
  # Split the text into sentences using the pattern
  sentences <- unlist(strsplit(text, sentence_pattern))
  
  # Remove empty sentences
  sentences <- sentences[sentences != ""]
  
  return(sentences)
}

#Colours

THF_red <- '#dd0031'
THF_50pct_light_blue <- '#53a9cd'

# Secondary palette
THF_1_purple <- '#744284'
THF_2_yellow <- '#ffd412'
THF_3_teal <- '#2a7979'
THF_4_coral <- '#ee9b90'
THF_5_darkgreen <- '#0c402b'
THF_6_turquoise <- '#a6d7d3'
THF_7_blue <- '#005078'
THF_8_orange <- '#f39214'
THF_9_green <- '#2ca365'

# Tertiary palette
# to show changes of scale within data category
THF_75pct_rose <- '#ee7375'
THF_50pct_rose <- '#f2a0a2'

THF_75pct_light_blue <- '#7fbfda'

pal_THF_cont <- c(THF_red, THF_50pct_rose, THF_75pct_rose, THF_50pct_light_blue, THF_75pct_light_blue, THF_7_blue)

#table from mural 

#table from mural 

tally_one_level<-function (raw=data_applied){
  
  df<-raw() %>% 
    clean_names()
  
  colnames <-df[1,] %>% 
    t() %>% 
    as.data.frame()
  
  colnames<-colnames %>% 
    mutate(cat=row.names(colnames)) %>% 
    rename(group=V1) %>% 
    mutate(cat=gsub("[0-9]","",cat)) 
  
  rownames(colnames)<-NULL
  
  tab_df<-df %>% 
    row_to_names(1) %>% 
    pivot_longer(everything(),names_to="group", values_to="desc") %>% 
    separate(desc, into=c("study_id", "desc"), sep="]-") %>% 
    mutate(study_id=(gsub("\\[|\\]", "", study_id))) %>%
    mutate(study_id=(gsub("['\"]", "", study_id))) %>% 
    filter(!is.na(study_id)) %>% 
    left_join(colnames) %>% 
    select(study_id, cat, group) %>% 
    group_by(study_id, cat) %>% 
    mutate(dups_cat=cumsum(n())) %>% 
    left_join (demog %>% 
                 ungroup() %>% 
                 select(study_id=covidence_number, type)) %>% 
    ungroup()
  
  # tally_findings<-tab_df %>%
  #   filter(dups==0) %>%
  #   select(cat, type) %>%
  #   group_by(cat, type) %>%
  #   summarise(n())

  
  tally_findings<-tab_df %>%
    filter(dups_cat<2) %>% 
    select(cat, type) %>% 
    tbl_summary(by=type) %>% 
    add_overall() %>% 
    as_tibble()
  
  # Save the result into the global environment
  assign("tally_findings", tally_findings, envir = .GlobalEnv)
  
}

references_one_level<-function(raw=results){
  
  df<-raw %>% 
    clean_names()
  
  
  df<-results %>% 
    clean_names()
  
  
  colnames <-df[1,] %>% 
    t() %>% 
    as.data.frame()
  
  colnames<-colnames %>% 
    mutate(cat=row.names(colnames)) %>% 
    rename(group=V1) %>% 
    mutate(cat=gsub("[0-9]","",cat)) 
  
  rownames(colnames)<-NULL
  
  tab_df<-df %>% 
    row_to_names(1) %>% 
    pivot_longer(everything(),names_to="group", values_to="desc") %>% 
    separate(desc, into=c("study_id", "desc"), sep="]-") %>% 
    mutate(study_id=(gsub("\\[|\\]", "", study_id))) %>%
    mutate(study_id=(gsub("['\"]", "", study_id))) %>% 
    filter(!is.na(study_id)) %>% 
    left_join(colnames) %>% 
    select(study_id, cat, group) %>% 
    group_by(study_id, cat, group) %>% 
    mutate(dups_groups=n()) %>% 
    filter(dups_groups<2) %>% 
    ungroup() %>% 
    group_by(study_id, cat) %>% 
    mutate(dups_cat=n()) %>% 
    #filter(dups_cat<2) %>% 
    ungroup %>% 
    left_join (demog %>% 
                 ungroup() %>% 
                 select(study_id=covidence_number, author, type)) %>% 
    ungroup()
  
  assign("tab_df", tab_df, envir = .GlobalEnv)
  
}


references_two_level<-function(raw=barriers){
  
  df<-raw %>% 
    clean_names()
  
  colnames <-df[1:2,] %>% 
    t() %>% 
    as.data.frame()
  
  colnames<-colnames %>% 
    mutate(cat=row.names(colnames)) %>% 
    rename(group=V1, lowergroup=V2) %>% 
    mutate(cat=gsub("[0-9]","",cat)) 
  
  rownames(colnames)<-NULL
  
  tab_df<-df %>% 
    row_to_names(2) %>% 
    pivot_longer(everything(),names_to="lowergroup", values_to="desc") %>% 
    separate(desc, into=c("study_id", "desc"), sep="]-") %>% 
    mutate(study_id=(gsub("\\[|\\]", "", study_id))) %>%
    mutate(study_id=(gsub("['\"]", "", study_id))) %>% 
    filter(!is.na(study_id)) %>% 
    left_join(colnames) %>% 
    select(study_id, cat, group, lowergroup) %>% 
    group_by(study_id, cat, group, lowergroup) %>% 
    mutate(dups_lowergroups=ifelse(n()>1, 1,0)) %>% 
    ungroup() %>% 
    group_by(study_id, cat, group) %>%
    mutate(dups_groups=ifelse(n()>1,1,0)) %>% 
    ungroup() %>% 
    group_by(study_id, cat) %>% 
    mutate(dups_cat=ifelse(n()>1,1,0)) %>% 
    ungroup %>% 
    filter(dups_lowergroups==0) %>% 
    distinct() %>% 
    left_join (demog %>% 
                 ungroup() %>% 
                 select(study_id=covidence_number, author)) %>% 
    ungroup()
  
  assign("tab_df", tab_df, envir = .GlobalEnv)
  
}

