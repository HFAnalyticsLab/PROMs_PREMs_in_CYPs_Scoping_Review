#Text mining for abstract 

rm(list=ls())

# Setup -------------------------------------------------------------------


library(here)
library(tidyverse)
library(janitor)
library(stringr)
library(tm)
library(tidytext)
library(wordcloud)
library(gridExtra)
library(wordcloud2)
library(textstem)

source(here('for_reference', 'source.R'))
source(here('functions.R'))


# Data load ---------------------------------------------------------------


raw<-read.csv(TIAB_loc)

# clean<-raw %>% 
#   clean_names() %>% 
#   select(covidence, extraction_date, title, abstract)


# Processing abstract -------------------------------------------

#for abstract only 
clean<-raw%>% 
  clean_names() %>%
  select(doc_id=covidence, text=abstract, extraction_date) %>% 
  mutate(text=stripWhitespace(text)) %>% 
  mutate(text=tolower(text)) %>% 
  mutate(text=custom_remove_punctuation_preseve_number(text)) %>% 
  mutate(text=custom_remove_punctuation_everything_else(text)) %>% 
  mutate(text=str_replace_all(text, "quality of life", "quality-of-life")) %>% 
  mutate(text=str_replace_all(text, "patient reported", "patient-reported")) %>% 
  mutate(text=str_replace_all(text, "patient reported outcome", "patient-reported-outcomes")) %>% 
  mutate(text=str_replace_all(text, "patient-reported outcome", "patient-reported-outcomes")) %>% 
  mutate(text=str_replace_all(text, "patient reported experience", "patient-reported-experience")) %>% 
  mutate(text=str_replace_all(text, "patient-reported experience", "patient-reported-experience")) 



# Apply the custom function using dplyr
clean <- clean %>%
  rowwise() %>%
  mutate(text =custom_remove_stopwords(text, stop_words)) %>%
  ungroup()




#Remove pre-specified words 
list_words<-c("pediatric", 
              "peadiatric", 
              # "questionnaire", 
              # "measure", 
              "aim",
              "result", 
              "method", 
              "conclusion", 
              "background", 
              "scores", 
              "copyright", 
              "reserved", 
              "www",
              "pro", 
              "rights", 
              "NULL", 
              # "report", 
              "study", 
              "year", 
              "author",
              "publish", 
              "inc", 
              "objective",
              "introduction") 


pattern <- paste0(paste(list_words, collapse = "|"))

clean <- clean %>%
  rowwise() %>%
  mutate(text =custom_remove_words(text, pattern)) %>%
  ungroup()


clean_df<-clean %>%
  group_by(doc_id) %>%
  mutate(text=str_split(text, pattern=" ")) %>%
  ungroup() 



# Tokenizing ---------------------------------------------------------

df_tokenized<-clean_df %>% 
  unnest(text) %>% 
  mutate(text = sapply(text, remove_standalone_numbers)) %>% 
  filter(text!="") %>% 
  mutate(text=ifelse(grepl("[a-zA-Z]", text), gsub("\\.$", "", text), text)) %>%
  filter(grepl("[A-Za-z-]+", text) & !grepl("\\.", text)) %>%
  mutate(lem=lemmatize_strings(text)) %>% 
  mutate(lem=ifelse(str_detect(text, "electronic"), "electronic", lem)) %>% 
  mutate(lem=ifelse(str_detect(text, "paper|pen-and"), "pen - and - paper", lem)) %>% 
  mutate(lem=ifelse(str_detect(text, "implement"), "implementation",lem)) %>% 
  mutate(lem=ifelse(str_detect(text, "patient-reported-out"), "patient - report - outcome", lem)) %>% 
  mutate(lem=ifelse(str_detect(text, "out - patient"), "outpatient", lem)) %>% 
  mutate(lem=ifelse(str_detect(text,"in - patient"), "inpatient", lem))


#Get rid of some single words

list_words<-letters

df_tokenized<-df_tokenized %>% 
  filter(!(text %in% list_words))  

#count the terms  
df_tokenized_count <- df_tokenized %>%
  select(-c(doc_id, extraction_date)) %>% 
  group_by(text) %>%
  mutate(text_count = n()) %>%
  ungroup() %>%
  group_by(lem) %>%
  mutate(lem_count = n()) %>%
  ungroup() %>% 
  distinct()

# 
# test<-df_tokenized_count %>% 
#   filter(str_detect(text, "patient"))


df_top<-df_tokenized_count %>% 
  select(lem, lem_count) %>% 
  distinct() %>% 
  top_n(50) %>% 
  arrange(desc(lem_count))


df_top %>% 
  mutate(lem = reorder(lem, lem_count)) %>%
  ggplot(aes(lem_count, lem)) +
  geom_col() +
  labs(y = NULL)+
  theme_minimal()


# Wordcloud ---------------------------------------------------------------

#Top 50 words wordcloud 
wordcloud2(df_top, color = "random-dark", backgroundColor = "white")

#edit shape
wordcloud2(df_top, color = "random-dark", backgroundColor = "white", shape="pentagon")


df_top %>% 
  filter(lem=="electronic")

# Look at term ranks per extraction date  ---------------------------------------------

#Count terms by search 
df_tokenized_count_search <- df_tokenized %>%
  select(-doc_id) %>% 
  group_by(extraction_date, text) %>%
  mutate(text_count = n()) %>%
  ungroup() %>%
  group_by(extraction_date,lem) %>%
  mutate(lem_count = n()) %>%
  ungroup() %>% 
  distinct()

#df_tokenized_count_search <- df_tokenized_count_search %>%
# filter(lem!="patient")

original_search<-df_tokenized_count_search %>% 
  select(extraction_date, lem, lem_count) %>% 
  distinct() %>% 
  filter(extraction_date=="Original") %>% 
  arrange(desc(lem_count)) %>% 
  mutate(rank_original=row_number())

update_search<-df_tokenized_count_search %>% 
  select(extraction_date, lem, lem_count) %>% 
  distinct() %>% 
  filter(extraction_date!="Original") %>% 
  arrange(desc(lem_count)) %>% 
  mutate(rank_update=row_number())


#Look at how the ranking of the top terms have changed over the 2 search dates

df<-df_tokenized_count %>% 
  select(lem, lem_count) %>% 
  distinct() %>% 
  top_n(25) %>% 
  arrange(desc(lem_count)) %>% 
  mutate(rank=row_number()) %>% 
  select(lem, rank) %>% 
  left_join(original_search %>%  
              select(lem, original=rank_original), by="lem") %>% 
  left_join(update_search %>%  
              select(lem, new=rank_update), by="lem") %>% 
  mutate(class=case_when(original<new~ "yellow", 
                         original==new~ "grey", 
                         original>new~"blue")) %>% 
  select(-rank) %>% 
  mutate(left_label= paste(lem,original,sep=", "), 
         right_label= paste(lem, new, sep=",")) %>%  
  mutate(check=original-new) %>% 
  # filter(check!=0) %>%
  mutate(new=ifelse(lem=="practice", 35, new)) %>% 
  mutate(new=ifelse(lem=="hrqol", 37, new)) %>% 
  mutate(new=ifelse(lem=="screen", 39, new)) 


palette.colors(palette = "Okabe-Ito")

p <- ggplot(df) + geom_segment(aes(x=1, xend=1.5, y=original, yend=new, col=class), size=.75, show.legend=FALSE) + 
  geom_vline(xintercept=1, linetype="dashed", size=2, alpha=0.5, colour="#56B4E9") + 
  geom_vline(xintercept=1.50, linetype="dashed", size=2, alpha=0.5, colour="#56B4E9") +
  scale_color_manual(labels = c("Up", "Down", "Same"), 
                     values = c("blue"="#0072B2", "yellow"="#F0E442", "grey"="#999999")) +  # color of lines
  labs(x="", y="") +  # Axis labels
  xlim(.5, 2.5) + ylim(40,0)# X and Y axis limits

p

# Add texts
p<-p + geom_text(aes(label=df$left_label, y=df$original, x=rep(1, NROW(df))), hjust=1.1, size=8)+
  geom_text(aes(label=df$right_label, y=df$new, x=rep(1.5, NROW(df))), hjust=-0.05, size=8)+
  geom_text(label="2021 Search", x=0.85, y=2, vjust=1.2, size=8)  + # title
  geom_text(label="2023 Search", x=1.62, y=0.85, vjust=-0.1, size=8)  # title

p<-p + geom_text(aes(x = 1.35, y = 40, label = "*KLIK- PROMs portal used in hospitals in the Netherlands", color = "#CC79A7"),
                 size = 7, vjust=0.6, hjust=-0.2, show.legend = FALSE)
p
# Minify theme
p<-p + theme(panel.background = element_blank(), 
             panel.grid = element_blank(),
             axis.ticks = element_blank(),
             axis.text.x = element_blank(),
             panel.border = element_blank(), 
             axis.text.y= element_blank()
)

p


ggsave("plot.png", plot = p, dpi = 300)



# Further refinement of words --------------------------------------------

#Investigate the list of words and see if it makes sense to remove some of them 

# try<-df_tokenized %>% 
#   filter(lem=="age")

# unique(try$text)
#Not sure about life
#Not sure about analysis but have excluded it for now 

#exclude other words that might not be needed "patients"
# list_words<-c("patient",
#               "use", 
#               "complete",
#               "care", 
#               "group", 
#               "hospital", 
#               "time", 
#               "rate", 
#               "high", 
#               "quality", 
#               "analysis", 
#               "month")
# # 
# 
# 
# df_tokenized_count <- df_tokenized_count %>%
#   filter(!lem %in% list_words)
# p
# 
# 
# ggsave("plot.png", plot = p, dpi = 300)
