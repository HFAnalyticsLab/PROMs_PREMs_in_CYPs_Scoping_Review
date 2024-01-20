
# Data extraction ---------------------------------------------------------
rm(list=ls())

# Libraries ---------------------------------------------------------------

library(here)
library(tidyverse)
library(tidyr)
library(janitor)
library(stringr)
library(gtsummary)
library(readxl)
library(ggh4x)
library(DescTools)
library(THFstyle)

source(here('for_reference', 'source.R'))

source(here('functions.R'))

# Data load ---------------------------------------------------------------

raw<-read_csv(paste0(data_loc,'/raw_data4.csv'))


# Data cleaning -----------------------------------------------------------

raw<-raw %>% 
  clean_names() %>% 
  mutate(collection= case_when(str_detect(collection_method, 
                                          regex("mix", ignore_case = TRUE))~"mixed", 
                               str_detect(collection_method,
                                          regex("phone and", ignore_case = TRUE))~"mixed",
                               str_detect(collection_method,
                                          regex("and electronic", ignore_case = TRUE))~"mixed",
                               str_detect(collection_method, 
                                          regex("pen", ignore_case = TRUE))~"pen_and_ppr",
                               str_detect(collection_method, 
                                          regex("paper", ignore_case = TRUE))~"pen_and_ppr",
                               str_detect(collection_method, 
                                          regex("stated", ignore_case = TRUE))~"not_stated",
                               str_detect(collection_method, 
                                          regex("specified", ignore_case = TRUE))~"not_stated",
                               str_detect(collection_method, 
                                          regex("mention", ignore_case = TRUE))~"not_stated",
                               str_detect(collection_method, 
                                          regex("telephone", ignore_case = TRUE))~"telephone",
                               str_detect(collection_method, 
                                          regex("ePRO", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("electronic", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("elec", ignore_case = TRUE))~"electronic", 
                               str_detect(collection_method, 
                                          regex("web", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("online", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("KLIK", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("ele", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("computer", ignore_case = TRUE))~"electronic",
                               str_detect(collection_method, 
                                          regex("n/a", ignore_case = TRUE))~"not applicable",
                               is.na(collection_method)~ "not applicable",
                               TRUE~"not applicable"))

collection<-raw %>% 
  select(study_id_2, title_3,collection_method, collection )


clean_df<-raw %>% 
  mutate(type=case_when((is.na(prem_s_used_including_tool_details) | str_detect(prem_s_used_including_tool_details, 
                                                                                regex("n/a", ignore_case = TRUE))) 
                        & (!is.na(prom_s_reported_including_tool_details)| 
                             !str_detect(prom_s_reported_including_tool_details,regex("n/a", ignore_case = TRUE)))~ "PROM", 
                        (!is.na(prem_s_used_including_tool_details) | !str_detect(prem_s_used_including_tool_details, 
                                                                                  regex("n/a", ignore_case = TRUE))) 
                        & (is.na(prom_s_reported_including_tool_details)| str_detect(prom_s_reported_including_tool_details,
                                                                                     regex("n/a", ignore_case = TRUE)))~ "PREM", 
                        (!is.na(prem_s_used_including_tool_details) | !str_detect(prem_s_used_including_tool_details, 
                                                                                  regex("n/a", ignore_case = TRUE))) 
                        & (!is.na(prom_s_reported_including_tool_details)| !str_detect(prom_s_reported_including_tool_details,
                                                                                       regex("n/a", ignore_case = TRUE)))~ "PROM_and_PREM")) 


type<-clean_df %>% 
  select(study_id_2, title_3, prom_s_reported_including_tool_details,prem_s_used_including_tool_details, type)

clean_df<-clean_df %>% 
  mutate(participant=case_when(participant_type_s=="Patient"~ "Patient", 
                               participant_type_s=="Proxy"~ "Proxy", 
                               participant_type_s=="Clinician"~ "Clinician", 
                               participant_type_s=="Patient and proxy"~ "Patient and proxy",
                               participant_type_s=="Clinician and proxy"~ "Clinician and proxy",
                               participant_type_s=="Patient, proxy and clinician"~ "Patient, proxy and clinician", 
                               TRUE ~ NA_character_)) 
participant<-clean_df %>% 
  select(study_id_2, title_3, participant_type_s, participants_details_e_g_age_sex_and_number, participant)

clean_df<-clean_df %>% 
  mutate(outpatient=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("outpatient", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(inpatient=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("inpatient", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("wad", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("in and", ignore_case = TRUE))~1,
                             TRUE~0)) %>%
  rowwise() %>% 
  mutate(mixture=sum(across(c(outpatient:inpatient)))) %>% 
  ungroup() %>% 
  mutate(mixture=ifelse(mixture>1,1,0)) %>% 
  mutate(not_stated=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("stated", ignore_case = TRUE))~1,
                              (inpatient==0 & outpatient==0 & mixture==0) ~ 1,
                              TRUE~0)) %>% 
  rowwise() %>% 
  mutate(check_setting=sum(across(c(outpatient:not_stated)))) %>% 
  ungroup() 

setting<-clean_df %>% 
  select(study_id_2, title_3,context_setting_e_g_ward_emergency_care, outpatient, inpatient, mixture, not_stated)
  

clean_df<-clean_df %>%   
  mutate(cardio=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                     regex("cardi", ignore_case = TRUE))~1, 
                          str_detect(context_setting_e_g_ward_emergency_care, 
                                     regex("heart", ignore_case = TRUE))~1,
                          TRUE~0)) %>% 
  mutate(mental_health=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                            regex("mental", ignore_case = TRUE))~1, 
                                 str_detect(context_setting_e_g_ward_emergency_care, 
                                            regex("CAMHS", ignore_case = TRUE))~1,
                                 TRUE~0)) %>% 
  mutate(surgery=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("surgery", ignore_case = TRUE))~1, 
                           TRUE~0)) %>% 
  mutate(oncology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                       regex("oncology", ignore_case = TRUE))~1,
                            str_detect(context_setting_e_g_ward_emergency_care, 
                                       regex("cancer", ignore_case = TRUE))~1,
                            str_detect(context_setting_e_g_ward_emergency_care, 
                                       regex("cance", ignore_case = TRUE))~1,
                            TRUE~0)) %>%  
  mutate(palliative=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("palliative", ignore_case = TRUE))~1,
                              str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("end", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(respiratory=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                          regex("asthma", ignore_case = TRUE))~1,
                               str_detect(context_setting_e_g_ward_emergency_care, 
                                          regex("respiratory", ignore_case = TRUE))~1,
                               str_detect(context_setting_e_g_ward_emergency_care, 
                                          regex("cystic", ignore_case = TRUE))~1,
                               TRUE~0)) %>% 
  mutate(burn=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                   regex("burn", ignore_case = TRUE))~1,
                        TRUE~0)) %>% 
  mutate(dermatology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                          regex("derm", ignore_case = TRUE))~1,
                               TRUE~0)) %>% 
  mutate(psychology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("psycho", ignore_case = TRUE))~1,
                              str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("DBT", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(psychiatry=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("psychi", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(nephrology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("neph", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(neurology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("neuro", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("brain", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("Epili", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("spinal", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("cranio", ignore_case = TRUE))~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        "Epilepsy")~1,
                             str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("TSC", ignore_case = TRUE))~1,
                             TRUE~0)) %>% 
  mutate(rheumatology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("rheu", ignore_case = TRUE))~1,
                                str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("athritis", ignore_case = TRUE))~1, 
                                str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("JIA", ignore_case = TRUE))~1,
                                str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("Systemic Sclerosis", ignore_case = TRUE))~1,
                                TRUE~0)) %>%  
  mutate(haematology =case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("hemo", ignore_case = TRUE))~1,
                                str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("hema", ignore_case = TRUE))~1, 
                                str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("blood", ignore_case = TRUE))~1,
                                str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("sickle", ignore_case = TRUE))~1,
                                TRUE~0)) %>% 
  mutate(gastroenterology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                               regex("gastro", ignore_case = TRUE))~1,
                                    str_detect(context_setting_e_g_ward_emergency_care, 
                                               regex("IBD", ignore_case = TRUE))~1,
                                    TRUE~0)) %>%  
  mutate(obstetrics_neonatology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                                     regex("obs", ignore_case = TRUE))~1,
                                          str_detect(context_setting_e_g_ward_emergency_care, 
                                                     regex("neona", ignore_case = TRUE))~1,
                                          TRUE~0)) %>% 
  mutate(opthalmology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                           regex("Ophthal", ignore_case = TRUE))~1,
                                str_detect(context_setting_e_g_ward_emergency_care,"Opthalmology")~1,
                                TRUE~0)) %>% 
  mutate(endocrinology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                            regex("endo", ignore_case = TRUE))~1,
                                 str_detect(context_setting_e_g_ward_emergency_care, 
                                            regex("diabetes", ignore_case = TRUE))~1,
                                 str_detect(context_setting_e_g_ward_emergency_care, 
                                            regex("adrenal", ignore_case = TRUE))~1,
                                 TRUE~0)) %>% 
  mutate(orthopaedic=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                          regex("ortho", ignore_case = TRUE))~1,
                               TRUE~0)) %>% 
  mutate(immunology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("immu", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(gender_services=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                              regex("gender", ignore_case = TRUE))~1,
                                   TRUE~0)) %>% 
  mutate(pain=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                   regex("pai", ignore_case = TRUE))~1,
                        str_detect(context_setting_e_g_ward_emergency_care,"pain")~1,
                        str_detect(context_setting_e_g_ward_emergency_care, 
                                   regex("pain", ignore_case = TRUE))~1,
                        TRUE~0)) %>% 
  mutate(urology=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("urology", ignore_case = TRUE))~1,
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      "urology")~1,
                           TRUE~0)) %>% 
  mutate(occupational_therapy=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                                   regex("Occupational", ignore_case = TRUE))~1,
                                        TRUE~0)) %>% 
  mutate(transplant=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("transp", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(rehabilitation=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                             regex("rehab", ignore_case = TRUE))~1,
                                  TRUE~0)) %>% 
  mutate(intensive=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                        regex("intensive", ignore_case = TRUE))~1,
                             TRUE~0)) %>% 
  mutate(transition=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                         regex("transition", ignore_case = TRUE))~1,
                              TRUE~0)) %>% 
  mutate(allergy=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("allergy", ignore_case = TRUE))~1,
                           TRUE~0)) %>% 
  mutate(not_applicable=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                             regex("n/a", ignore_case = TRUE))~1,
                                  str_detect(context_setting_e_g_ward_emergency_care, 
                                             regex("NA", ignore_case = FALSE))~1,
                                  is.na(context_setting_e_g_ward_emergency_care)~1,
                                  TRUE~0)) %>% 
  rowwise() %>% 
  mutate(sum_care=sum(across(c(cardio:not_applicable)))) %>% 
  ungroup() %>% 
  mutate(not_stated_speciality=case_when(str_detect(context_setting_e_g_ward_emergency_care, 
                                                    regex("stated", ignore_case = TRUE))~1,
                                         sum_care==0 ~ 1,
                                         TRUE~0)) %>% 
  ungroup() 

speciality<-clean_df %>% 
  select(study_id_2, title_3, context_setting_e_g_ward_emergency_care, cardio:not_stated_speciality)
  
  
clean_df<-clean_df %>%   
  mutate(context=case_when(is.na(context_setting_e_g_ward_emergency_care)~ "not_stated", 
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("n/a", ignore_case = TRUE))~"not_stated",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("network", ignore_case = TRUE))~"registry_network",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("registry", ignore_case = TRUE))~"registry_network",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("centres", ignore_case = TRUE))~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      "^centres$")~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("hospitals", ignore_case = TRUE))~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("clinics", ignore_case = TRUE))~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("trusts", ignore_case = TRUE))~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("units", ignore_case = TRUE))~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("groups", ignore_case = TRUE))~"multicentres",
                           str_detect(context_setting_e_g_ward_emergency_care, 
                                      regex("stated", ignore_case = TRUE))~"not_stated",
                           TRUE~"single_centre")) 

context<-clean_df %>% 
  select(study_id_2, title_3, context_setting_e_g_ward_emergency_care, context)

clean_df<-clean_df %>% 
  mutate(country=ifelse(str_detect(country_in_which_the_study_conducted, "Other: [A-Z]"), gsub("Other: ", "",country_in_which_the_study_conducted),
                        country_in_which_the_study_conducted)) %>% 
  mutate(country=gsub("Other:", "Other", country)) %>% 
  mutate(country=gsub("UK", "United Kingdom", country)) %>% 
  mutate(country=gsub("USA", "United States", country)) %>% 
  mutate(country=gsub("US", "United States", country)) %>% 
  mutate(country=ifelse(str_detect(country, "and "),"multiple_countries", country)) %>% 
  mutate(country=ifelse(is.na(country), "Not stated", country)) %>% 
  mutate(country=gsub("Europe", "multiple_countries", country))

country<-clean_df %>% 
  select(study_id_2, title_3, country_in_which_the_study_conducted, country)



# Results -----------------------------------------------------------------


#Summary statistics
clean_df %>% 
  select(c(collection:not_stated, cardio:not_stated_speciality, context, country)) %>%
  tbl_summary() %>% 
  bold_labels() 

list_files<-list(collection=collection, type=type, participant=participant, setting=setting, speciality=speciality, context=context,country=country)

write_xlsx(list_files, path = here('results', 'table1.xlsx'))

klik<-clean_df %>% 
  filter(str_detect(collection_method,  regex("KLIK", ignore_case = TRUE)))
#23 studies that mentioned KLIK 


#Full list of studies (in supplementary)
demog<-clean_df %>% 
  mutate(covidence_number=as.character(covidence_number)) %>% 
  select(covidence_number, author=study_id_2, title=title_3,year_of_publication, type, country,
         collection,context,participant, study_design, participants_details_e_g_age_sex_and_number, prom_s_reported_including_tool_details, 
         prem_s_used_including_tool_details,
         cardio:not_stated_speciality) %>%
  select(-sum_care) %>%
  pivot_longer(cardio:not_stated_speciality, names_to="speciality", values_to="count") %>% 
  group_by(covidence_number, author, type, country,
           collection,context,participant, study_design, participants_details_e_g_age_sex_and_number, prom_s_reported_including_tool_details, 
           prem_s_used_including_tool_details) %>% 
  filter(count>0) %>% 
  summarise(speciality=paste(speciality, collapse=",")) %>% 
  mutate_at(vars(!starts_with("t")), list(clean_up)) %>% 
  mutate_at(vars(!starts_with("t")), list(title)) 

#Figure 1- Year of publication and method of collection 

t<-clean_df %>%
  group_by(year_of_publication, collection) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from=collection, values_from=count, values_fill = 0) %>% 
  mutate(total=pen_and_ppr+not_stated+electronic+`not applicable`+telephone+mixed) %>% 
  pivot_longer(c(pen_and_ppr:total), names_to = "type", values_to="count") %>%
  ungroup() 


cum_sum<-t %>% 
  filter(type=="total") %>% 
  mutate(cum_sum = cumsum(count))

palette <- pal_THF_cont

t<-t %>% 
  left_join(cum_sum) %>% 
  mutate(year_lab=ifelse(year_of_publication=="2023", "Up to April 2023", year_of_publication)) %>% 
  mutate(type_lab=ifelse(type=="pen_and_ppr", "pen and paper", ifelse(type=="not_stated", "not stated", type))) %>% 
  mutate(type_lab=title(type_lab))




plot<-ggplot(t, aes(x = year_of_publication, y = count)) +
  scale_x_continuous(breaks=unique(t$year_of_publication), labels = unique(t$year_lab))+
  geom_bar(data = filter(t, type != "total"), aes(fill = type_lab), stat = "identity") +
  geom_text(data = filter(t, type == "total" & year_of_publication>2013), aes(label = count), vjust = -0.5, hjust = 0.5)+
  geom_line(data = filter(t, type == "total"), aes(y=cum_sum), size = 0.5, type=5) +
  geom_text(data = filter(t, type == "total"),aes(y=cum_sum, label=cum_sum, vjust=-0.5, hjust=0.7))+
  scale_fill_manual(values = palette)+
  labs(x="Year of publication", y="Count", caption="Line chart showing the cumulative total publications over time and a bar chart showing the number of publications by year and type of collection. 
  This only includes publications up to April 2023 only. Not applicable represents studies exploring views, barriers and facilitators.") +
  theme_minimal()+
  theme(panel.grid = element_blank(),
        plot.background = element_rect(colour = 'white'),
        panel.border = element_rect(color = "black", fill = "NA"), 
        axis.text.x =  element_text(size = 11, angle=45, hjust=1), 
        axis.text.y =  element_text(size = 11) , # Change the font size for axis text
        axis.title = element_text(size = 14),  # Change the font size for axis titles
        legend.text = element_text(size = 10),  # Change the font size for legend text
        legend.title = element_text(size = 12), 
        plot.caption = element_text(size=10, hjust=-0.1))+  # Change the font size for legend title
  guides(fill = guide_legend(title = "Type of collection"))


plot
ggsave(here('results', 'plot_1.png'), plot, dpi=300,width = 10, height =6.5)




