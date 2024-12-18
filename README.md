# PROMs_PREMs_in_CYPs_Scoping_Review
#### Project Status: [Published](https://systematicreviewsjournal.biomedcentral.com/articles/10.1186/s13643-024-02706-x)

## Project Description
Patient-reported outcome measures (PROMs) are questionnaires measuring patients’ views of their health status whereas patient-reported experience measures (PREMs) are questionnaires measuring patients’ perceptions of their experience whilst receiving healthcare. PROMs and PREMs have the potential to involve children and young people (CYP) with decisions about their care and improve the quality of their care but it is not clear how often PROMs/PREMs are incorporated as part of standard care of CYP in hospitals. 

We conducted a scoping review to understand the extent of the literature and map the available evidence on the use, benefits and barriers and facilitators of PROMs/PREMs as part of standard care and treatment of CYP in hospital.

Search terms were developed and seven databases searched. The initial search was from 1.1.2008-21.2.2021 and updated the search from 22.2.2021-4.4.2023. Data were analysed thematically in relation to the research questions and will be reported in a peer-review journal. 

To keep track of changes in the literature we also explored text mining methods to characterise frequently occuring terms in the included abstracts between the initial and updated search. 

## Data source
 * Ovid (Emcare, Embase MEDLINE, APA PsychInfo), Scopus and Web of Science were searched.
 * Manual reviews were initially performed in Covidence and using the elegibility criteria.
 * 172 studdies were included in the final review. 

## How does it work?
To get started you will need to create a **for_reference** folder and then include a **source.R**  that states your file paths. 

* functions.R- includes functions to clean your abstract, this includes removing punctuation, preserving hyphens, keeping numbers within a hyphen etc.  

**Part 1- Text mining analysis to analyse frequently occurring terms in abstracts and explore how this has changed between the initial and updated search.**  

*Data*

* Abstracts of included studies and a flag if it was identified in the initial or updated search.

*Script*

* 01_text_mining- this processes data, identifies top 50 frequently occurring terms, the ranking of the top 25 terms in the initial and updated search and how this has changed across the two periods. 

**Part 2- Processing of the extracted data for the analysis of the scoping review**

*Data*

* Extracted data from the included studies based on the extraction table. The extraction was done in covidence. 
* Also requires data from [mural](https://app.mural.co/t/promspremsscopingreview1752/m/promspremsscopingreview1752/1691762102261/a248b97f0fd34ecc1479cd5e532c4fa9e7e47fab?sender=ua702a75d65081b93fc770599) which shows each studies that belong in the specific themes.

*Script*

* 02_data_extraction_and_desciriptive_table- Data cleaning for the extracted data and includes compiling descriptive table for the reuslts. 
* 03_themes_data_extraction_and_tables- Data cleaning fro the themes from mural and creating summary and tally tables for the results. This also extracts the references for the specific themes. 

### Requirements 

These scripts were written in R version 4.0.5 and RStudio Version 1.1.383.

The following R packages (available on CRAN) are needed: 
* [**tidyverse**](https://www.tidyverse.org/)
* [**janitor**](https://cran.r-project.org/web/packages/janitor/vignettes/janitor.html)
* [**ggplot2**](https://ggplot2.tidyverse.org/)
* [**tm**](https://cran.r-project.org/web/packages/tm/index.html)
* [**textstem**](https://cran.r-project.org/web/packages/textstem/textstem.pdf)
* [**wordcloud2**](https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html)
* [**tidytext**](https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html)

## Code Authors

* Anne Alarilla - [Twitter](https://twitter.com/alarillaanne)

## License
