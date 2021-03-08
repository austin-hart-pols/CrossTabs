##  Empirical Relationships: CROSS TABULATION
##  Required data: "biopics.xls"

# SETUP ---------------------

# load packages
  library(tidyverse)

# set directory
  setwd("mypath") #adjust to your own directory

# load data
  film = readxl::read_excel("biopics.xls")
  
# ANALYSIS ------------------
  
# full sample tabulation
  tab = 
    film %>%
    count(SubjectSex) %>%
    mutate(Percent = 100 * n/sum(n))
  
  tab
  
  
# bar plot  
  barplot(Percent ~ SubjectSex, data = tab,
          main = "Biopic Stars, 1915-2014", xlab = NULL,
          ylim = c(0,100), col="#69b3a2")
  
  
# Big cross-tab  
  xtab =
    film %>% 
    count(SubjectSex,Period) %>%  
    na.omit() %>%
    pivot_wider(
      names_from = Period,
      values_from = n, 
      values_fill = 0
    ) %>%
    mutate_if(
      is.integer, list(Percent = ~./sum(.) * 100)
    ) %>% 
    select(SubjectSex,sort(tidyselect::peek_vars()))  
  
  xtab
  
  