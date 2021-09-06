# Proportion Flyers

library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(circular)
library(ggplot2)
library(rstudioapi)
library(NISTunits)

# Main database
prop_flyers_main <- read.csv("C:/Users/17196/Box/Monarch Data/R_Projects/Flightmill_Study/prop_flyers.csv")
   View(prop_flyers_main)


# Group by distance Cant figure out how to get the damn df into the right format

# ATTEMPT
# Need to first aggregate flyers and non flyers for each distance
# Then calculate the proportion flyers to non-flyers
   

  
prop_flyers<- prop_flyers_main %>%
     group_by(Distance)%>%
      summarize(Flyers = sum(Flyer=="Y"),
                nonflyers= sum(Flyer=="N"))
   
# Was the proportion of flyers to non-flyers significant by distance?  
   chisq.test(prop_flyers[,2:3]) 
#	Pearson's Chi-squared test
#   data:  prop_flyers[, 2:3]
#   X-squared = 3.1086, df = 3, p-value = 0.3752   
# statistically not sig, but there are almost 2X as many non-flyers as flyers
# at each distance.
 
# Total flyers by distance     
  flyers_by_sex<- prop_flyers_main %>%
  filter(prop_flyers_main$Flyer=="Y") %>%
# 109 total flyers of 326 = 33.4%  
  group_by(Distance)%>%
  summarize(total_f_fly = sum(Sex=="F"),
            total_m_fly = sum(Sex=="M"))

#  Is there a difference in the proportion of male to female flyers?  
chisq.test(flyers_by_sex[,2:3]) 
#Pearson's Chi-squared test
#data:  total_flyers[, 2:3]
#X-squared = 1.9355, df = 3, p-value = 0.585


# Below is a roundabout version of the above, but confirms data is correct
# Flyers by sex    
   unique(prop_flyers_main$Sex)
   
    # 42% females (136/321)
   females<- prop_flyers_main %>%
     filter(prop_flyers_main$Sex=="F")
   F.flyers<-sum(females$Flyer=="Y")
   #51 of 136 total females flew = 37.5%
   
   # 57% males (185/321)
   males<- prop_flyers_main %>%
     filter(prop_flyers_main$Sex=="M")
   M.flyers<-sum(males$Flyer=="Y")
   # 58 of 185 total males flew = 31.1%

# Proportion of male to female flyers by distance
#0m
nores_flyers<- total_flyers %>%
  filter(total_flyers$Distance=="0")
sum(nores_flyers$Sex=="M")
# 13 males
# 7 females

# 3m
threem_flyers<- total_flyers %>%
  filter(total_flyers$Distance=="3")
sum(threem_flyers$Sex=="M") 
# 13 males
# 16 females

#10m   
tenm_flyers<- total_flyers %>%
  filter(total_flyers$Distance=="10")
sum(tenm_flyers$Sex=="M")
# 16 males
# 14 females

#25m
twofivem_flyers<- total_flyers %>%
  filter(total_flyers$Distance=="25")
sum(twofivem_flyers$Sex=="M")
# 16 males
# 14 females
   
  
   
   
   
# Truncated table of flyers by distance 
table_prop_flyers <- read.csv("C:/Users/17196/Box/Monarch Data/R_Projects/Flightmill_Study/Table_prop_flyers.csv")
   View(table_prop_flyers)
   
nonflyers<- table_prop_flyers %>%
  mutate(non_flyers = Total.butterflies - Flyers)

summary_data<- nonflyers %>%
  group_by(Distance)%>%
  summarize(total_flyers = sum(Flyers),
            total_nonflyers = sum(non_flyers))

chisq.test(summary_data[,2:3])

#Pearson's Chi-squared test
#data:  summary_data[, 2:3]
#X-squared = 2.4106, df = 3, p-value = 0.4917
