# mixed-model

---
output:
  pdf_document: default
  html_document: default
  word_document: default
---
#library(lme4) # for the analysis
#library(haven) # to load the SPSS .sav file
#library(tidyverse) # needed for data manipulation.
#library(RColorBrewer) # needed for some extra colours in one of the graphs
#library(lmerTest)# to get p-value estimations that are not part of the standard lme4 packages

#read.csv("Milad325.csv")
#head(Milad325)

#str(Milad325)
library(car)
require(car)
library(MASS)
# This is so that distributions that must be non-zero can make sense of my data
require(MASS)
#library(EBSeq)
#qqp(Milad325$Total_agg_ST, "norm")


#qqp(Milad325$Total_agg_ST.t, "lnorm")


#nbinom <- fitdistr(Milad325$Total_agg_ST.t, "Negative Binomial")
#qqp(Milad325$Total_agg_ST.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])


#poisson <- fitdistr(Milad325$Total_agg_ST.t, "Poisson")
#qqp(Milad325$Total_agg_ST.t, "pois", poisson$estimate)


#gamma <- fitdistr(Milad325$Total_agg_ST.t, "gamma")
#qqp(Milad325$Total_agg_ST.t, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

library(ggplot2)
library(magrittr)
library(dplyr)

library(tidytext)

library(stringr) # to deal with strings
library(wordcloud) # to render wordclouds
library(knitr) # for tables
library(DT) # for dynamic tables
library(tidyr)

ggplot(data  = Milad325,
       aes(x = GPA,
           y = Total_agg_ST,
           col = study_major,
           group = study_major))+
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter")+# to add some random noise for plotting purposes
             
              geom_smooth(method = lm,
              se     = FALSE, 
              col    = "black",
              size   = .5, 
              alpha  = .8)+ # to add regression line
             
  theme_minimal()+
  
  theme(legend.position = "none")+
  scale_color_gradientn(colours = rainbow(100))+
  
    geom_smooth(method = lm,
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+ # to add regression line
  
  labs(title = "ST and Major of Study", subtitle = "add colours for different classes and regression lines")

# To colour code the extremes, we need to write a small function that calculates the regression lines and adds a collumn indicating which clusters have the most extreme.
f1 <- function(data, x, y, grouping, n.highest = 3, n.lowest = 3){
  groupinglevel <- data[,grouping]
  res           <- data.frame(coef = rep(NA, length(unique(groupinglevel))), group = unique(groupinglevel))
  names(res)    <- c("coef", grouping)
  for(i in 1:length(unique(groupinglevel))){
    data2    <- as.data.frame(data[data[,grouping] == i,])
    res[i,1] <- as.numeric(lm(data2[, y] ~ data2[, x])$coefficients[2])
  }
  top    <- res %>% top_n(n.highest, coef)
  bottom <- res %>% top_n(-n.lowest, coef)
  res    <- res %>% mutate(high_and_low = ifelse(coef %in% top$coef, "top",  ifelse(coef %in% bottom$coef, "bottom", "none")))
  data3  <- left_join(data, res)
  return(data3)
}
                           
 
 f1(data = as.data.frame(Milad325), 
   x    = "GPA",
   y    = "Total_agg_ST",
   grouping = "study_major",
   n.highest = 3, 
   n.lowest = 3) %>%
  ggplot()+
  geom_point(aes(x     = GPA,
                 y     = Total_agg_ST, 
                 fill  = study_major, 
                 group = study_major),
             size     =  1, 
             alpha    = .5, 
             position = "jitter", 
             shape    = 21, 
             col      = "white")+
  geom_smooth(aes(x     = GPA,
                  y     = Total_agg_ST,
                  col   = high_and_low,
                  group = study_major,
                  size  = as.factor(high_and_low),
                  alpha = as.factor(high_and_low)),
              method = lm,
              se     = FALSE)+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_gradientn(colours = rainbow(100))+
  scale_color_manual(values=c("top"      = "blue",
                              "bottom"   = "red",
                              "none"     = "grey40"))+
  scale_size_manual(values=c("top"       = 1.2,
                              "bottom"   = 1.2,
                              "none"     = .5))+
  scale_alpha_manual(values=c("top"      = 1,
                             "bottom"    = 1,
                             "none"      =.3))+
  labs(title="Linear Relationship Between ST and GPA for 13 mojors of studies",
       subtitle="The 6 with the most extreme relationship have been highlighted red and blue") 
       
       
       
       
       
       
     interceptonlymodel <- lmer(formula = Total_agg_ST ~ 1 + (1|study_major),
                           data    = Milad325) #to run the model  
       
       
       summary(interceptonlymodel) #to get paramater estimates.

       #library(sjstats)
#icc(interceptonlymodel)
       
      library(lme4)
      library(lmerTest)# to get p-value estimations that are not part of the standard lme4 packages
      library(haven) # to load the SPSS .sav file
 library(tidyverse) # needed for data manipulation.
 library(RColorBrewer) # needed for some extra colours in one of the graphs
 
       ggplot(data = Milad325, 
       aes(x   = GPA,
           y   = Total_agg_ST, 
           col = as.factor(Gender)))+
  geom_point(size     = 1, 
             alpha    = .7, 
             position = "jitter")+
  geom_smooth(method   = lm,
              se       = T, 
              size     = 1.5, 
              linetype = 1, 
              alpha    = .7)+
  theme_minimal()+
  labs(title    = "Linear Relationship Between ST and GPA for Genders", 
       subtitle = "The linear relationship between the two is different for male and femals")+
  scale_color_manual(name   =" Gender",
                     labels = c("prefer not to disclose", "male", "female"),
                     values = c("lightgreen", "lightblue", "pink"))
       
       model1 <- lmer(formula =Total_agg_ST ~ 1 + Total_RAVEN_score + Total_RAT_score	+ Total_series_score	+	Age +	Parents_checking +	Parent_concern_need +	Father_education	+ Mother_education	+ Sibling +	Houshold_status +	Financial_aids +	Grant	+ Scholarship	+ Loan +	Time_Academic +	Time_Extracurricular +	Time_work +	Time_social +	Time_family +	Time_life	+ Co_op +	Internship +	GPA +	Ethinicity_1 +	Ethinicity_2	+ Ethinicity_3 +	Ethinicity_4 +	Ethinicity_5 +	Ethinicity_6 +	Ethinicity_7 +	Ethinicity_8	+ Houshold_living_1	+ Houshold_living_2	+ Houshold_living_3 +	Houshold_living_4	+ Houshold_living_5	+ car_1	+ car_2 +	car_3 + gender_1	+ gender_2	+ gender_3
 + (1|study_major), 
               data    = Milad325)
summary(model1)
       
      # require(lmerTest)
      # ranova(model1)
       
       
       
       
       model22 <- lmer(formula =Total_agg_ST ~ 1 + Total_RAVEN_score + Total_RAT_score	+ Total_series_score	+	Age +	Parents_checking +	Parent_concern_need +	Father_education	+ Mother_education	+ Sibling +	Houshold_status +	Financial_aids +	Grant	+ Scholarship	+ Loan +	Time_Academic +	Time_Extracurricular +	Time_work +	Time_social +	Time_family +	Time_life	+ Co_op +	Internship +	GPA +	Ethinicity_1 +	Ethinicity_2	+ Ethinicity_3 +	Ethinicity_4 +	Ethinicity_5 +	Ethinicity_6 +	Ethinicity_7 +	Ethinicity_8	+ Houshold_living_1	+ Houshold_living_2	+ Houshold_living_3 +	Houshold_living_4	+ Houshold_living_5	+ car_1	+ car_2 +	car_3 + Gender
       + (1 + Total_RAVEN_score + Total_RAT_score	+ Total_series_score	+	Age +	Parents_checking +	Parent_concern_need +	Father_education	+ Mother_education	+ Sibling +	Houshold_status +	Grant	+ Scholarship	+ Loan +	Time_Academic +	Time_Extracurricular +	Time_work +	Time_social +	Time_family +	Time_life	+ Co_op +	Internship +	GPA + Gender |study_major), 
               data    = Milad325)
summary(model22)
```  


```{r}

library(ggplot2)
library(magrittr)
library(dplyr)

library(tidytext)

library(stringr) # to deal with strings
library(wordcloud) # to render wordclouds
library(knitr) # for tables
library(DT) # for dynamic tables
library(tidyr)
#str(Milad325)
library(car)
require(car)
library(MASS)
# This is so that distributions that must be non-zero can make sense of my data
require(MASS)
#library(EBSeq)

```

```{r}
ggplot(data  = Milad325,
       aes(x = GPA,
           y = Total_agg_ST,
           col = study_major,
           group = study_major))+
  geom_point(size = 1.2,
             alpha = .8,
             position = "jitter")+# to add some random noise for plotting purposes
             
              geom_smooth(method = lm,
              se     = FALSE, 
              col    = "black",
              size   = .5, 
              alpha  = .8)+ # to add regression line
             
  theme_minimal()+
  
  theme(legend.position = "none")+
  scale_color_gradientn(colours = rainbow(100))+
  
    geom_smooth(method = lm,
              se     = FALSE,
              size   = .5, 
              alpha  = .8)+ # to add regression line
  
  labs(title = "ST and Major of Study", subtitle = "add colours for different classes and regression lines")

# To colour code the extremes, we need to write a small function that calculates the regression lines and adds a collumn indicating which clusters have the most extreme.
f1 <- function(data, x, y, grouping, n.highest = 3, n.lowest = 3){
  groupinglevel <- data[,grouping]
  res           <- data.frame(coef = rep(NA, length(unique(groupinglevel))), group = unique(groupinglevel))
  names(res)    <- c("coef", grouping)
  for(i in 1:length(unique(groupinglevel))){
    data2    <- as.data.frame(data[data[,grouping] == i,])
    res[i,1] <- as.numeric(lm(data2[, y] ~ data2[, x])$coefficients[2])
  }
  top    <- res %>% top_n(n.highest, coef)
  bottom <- res %>% top_n(-n.lowest, coef)
  res    <- res %>% mutate(high_and_low = ifelse(coef %in% top$coef, "top",  ifelse(coef %in% bottom$coef, "bottom", "none")))
  data3  <- left_join(data, res)
  return(data3)
}
```                        
 


       
       
