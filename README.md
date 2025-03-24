# Missing_data
---
title: "Missing_data"
author: "JCA"
date: ' Data Camp Last edited `r Sys.Date()`'
output:
  html_document:
    toc: yes
  pdf_document:
    number_sections: yes
    toc: yes
  word_document:
    toc: yes
editor_options:
  chunk_output_type: console
---

![](/Users/jean-christopheaubert/Desktop/Data Camp/Biotech.jpg){#id .class width=75% height=100%}

# Missing_Data
## Introduction
```{r, 1}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(plyr)
library(dplyr)
library(MASS)
library(KernSmooth)
library(knitr)
library(RColorBrewer)
library(stats4)
library(data.table)
library(gcookbook)
library(tinytex)
library(data.table)
library(gtExtras)
library(naniar)
library(imputeTS)
library(VIM)
library(boot)
library(mice)
library(nonmemica)
library(class)
library(simputation)

# Revue importante : https://journal.r-project.org/issues.html

# useful : https://www.youtube.com/@RProgramming101


# useful : https://www.google.com/search?client=firefox-b-d&sca_esv=b08837490d8f3546&sxsrf=ADLYWIIXVa0A7dT-9JGZODqyUY__VWBtzA:1728709831195&q=missing+values+in+r&tbm=vid&source=lnms&fbs=AEQNm0CbCVgAZ5mWEJDg6aoPVcBgTlosgQSuzBMlnAdio07UCId2t1azIRgowYJD0nDbqEIN7XYIyS3uBYzHmWPp2pnW1aUeS8cvBgTxtkh0oXYZb0-zZA8Hl8jwD4NybFrDe_ihITZ9YvH4KP5WNn7NesXVafJYDPviTSN1KR7gzYhX3ab4oOfz6VDN8oYFI2dMgJtb0lqX&sa=X&ved=2ahUKEwjbyqH7iYiJAxUuxQIHHTliKn0Q0pQJegQIHhAB&biw=1400&bih=704&dpr=2#fpstate=ive&vld=cid:917982d3,vid:kIKg5s_jDAk,st:0/

theme_blue<-function(){
        theme(
                plot.background = element_rect(fill="lightblue"),
                panel.background = element_rect(fill="gray95"),
                plot.margin = unit(c(3,7,3,7), units ="mm"),
                axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14),
                 panel.border= element_rect(colour = "darkred",size = 2, fill=NA)
        )
}



Sys.Date()

airquality<-as.data.frame(airquality)
head(airquality)

str(airquality)

starwars<-as.data.frame(starwars)
head(starwars)


# First question : Howmany missing values do we have

miss_var_summary(airquality)
miss_var_summary(starwars)

# Or

# Get the number of missing values per column
airquality %>%
  is.na() %>% 
  colSums()

# Get the number of missing values per column
starwars %>%
  is.na() %>% 
  colSums()
# Better
miss_var_summary(airquality)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables")                          
# pct_miss means the percentage of missing values

# Plot the result

gg_miss_var(airquality)+theme_blue()

# Table of observations with missing values

Sairquality<-airquality %>%
        filter(!complete.cases(.))%>%
        head(10)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title= "Rows that contain missing data");Sairquality

Sairquality<-as.data.frame(Sairquality)
# Visualization of the distribution of missing values


vis_miss(airquality)+theme_blue()


# More informations about missing data
gg_miss_fct(x= airquality, fct = Month)
n_miss(airquality)
n_miss(airquality$Ozone)
prop_miss(airquality)
n_complete(airquality)
prop_complete(airquality)

# The relationship to one variable
Nairquality<-airquality %>%
       mutate( Missing_Ozone = factor(is.na(Ozone),
                                       levels=c("TRUE", "FALSE"),
                                       labels = c("Missing", "Not Missing") ) );Nairquality

Nairquality<-as.data.frame(Nairquality)

 ggplot(Nairquality,aes(x= Wind, fill= Missing_Ozone))+
        geom_histogram(position = "stack")+
                labs(title = "Distribution of Wind Speeds for Missing ",
        x="Wind speed",
        y= "Ozone \n Observations",
        fill= "Missingness")+
                theme_blue()
 
 # Find if there is ny pattern
 airquality<-as.data.table(airquality)
 
 airquality %>%
         dplyr::select(Ozone,Solar.R,Wind,Temp) %>%
         ggplot(aes(x= Wind,
                    y= Temp,
                    size= Solar.R,color=is.na(Ozone)))+
         geom_point(alpha=0.7)+
         facet_wrap(~is.na(Ozone))+
         labs(title="Missing Ozone Data by Wind",
             x=" Wind Speed",
             y= "T\n e\n m\n  p\n e\n r\n a\n t\n u\n r\n e")+
         theme_blue()
 
 # Replace an NA by the median
 starwars$height
 starwars%>%
         mutate(height=case_when(
                 is.na(height)~median(starwars$height, na.rm=TRUE),
                 TRUE ~ height)) %>%
         dplyr::select(name, height)%>%
         arrange(name) %>%
         gt() %>%
         gt_theme_guardian()%>%
         tab_header(title = "Starwars characters")
 
 # What is the median        
 median(starwars$height, na.rm=TRUE)
 
                                      
   starwars$height
   
  # Replace by the mean  
   
 starwars%>%
         mutate(height=case_when(
                 is.na(height)~mean(starwars$height, na.rm=TRUE),
                 TRUE ~ height)) %>%
         dplyr::select(name, height)%>%
         arrange(name) %>%
         gt() %>%
         gt_theme_guardian()%>%
         tab_header(title = "Starwars characters")
      
mean(starwars$height,na.rm=TRUE)

# Replace NA by median

# Firts for Ozone

airquality %>%
         mutate(Ozone=case_when(
                 is.na(Ozone)~ median(airquality$Ozone, na.rm=TRUE),
                 TRUE ~ Ozone)) %>%
         gt() %>%
         gt_theme_guardian()%>%
         tab_header(title = "New_data")


# Second for Solar.R

airquality %>%
         mutate(Solar.R=case_when(
                 is.na(Solar.R)~ median(airquality$Solar.R, na.rm=TRUE),
                 TRUE ~ Solar.R)) %>%
         gt()%>%
         gt_theme_guardian()%>%
         tab_header(title = "New_data")

# Put together the 2 modifications

airquality_1<-airquality %>%
         mutate(Ozone=case_when(
                 is.na(Ozone)~ median(airquality$Ozone, na.rm=TRUE),
                 TRUE ~ Ozone));airquality_1 

airquality_2<-airquality_1 %>%
         mutate(Solar.R=case_when(
                 is.na(Solar.R)~ median(airquality_1$Solar.R, na.rm=TRUE),
                 TRUE ~ Solar.R)) %>%
         gt() %>%
         gt_theme_guardian()%>%
         tab_header(title = "New_data_2");airquality_2
head(airquality_2)


# Exercice with Polar_result

Polar_R<-read.csv("/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/Statistics with R/2.-CSV File/Polar_résultats_17.csv")
head(Polar_R)

dim(Polar_R)
Polar_R<-as.data.table(Polar_R)
str(Polar_R)

summary(Polar_R)

# Missing Values

miss_var_summary(Polar_R)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables") 


 # Replace by the mean  
   
Complte_Polar_R<-Polar_R%>%
         mutate(S=case_when(
                 is.na(S)~mean(Polar_R$S, na.rm=TRUE),
                 TRUE ~ S)) %>%
       
         gt() %>%
         gt_theme_guardian()%>%
         tab_header(title = "Complet Polar_R");Complte_Polar_R

New_Polar_R<-Polar_R%>%
         mutate(S=case_when(
                 is.na(S)~mean(Polar_R$S, na.rm=TRUE),
                 TRUE ~ S)) %>%
         dplyr::select(Date, S)%>%
         arrange(Date) %>%
         gt() %>%
         gt_theme_guardian()%>%
         tab_header(title = "Complet Polar_R");New_Polar_R




```


## If else and case_when

```{r,2}
# First If else
starwars %>%
        select(name, hair_color, species)%>%
        head(10)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Starwars characters")%>%
        gt_highlight_rows(rows= species == "Droid",
fill= "lightblue")%>%
        gt_highlight_rows(rows=hair_color =="brown",
                          fill="brown")


starwars%>%
        select(name, hair_color, species)%>%
        mutate(hair_color = if_else(is.na(hair_color)&
                                            species =="Droid",
                                    "no hair",hair_color))%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title = "Starwars characters")%>%
        gt_highlight_rows(rows=species =="Droid", fill= "lightblue")


# Second : case_when

starwars%>%
        select(name, hair_color, species)%>%
        mutate(hair_color = case_when(
                is.na(hair_color)&
                        species =="Droir" ~"no hair",
                hair_color =="brown"~"green",
                TRUE ~hair_color))%>%
        head(10) %>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Starwars chararcters")%>%
        gt_highlight_rows(rows=species =="Droid",
                           fill="lightblue")%>%
        gt_highlight_rows(rows =hair_color =="green", fill= "green")




```


## T Test

```{r,3}
library(tidyverse)
my_data<-starwars %>%
        select(sex, height)%>%
        filter(sex%in% c("male", "female")) %>%
        drop_na(height)

t.test(height ~sex, data = my_data)

```


## Break with London
```{r,4}
London<-read.csv("/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/New_Statistics_with_R/CSV_file_new/London.csv"); head(London)


dim(London)

miss_var_summary(London)

# Better
miss_var_summary(London)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables") 

gg_miss_var(London)+theme_blue()

vis_miss(London)+theme_blue()

# Replace missing data by the median

London %>%
         mutate(smoking=case_when(
                 is.na(smoking)~ median(London$smoking, na.rm=TRUE),
                 TRUE ~ smoking)) %>%
         gt() %>%
         gt_theme_guardian()%>%
         tab_header(title = "New_data")

```

## Break with msleep

```{r,5}
library(forcats)
msleep%>%
        group_by(order)%>%
        summarise(mean_sleep = mean(sleep_total))%>%
       mutate(order = fct_reorder(order,  mean_sleep))%>%
        ggplot(aes(x = order, 
                   y= mean_sleep))+
        theme_blue()+
        ##
         labs(x ="Mamals ", y="H\no\nu\nr\ns", 
     title = " Sleep time of mamals ",
     subtitle = "Graph",
     
     caption = "source : jca / R Missing_data [r,5] Rmd")+
        
        ##
      
        theme(axis.text.x=
                      element_text(angle=45,
                                   vjust=1,
                                   hjust=1),
              axis.text.y=element_text(face="bold"),
              plot.title = element_text(hjust=0.5,
                                        size=25,
                                        face="bold"))+
        theme(
                axis.text.x = element_text(color= "lightblue"),
                axis.text.y = element_text(color= "lightblue"),
                axis.title.y = element_text(color="lightblue"),
                axis.title.x = element_text(color="lightblue"),
                plot.title= element_text(color = "lightblue"),
                plot.subtitle= element_text(color = "lightblue"),
                 plot.caption= element_text(color = "lightblue"),
                axis.line = element_line(color = "lightblue"),
                axis.ticks = element_line(color = "lightblue"),
                panel.background = element_rect(fill="black"),
                plot.background = element_rect(fill="black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.position = "none"
        )+
        geom_hline(yintercept = mean(msleep$sleep_total),color="lightblue",
                   size = 1)+
        geom_segment(aes(x= order,
                         y = mean(msleep$sleep_total),
                         xend=order,
                         yend=mean_sleep),
                     color="lightblue")+
        geom_point(aes(color=mean_sleep),
                   size =5)+
        scale_color_gradient(low="hotpink",
                             high="yellow")+
        annotate("text", 
                 x=4,
                 y= max(msleep$sleep_total)-4,
                 label = "Average sleep\nfor all mammals", color = "#f4cccc",
                 size = 4,
                 fontface="bold",
                 hjust =0)
        

```


## Stat and Anova

```{r,6}


London<-read.csv("/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/New_Statistics_with_R/CSV_file_new/London.csv"); head(London)

summary(London)

# Quantile of any order
quantile(London$age, probs = c(0.1, 0.3, 0.5, 0.7, 0.9))

head(economics)

cor(economics$pce, economics$psavert)

# With data.table
economics<-as.data.table(economics)

cor(economics[,2], economics[,4])


data(tips, package = "reshape2")

head(tips)



mean(tips$tip)

# or



mean(tips[,2])

# Here we reject
t.test(tips[,2], alternative="two.sided", mu= 2.5)

# Here we do not reject
t.test(tips[,2], alternative="two.sided", mu= 2.99)

# Two-Sample T-Test

aggregate(tip ~sex, data = tips, var)
aggregate(tip ~sex, data = tips, mean)

ansari.test(tip ~sex, tips)

t.test(tip ~sex, data = tips, var.equal = TRUE)

# Is any NA

miss_var_summary(tips)

# Better
miss_var_summary(tips)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables") 

library(plyr)

tipSummary<- ddply(tips, "sex", summarize,
                    tip.mean=mean(tip),
                    tip.sd = sd(tip),
                    Lower = tip.mean-2*tip.sd/sqrt(NROW(tip)),
                    Upper = tip.mean+2*tip.sd/sqrt(NROW(tip)) );tipSummary

ggplot(tipSummary, aes(x=tip.mean, y= sex))+
               geom_point()+
        geom_errorbarh(aes(xmin= Lower, xmax=Upper), height = 0.2)+
        theme_blue()+
        labs(x ="tip mean ", y="sex", 
     title = "Two Sample t-test ",
     subtitle = "tips",
     
     caption = "source : jca / R Mt-test [r,6] Rmd")
   

# Example with smoker  

smokeSummary<- ddply(tips, "smoker", summarize,
                    tip.mean=mean(tip),
                    tip.sd = sd(tip),
                    Lower = tip.mean-2*tip.sd/sqrt(NROW(tip)),
                    Upper = tip.mean+2*tip.sd/sqrt(NROW(tip)) );tipSummary

ggplot(smokeSummary, aes(x=tip.mean, y= smoker))+
               geom_point()+
        geom_errorbarh(aes(xmin= Lower, xmax=Upper), height = 0.2)+
        theme_blue()+
        labs(x ="tip mean ", y="smoker", 
     title = "Two Sample t-test ",
     subtitle = "tips",
     
     caption = "source : jca / R Mt-test [r,6] Rmd")

t.test(tip ~smoker, data = tips, var.equal = TRUE)

```


## Handle with missing data with imputeTS 

![](/Users/jean-christopheaubert/Desktop/Data Camp/Missing_values.png){#id .class width=110% height=110%}

```{r,7}

# Steffen Moritz

# Here is the refernce

# https://www.youtube.com/watch?v=dQJNdP8_4SA

library(imputeTS)






# INFORMATIONS

# Prerequisite: Create Time series with missing values
x <- ts(c(NA, 3, 4, 5, 6, NA, 7, 8));x

# Example 1: Perform LOCF
na_locf(x)

# Example 2: Perform NOCF
na_locf(x, option = "nocb")

# Example 3: Perform LOCF and remove remaining NAs
na_locf(x, na_remaining = "rm")

# Example 4: Same as example 1, just written with pipe operator
x %>% na_locf()


# Examples with interpolation
# linear, spline, stine

# Prerequisite: Create Time series with missing values
x <- ts(c(2, 3, 4, 5, 6, NA, 7, 8));x

# Example 1: Perform linear interpolation
na_interpolation(x)

# Example 2: Perform spline interpolation
na_interpolation(x, option = "spline")

# Example 3: Perform stine interpolation
na_interpolation(x, option = "stine")

# Example 4: Perform linear interpolation, with additional parameter pass through from spline()

# Take a look at the 'Details' section of the na_interpolation documentation 

# for more information about advanced parameter pass through options
na_interpolation(x, option ="spline", method ="natural")

# Example 5: Same as example 1, just written with pipe operator
x %>% na_interpolation()

# Example 6: Same as example 2, just written with pipe operator
x %>% na_interpolation(option = "spline")


xx<-c(102,NA,99)
xx%>%na_interpolation()

xxx<-c(110, NA, NA, 99)
xxx%>%na_interpolation()


# Example with Clinical Nutrition

Clinical_Nutrition <- read.csv2( "/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/New_Statistics_with_R/CSV_file_new/Clinical Nutrition.csv")

Clinical_Nutrition<-as.data.frame(Clinical_Nutrition)
Clinical_Nutrition<-as.data.table(Clinical_Nutrition)


# Looking for missing values
miss_var_summary(Clinical_Nutrition)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables")


# Plot the result
gg_miss_var(Clinical_Nutrition)+theme_blue()

# 1er with interpolation
na_interpolation(Clinical_Nutrition)

# 2nd with the mean
na_mean(Clinical_Nutrition)

# 3rd withe the previous value
na_locf(Clinical_Nutrition)

# 4th with Kalman smoothing
na_kalman(Clinical_Nutrition)

# 5th remove the missing value
# Only works with univariate input
na_remove(Clinical_Nutrition[,2])


# Some more
# https://www.youtube.com/watch?v=_BFMS1IefzE

A<-c(1,5,NA,6,3,NA)

nafill(A, "locf")
nafill(A, "nocb")


# Or with the library nonmemica
library(nonmemica)

locf(A)
nocb(A)
forbak(A)

B<-c(NA,4,6,8,NA)
locf(B)
nocb(B)
forbak(B)

# An Example
removedata<-sample(1:nrow(airquality))[1:10]
airquality_missing <-airquality

airquality_missing$Temp[removedata]<-NA
airquality_missing$Temp<-forbak(airquality_missing$Temp)

original<-airquality$Temp[removedata]
imputed<-airquality_missing$Temp[removedata]

original<-as.numeric(original)
imputed<-as.numeric(imputed)

plot(original, imputed)
abline(a=1, b=1)

plot(airquality$Temp)
plot(airquality_missing$Temp)

```

## Missing values with KNN

```{r,8}
# we shal use the data iris

df<-iris

str(df)
# the norm function
nor<-function(x){
        (x-min(x))/(max(x)-min(x))
}
# let's run the normalization on df[, c(1,2,3,4)]

df_norm<-as.data.frame(lapply(df[, c(1,2,3,4)], nor))

ran<-sample(1:nrow(df), 0.9*nrow(df))

df_train<-df_norm[ran,]
df_test<-df_norm[-ran,]

df_target_category<-iris[ran,5]
df_test_category<-iris[-ran,5]

library(class)
predicted<-knn(df_train, df_test, cl=df_target_category, k=3)

# Create a confusion matrix

tab<-table(predicted, df_test_category);tab

accuracy<-function(x){
        sum(diag(x)/(sum(rowSums(x))))*100
}

accuracy(tab)

library(DMwR2)

ran2<-sample(1:nrow(df), 0.1*nrow(df))
df_missing<-df[,-5]
df_missing$Sepal.Length[ran2]<-"NA"

# warning : df_missing$Sepal.Lenght is no more numeric but character

df_missing$Sepal.Length<-as.numeric(df_missing$Sepal.Length)
head(df_missing)

 
df_missing<-as.data.frame(df_missing)

str(df_missing)
miss_var_summary(df_missing)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables") 


miss_var_summary(df_missing)

# KNN inputation
df_imputed<-knnImputation(df_missing, k=3)

tab<-c()
tab$original<-df$Sepal.Length[ran2]
tab$imputed<-as.numeric(df_imputed$Sepal.Length[ran2])
tab$diff<-tab$original-tab$imputed

plot(df$Sepal.Length)
plot(df_imputed$Sepal.Length)

percentage_df<-tab$diff/tab$original*100
barplot(percentage_df)









```


## Date modification

```{r,9}
concession<-read.csv2("/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/New_Statistics_with_R/CSV_file_new/Concession.csv");concession

concession_N<-concession |>
        mutate(Total = number1+number2);concession_N
concession_d<-as.Date(concession$Month)
concession_NN<-format(concession_d,"%Y-%b");concession_NN

concession_ND<-concession_N|>
        mutate(Month =concession_NN);concession_ND

# New example
PBS<-read.csv2("/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/New_Statistics_with_R/CSV_file_new/PBS.csv");PBS

Date1<-as.Date(PBS$Month)
Date2<-format(Date1,"%Y-%b")

PBS_new<-PBS%>%
        mutate(Month= Date2);PBS_new

ggplot(PBS_new, aes(x = reorder(Month, NN), y = cost))+
        geom_point()+
      
  
       
        theme_blue()+
        labs(x ="Month", y="cost", 
     title = " Expence ",
     subtitle="Medical data",
     
     caption = "source : jca / R Missing data in R [8] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
        scale_y_continuous(breaks= seq(30000, 100000,10000))



ggplot(PBS_new, aes(x =  NN, y = cost))+
        geom_point(color = "red")+
      geom_line(color="yellow")+
  
       
        theme_blue()+
        labs(x ="Month", y="cost", 
     title = " Expence ",
     subtitle="Medical data",
     
     caption = "source : jca / R Missing data in R [8] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
        scale_y_continuous(breaks= seq(30000, 100000,10000))+
        scale_x_continuous(breaks= seq(1,12,1))



```

## More about Missing Value with important visual functions (gg_miss_upset)

```{r, 10}
head(riskfactors)
str(riskfactors)
riskfactors<-as.data.table(riskfactors)

# Missing values
miss_var_summary(riskfactors)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables")      

# Visualization of the distribution of missing values


vis_miss(riskfactors)+theme_blue()


# More informations about missing data
gg_miss_fct(x= riskfactors, fct = sex)
gg_miss_fct(x= riskfactors, fct = age)
gg_miss_fct(x= riskfactors, fct = marital)

n_miss(riskfactors)
n_miss(riskfactors$pregnant)
prop_miss(riskfactors)
n_complete(riskfactors)
prop_complete(riskfactors)

# to give an overall pattern of missingness
gg_miss_upset(riskfactors)


# some precisions with the different variables
gg_miss_upset(airquality)
# Which means that 2 missing are together in Ozone and Solar, 35 in Ozone only and 5 in Solar only



#  for a dataset that has a factor of interest: sex.
gg_miss_fct(riskfactors, fct = sex)

# to explore the missingness in a time series dataset
# gg_miss_span() not used here.

# Visualize the number of missings in cases using `gg_miss_case()`
gg_miss_case(riskfactors)

# Explore the number of missings in cases using `gg_miss_case()` 
# and facet by the variable `education`
gg_miss_case(riskfactors, facet = education)

# Visualize the number of missings in variables using `gg_miss_var()`
gg_miss_var(riskfactors)

# Explore the number of missings in variables using `gg_miss_var()` 
# and facet by the variable `education`
gg_miss_var(riskfactors, facet = education)


# Visualize all of the missingness in the `riskfactors`  dataset
vis_miss(riskfactors)

# Visualize and cluster all of the missingness in the `riskfactors` dataset
vis_miss(riskfactors, cluster = TRUE)

# visualize and sort the columns by missingness in the `riskfactors` dataset
vis_miss(riskfactors, sort_miss = TRUE)


head(pedestrian)
pedestrain<-as.data.table(pedestrian)

# Calculate the summaries for each run of missingness for the variable, hourly_counts
miss_var_run(pedestrian, var = hourly_counts)

# Calculate the summaries for each span of missingness, 
# for a span of 4000, for the variable hourly_counts
miss_var_span(pedestrian, var = hourly_counts, span_every = 4000)

# For each `month` variable, calculate the run of missingness for hourly_counts
pedestrian %>% group_by(month) %>% miss_var_run(var = hourly_counts)

# For each `month` variable, calculate the span of missingness 
# of a span of 2000, for the variable hourly_counts
pedestrian %>% group_by(month) %>% miss_var_span(var = hourly_counts, span_every = 2000)

chaos <- tibble::tibble(
  score =   c(3L, -99L, 4L, -99L, 7L, 10L, 12L, 16L, 9L),
  grade = c("N/A", "E", "missing", "na", "n/a", " ", ".", NA, "N/a"),
  place = c(-99, 97, 95, 92, -98, "missing", 88, ".", 86)
)
chaos
chaos<-as.data.table(chaos)
chaos<-as.data.frame(chaos)



# Searching for missing data
chaos%>%
        miss_scan_count(search= list("N/A", "N/a"))

# Replacing missing values
chaos%>%
        replace_with_na(replace = list(grade = c("N/A", "N/a")))
        
chaos%>%
        replace_with_na_all(condition = ~.x ==-99)

chaos_2<-chaos%>%
        replace_with_na_all(condition = ~.x %in% c("N/A", "missing", "na", ".", " ", "n/a", "N/a"));chaos_2

gg_miss_upset(chaos_2)

tetris<-read.csv2("/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/New_Statistics_with_R/CSV_file_new/tetris.csv"); tetris

tetris<-as.data.frame(tetris)
tetris<-as.data.table(tetris)


tetris%>%
        tidyr::complete(name, time)

tetris_wide<-dcast(tetris, name ~time, value.var = "value"); tetris_wide


tetris_2<-read.csv2("/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/New_Statistics_with_R/CSV_file_new/tetris_2.csv"); tetris_2

tetris_2<-as.data.frame(tetris_2)
tetris_2<-as.data.table(tetris_2)

# Complete a non numerical variable
tetris_2 %>% tidyr::fill(name)

# An other example

tetris_3<-read.csv2("/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/New_Statistics_with_R/CSV_file_new/tetris_3.csv"); tetris_3

tetris_3<-as.data.frame(tetris_3)
tetris_3<-as.data.table(tetris_3)

# First complete the name
tetris_4<-tetris_3 %>% tidyr::fill(name);tetris_4

# Observe the NA
gg_miss_upset(tetris_4)

vis_miss(tetris_4)
vis_miss(tetris_4, cluster=TRUE)


# Perform linear interpolation
tetris_5<-na_interpolation(tetris_4);tetris_5

gg_miss_fct(x= tetris_4, fct = name)
# we see that time is missing in Alex and Christopher
# we also see that Jess has missing values in time, value and distance

# with riskfactors [allready done]
gg_miss_upset(riskfactors)


# Caution : Don't impute missing data unless there is 5% of missing
```


## Missing data dependence

```{r,11}
# Caution : Don't impute missing data unless there is 5% of missing

# MCAR - Missing Completely at Random
# MAR - Missing At Random
# MNAR - Missing Not At Random. 

#  MCAR: What is it?
# Missing Completely at Random, or MCAR is where the missingness has no association with any data you have observed, or not observed.


# MAR: What is it?
# Missing at Random, or MAR is where missingness depends on data you have observed, but not data unobserved. 

# MNAR: What is it?
# Data MNAR is where the response missingness is related to an unobserved value relevant to the assessment of interest. So



head(oceanbuoys)
oceanbuoys<-as.data.frame(oceanbuoys)
oceanbuoys<-as.data.table(oceanbuoys)

# Arrange by year
oceanbuoys %>% arrange(year) %>% vis_miss()

# Arrange by latitude
oceanbuoys%>% arrange(latitude) %>% vis_miss()

# Arrange by wind_ew (wind east west)
oceanbuoys %>% arrange(wind_ew) %>% vis_miss()

# Using the information from earlier on the oceanbuoys dataset, which of these statements makes the most appropriate statement on the missingness type?
gg_miss_var(oceanbuoys, facet = year)

# Very useful
gg_miss_upset(oceanbuoys)

miss_var_summary(oceanbuoys)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables")  

gg_miss_var(oceanbuoys)

gg_miss_fct(x= oceanbuoys, fct = year)

# Data is MAR: year and location are both important for explaining missingness

# Transform the data in a shadow matrix
gg_miss_upset(airquality)

as_shadow(airquality)
bind_shadow(airquality)

# Perform some summaries

airquality %>%
        bind_shadow()%>%
        group_by(Ozone_NA)%>%
        summarize(mean= mean(Wind))

# The result give the mean of the Wind, when Ozone is missing and when Ozone is not missing. 
# Here we have almost the same result.

# Withn the data oceanbuoys
gg_miss_upset(oceanbuoys)

# Create shadow matrix data with `as_shadow()`
as_shadow(oceanbuoys)

# Create nabular data by binding the shadow to the data with `bind_shadow()`
bind_shadow(oceanbuoys)

# Bind only the variables with missing values by using bind_shadow(only_miss = TRUE)
bind_shadow(oceanbuoys, only_miss = TRUE)


oceanbuoys %>%
        bind_shadow()%>%
        group_by(humidity_NA)%>%
        summarize(mean= mean(wind_ew))

# Or

# `bind_shadow()` and `group_by()` humidity missingness (`humidity_NA`)
oceanbuoys %>%
  bind_shadow() %>%
  group_by(humidity_NA) %>% 
  summarize(wind_ew_mean = mean(wind_ew), # calculate mean of wind_ew
            wind_ew_sd = sd(wind_ew)) # calculate standard deviation of wind_ew
  
# Repeat this, but calculating summaries for wind north south (`wind_ns`).
oceanbuoys %>%
  bind_shadow() %>%
  group_by(humidity_NA) %>%
  summarize(wind_ns= mean(wind_ns),
            wind_ns_sd = sd(wind_ns))



# Summarize wind_ew by the missingness of `air_temp_c_NA`
oceanbuoys %>% 
  bind_shadow() %>%
  group_by(air_temp_c_NA) %>%
  summarize(wind_ew_mean = mean(wind_ew),
            wind_ew_sd = sd(wind_ew),
            n_obs = n())

# Summarize wind_ew by missingness of `air_temp_c_NA` and `humidity_NA`
oceanbuoys %>% 
  bind_shadow() %>%
  group_by(air_temp_c_NA, humidity_NA) %>%
  summarize(wind_ew_mean = mean(wind_ew),
            wind_ew_sd = sd(wind_ew),
            n_obs = n())

ggplot(airquality, aes(x= Temp))+
        geom_density()+
        theme_blue()

airquality %>%
        bind_shadow()%>%
        ggplot(aes(x= Temp, color= Ozone_NA))+
        geom_density()+
        theme_blue()

airquality %>%
        bind_shadow()%>%
        ggplot(aes(x= Ozone_NA, 
                   y= Temp))+
        geom_boxplot()+
        theme_blue()

airquality %>%
        bind_shadow()%>%
        ggplot(aes(x= Temp))+
        geom_density()+
        facet_wrap(~Ozone_NA)+theme_blue()

airquality %>%
        bind_shadow()%>%
        ggplot(aes(x= Temp, y= Wind))+
        geom_jitter()+
        facet_wrap(~Ozone_NA)+
        theme_blue()


airquality %>%
        bind_shadow()%>%
        ggplot(aes(x= Temp,
                   y= Wind,
                   color= Ozone_NA))+
        geom_jitter()+
       
        theme_blue()



airquality %>%
        bind_shadow()%>%
        ggplot(aes(x= Temp,
                   color= Ozone_NA))+
        geom_density()+
        theme_blue()


# First explore the missingness structure of `oceanbuoys` using `vis_miss()`
vis_miss(oceanbuoys)

# Explore the distribution of `wind_ew` for the missingness  
# of `air_temp_c_NA` using  `geom_density()`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew, 
             color = air_temp_c_NA)) + 
  geom_density()

# Explore the distribution of sea temperature for the  
# missingness of humidity (humidity_NA) using  `geom_density()`
 bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = sea_temp_c,
             color = humidity_NA)) + 
  geom_density()+theme_blue()
 
 
 # Explore the distribution of wind east west (wind_ew) for the missingness of air temperature 
# using geom_density() and faceting by the missingness of air temperature (air_temp_c_NA).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = wind_ew)) + 
  geom_density() + 
  facet_wrap(~air_temp_c_NA)

# Build upon this visualization by coloring by the missingness of humidity (humidity_NA).
oceanbuoys%>%
  bind_shadow() %>%
  ggplot(aes(x = wind_ew,
             color = humidity_NA)) + 
  geom_density() + 
   facet_wrap(~air_temp_c_NA)


# Explore the distribution of wind east west (`wind_ew`) for  
# the missingness of air temperature using  `geom_boxplot()`
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = air_temp_c_NA,
             y = wind_ew)) + 
  geom_boxplot()+
        theme_blue()

# Build upon this visualization by faceting by the missingness of humidity (`humidity_NA`).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = air_temp_c_NA,
             y = wind_ew)) + 
  geom_boxplot() + 
  facet_wrap(~humidity_NA)+
        theme_blue()

# To explore the missings in a scatter plot, we can use geom_miss_point. geom_miss_point visualizes the missing values by placing them in the margins. On the left in red we can see the values of solar-dot-radiation when Ozone is missing. This shows us that the values of solar radiation are reasonably uniform, The values of Ozone when Solar-dot-R is missing are shown in red on the bottom, this shows us that the missing values tend to occur at lower values of Ozone. In the bottom left we show cases where there are missings in both Ozone and Solar Radiation. To explain how and why this visualization works, we are going to take a brief moment to unpack the data transformation that occurs here. 

ggplot(airquality, 
       aes(x =Ozone,
           y= Solar.R))+
        geom_miss_point()+
        theme_blue()+
       geom_hline(yintercept=0, size = 0.5)+
        geom_vline(xintercept=0, size = 0.5)

# geom_miss_point performs a transformation on the data and actually imputes - that is, fills in, the values that are missing. Taking an example of just the Ozone data. It does a special imputation, and imputes the data 10% below the minimum value - as shown here in the column ozone_shift, and then keeps track of where it is, to show it in the visualization - as shown here with Ozone_NA. We'll come back to this idea of tracking missing values in the next chapter. 


# we may use what ggplot can do

ggplot(airquality,
       aes(x = Wind,
           y = Ozone))+
        geom_miss_point()+
        facet_wrap(~Month)+
        theme_blue()

airquality %>%
        bind_shadow()%>%
        ggplot(aes(x = Wind,
           y = Ozone))+
        geom_miss_point()+
        facet_wrap(~ Solar.R_NA)+
        theme_blue()


# Explore the missingness in wind and air temperature, and  
# display the missingness using `geom_miss_point()`
ggplot(oceanbuoys,
       aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point()+
        theme_blue()

# Explore the missingness in humidity and air temperature,  
# and display the missingness using `geom_miss_point()`
ggplot(oceanbuoys,
       aes(x = humidity,
           y = air_temp_c)) + 
  geom_miss_point()+
        theme_dark()


# Explore the missingness in wind and air temperature, and display the 
# missingness using `geom_miss_point()`. Facet by year to explore this further.
ggplot(oceanbuoys,
       aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~year)+
        theme_bw()

# Explore the missingness in humidity and air temperature, and display the 
# missingness using `geom_miss_point()` Facet by year to explore this further.
ggplot(oceanbuoys,
       aes(x=humidity,
           air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~year)+
        theme_linedraw()

# Use geom_miss_point() and facet_wrap to explore how the missingness  
# in wind_ew and air_temp_c is different for missingness of humidity
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~humidity_NA)+
        theme_update()

# Use geom_miss_point() and facet_grid to explore how the missingness in wind_ew and air_temp_c 
# is different for missingness of humidity AND by year - by using `facet_grid(humidity_NA ~ year)`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew,
             y = air_temp_c)) + 
  geom_miss_point() + 
  facet_grid(humidity_NA~year)+
        theme_update()



vec <- factor(LETTERS[1:10]);vec

vec[sample(1:10, 3)] <- NA

vec

impute_factor(vec, "wat")


airquality %>%
  impute_below_if(.predicate = is.numeric)

```




## Imputation

```{r,12}

library(simputation)

# We may use impute_bellow
# Not very good. Choose a number bellow each values of the data

impute_below(c(3,5,4,7,3.5,NA,8,10))

# or

Ex<-c(3,5,4,7,3.5,NA,8,10)
Ex<- as.data.frame(Ex)

# Follow the NA

bind_shadow(Ex)%>%impute_below_all()


impute_below_all(Ex)

# another example

Ex2<-c(NA,-4,0,5,3.7,NA,NA,-2)

Ex2<-as.data.frame(Ex2)

bind_shadow(Ex2)%>%impute_below_all()


impute_below_all(Ex2)








# Impute and track the missing values
ocean_imp_track <- bind_shadow(oceanbuoys) %>% 
  impute_below_all() 

# Visualize the missingness in wind and air temperature,  
# coloring missing air temp values with air_temp_c_NA
ggplot(ocean_imp_track, 
       aes(x = wind_ew, y = air_temp_c, color = air_temp_c_NA)) + 
  geom_point()+
        theme_blue()

# Visualize humidity and air temp, coloring any missing cases using the variable any_missing
ggplot(ocean_imp_track, 
       aes(x = humidity, y = air_temp_c, color=air_temp_c_NA)) +  
  geom_point()+
        theme_blue()



tetris_4<-read.csv2("/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/New_Statistics_with_R/CSV_file_new/tetris_4.csv");tetris_4
tetris_4<-as.data.frame(tetris_4)
tetris_4<-as.data.table(tetris_4)


# first 

tetris_4 %>% na_interpolation()

# or with information
tetris_4%>%        
        bind_shadow(only_miss = TRUE)%>%
        add_label_shadow()%>%
       na_interpolation()


# second
tetris_4%>%
        bind_shadow(only_miss = TRUE)%>%
        add_label_shadow()%>%
        impute_lm(y ~x1 + x2)

# third
tetris_4%>%        
        bind_shadow(only_miss = TRUE)%>%
        add_label_shadow()%>%
        impute_lm(y ~x1)

# fourth
tetris_4%>%        
        bind_shadow(only_miss = TRUE)%>%
        add_label_shadow()%>%
        impute_lm(y ~x2)


# Another example
head(airquality)

airquality$Solar.R<-as.numeric(airquality$Solar.R)
airquality$Ozone<-as.numeric(airquality$Ozone)


airquality_3 <- airquality %>% 
        bind_shadow(only_miss = TRUE) %>% 
        add_label_shadow() %>%  
        impute_lm(Solar.R ~ Wind+ Temp + Month)%>%
        impute_lm(Ozone ~ Wind + Temp + Month); airquality_3

airquality_4<-airquality%>%
          bind_shadow(only_miss = TRUE)%>%
        add_label_shadow()%>%
       na_interpolation();airquality_4



# Trackingmissingvalues. Representation of the values and the missing values

# 1.- multi-linear regression
ggplot(airquality_3,aes(x = Solar.R,y = Ozone,color = any_missing)) +  geom_point()+
        theme_blue()+
labs(x ="Solar.R", y="Ozone", 
     title = " Missing values ",
     subtitle=" Method : linear model",
     caption = "source : jca / Imputation in R [12 - 1440] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
         scale_y_continuous(breaks= seq(0, 160,20))

# 2.- Single linear regression
ggplot(airquality_4,aes(x = Solar.R,y = Ozone,color = any_missing)) +  geom_point()+
        theme_blue()+
        labs(x ="Solar.R", y="Ozone", 
     title = " Missing values ",
     subtitle=" Method : Interpolation regarding to a single variable",
     caption = "source : jca / Imputation in R [12 - 1450] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
         scale_y_continuous(breaks= seq(0, 160,20))

# If we have more than one variable, the multi-linear regression is better 

# What about the mean

mean(airquality$Ozone, na.rm=TRUE)
mean(airquality$Solar.R, na.rm=TRUE)


airquality_5<-airquality%>%
          bind_shadow(only_miss = TRUE)%>%
        add_label_shadow()%>%
       na_mean();airquality_5

ggplot(airquality_5,aes(x = Solar.R,y = Ozone,color = any_missing)) +  geom_point()+
        theme_blue()+
        labs(x ="Solar.R", y="Ozone", 
     title = " Missing values ",
     subtitle=" Method : mean",
     caption = "source : jca / Imputation in R [12 - 1480] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
         scale_y_continuous(breaks= seq(0, 160,20))


# It's not good at all.


# Kalmann

airquality_6<-airquality%>%
          bind_shadow(only_miss = TRUE)%>%
        add_label_shadow()%>%
       na_kalman();airquality_6

ggplot(airquality_6,aes(x = Solar.R,y = Ozone,color = any_missing)) +  geom_point()+
        theme_blue()+
        labs(x ="Solar.R", y="Ozone", 
     title = " Missing values ",
     subtitle=" Method : Kalman",
     caption = "source : jca / Imputation in R [12 - 1500] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
         scale_y_continuous(breaks= seq(0, 160,20))


######################## 
# Remember
airquality_6 %>% 
        select(Ozone, Solar.R, Wind,Temp, Month, Day, Ozone_NA, Solar.R_NA, any_missing)%>%
        mutate(aa = Ozone*Solar.R)



########################


# Impute the oceanbuoys data below the range using `impute_below`.
ocean_imp <- impute_below_all(oceanbuoys)

# Visualize the new missing values
ggplot(ocean_imp, 
       aes(x = wind_ew, y = air_temp_c)) +  
  geom_point()+theme_blue()

# Impute and track data with `bind_shadow`, `impute_below_all`, and `add_label_shadow`
ocean_imp_track <- bind_shadow(oceanbuoys) %>% 
  impute_below_all()

# Look at the imputed values
ocean_imp_track


oceanbuoys_2<-oceanbuoys%>%
          bind_shadow(only_miss = TRUE)%>%
        add_label_shadow()%>%
       na_interpolation();oceanbuoys_2


ggplot(oceanbuoys_2,aes(x = wind_ew,y = humidity,color = any_missing)) +  geom_point()+
        theme_blue()+
labs(x ="wind_ew", y="humidity", 
     title = " Missing values ",
     subtitle=" Method : linear model",
     caption = "source : jca / Imputation in R [12 - 1550] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
        scale_y_continuous(breaks= seq(75, 95,5))

miss_var_summary(oceanbuoys)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables") 

vis_miss(oceanbuoys)
gg_miss_upset(oceanbuoys)


```

## One complete example

```{r,13}

library(VIM)
head(oceanbuoys,10)
str(oceanbuoys)
oceanbuoys<-as.data.table(oceanbuoys)


# Missing variables

miss_var_summary(oceanbuoys)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables") 

# 93+81+3 values are missing in3 vairiables : humidity, air_temp_c, sea_temp_c

vis_miss(oceanbuoys)
gg_miss_upset(oceanbuoys)
oceanbuoys%>%aggr(combined = TRUE, numbers = TRUE)
# oceanbuoys%>%select(oceanbuoys$year, oceanbuoys$humidity)%>%spineMiss()
# nhanes%>%select(Gender, TotChol)%>%spineMiss()

# display the missingness using `geom_miss_point()`
ggplot(oceanbuoys,
       aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point()+
        theme_blue()

# Explore the missingness in wind and air temperature, and display the 
# missingness using `geom_miss_point()`. Facet by year to explore this further.
ggplot(oceanbuoys,
       aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~year)+
        theme_bw()

# Explore the missingness in humidity and air temperature, and display the 
# missingness using `geom_miss_point()` Facet by year to explore this further.
ggplot(oceanbuoys,
       aes(x=humidity,
           air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~year)+
        theme_linedraw()

# Use geom_miss_point() and facet_wrap to explore how the missingness  
# in wind_ew and air_temp_c is different for missingness of humidity
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~humidity_NA)+
        theme_update()

# Use geom_miss_point() and facet_grid to explore how the missingness in wind_ew and air_temp_c 
# is different for missingness of humidity AND by year - by using `facet_grid(humidity_NA ~ year)`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew,
             y = air_temp_c)) + 
  geom_miss_point() + 
  facet_grid(humidity_NA~year)+
        theme_update()

gg_miss_var(oceanbuoys)+theme_blue()+
        labs(x ="Missing", y="Variables", 
     title = " Missing values ",
     subtitle=" Method : Global vison",
     caption = "source : jca / Missing in R [13 - 1650] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))
        

# More informations about missing data
gg_miss_fct(x= oceanbuoys, fct = year)

oceanbuoys_1997<-oceanbuoys%>%
        filter(year == 1997)

miss_var_summary(oceanbuoys_1997)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables") 

gg_miss_fct(x= oceanbuoys_1997, fct = year)

# let's look close
oceanbuoys_1993<-oceanbuoys%>%
        filter(year == 1993)

miss_var_summary(oceanbuoys_1993)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables") 

gg_miss_fct(x= oceanbuoys_1993, fct = year)

# Now some imputation :

# By the mean 

oceanbuoys_mean<-oceanbuoys%>%
          bind_shadow(only_miss = TRUE)%>%
        add_label_shadow()%>%
       na_mean();oceanbuoys_mean

ggplot(oceanbuoys_mean,aes(x = air_temp_c,y = humidity,color = any_missing)) +  geom_point()+
        theme_blue()+
        labs(x ="air_temp_c", y="humidity", 
     title = " Missing values ",
     subtitle=" Method : mean",
     caption = "source : jca / Imputation in R [13 - 1700] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
         scale_y_continuous(breaks= seq(70, 95,5))+
           scale_x_continuous(breaks= seq(22, 28,1))




                       

oceanbuoys_mean%>%select(air_temp_c,humidity)%>%marginplot(delimiter="imp")


# Second Kalman

# Kalmann

oceanbuoys_kalman<-oceanbuoys%>%
          bind_shadow(only_miss = TRUE)%>%
        add_label_shadow()%>%
       na_kalman();oceanbuoys_kalman

ggplot(oceanbuoys_kalman,aes(x = air_temp_c,y = humidity,color = any_missing)) +  geom_point()+
        theme_blue()+
        labs(x ="air_temp_c", y="humidity", 
     title = " Missing values ",
     subtitle=" Method : Kalman",
     caption = "source : jca / Imputation in R [12 - 1720] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
         scale_y_continuous(breaks= seq(0, 160,20))

# With a linear model :

oceanbuoys_2<-oceanbuoys%>%
          bind_shadow(only_miss = TRUE)%>%
        add_label_shadow()%>%
       na_interpolation();oceanbuoys_2


ggplot(oceanbuoys_2,aes(x = wind_ew,y = humidity,color = any_missing)) +  geom_point()+
        theme_blue()+
labs(x ="wind_ew", y="humidity", 
     title = " Missing values ",
     subtitle=" Method : linear model",
     caption = "source : jca / Imputation in R [12 - 1550] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
        scale_y_continuous(breaks= seq(75, 95,5))



oceanbuoys_LM <- oceanbuoys %>% 
        bind_shadow(only_miss = TRUE) %>% 
        add_label_shadow() %>%  
        impute_lm(humidity ~ sea_temp_c+ air_temp_c)%>%
        impute_lm(wind_ew ~ sea_temp_c+ air_temp_c); oceanbuoys_LM


# 1.- multi-linear regression
ggplot(oceanbuoys_LM,aes(x = humidity,y = wind_ew,color = any_missing)) +  geom_point()+
        theme_blue()+
labs(x ="humidity", y="wind_ew", 
     title = " Missing values ",
     subtitle=" Method : linear model",
     caption = "source : jca / Imputation in R [12 - 1770] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
         scale_y_continuous(breaks= seq(0, 160,20))



# The data are MCAR when the locations of missing values in the dataset are purely random, do not depend on any other data. For example, imagine a weather sensor is measuring temperature and sending the data to a database. There are some missing entries in the database for when the sensor broke down, which happens randomly. 

# The data are MAR when the locations of missing values in the dataset depend on some other, observed data. For instance, say there are some missing temperature values in the database for when the sensor was switched off for maintenance. As the maintenance team never work on the weekends, the locations of missing values depend on the day of the week. 

# The data are MNAR when the locations of missing values in the dataset depend on the missing values themselves. Continuing our weather sensor story, imagine that when it's extremely cold, the sensor freezes and stops working. So, it does not record very low temperatures. Thus, the locations of missing values in the temperature variable depend on the values of this variable themselves. 

```


## Secund complete example

```{r,14}

library(VIM)
biopics<-read.csv("/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/New_Statistics_with_R/CSV_file_new/biopics.csv")

summary(biopics)
str(biopics)
biopics<-as.data.table(biopics)


# Draw an aggregation plot of biopics
biopics %>% 
	aggr(combined = TRUE, numbers = TRUE)
# Based on the aggregation plot you have just created, which of the following statements is false?

# missing values in sub_race than in earnings.

# Draw a spine plot to analyse missing values in earnings by sub_race
# biopics %>% select(sub_race,earnings)%>%spineMiss()

# spineMiss(biopics[,c("earnings","sub_race")])

# spineMiss(kNN(sleep[, c("Exp", "Sleep")]), delimiter = "_imp")

# spineMiss(sleep[, c("Exp", "Sleep")])



# Prepare data for plotting and draw a mosaic plot
biopics %>%
	# Create a dummy variable for US-produced movies
	mutate(is_US_movie = grepl("US", country)) %>%
	# Draw mosaic plot
	mosaicMiss(highlight = "earnings", 
             plotvars = c("is_US_movie", "sub_sex"))




# Print first 10 observations
head(tao, 10)

# Get the number of missing values per column
tao %>%
  is.na() %>% 
  colSums()

# or

miss_var_summary(tao)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables")

# Calculate the number of missing values in air_temp per year
tao %>% 
  group_by(Year) %>% 
  summarise(num_miss = sum(is.na(Air.Temp)))

# Calculate the number of missing values in Humidity per year
tao %>% 
  group_by(Year) %>% 
  summarise(num_miss = sum(is.na(Humidity)))



# Impute air_temp in tao with hot-deck imputation
tao_imp <- hotdeck(tao, variable = "Air.Temp")

# Check the number of missing values in each variable
tao_imp %>% 
	is.na() %>% 
	colSums()

# Or better

miss_var_summary(tao_imp)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables") 

# Draw a margin plot of air_temp vs sea_surface_temp
tao_imp %>% 
	select(Air.Temp, Sea.Surface.Temp, Air.Temp_imp) %>% 
	marginplot(delimiter = "imp")

# Or

tao_nextvalue<-tao%>%na_locf();head(tao_nextvalue,5)

 tao_nextvalue%>% 
	select(Air.Temp, Sea.Surface.Temp, Air.Temp) %>% 
	marginplot()
 
 
tao_nv<-tao%>%
          bind_shadow(only_miss = TRUE)%>%
        add_label_shadow()%>%
      na_locf();tao_nv

ggplot(tao_nv,aes(x = Air.Temp,y = Sea.Surface.Temp,color = any_missing)) +  geom_point()+
        theme_blue()+
        labs(x ="air_temp_c", y="sea_surface", 
     title = " Missing values ",
     subtitle=" Method : next value",
     caption = "source : jca / Imputation in R [13 - 1700] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
         scale_y_continuous(breaks= seq(70, 95,5))+
           scale_x_continuous(breaks= seq(22, 28,1))

# CDC

Diab<-read.csv("/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/New_Statistics_with_R/CSV_file_new/Diabetes Missing Data.csv")

head(Diab,5)
Diab<-as.data.table(Diab)
str(Diab)

# MISSING

miss_var_summary(Diab)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables") 

vis_miss(Diab)
gg_miss_upset(Diab)
Diab%>%aggr(combined = TRUE, numbers = TRUE)


# Imputation with a linear model

Diab_2<-Diab%>%
          bind_shadow(only_miss = TRUE)%>%
        add_label_shadow()%>%
       na_interpolation();head(Diab_2, 5)

# Check the number of missing values in each variable.
# There is no more missing data
Diab_2 %>% 
	is.na() %>% 
	colSums()


# Plot
ggplot(Diab_2,aes(x = Serum_Insulin,y = Skin_Fold,color = any_missing)) +  geom_point()+
        theme_blue()+
labs(x ="Serum_Insulin", y="Skin_Fold", 
     title = " Missing values ",
     subtitle=" Method : linear model",
     caption = "source : jca / Imputation in R [12 - 1970] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
        scale_y_continuous(breaks= seq(0, 105,5))


# Check the number of missing values in each variable
Diab_2 %>% 
	is.na() %>% 
	colSums()


# Draw a margin plot of air_temp vs sea_surface_temp
# But ggplot is better
Diab_2 %>% 
	select(Serum_Insulin, Skin_Fold) %>% 
	marginplot(delimiter = "imp")

Diab_3<-na.omit(Diab)

ggplot(Diab_3,aes(x = Serum_Insulin,y = Skin_Fold)) +  geom_point()+
        theme_blue()+
        labs(x ="Serum_Insulin", y="Skin_Fold", 
     title = " na.omit ",
     subtitle=" Method : -",
     caption = "source : jca / Imputation in R [12 - 2000] Rmd")+
         theme(plot.title = element_text(colour = "red", size = 20, face = "bold"))+
        theme(axis.text.x=element_text(angle=30, vjust=1, hjust=1, family="serif", colour ="darkred"))+
        theme(text=element_text(family="serif",  colour ="darkred"), axis.title = element_text(siz=(11)),panel.background=element_rect(fill="grey90"))+
                  theme(plot.title = element_text(face = "bold", size = 15))+
        theme (axis.title.y=element_text(angle=0, face ="italic", colour = "darkred", size = 14),axis.title.x=element_text(angle=0, face ="italic", colour = "darkred", size = 14))+
        theme(plot.margin = unit(c(5,10,5,10), units ="mm"))+
        theme(panel.border= element_rect(colour = "blue",size = 2, fill=NA))+
        scale_y_continuous(breaks= seq(0, 105,5))
```



![](/Users/jean-christopheaubert/Desktop/Data Camp/Bootstrapping.png){#id .class width=75% height=100%}


![](/Users/jean-christopheaubert/Desktop/Data Camp/Themice algorithme.png){#id .class width=75% height=100%}




## Bootstrapping

```{r,15}
library(boot)


oceanbuoys<-as.data.table(oceanbuoys)

str(oceanbuoys)
# Examination of the data
miss_var_summary(oceanbuoys)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables") 

vis_miss(oceanbuoys)
gg_miss_upset(oceanbuoys)
oceanbuoys%>%aggr(combined = TRUE, numbers = TRUE)

# Bootstrapping

calc_correlation<-function(oceanbuoys, indices){
        data_boot<-oceanbuoys[indices,]
        data_imp<- na_interpolation(data_boot)
        corr_coeff<-cor(data_imp$humidity, data_imp$air_temp_c)
        return(corr_coeff)
}

boot_results<-boot(oceanbuoys, statistic = calc_correlation, R = 50)

# Results
print(boot_results)

# Plot Results
plot(boot_results)

# Confident Interval
boot_ci<- boot.ci(boot_results, conf= 0.95,type="norm");boot_ci




#############
#  With Biopics

calc_gender_coef <- function(biopics, indices) {
  # Get bootstrap sample
  data_boot <- biopics[indices, ]
  # Impute with kNN imputation
  data_imp <- na_interpolation(data_boot, k = 5)
  # Fit linear regression
  linear_model <- lm(earnings ~sub_sex + sub_type + year, data_imp)
  # Extract and return gender coefficient
  gender_coefficient <- coef(linear_model)[2]
  return(gender_coefficient) }


# Run bootstrapping on biopics data
boot_results <- boot(biopics, statistic = calc_gender_coef, R = 50)

# Print and plot bootstrapping results
print(boot_results)
plot(boot_results)

# Plot and print boot_results
plot(boot_results)
print(boot_results)

# Calculate and print confidence interval
boot_ci <- boot.ci(boot_results, conf = 0.95, type = "norm")
print(boot_ci)

#Question

# Judging by the plot you have created and the boot_ci you see printed in the console, does the model suggest that movies featuring females earn less, on average, than those featuring males?

# No, one cannot be sure. After accounting for the uncertainty from imputation in this dataset, one cannot be sure of the sign of the female-effect coefficient. Although it being negative is slightly more likely.



# Multiple Imputation by chained equations




library(mice)

# Impute biopics with mice using 5 imputations
biopics_multiimp <- mice(biopics, m = 5, seed = 3108)

# Fit linear regression to each imputed data set 
lm_multiimp <- with(biopics_multiimp, lm(earnings ~ year + sub_type))

# Pool and summarize regression results
  # lm_pooled <- pool(lm_multiimp)
#summary(lm_pooled, conf.int = TRUE, conf.level = 0.95)


# Impute biopics using the methods specified in the instruction
biopics_multiimp <- mice(biopics, m = 10, 
                         defaultMethod = c("cart","lda","pmm","polr"))

# Print biopics_multiimp
print(biopics_multiimp)


# Create predictor matrix with minimum correlation of 0.1
pred_mat <- quickpred(biopics, mincor = 0.1)

# Impute biopics with mice
biopics_multiimp <- mice(biopics, 
                         m=10, 
                         predictorMatrix = pred_mat,
                         seed = 3108)

# Print biopics_multiimp
print(biopics_multiimp)

# Look at the predictor matrix you've used that is printed in the console. Which variables have been used as predictors to impute earnings?

# Answer :
# country, year and non_white.

head(biopics, 10)

# Bootstraping with tetris_4

# Bootstrapping

# tetris_4

#       calc_correlation<-function(tetris_4, indices){
 #       data_boot<-tetris_4[indices,]
#        data_imp<- na_interpolation(data_boot)
#        corr_coeff<-cor(data_imp$value, data_imp$distance)
#        return(corr_coeff)
#}

# boot_results<-boot(tetris_4, statistic = calc_correlation, R = 50)

# Results
# print(boot_results)

# Plot Results
# plot(boot_results)


# tetris_multiimp <- mice(tetris_4, m =5, defaultMethod ="pmm")
# stripplot(tetris_multiimp,time ~ distance | .imp, pch =20, cex =2)




### BOOK : https://stefvanbuuren.name/fimd/foreword.html

### Github : https://rdrr.io/github/stefvanbuuren/mice/f/vignettes/overview.Rmd

# Draw a combined aggregation plot of tetris_4 1400

```


## ROC_curve

```{r,16}


# https://cran.r-project.org/web/packages/plotROC/vignettes/examples.html
library(plotROC)

set.seed(2529)
D.ex <- rbinom(200, size = 1, prob = .5)
M1 <- rnorm(200, mean = D.ex, sd = .65)
M2 <- rnorm(200, mean = D.ex, sd = 1.5)

test <- data.frame(D = D.ex, D.str = c("Healthy", "Ill")[D.ex + 1], 
                   M1 = M1, M2 = M2, stringsAsFactors = FALSE)

head(test)
basicplot <- ggplot(test, aes(d = D, m = M1)) + geom_roc()+theme_blue()
basicplot

ggplot(test, aes(d = D.str, m = M1)) + geom_roc()+theme_blue()

longtest <- melt_roc(test, "D", c("M1", "M2"))
head(longtest)

ggplot(longtest, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()+ theme_blue()

ggplot(longtest, aes(d = D, m = M)) + geom_roc() + facet_wrap(~ name) + style_roc()+ theme_blue()

ggplot(longtest, aes(d = D, m = M, linetype = name)) + geom_roc() + geom_rocci()+ theme_blue()

pairplot <- ggplot(longtest, aes(d = D, m = M, color = name)) + 
  geom_roc(show.legend = FALSE) + style_roc()+ theme_blue()
direct_label(pairplot)


# New Example
Miss<-read.csv2("/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/New_Statistics_with_R/CSV_file_new/Mis_sing.csv")

Miss<-as.data.table(Miss);head(Miss)


miss_var_summary(Miss)%>%
        gt()%>%
        gt_theme_guardian()%>%
        tab_header(title="Missingness of variables")

gg_miss_upset(Miss)

ggplot_na_distribution2(Miss$A1)+theme_blue()
ggplot_na_distribution2(Miss$A2)+theme_blue()
ggplot_na_distribution2(Miss$A3)+theme_blue()
ggplot_na_distribution2(Miss$A4)+theme_blue()


# Na Interpolation

Miss_no<-Miss%>%na_interpolation();head(Miss_no)
is.na(Miss_no)


```



## SSBtools package and success
```{r,17}

# https://journal.r-project.org/articles/RJ-2023-088/

library(SSBtools)

M<-read.csv2("/Users/jean-christopheaubert/Desktop/HARVARD-MIT-edX/New_Statistics_with_R/CSV_file_new/M.csv");M

ModelMatrix(M, formula = ~age + geo)
t(ModelMatrix(M, formula = ~age + geo)) %*% M$value

# Easier
FormulaSums(M, formula = value~age + geo)

ModelMatrix(M, formula = ~age*eu + geo, crossTable = TRUE)

ModelMatrix(M, formula = ~age*eu + geo, crossTable = TRUE)$modelMatrix

ModelMatrix(M, formula = ~age*eu + geo, crossTable = TRUE)$crossTable

dimLists <- FindDimLists(M[c("age", "geo", "eu")])

dimLists$age
dimLists$geo

###############################################################
# https://journal.r-project.org/articles/RJ-2023-095/

library(success)
library(survival)

data("surgerydat", package = "success")

head(surgerydat,5)

assisted_parameters <- parameter_assist(
  baseline_data = subset(surgerydat, entrytime < 365), 
  data = subset(surgerydat, entrytime >= 365 & unit == 1), 
  formula = ~age + sex + BMI, 
  followup = 30,
  theta = log(2),
  time = 365,
  alpha = 0.05)

names(assisted_parameters)

bernoulli_control <- bernoulli_control_limit(assist = assisted_parameters)
bk_control <- bk_control_limit(assist = assisted_parameters)
cgr_control <- cgr_control_limit(assist = assisted_parameters)


bernoulli_assist <- bernoulli_cusum(assist = assisted_parameters, 
                                    h = bernoulli_control$h)
bk_assist <- bk_cusum(assist = assisted_parameters, h = bk_control$h)
cgr_assist <- cgr_cusum(assist = assisted_parameters, h = cgr_control$h)


plot(bernoulli_assist)
plot(bk_assist)
plot(cgr_assist)

runlength(bernoulli_assist, h = bernoulli_control$h)

funnel_assist <- funnel_plot(assist = assisted_parameters)
plot(funnel_assist, label_size = 2) + ggtitle("Funnel plot of surgerydat")

baseline_data <- subset(surgerydat, entrytime <= 365)
followup <- 30
glm_risk_model <- glm((survtime <= followup) & (censorid == 1) ~ age + sex + BMI,
                      data = baseline_data, family = binomial)

coxph_risk_model <- coxph(Surv(survtime, censorid) ~ age + sex + BMI,
                          data = baseline_data)

RA_manual <- list(formula = ~ age + sex + BMI, 
                  coefficients = c(age = 0.003, BMI = 0.02,  
                                         sexmale = 0.2)) 


funnel <- funnel_plot(data = surgerydat, ctime = 365, 
                      glmmod = glm_risk_model, followup = 30)


plot(funnel, label_size = 2) + ggtitle("Funnel plot of surgerydat")

head(summary(funnel), 5)

# or with kable
da<-funnel$data
T1<-da$unit
T2<-da$observed
T3<-da$expected
T4<-da$numtotal
T5<-da$p
T6<-da$"0.95"
T7<-da$"0.99"

T44<-cbind(T1,T2,T3,T4,T5,T6,T7)
colnames(T44)<-c("unit","observed","expected","numtotal","p","0.95","0.99")



kable(T44, caption = "Summary statistics for the funnel plot")
```


