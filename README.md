# 304-a
---
title: 'Analyse of Canadian General Social Survey '
author: "Shengfu Zhu,Zhixin Zhu,Yining Chen"
date: "Oct 19"
output:
  word_document: default
  bookdown: default
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE)
library(dplyr)
library(ggplot2)
library(scales)
library(knitr)
```


## Abstract
This analysis survey is using Canadian General Social Survey (GSS) and a regression model to analyse some aspect of interest.

## Introduction
The goal of this study is to get a deeper understand at the relationship between age, total children number and respondent feelings of life, and as people get older and give birth to more baby, their total children number is expected to continue to grow or at least remain stable. The most important is the correlation between their total children number and age also their feelings of life. We want to convince whether people has higher feelings of life are more willing to give birth to baby. On the other hands, this survey focus on the question about does higher feelings of life people have more children than that in low feelings of life?

## Data
This guide is prepared for 2017 Universal Social Public Use Microdata File users Household survey. Its purpose is to provide certain and background information to familiarize yourself with Explain to users the content of the survey and describe the procedures and concepts related to data quality, Estimate, collection, processing and methods.2017 GSS will be implemented on February 2nd to November 30, 2017, design a sample survey for the cross section. The target population includes all people who are 15 years of age and older who are not in institutions,Live in 10 provinces of Canada. The survey uses a new framework created in 2013 that combines the two Telephone numbers (fixed phones and cellular phones) are registered with statistical Canadian addresses and passed phone. The data is affected by sampling error and non-sampling error.The main target population for the 2017 GSS included all persons 15 years of age and older in Canada.The overall response rate for the 2017 GSS was 52.4%.
They used Stratified sampling. The weakness is through the The disadvantage is that it takes a long time to conduct the investigation because of the telephone investigation, which will lead to the instability of the investigation results. After a few days, someone may have a new change.



## Model
We use the Fitted Model for multiple linear regression model to discover the relationship between the  total number of children,age and feelings about life as a whole.In this model,$y_i$ is the dependent variable which is the total number of children. $x_1$ is the independent variable of age and $x_2$ is the other independent variable of feelings about life as a whole.$\beta_0$ is the value when age and feelings about life is equal to 0. In this case, $\beta_0$ has no practical meaning since age cannot be 0.$\beta_1$ is the change in the average number of  children when age changes by 1 unit and $\beta_2$ is the change in the average number of children when feelings about life changes by 1 unit.$e_i$ is the residuals since there are lots of other factors that can affect the total number of children,such as people's wellness and family wealth. Our alternative hypothesis is that total number of children for a family is dependent on parents' age and feeling about life as a whole(Happiness).When people get old, their breeding ability will decline cause the number of children will remain the same.The children will lead to more happiness of life, so higher feeling about life will have more children. On the other hands,the special case is that some family does not like children because of they think children will bring problems that will decrease their feeling about their life.This study also wants to correctly understand people’s views on children in contemporary society 


The Fitted Model for multiple linear regression:

$$
y_i = \beta_0 + \beta_1 x_{1,i} + \beta_2 x_{2,i} + \epsilon_i
$$

## Results
```{r}
data1 <- read.csv("gss.csv")
boxplot(age ~ total_children, data = data1, ylab="age", xlab="total_children",
        col = "orange", border = "brown", 
        main = "how total children number is correlated with age-graph1") 

```
From this boxplot, it claim that as the total chidren number is increasing, the median age is increasing. Though there is  a outlier from the boxplot that total children number is 6, it does not influence we consider the relationship between total children number and respondents' age.

```{r}
p<-ggplot(data = data1, aes(x = data1$age, y = data1$total_children )) + geom_point(alpha = 0.1)+scale_colour_gradient(low = 'lightblue', high = 'darkblue')
p<-p + labs(title = "relationship between age and total children number-graph2", subtitle = "plot of total children number and respondent age ")
print(p)
```
This plot shows that  most respondent do not have child, and only few respondent have over 6 children, most of them are close to 80 years old.
```{r}
data1 <- data1 %>% filter(!is.na(total_children))
summ <- data1 %>% group_by(total_children) %>% summarise(mean = mean(feelings_life, na.rm=T),median = median(feelings_life, na.rm = T), sd =sd(feelings_life, na.rm = T),IQR=IQR(feelings_life, na.rm = T))

kable(summ, caption="Summary of feelings of life for different  total children number-table3", digits=3, format="markdown")
```
According to this table, we can conclude that for different total children number, their median feelings of life are all 8, while for respondent which has 5 children, their feelings of life is more spread out. Additionally, the mean feelings of lifes value for different total children number is close to 8.
```{r}

hist(data1$total_children, main = "distribution of total children number-graph3", xlab = "total children number" ,col = "yellow",border = "blue")

```
The histogram distribution of the total children number is right-skewed, which shows that most respondents have less than two children, seldom respondent have over 5 children.
```{r}
mod <- lm(total_children ~ feelings_life+age, data=data1)
summary(mod)
kable(summary(mod)$coefficients, caption="Summary how feelings of life and age affect total children number-table2", digits=3)
```
According to the model we use in this case, we can conclude that there is a positive correlation between total number of children and feelings of life, while the age is elder, they are more willing to give birth to baby.
Firstly, we choose the multiple linear regression model, suppose age and the feelings of life are two independent variable for studying whether exist correlation between those two and total children number. After we draw the graph1, median age for different total children number has been shown, which refer that when age is getting elder, people will have more children. From the summary of the model, we collect the value in table 2, it claim that the coefficient for age and feelings of life are all over zero, which means the correlation is positive.

## Discussion

From this dataset, we conclude that the correlation is positive. In other words, person who is elder age and has better feelings of life will have more children. In the process, we use different graph to analyze how different  single variable affect the total children number. However, there is still some weakness  from this study, since some other element will affect the number of total children such as DINK family. we need have more detailed and specified dataset to make further analysis.



# Weaknesses
We assume that there is a positive relation between number of children and age and feelings about life as a whole.This means that people have a older age and higher feelings about life will tend to have more children.However,the dataset may also include groups such as gay and DINK who do not have plans on having children.No matter how their ages and feelings about life change,the number of children will always be 0.This could make our parameters smaller than it actually should be.Also,beyond a certain age level,people may not consider having children anymore or physiologically unable to have children, the total number of children remains fixed.This will make our regression model less accurate in estimating the total number of children.Another weakness is that the result is a digital number,while the number of children has to be an integral.If we get a number of 0.5,it is hard to say that the person has 1 child or 0 child.


# Next Steps
We can add several more independent variables into our multiple regression model,such as wealth/income level or age at marriage to help us estimate total number of children better.
## References

https://sda-artsci-utoronto-ca.myaccess.library.utoronto.ca/sdaweb/dli2/gss/gss31/gss31/more_doc/index.htm

https://sda-artsci-utoronto-ca.myaccess.library.utoronto.ca/sdaweb/dli2/gss/gss31/gss31/more_doc/GSS31_User_Guide.pdf (Pg 3 and Pg 8)
