#DATA CLEANING POST Z SCORE CALCULATION

#remove.packages("rlang")
#install.packages("rlang")

library(tidyr)
#install.packages("ggplot2")
library(ggplot2)          #For graphs
library(RColorBrewer)     #For graphs
#install.packages('reshape2')
library(reshape2)          #For reshaping tables (longer/wider), used for graphing
library(partykit)
#install.packages("dunn.test")
library(dunn.test)          #Post hoc test for kruskal wallis during multivariate analysis
#install.packages("tidymodels")
library(tidymodels)          #For decision trees under multivariate analysis
#install.packages("strucchange")
library(strucchange)          #For feature selection under multivariate analysis
library(caret)                #For many many things (feature select, modeling, etc)
#install.packages("randomForest")
library(randomForest)          #For feature selection under multivariate analysis
library(e1071)                #For Naive Bayes
#install.packages("gmodels")
library(gmodels)              #logistic regression
library(rpart)                #Decision trees
library(rpart.plot)           #Rpart decision tree plots
#install.packages("klaR")
library(klaR)                 #All models after statified k fold cross validation


#IMPORTING UPDATED DATA WITH Z SCORES 
AllDataWALA<-read.csv(file='C:/Users/leahg/Documents/CIND820/AnthroAnalyzer/WALAclean_zscore.csv',header=T,sep=",")
head(AllDataWALA)
is.data.frame(AllDataWALA)
str(AllDataWALA)

#Cleaning data after processing with WHO Anthro Analyzer ----
    #Some variables added for the express purpose of analysis by the tool, no longer needed.
unique(AllDataWALA$included)
    #All the instances were included in the Anthro Analyzer's analysis, this column can be deleted. Same with X, an index
AllDataWALA = subset(AllDataWALA, select = -c(included, X))

    #age_in_days is not necessary, and age_in_months, cmeasure, clenhei and csex are duplicates, remove
AllDataWALA = subset(AllDataWALA, select = -c(age_in_days, age_in_months, cmeasure, clenhei, csex))

    #age_group and cbmi may be useful, leave in but rename (personal preference)
names(AllDataWALA)[names(AllDataWALA) == 'age_group'] <- 'ageGroup'
names(AllDataWALA)[names(AllDataWALA) == 'cbmi'] <- 'BMI'

    #Fewer than 1% of survey responses were flagged due to high z scores (<-6/-5 or >5/6, depending on the original measurement), signalling implausible values
    #Z scores for BMI will not be considered, as it will not be used for this project. Will focus on length/height, weight, and length/height for weight only
flaggedData <- subset(AllDataWALA, zlen_flag == 1 | zwei_flag == 1| zwfl_flag == 1)
    #15 rows were flagged and reviewed
    #For rows where z scores were positive (taller or heavier than most of group, 5 total), reviewed the data and did not see any major issues with values for most.
    #Left as is, since I am focused on underweight/stunted/wasting, so low
    #Reviewed weight and length/height first, then will see if that corrects issues with weight for length/height flags

    #For two rows with <-6 z score for height/length, found it was over 10 cm lower than other children in that month. Removed height and length
AllDataWALA$comboHtLgth<-replace(AllDataWALA$comboHtLgth, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2493, NA) #confirmed no duplicate anon code combos first
AllDataWALA$BMI<-replace(AllDataWALA$BMI, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2493, NA)
AllDataWALA$zlen<-replace(AllDataWALA$zlen, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2493, NA)
AllDataWALA$zlen_flag<-replace(AllDataWALA$zlen_flag, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2493, NA)
AllDataWALA$zbmi<-replace(AllDataWALA$zbmi, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2493, NA)
AllDataWALA$zbmi_flag<-replace(AllDataWALA$zbmi_flag, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2493, NA)
AllDataWALA$zwfl<-replace(AllDataWALA$zwfl, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2493, NA)
AllDataWALA$zwfl_flag<-replace(AllDataWALA$zwfl_flag, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2493, NA)

AllDataWALA$comboHtLgth<-replace(AllDataWALA$comboHtLgth, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 836, NA)
AllDataWALA$BMI<-replace(AllDataWALA$BMI, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 836, NA)
AllDataWALA$zlen<-replace(AllDataWALA$zlen, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 836, NA)
AllDataWALA$zlen_flag<-replace(AllDataWALA$zlen_flag, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 836, NA)
AllDataWALA$zbmi<-replace(AllDataWALA$zbmi, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 836, NA)
AllDataWALA$zbmi_flag<-replace(AllDataWALA$zbmi_flag, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 836, NA)
AllDataWALA$zwfl<-replace(AllDataWALA$zwfl, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 836, NA)
AllDataWALA$zwfl_flag<-replace(AllDataWALA$zwfl_flag, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 836, NA)

    #for height/length, rows with less than 10 cm differences between the flagged height/length and the next observation were left as is to account for 
    #potential extreme cases of stunting instead of survey error (index 1227, 1536, 2712)

    #for index 190, weight is lower for age and very low for height, but not so much that it appears as an error. Leave

    #for row index 2007, appears to be a 95 that was missed during cleaning (15 cm longer than other children that age), corrected here
AllDataWALA$comboHtLgth<-replace(AllDataWALA$comboHtLgth, AllDataWALA$comboHtLgth==95 & AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2232, NA)
AllDataWALA$BMI<-replace(AllDataWALA$BMI, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2232, NA)
AllDataWALA$zlen<-replace(AllDataWALA$zlen, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2232, NA)
AllDataWALA$zlen_flag<-replace(AllDataWALA$zlen_flag, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2232, NA)
AllDataWALA$zbmi<-replace(AllDataWALA$zbmi, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2232, NA)
AllDataWALA$zbmi_flag<-replace(AllDataWALA$zbmi_flag, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2232, NA)
AllDataWALA$zwfl<-replace(AllDataWALA$zwfl, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2232, NA)
AllDataWALA$zwfl_flag<-replace(AllDataWALA$zwfl_flag, AllDataWALA$childCode == 2 & AllDataWALA$childQNum == 2232, NA)

    #for row index 946, weight was listed as 6 kg, 1.5 kg smaller than the next measurement. Appears to be an error, removed weight measurement
AllDataWALA$weight<-replace(AllDataWALA$weight, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 1449, NA)
AllDataWALA$BMI<-replace(AllDataWALA$BMI, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 1449, NA)
AllDataWALA$zwei<-replace(AllDataWALA$zwei, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 1449, NA)
AllDataWALA$zwei_flag<-replace(AllDataWALA$zwei_flag, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 1449, NA)
AllDataWALA$zbmi<-replace(AllDataWALA$zbmi, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 1449, NA)
AllDataWALA$zbmi_flag<-replace(AllDataWALA$zbmi_flag, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 1449, NA)
AllDataWALA$zwfl<-replace(AllDataWALA$zwfl, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 1449, NA)
AllDataWALA$zwfl_flag<-replace(AllDataWALA$zwfl_flag, AllDataWALA$childCode == 17 & AllDataWALA$childQNum == 1449, NA)

    #for row index 2615, weight is only 6 kg for 53 month old. Likely was supposed to be 16 but no way of confirming (no weight record previously to trend 
    #with), removed and corrected here
AllDataWALA$weight<-replace(AllDataWALA$weight, AllDataWALA$weight==6.2 & AllDataWALA$childCode == 16 & AllDataWALA$childQNum == 1407, NA)
AllDataWALA$BMI<-replace(AllDataWALA$BMI, AllDataWALA$comboHtLgth==106.3 & AllDataWALA$childCode == 16 & AllDataWALA$childQNum == 1407, NA)
AllDataWALA$zwei<-replace(AllDataWALA$zwei, AllDataWALA$comboHtLgth==106.3 & AllDataWALA$childCode == 16 & AllDataWALA$childQNum == 1407, NA)
AllDataWALA$zwei_flag<-replace(AllDataWALA$zwei_flag, AllDataWALA$comboHtLgth==106.3 & AllDataWALA$childCode == 16 & AllDataWALA$childQNum == 1407, NA)
AllDataWALA$zbmi<-replace(AllDataWALA$zbmi, AllDataWALA$comboHtLgth==106.3 & AllDataWALA$childCode == 16 & AllDataWALA$childQNum == 1407, NA)
AllDataWALA$zbmi_flag<-replace(AllDataWALA$zbmi_flag, AllDataWALA$comboHtLgth==106.3 & AllDataWALA$childCode == 16 & AllDataWALA$childQNum == 1407, NA)
AllDataWALA$zwfl<-replace(AllDataWALA$zwfl, AllDataWALA$comboHtLgth==106.3 & AllDataWALA$childCode == 16 & AllDataWALA$childQNum == 1407, NA)
AllDataWALA$zwfl_flag<-replace(AllDataWALA$zwfl_flag, AllDataWALA$comboHtLgth==106.3 & AllDataWALA$childCode == 16 & AllDataWALA$childQNum == 1407, NA)

    #For index 1981, weight was very low and height was very high for age. Can't determine where the error is so removed entire row
AllDataWALA <- AllDataWALA[-c(1981), ]

    #to confirm changes were made
flaggedDataUpdate <- subset(AllDataWALA, zlen_flag == 1 | zwei_flag == 1| zwfl_flag == 1)
    #the six instances were removed from the flagged group

#finish cleaning variables in complete dataset
    #flag and bmi z score columns can be removed
AllDataWALA = subset(AllDataWALA, select = -c(zlen_flag, zwei_flag, zbmi, zbmi_flag, zwfl_flag))

    #Rename remaining columns
names(AllDataWALA)[names(AllDataWALA) == 'zlen'] <- 'lengthZ'
names(AllDataWALA)[names(AllDataWALA) == 'zwei'] <- 'weightZ'
names(AllDataWALA)[names(AllDataWALA) == 'zwfl'] <- 'wt4LgthZ'


#Creating dependent variables ----
`%notin%` <- Negate(`%in%`)
AllDataWALA$stunted = NA
AllDataWALA[AllDataWALA$lengthZ < -2 & AllDataWALA$lengthZ %notin% NA,]$stunted <- TRUE
AllDataWALA[AllDataWALA$lengthZ >= -2 & AllDataWALA$lengthZ %notin% NA,]$stunted <- FALSE
table(AllDataWALA$stunted)
sum(is.na(AllDataWALA$stunted))

    #WHO classifies children with oedema of the lower limbs as automatically severely malnourished, and weight related z-scores are not 
    #calculated as they are misrepresented by retained fluid. Reflected here for wasting and underweight status
AllDataWALA$wasting = NA
AllDataWALA[AllDataWALA$wt4LgthZ < -2 & AllDataWALA$wt4LgthZ %notin% NA,]$wasting <- TRUE
AllDataWALA[AllDataWALA$oedema == 'y',]$wasting <- TRUE
AllDataWALA[AllDataWALA$wt4LgthZ >= -2 & AllDataWALA$wt4LgthZ %notin% NA,]$wasting <- FALSE
table(AllDataWALA$wasting)
sum(is.na(AllDataWALA$wasting))

AllDataWALA$underweight = NA
AllDataWALA[AllDataWALA$weightZ < -2 & AllDataWALA$weightZ %notin% NA,]$underweight <- TRUE
AllDataWALA[AllDataWALA$oedema == 'y',]$underweight <- TRUE
AllDataWALA[AllDataWALA$weightZ >= -2 & AllDataWALA$weightZ %notin% NA,]$underweight <- FALSE
table(AllDataWALA$underweight)
sum(is.na(AllDataWALA$underweight))

dependentNA <- subset(AllDataWALA, stunted %in% NA & underweight %in% NA & wasting %in% NA)
    #19 survey responses had NAs for stunted, underweight, and wasting variables, all due to no listed gender
    #(Anthro Analyzer cannot calculate Z scores without it). Removed here as they will not have a dependent variable
AllDataWALA <- AllDataWALA[-which(AllDataWALA$stunted %in% NA & AllDataWALA$underweight %in% NA & AllDataWALA$wasting %in% NA),]
    #to confirm I removed them all
subset(AllDataWALA, stunted %in% NA & underweight %in% NA & wasting %in% NA)

#create combined dependent variable (malnourished)
AllDataWALA$malnourished = FALSE
AllDataWALA[AllDataWALA$underweight == TRUE & AllDataWALA$underweight %notin% NA | AllDataWALA$stunted == TRUE & AllDataWALA$stunted %notin% NA | AllDataWALA$wasting == TRUE & AllDataWALA$wasting %notin% NA,]$malnourished <- TRUE
table(AllDataWALA$malnourished)
sum(is.na(AllDataWALA$malnourished))

#Additional cleaning found during exploratory data analysis (some previously cleaned,
    #but I think the Anthro Anayzer did some funny things to it)

    #NA  missing from howMeasure column
AllDataWALA[AllDataWALA$comboHtLgth %in% NA,]$howMeasure <- NA
sum(is.na(AllDataWALA$howMeasure))

    #Same NA issue with childDoc
AllDataWALA$childDoc<-replace(AllDataWALA$childDoc, AllDataWALA$childDoc=='', NA)
sum(is.na(AllDataWALA$childDoc))

    #And with startBreastfeeding
AllDataWALA$startBreastfeed<-replace(AllDataWALA$startBreastfeed, AllDataWALA$startBreastfeed=='', NA)
sum(is.na(AllDataWALA$startBreastfeed))
    #Moral of the story, ALWAYS be suspicious of 0 NA unless I absolutely know it's true
    #Great tripping point after extracting then uploading data again to make sure cleaning from
    #earlier carries over

    #one instance of child eating yogurt 22 times the day before, assume this was
    #2 times and incorrectly input. Corrected here
AllDataWALA$liquidYogurtNum<-replace(AllDataWALA$liquidYogurtNum, AllDataWALA$liquidYogurtNum==22, 2)




#EXPLORATORY DATA ANALYSIS - univariate analysis ----

str(AllDataWALA)
summary(AllDataWALA)
apply(X = is.na(AllDataWALA), MARGIN = 2, FUN = sum)

#All independent variables ----
    #Gathered summary statistics and graphed to visually inspect normalcy (box plot, bar graph, density plot)

    #District
ggplot(AllDataWALA, aes(x=districtName)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='District Name', y= "Count", title= 'Survey responses by district') +  theme_bw() + theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))

    #Sex of the child
ggplot(AllDataWALA, aes(x=childSex)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Child Sex (Male, Female)', y= "Count", title= 'Survey responses by child sex') +  theme_bw()

    #biomom
ggplot(AllDataWALA, aes(x=bioMom)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x = 'Respondent',y= "Count", title= 'Relationship of the respondent to the child') +  theme_bw()

    #birthYear
ggplot(AllDataWALA, aes(x=birthYear)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x = 'Year',y= "Count", title= "Child's year of birth") +  theme_bw()

    #ageMonth
ggplot(AllDataWALA, aes(x=ageMonth)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x = 'Age',y= "Count", title= "Child's age in months") +  theme_bw()
    #visually awful, so adjust
boxplot(AllDataWALA$ageMonth, ylab = "Number of months", col = '#DCEFFF')
title("Child's age in months")

    #ifInfant
ggplot(AllDataWALA, aes(x=ifInfant)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x = 'Status',y= "Count", title= 'If the child is an infant (under 1 year) or not (a year or older)') +  theme_bw()

    #DisposeFaeces
ggplot(AllDataWALA, aes(x=disposeFaeces)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Disposal Location', y= "Count", title= 'How faeces was disposed of') +  theme_bw() + theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))

    #All handwashing practices, want everything on one graph
    #Large differences between different practices so want column's labeled, stick with ggplot
    #Developed dummy dataset with totals

whenWash <- c('never handwash', 'never handwash', 'before food prep', 'before food prep', 'after food prep', 'after food prep',
              'before feeding children', 'before feeding children', 'after feeding children', 'after feeding children', 
              'before cleaning children', 'before cleaning children', 'after using toilet', 'after using toilet', 'after child defecates', 
              'after child defecates', 'before eating', 'before eating', 'after eating', 'after eating', 'other', 'other')
logicWash <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
numberWash <- c(258, 2440, 569, 2129, 122, 2576, 2446, 252, 107, 2591, 119, 2579, 2108, 590, 1679, 1019, 728, 1970, 259, 2439, 41, 2657)

washDF <- data.frame(whenWash, logicWash, numberWash)

ggplot(washDF, aes(whenWash, numberWash, fill = logicWash)) + geom_bar(colour = 'black', stat="identity", position = "dodge") + scale_fill_manual(values=c('#DCEFFF', '#D6B4FC')) +
  geom_text(aes(label = numberWash, group = logicWash), position = position_dodge(width=1), vjust = -0.2, hjust = 0.5) +
  labs(x= 'Situations', y="count", title="When respondents wash their hands") + 
  theme_bw()+ theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))

    #Bednet
ggplot(AllDataWALA, aes(x=bedNet)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Bednet', y= "Count", title= 'If the child slept under a bednet last night') +  theme_bw()

    #Weighin
ggplot(AllDataWALA, aes(x=weighIn)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Weigh in', y= "Count", title= 'If the child was weighed at a GMP session before') +  theme_bw()

    #Child document
ggplot(AllDataWALA, aes(x=childDoc)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Document?', y= "Count", title= 'If the child has official documents') +  theme_bw()

    #childWtMonth
ggplot(AllDataWALA, aes(x=childWtMonth)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Were they weighed', y= "Count", title= 'If the child was weighed in the last month') +  theme_bw()

    #childWt3Month
ggplot(AllDataWALA, aes(x=childWt3Month)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Were they weighed', y= "Count", title= 'If the child was weighed twice in the last three months') +  theme_bw()

    #drankBreastmilk
ggplot(AllDataWALA, aes(x=drankBreastmilk)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Breastmilk?', y= "Count", title= 'Was the child breastfed or fed breastmilk in some way') +  theme_bw()

    #startedBreastfeeding
ggplot(AllDataWALA, aes(x=startBreastfeed)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Date', y= "Count", title= 'When the child started breastfeeding after birth') +  theme_bw()

    #other3day
ggplot(AllDataWALA, aes(x=other3Day)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Anything besides breastmilk?', y= "Count", title= 'If the child received anything other than breastmilk in the first three days of life') +  theme_bw()

    #other6Month
ggplot(AllDataWALA, aes(x=other6Month)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Anything besides breastmilk?', y= "Count", title= 'If the child received anything other than breastmilk in the first six months of life') +  theme_bw()

    #stillBreastfed
ggplot(AllDataWALA, aes(x=stillBreastfed)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Status?', y= "Count", title= 'If the child is still breastfed') +  theme_bw()

    #while it is a bivariate analysis, I am curious about breastfeeding status by age here

ggplot(data=AllDataWALA, aes(x=ageGroup, fill = stillBreastfed)) + geom_bar(colour = 'black', position = "dodge") + 
  scale_fill_manual(values=c('#DCEFFF', '#D6B4FC', '#F9D7D7'))+  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, hjust = 0.5) +
  labs(x='Age Group', y= "Count", title= 'Number of children breastfed by Age Group') +  theme_bw() + 
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))


    #breastfedYday
ggplot(AllDataWALA, aes(x=breastfedYday)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Breastfed yesterday?', y= "Count", title= 'If the child was breastfed yesterday') +  theme_bw()

    #breastfedYdayNum
ggplot(AllDataWALA, aes(x=breastfedYdayNum)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Number of times breastfed?', y= "Count", title= 'How many times the child breastfed yesterday') +  theme_bw()

    #Now that I'm seeing this graph, think the 40 and 50 are actually outliers due to survey entry error, should be
    #4 and 5. Correct here
AllDataWALA$breastfedYdayNum<-replace(AllDataWALA$breastfedYdayNum, AllDataWALA$breastfedYdayNum==40, 4)
AllDataWALA$breastfedYdayNum<-replace(AllDataWALA$breastfedYdayNum, AllDataWALA$breastfedYdayNum==50, 5)
summary(AllDataWALA$breastfedYdayNum)

ggplot(AllDataWALA, aes(x=breastfedYdayNum)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Number of times breastfed?', y= "Count", title= 'How many times the child breastfed yesterday') +  theme_bw()


    #Number of times a liquid was consumed (with all liquid counts on one graph)
liquidData <- AllDataWALA[c('liquidFormulaNum', 'liquidMilkNum', 'liquidYogurtNum')]
summary(liquidData)
summary(AllDataWALA$liquidFormulaNum)
summary(AllDataWALA$liquidMilkNum)
summary(AllDataWALA$liquidYogurtNum)
    #Checking everything moved correctly

newLiquidData <- liquidData %>% pivot_longer(everything(), names_to = "liquidType", values_to = "number", values_drop_na = FALSE)
    #Creating longer pivot table instead of wider

    #Did not include NA in graph as that obscured ability to read it, but left in data just in case it was needed
liquidTable <- table(newLiquidData)
liquidTable
rownames(liquidTable) = c("Formula", "Milk", "Yogurt")
liquidTable = t(liquidTable)

display.brewer.all()
barplot(liquidTable, legend.text = TRUE, beside = TRUE, main = "Number of times specific liquids were consumed", 
        xlab = "Liquid", ylab = "Count", col = brewer.pal(4, "Pastel2"), cex.main = 0.75)

    #I really want the columns to be labeled with the count, so switching to ggplot
liquidType <- c('Formula', 'Formula', 'Formula', 'Formula', 'Milk', 'Milk', 'Milk', 'Milk', 'Yogurt', 
                'Yogurt', 'Yogurt', 'Yogurt')
NumConsm <- c('1', '2', '3', '4', '1', '2', '3', '4', '1', '2', '3', '4') 
number <- c(10, 4, 2, 0, 22, 10, 2, 0, 47, 11, 3, 1)

liquidDF <- data.frame(liquidType, NumConsm, number)

ggplot(liquidDF, aes(liquidType, number, fill = NumConsm)) + geom_bar(colour = 'black', stat="identity", position = "dodge") + 
  scale_fill_manual(values=c('#DCEFFF', '#D6B4FC', '#F9D7D7', '#FFFCB7')) +
  geom_text(aes(label = number, group = NumConsm), position = position_dodge(width=1), vjust = -0.2, hjust = 0.5) +
  labs(x= 'Liquid Type', y="count", title="What children drank yesterday") + 
  theme_bw()+ theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))


    #Types of liquids consumed
liquidOnly <- AllDataWALA[c('liquidORS', 'liquidWater', 'liquidFormula', 'liquidMilk', 'liquidJuice', 'liquidBroth', 
                            'liquidYogurt', 'liquidPorridge', 'liquidTea', 'liquidOther')]
summary(liquidOnly)


liquidPivot <- liquidOnly %>% pivot_longer(everything(), names_to = "liquidType", values_to = "number", values_drop_na = FALSE)
    #Creating longer pivot table instead of wider for data
table(liquidPivot, useNA = 'ifany')

    #With the big differences in numbers, I want to make sure the rows are labeled for easy review (ggplot), so manual input it is
liquidType <- c('Broth', 'Broth', 'Formula', 'Formula', 'Juice', 'Juice', 'Milk', 'Milk', 'ORS', 'ORS', 'Other', 'Other', 
                'Porridge', 'Porridge', 'Tea', 'Tea', 'Water', 'Water', 'Yogurt', 'Yogurt')
liquidLogic <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE) 
liquidNumber <- c(2677, 19, 2678, 18, 2465, 231, 2627, 69, 2655, 40, 2464, 232, 2269, 427, 2458, 238, 258, 2438, 2633, 63)

liquidTypeDF <- data.frame(liquidType, liquidLogic, liquidNumber)

ggplot(liquidTypeDF, aes(liquidType, liquidNumber, fill = liquidLogic)) + geom_bar(colour = 'black', stat="identity", position = "dodge") + 
  scale_fill_manual(values=c('#DCEFFF', '#D6B4FC', '#FFFCB7')) +
  geom_text(aes(label = liquidNumber, group = liquidLogic), position = position_dodge(width=1), vjust = -0.2, hjust = 0.5) +
  labs(x= 'Liquid Type', y="count", title="What children drank yesterday") + 
  theme_bw()+ theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))


    #Types of food consumed
foodData <- AllDataWALA[c('foodGrain', 'foodYellow', 'foodWhite', 'foodGreen', 'foodMangoPap',
                          'foodProduce', 'foodOrgan', 'foodMeat', 'foodEgg', 'foodFish', 'foodBean',
                          'foodDairy', 'foodFats', 'foodSugar', 'foodCondiment', 'foodInsect', 'foodRedPalm',
                          'foodOther', 'foodSalt')]
summary(foodData)

newFoodData <- foodData %>% pivot_longer(everything(), names_to = "Food Type", values_to = "True or False", values_drop_na = FALSE)
    #Creating longer pivot table

foodTable <- table(newFoodData)
foodTable
rownames(foodTable) = c("Beans", "Condiments", "Dairy", "Egg", "Fats", "Fish", "Grain", "Green Produce", "Insects", "Mango or Papaya",
                        "Meat", "Organ Meat", "Other Foods", "Other Produce", "Red Palm", "Salt", "Sugar", "White Roots", "Yellow Produce")
foodTable = t(foodTable)

barplot(foodTable, legend.text = TRUE, beside = TRUE, main = "Type of foods consumed", 
        xlab = "Food Group", ylab = "Count", col = c('#DCEFFF', '#D6B4FC'), cex.main = 0.75)
    #Not a huge fan of this one either, switch to ggplot

foodType <- c('Beans', 'Beans', 'Condiments', 'Condiments', 'Dairy', 'Dairy', 'Egg', 'Egg', 'Fats', 'Fats', 'Fish', 'Fish', 'Grain', 'Grain', 
              'Green Produce', 'Green Produce', 'Insects', 'Insects', 'Mango or Papaya', 'Mango or Papaya', 'Meat', 'Meat', 'Organ Meat', 'Organ Meat', 
              'Other Foods', 'Other Foods', 'Other Produce', 'Other Produce', 'Red Palm', 'Red Palm', 'Salt', 'Salt', 'Sugar', 'Sugar', 'White Roots', 
              "White Roots", 'Yellow Produce', 'Yellow Produce')
foodLogic <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, 
              FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE) 
numFood <- c(1536, 1161, 2539, 158, 2637, 59, 2552, 142, 1355, 1342, 1710, 987, 693, 2004, 1475, 1221, 2632, 65, 994, 1702, 2584, 113, 2682, 15, 
            766, 1930, 2352, 345, 2688, 9, 305, 2392, 2146, 551, 2124, 573, 2604, 93)

foodDF <- data.frame(foodType, foodLogic, numFood)

ggplot(foodDF, aes(foodType, numFood, fill = foodLogic)) + geom_bar(colour = 'black', stat="identity", position = "dodge") + 
  scale_fill_manual(values=c('#DCEFFF', '#D6B4FC')) +
  geom_text(aes(label = numFood, group = foodLogic), position = position_dodge(width=1), vjust = -0.2, hjust = 0.5) +
  labs(x= 'Food Type', y="count", title="What children ate yesterday") + 
  theme_bw()+ theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))

    #foodNum
boxplot(AllDataWALA$foodNum, ylab = "Number", col = '#DCEFFF')
title("Number of times the child ate yesterday")
    #Skewed to the right
    #0 is identified as an outlier, but it is feasible given the data and situation, will leave as is
    #Meal numbers over 4 were identified as outliers, however it is feasible that a child ate many small
    #snacks during the day. Will leave as is but keep in mind the potential of these outliers during future analysis

    #Can also be visualized as bar graph to see the skewing
ggplot(AllDataWALA, aes(x=foodNum)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Number', y= "Count", title= 'Number of times the child ate yesterday') +  theme_bw()

    #weight
boxplot(AllDataWALA$weight, ylab = "Kilograms", col = '#DCEFFF')
title("Children's weight")

ggplot(AllDataWALA, aes(x=weight)) + 
  geom_density(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +
  labs(x='Kilograms', y= "Count", title= "Children's Weight") +  theme_bw()

    #Height and length
boxplot(AllDataWALA$comboHtLgth, ylab = "Centimeters", col = '#DCEFFF')
title("Children's Height or Length")

ggplot(AllDataWALA, aes(x=comboHtLgth)) + 
  geom_density(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +
  labs(x='Centimeters', y= "Count", title= "Children's Height or Length") +  theme_bw()

    #curious about age differences here for length/height and weight
ggplot(data=AllDataWALA, aes(x=ageGroup, y = comboHtLgth)) + geom_boxplot(colour = 'black', position = "dodge") +
  labs(x='Age Group', y= "Height or length", title= 'Height and length of children, by age group') +  theme_bw() + 
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))
    #can see the spread/number of instances of length/height in children 0-5 months is much narrower compared to other
    #age groups. Checked the data, and many of the children were only weighed and not measured. To be considered for future modeling

ggplot(data=AllDataWALA, aes(x=ageGroup, y = weight)) + geom_boxplot(colour = 'black', position = "dodge") +
  labs(x='Age Group', y= "Weight", title= 'Weight of children, by age group') +  theme_bw() + 
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))

    #howMeasure
ggplot(AllDataWALA, aes(x=howMeasure)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Length or Height', y= "Count", title= 'How the child was measured for the anthropometric section of the survey') +  theme_bw()

    #Oedema
ggplot(AllDataWALA, aes(x=oedema)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Oedema', y= "Count", title= 'If the child exhibited oedema') +  theme_bw()

    #resultsMeasure
ggplot(AllDataWALA, aes(x=resultsMeasure)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='What was measured', y= "Count", title= 'Whether height/length and weight were measured for the anthropometric section of the survey') +  theme_bw()

    #Treatment group
ggplot(AllDataWALA, aes(x=Treatment)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Treatment group', y= "Count", title= 'If the child was from a WALA Community') +  theme_bw()

    #ageGroup
ggplot(AllDataWALA, aes(x=ageGroup)) + 
  geom_bar(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2) +
  labs(x='Age', y= "Count", title= 'Number of children in each age group') +  theme_bw()

    #BMI
boxplot(AllDataWALA$BMI, ylab = "BMI", col = '#DCEFFF')
title("Children's BMI")

ggplot(AllDataWALA, aes(x=BMI)) + 
  geom_density(colour = 'black', fill='#DCEFFF', na.rm = FALSE) +
  labs(x='BMI', y= "Count", title= "Children's BMI") +  theme_bw()

    #Z scores
zData <- AllDataWALA[c('lengthZ', 'weightZ', 'wt4LgthZ')]
colnames(zData) = c("Length Z Score", "Weight Z Score", "Weight for Length Z Score")
summary(zData)
newZData <- zData %>% pivot_longer(everything(), names_to = "zScore", values_to = "number", values_drop_na = FALSE)
zTable <- table(newZData)
zTable

boxplot(number ~ zScore, newZData, ylab = "Value", xlab = "Measurement", col = '#DCEFFF')
title("Z Scores")
    #Not great visualization, switch to ggplot for more adjustability

ggplot(newZData, aes(x=zScore, y=number)) + geom_boxplot() +
  scale_fill_manual(values=c('#DCEFFF', '#D6B4FC', '#FFFCB7')) +
  labs(x= 'Measurement', y="Z score value", title="Children's Z Scores") + 
  theme_bw()+ theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))

    #Density graph for easier visibility
ggplot(newZData, aes(x=number, colour = zScore)) + 
  geom_density(na.rm = FALSE) + 
  labs(x='Z Score', y= "Density", title= "Children's Z Scores") +  theme_bw()




#Logical data DEPENDENT ----
depData <- AllDataWALA[c('stunted', 'wasting', 'underweight', 'malnourished')]
summary(depData)
newDepData <- depData %>% pivot_longer(everything(), names_to = "Dependent Variable", values_to = "True or False", values_drop_na = FALSE)
depTable <- table(newDepData)
depTable
depTable <- t(depTable)

barplot(depTable, legend.text = TRUE, beside = TRUE, main = "Malnutrition Status", 
        xlab = "Dependent Variable", ylab = "Count", col = c('#DCEFFF', '#D6B4FC'), cex.main = 0.75)

    #ggplot, to allow for more customizing and make my life harder
depType <- c('Malnourished', 'Malnourished', 'Stunted', 'Stunted', 'Underweight', 'Underweight', 'Wasting', 'Wasting')
depTorF <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE) 
numDep <- c(1740, 958, 1610, 809, 2295, 397, 2247, 181)

depDF <- data.frame(depType, depTorF, numDep)

ggplot(depDF, aes(depType, numDep, fill = depTorF)) + geom_bar(colour = 'black', stat="identity", position = "dodge") + 
  scale_fill_manual(values=c('#DCEFFF', '#D6B4FC')) +
  geom_text(aes(label = numDep, group = depTorF), position = position_dodge(width=1), vjust = -0.2, hjust = 0.5) +
  labs(x= 'Dependent Variable', y="Count", title="Malnutrition Status") + 
  theme_bw()+ theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))


    #Fixing breastmilk numbers, changing any who did not receive formula yesterday to 0 instead of NA
AllDataWALA$breastfedYday<-replace(AllDataWALA$breastfedYday, AllDataWALA$stillBreastfed == FALSE, FALSE)
AllDataWALA$breastfedYdayNum<-replace(AllDataWALA$breastfedYdayNum, AllDataWALA$breastfedYday == FALSE, 0)
summary(AllDataWALA$breastfedYday)
summary(AllDataWALA$breastfedYdayNum)

    #same with formula
AllDataWALA$liquidMilkNum<-replace(AllDataWALA$liquidMilkNum, AllDataWALA$liquidMilk == FALSE, 0)
summary(AllDataWALA$liquidMilkNum)

    #same with milk
AllDataWALA$liquidFormulaNum<-replace(AllDataWALA$liquidFormulaNum, AllDataWALA$liquidFormula == FALSE, 0)
summary(AllDataWALA$liquidFormulaNum)

#Forgot to correct NA here before deleting if they consumed liquid yogurt variable
#(after combining into overall dairy), pull data from previous version of dataset to
#correct here
AllDataWALA$liquidYogurtNum<-replace(AllDataWALA$liquidYogurtNum, AllDataWALA$liquidYogurt == FALSE, 0)
summary(AllDataWALA$liquidYogurtNum)




#EXPLORATORY DATA ANALYSIS - bivariate analysis ----

    #Most bivariate analyses/correlations become difficult with a mix of data types
    #To combat this, I will convert everything to numeric data types and use Kendall's correlation
    #(much of the data is not normally distributed, so need a non-parametric test)
    #I know this is not statistically sound (creating artificial fixed differences [1 to 2, 2 to 3] in
    #data that may not be ordinal or have fixed differences in this way). This will only be a starting point
    # If any "converted" variables are highly correlated (i.e. I could remove one), then I will explore 
    #that correlation further with the variables original data type before deleting anything

dataCorr <- AllDataWALA

    #District name
unique(dataCorr$districtName)
dataCorr$districtName<-replace(dataCorr$districtName, dataCorr$districtName=='BALAKA', 1)
dataCorr$districtName<-replace(dataCorr$districtName, dataCorr$districtName=='CHIKWAWA', 2)
dataCorr$districtName<-replace(dataCorr$districtName, dataCorr$districtName=='CHIRADZULU', 3)
dataCorr$districtName<-replace(dataCorr$districtName, dataCorr$districtName=='MACHINGA', 4)
dataCorr$districtName<-replace(dataCorr$districtName, dataCorr$districtName=='MULANJE', 5)
dataCorr$districtName<-replace(dataCorr$districtName, dataCorr$districtName=='NSANJE', 6)
dataCorr$districtName<-replace(dataCorr$districtName, dataCorr$districtName=='THYOLO', 7)
dataCorr$districtName<-replace(dataCorr$districtName, dataCorr$districtName=='ZOMBA', 8)
unique(dataCorr$districtName)
dataCorr$districtName <- sapply(dataCorr$districtName, as.numeric)
str(dataCorr$districtName)

    #Sex
dataCorr$childSex<-replace(dataCorr$childSex, dataCorr$childSex=='M', 1)
dataCorr$childSex<-replace(dataCorr$childSex, dataCorr$childSex=='F', 2)
unique(dataCorr$childSex)
dataCorr$childSex <- sapply(dataCorr$childSex, as.numeric)
str(dataCorr$childSex)

    #If the respondent is the child's biological mother
dataCorr$bioMom<-replace(dataCorr$bioMom, dataCorr$bioMom=='biological mother', 1)
dataCorr$bioMom<-replace(dataCorr$bioMom, dataCorr$bioMom=='caregiver', 2)
dataCorr$bioMom<-replace(dataCorr$bioMom, dataCorr$bioMom=='other', 3)
unique(dataCorr$bioMom)
dataCorr$bioMom <- sapply(dataCorr$bioMom, as.numeric)
str(dataCorr$bioMom)

    #If the child is an infant
dataCorr$ifInfant<-replace(dataCorr$ifInfant, dataCorr$ifInfant==TRUE, 1)
dataCorr$ifInfant<-replace(dataCorr$ifInfant, dataCorr$ifInfant==FALSE, 0)
unique(dataCorr$ifInfant)
dataCorr$ifInfant <- sapply(dataCorr$ifInfant, as.numeric)
str(dataCorr$ifInfant)

    #Where faeces was disposed of
unique(dataCorr$disposeFaeces)
dataCorr$disposeFaeces<-replace(dataCorr$disposeFaeces, dataCorr$disposeFaeces=='dropping into toilet facility', 1)
dataCorr$disposeFaeces<-replace(dataCorr$disposeFaeces, dataCorr$disposeFaeces=='rinsed/washed away', 2)
dataCorr$disposeFaeces<-replace(dataCorr$disposeFaeces, dataCorr$disposeFaeces=='other', 3)
dataCorr$disposeFaeces<-replace(dataCorr$disposeFaeces, dataCorr$disposeFaeces=='buried in yard', 4)
dataCorr$disposeFaeces<-replace(dataCorr$disposeFaeces, dataCorr$disposeFaeces=='throw outside the compound', 5)
dataCorr$disposeFaeces<-replace(dataCorr$disposeFaeces, dataCorr$disposeFaeces=='dropping in rubbish pit', 6)
dataCorr$disposeFaeces<-replace(dataCorr$disposeFaeces, dataCorr$disposeFaeces=='did nothing/left it there', 7)
dataCorr$disposeFaeces<-replace(dataCorr$disposeFaeces, dataCorr$disposeFaeces=='throw in the compound', 8)
unique(dataCorr$disposeFaeces)
dataCorr$disposeFaeces <- sapply(dataCorr$disposeFaeces, as.numeric)
str(dataCorr$disposeFaeces)

    #Never handwash
dataCorr$neverHW<-replace(dataCorr$neverHW, dataCorr$neverHW==TRUE, 1)
dataCorr$neverHW<-replace(dataCorr$neverHW, dataCorr$neverHW==FALSE, 0)
unique(dataCorr$neverHW)
dataCorr$neverHW <- sapply(dataCorr$neverHW, as.numeric)
str(dataCorr$neverHW)

    #Handwash before food prep
dataCorr$bfFoodPrepHW<-replace(dataCorr$bfFoodPrepHW, dataCorr$bfFoodPrepHW==TRUE, 1)
dataCorr$bfFoodPrepHW<-replace(dataCorr$bfFoodPrepHW, dataCorr$bfFoodPrepHW==FALSE, 0)
unique(dataCorr$bfFoodPrepHW)
dataCorr$bfFoodPrepHW <- sapply(dataCorr$bfFoodPrepHW, as.numeric)
str(dataCorr$bfFoodPrepHW)

    #After food prep
dataCorr$aftFoodPrepHW<-replace(dataCorr$aftFoodPrepHW, dataCorr$aftFoodPrepHW==TRUE, 1)
dataCorr$aftFoodPrepHW<-replace(dataCorr$aftFoodPrepHW, dataCorr$aftFoodPrepHW==FALSE, 0)
unique(dataCorr$aftFoodPrepHW)
dataCorr$aftFoodPrepHW <- sapply(dataCorr$aftFoodPrepHW, as.numeric)
str(dataCorr$aftFoodPrepHW)

    #Before feeding the child
dataCorr$bfFeedChildHW<-replace(dataCorr$bfFeedChildHW, dataCorr$bfFeedChildHW==TRUE, 1)
dataCorr$bfFeedChildHW<-replace(dataCorr$bfFeedChildHW, dataCorr$bfFeedChildHW==FALSE, 0)
unique(dataCorr$bfFeedChildHW)
dataCorr$bfFeedChildHW <- sapply(dataCorr$bfFeedChildHW, as.numeric)
str(dataCorr$bfFeedChildHW)

    #After feeding the child
dataCorr$aftFeedChildHW<-replace(dataCorr$aftFeedChildHW, dataCorr$aftFeedChildHW==TRUE, 1)
dataCorr$aftFeedChildHW<-replace(dataCorr$aftFeedChildHW, dataCorr$aftFeedChildHW==FALSE, 0)
unique(dataCorr$aftFeedChildHW)
dataCorr$aftFeedChildHW <- sapply(dataCorr$aftFeedChildHW, as.numeric)
str(dataCorr$aftFeedChildHW)

    #Before cleaning the child
dataCorr$bfCleanChildHW<-replace(dataCorr$bfCleanChildHW, dataCorr$bfCleanChildHW==TRUE, 1)
dataCorr$bfCleanChildHW<-replace(dataCorr$bfCleanChildHW, dataCorr$bfCleanChildHW==FALSE, 0)
unique(dataCorr$bfCleanChildHW)
dataCorr$bfCleanChildHW <- sapply(dataCorr$bfCleanChildHW, as.numeric)
str(dataCorr$bfCleanChildHW)

    #After using the toilet
dataCorr$aftToiletHW<-replace(dataCorr$aftToiletHW, dataCorr$aftToiletHW==TRUE, 1)
dataCorr$aftToiletHW<-replace(dataCorr$aftToiletHW, dataCorr$aftToiletHW==FALSE, 0)
unique(dataCorr$aftToiletHW)
dataCorr$aftToiletHW <- sapply(dataCorr$aftToiletHW, as.numeric)
str(dataCorr$aftToiletHW)

    #After a child defecates
dataCorr$aftChildDefHW<-replace(dataCorr$aftChildDefHW, dataCorr$aftChildDefHW==TRUE, 1)
dataCorr$aftChildDefHW<-replace(dataCorr$aftChildDefHW, dataCorr$aftChildDefHW==FALSE, 0)
unique(dataCorr$aftChildDefHW)
dataCorr$aftChildDefHW <- sapply(dataCorr$aftChildDefHW, as.numeric)
str(dataCorr$aftChildDefHW)

    #Before eating
dataCorr$bfFoodHW<-replace(dataCorr$bfFoodHW, dataCorr$bfFoodHW==TRUE, 1)
dataCorr$bfFoodHW<-replace(dataCorr$bfFoodHW, dataCorr$bfFoodHW==FALSE, 0)
unique(dataCorr$bfFoodHW)
dataCorr$bfFoodHW <- sapply(dataCorr$bfFoodHW, as.numeric)
str(dataCorr$bfFoodHW)

    #After eating
dataCorr$aftFoodHW<-replace(dataCorr$aftFoodHW, dataCorr$aftFoodHW==TRUE, 1)
dataCorr$aftFoodHW<-replace(dataCorr$aftFoodHW, dataCorr$aftFoodHW==FALSE, 0)
unique(dataCorr$aftFoodHW)
dataCorr$aftFoodHW <- sapply(dataCorr$aftFoodHW, as.numeric)
str(dataCorr$aftFoodHW)

    #Other handwashes
dataCorr$otherHW<-replace(dataCorr$otherHW, dataCorr$otherHW==TRUE, 1)
dataCorr$otherHW<-replace(dataCorr$otherHW, dataCorr$otherHW==FALSE, 0)
unique(dataCorr$otherHW)
dataCorr$otherHW <- sapply(dataCorr$otherHW, as.numeric)
str(dataCorr$otherHW)

    #Bednet
dataCorr$bedNet<-replace(dataCorr$bedNet, dataCorr$bedNet==TRUE, 1)
dataCorr$bedNet<-replace(dataCorr$bedNet, dataCorr$bedNet==FALSE, 0)
unique(dataCorr$bedNet)
dataCorr$bedNet <- sapply(dataCorr$bedNet, as.numeric)
str(dataCorr$bedNet)

    #If the child was weighed in a GMP session
dataCorr$weighIn<-replace(dataCorr$weighIn, dataCorr$weighIn==TRUE, 1)
dataCorr$weighIn<-replace(dataCorr$weighIn, dataCorr$weighIn==FALSE, 0)
unique(dataCorr$weighIn)
dataCorr$weighIn <- sapply(dataCorr$weighIn, as.numeric)
str(dataCorr$weighIn)

#If the child is an infant
dataCorr$ifInfant<-replace(dataCorr$ifInfant, dataCorr$ifInfant==TRUE, 1)
dataCorr$ifInfant<-replace(dataCorr$ifInfant, dataCorr$ifInfant==FALSE, 0)
unique(dataCorr$ifInfant)
dataCorr$ifInfant <- sapply(dataCorr$ifInfant, as.numeric)
str(dataCorr$ifInfant)

    #If the child has official documents
dataCorr$childDoc<-replace(dataCorr$childDoc, dataCorr$childDoc=='yes, seen', 1)
dataCorr$childDoc<-replace(dataCorr$childDoc, dataCorr$childDoc=='yes, not seen', 2)
dataCorr$childDoc<-replace(dataCorr$childDoc, dataCorr$childDoc=='no', 3)
unique(dataCorr$childDoc)
dataCorr$childDoc <- sapply(dataCorr$childDoc, as.numeric)
str(dataCorr$childDoc)

    #If the child was weighed last month
dataCorr$childWtMonth<-replace(dataCorr$childWtMonth, dataCorr$childWtMonth==TRUE, 1)
dataCorr$childWtMonth<-replace(dataCorr$childWtMonth, dataCorr$childWtMonth==FALSE, 0)
unique(dataCorr$childWtMonth)
dataCorr$childWtMonth <- sapply(dataCorr$childWtMonth, as.numeric)
str(dataCorr$childWtMonth)

    #If the child was weighed twice in the last three months
dataCorr$childWt3Month<-replace(dataCorr$childWt3Month, dataCorr$childWt3Month==TRUE, 1)
dataCorr$childWt3Month<-replace(dataCorr$childWt3Month, dataCorr$childWt3Month==FALSE, 0)
unique(dataCorr$childWt3Month)
dataCorr$childWt3Month <- sapply(dataCorr$childWt3Month, as.numeric)
str(dataCorr$childWt3Month)

    #If the child drank breastmilk
dataCorr$drankBreastmilk<-replace(dataCorr$drankBreastmilk, dataCorr$drankBreastmilk==TRUE, 1)
dataCorr$drankBreastmilk<-replace(dataCorr$drankBreastmilk, dataCorr$drankBreastmilk==FALSE, 0)
unique(dataCorr$drankBreastmilk)
dataCorr$drankBreastmilk <- sapply(dataCorr$drankBreastmilk, as.numeric)
str(dataCorr$drankBreastmilk)

    #When the child started breastfeeding
dataCorr$startBreastfeed<-replace(dataCorr$startBreastfeed, dataCorr$startBreastfeed=='same day', 1)
dataCorr$startBreastfeed<-replace(dataCorr$startBreastfeed, dataCorr$startBreastfeed=='1 day', 2)
dataCorr$startBreastfeed<-replace(dataCorr$startBreastfeed, dataCorr$startBreastfeed=='2 days', 3)
dataCorr$startBreastfeed<-replace(dataCorr$startBreastfeed, dataCorr$startBreastfeed=='3 days', 4)
unique(dataCorr$startBreastfeed)
dataCorr$startBreastfeed <- sapply(dataCorr$startBreastfeed, as.numeric)
str(dataCorr$startBreastfeed)

    #If the child received anything other than breast milk in the first three days
dataCorr$other3Day<-replace(dataCorr$other3Day, dataCorr$other3Day==TRUE, 1)
dataCorr$other3Day<-replace(dataCorr$other3Day, dataCorr$other3Day==FALSE, 0)
unique(dataCorr$other3Day)
dataCorr$other3Day <- sapply(dataCorr$other3Day, as.numeric)
str(dataCorr$other3Day)

    #If the child received anything other than breast milk in the first six months
dataCorr$other6Month<-replace(dataCorr$other6Month, dataCorr$other6Month==TRUE, 1)
dataCorr$other6Month<-replace(dataCorr$other6Month, dataCorr$other6Month==FALSE, 0)
unique(dataCorr$other6Month)
dataCorr$other6Month <- sapply(dataCorr$other6Month, as.numeric)
str(dataCorr$other6Month)

    #If the child is still breastfed
dataCorr$stillBreastfed<-replace(dataCorr$stillBreastfed, dataCorr$stillBreastfed==TRUE, 1)
dataCorr$stillBreastfed<-replace(dataCorr$stillBreastfed, dataCorr$stillBreastfed==FALSE, 0)
unique(dataCorr$stillBreastfed)
dataCorr$stillBreastfed <- sapply(dataCorr$stillBreastfed, as.numeric)
str(dataCorr$stillBreastfed)

    #If the child was breastfed yesterday
dataCorr$breastfedYday<-replace(dataCorr$breastfedYday, dataCorr$breastfedYday==TRUE, 1)
dataCorr$breastfedYday<-replace(dataCorr$breastfedYday, dataCorr$breastfedYday==FALSE, 0)
unique(dataCorr$breastfedYday)
dataCorr$breastfedYday <- sapply(dataCorr$breastfedYday, as.numeric)
str(dataCorr$breastfedYday)

    #If the child had liquid supplements yesterday
dataCorr$liquidSuppl<-replace(dataCorr$liquidSuppl, dataCorr$liquidSuppl==TRUE, 1)
dataCorr$liquidSuppl<-replace(dataCorr$liquidSuppl, dataCorr$liquidSuppl==FALSE, 0)
unique(dataCorr$liquidSuppl)
dataCorr$liquidSuppl <- sapply(dataCorr$liquidSuppl, as.numeric)
str(dataCorr$liquidSuppl)

    #If the child had oral rehydration solution yesterday
dataCorr$liquidORS<-replace(dataCorr$liquidORS, dataCorr$liquidORS==TRUE, 1)
dataCorr$liquidORS<-replace(dataCorr$liquidORS, dataCorr$liquidORS==FALSE, 0)
unique(dataCorr$liquidORS)
dataCorr$liquidORS <- sapply(dataCorr$liquidORS, as.numeric)
str(dataCorr$liquidORS)

    #If the child had water yesterday
dataCorr$liquidWater<-replace(dataCorr$liquidWater, dataCorr$liquidWater==TRUE, 1)
dataCorr$liquidWater<-replace(dataCorr$liquidWater, dataCorr$liquidWater==FALSE, 0)
unique(dataCorr$liquidWater)
dataCorr$liquidWater <- sapply(dataCorr$liquidWater, as.numeric)
str(dataCorr$liquidWater)

    #If the child had formula yesterday
dataCorr$liquidFormula<-replace(dataCorr$liquidFormula, dataCorr$liquidFormula==TRUE, 1)
dataCorr$liquidFormula<-replace(dataCorr$liquidFormula, dataCorr$liquidFormula==FALSE, 0)
unique(dataCorr$liquidFormula)
dataCorr$liquidFormula <- sapply(dataCorr$liquidFormula, as.numeric)
str(dataCorr$liquidFormula)

    #If the child had animal milk yesterday
dataCorr$liquidMilk<-replace(dataCorr$liquidMilk, dataCorr$liquidMilk==TRUE, 1)
dataCorr$liquidMilk<-replace(dataCorr$liquidMilk, dataCorr$liquidMilk==FALSE, 0)
unique(dataCorr$liquidMilk)
dataCorr$liquidMilk <- sapply(dataCorr$liquidMilk, as.numeric)
str(dataCorr$liquidMilk)

    #If the child had juice yesterday
dataCorr$liquidJuice<-replace(dataCorr$liquidJuice, dataCorr$liquidJuice==TRUE, 1)
dataCorr$liquidJuice<-replace(dataCorr$liquidJuice, dataCorr$liquidJuice==FALSE, 0)
unique(dataCorr$liquidJuice)
dataCorr$liquidJuice <- sapply(dataCorr$liquidJuice, as.numeric)
str(dataCorr$liquidJuice)

    #If the child had broth yesterday
dataCorr$liquidBroth<-replace(dataCorr$liquidBroth, dataCorr$liquidBroth==TRUE, 1)
dataCorr$liquidBroth<-replace(dataCorr$liquidBroth, dataCorr$liquidBroth==FALSE, 0)
unique(dataCorr$liquidBroth)
dataCorr$liquidBroth <- sapply(dataCorr$liquidBroth, as.numeric)
str(dataCorr$liquidBroth)

    #If the child had yogurt yesterday
dataCorr$liquidYogurt<-replace(dataCorr$liquidYogurt, dataCorr$liquidYogurt==TRUE, 1)
dataCorr$liquidYogurt<-replace(dataCorr$liquidYogurt, dataCorr$liquidYogurt==FALSE, 0)
unique(dataCorr$liquidYogurt)
dataCorr$liquidYogurt <- sapply(dataCorr$liquidYogurt, as.numeric)
str(dataCorr$liquidYogurt)

    #If the child had thin porridge yesterday
dataCorr$liquidPorridge<-replace(dataCorr$liquidPorridge, dataCorr$liquidPorridge==TRUE, 1)
dataCorr$liquidPorridge<-replace(dataCorr$liquidPorridge, dataCorr$liquidPorridge==FALSE, 0)
unique(dataCorr$liquidPorridge)
dataCorr$liquidPorridge <- sapply(dataCorr$liquidPorridge, as.numeric)
str(dataCorr$liquidPorridge)

    #If the child had tea yesterday
dataCorr$liquidTea<-replace(dataCorr$liquidTea, dataCorr$liquidTea==TRUE, 1)
dataCorr$liquidTea<-replace(dataCorr$liquidTea, dataCorr$liquidTea==FALSE, 0)
unique(dataCorr$liquidTea)
dataCorr$liquidTea <- sapply(dataCorr$liquidTea, as.numeric)
str(dataCorr$liquidTea)

    #If the child had any other liquids yesterday
dataCorr$liquidOther<-replace(dataCorr$liquidOther, dataCorr$liquidOther==TRUE, 1)
dataCorr$liquidOther<-replace(dataCorr$liquidOther, dataCorr$liquidOther==FALSE, 0)
unique(dataCorr$liquidOther)
dataCorr$liquidOther <- sapply(dataCorr$liquidOther, as.numeric)
str(dataCorr$liquidOther)

    #If the child had any grain yesterday
dataCorr$foodGrain<-replace(dataCorr$foodGrain, dataCorr$foodGrain==TRUE, 1)
dataCorr$foodGrain<-replace(dataCorr$foodGrain, dataCorr$foodGrain==FALSE, 0)
unique(dataCorr$foodGrain)
dataCorr$foodGrain <- sapply(dataCorr$foodGrain, as.numeric)
str(dataCorr$foodGrain)

    #If the child had yellow produce
dataCorr$foodYellow<-replace(dataCorr$foodYellow, dataCorr$foodYellow==TRUE, 1)
dataCorr$foodYellow<-replace(dataCorr$foodYellow, dataCorr$foodYellow==FALSE, 0)
unique(dataCorr$foodYellow)
dataCorr$foodYellow <- sapply(dataCorr$foodYellow, as.numeric)
str(dataCorr$foodYellow)

    #If the child had white roots yesterday
dataCorr$foodWhite<-replace(dataCorr$foodWhite, dataCorr$foodWhite==TRUE, 1)
dataCorr$foodWhite<-replace(dataCorr$foodWhite, dataCorr$foodWhite==FALSE, 0)
unique(dataCorr$foodWhite)
dataCorr$foodWhite <- sapply(dataCorr$foodWhite, as.numeric)
str(dataCorr$foodWhite)

    #If the child had green produce yesterday
dataCorr$foodGreen<-replace(dataCorr$foodGreen, dataCorr$foodGreen==TRUE, 1)
dataCorr$foodGreen<-replace(dataCorr$foodGreen, dataCorr$foodGreen==FALSE, 0)
unique(dataCorr$foodGreen)
dataCorr$foodGreen <- sapply(dataCorr$foodGreen, as.numeric)
str(dataCorr$foodGreen)

    #If the child had mango or papaya yesterday
dataCorr$foodMangoPap<-replace(dataCorr$foodMangoPap, dataCorr$foodMangoPap==TRUE, 1)
dataCorr$foodMangoPap<-replace(dataCorr$foodMangoPap, dataCorr$foodMangoPap==FALSE, 0)
unique(dataCorr$foodMangoPap)
dataCorr$foodMangoPap <- sapply(dataCorr$foodMangoPap, as.numeric)
str(dataCorr$foodMangoPap)

    #If the child had other produce yesterday
dataCorr$foodProduce<-replace(dataCorr$foodProduce, dataCorr$foodProduce==TRUE, 1)
dataCorr$foodProduce<-replace(dataCorr$foodProduce, dataCorr$foodProduce==FALSE, 0)
unique(dataCorr$foodProduce)
dataCorr$foodProduce <- sapply(dataCorr$foodProduce, as.numeric)
str(dataCorr$foodProduce)

    #If the child had organ meat yesterday
dataCorr$foodOrgan<-replace(dataCorr$foodOrgan, dataCorr$foodOrgan==TRUE, 1)
dataCorr$foodOrgan<-replace(dataCorr$foodOrgan, dataCorr$foodOrgan==FALSE, 0)
unique(dataCorr$foodOrgan)
dataCorr$foodOrgan <- sapply(dataCorr$foodOrgan, as.numeric)
str(dataCorr$foodOrgan)

    #If the child had meat yesterday
dataCorr$foodMeat<-replace(dataCorr$foodMeat, dataCorr$foodMeat==TRUE, 1)
dataCorr$foodMeat<-replace(dataCorr$foodMeat, dataCorr$foodMeat==FALSE, 0)
unique(dataCorr$foodMeat)
dataCorr$foodMeat <- sapply(dataCorr$foodMeat, as.numeric)
str(dataCorr$foodMeat)

    #If the child had eggs yesterday
dataCorr$foodEgg<-replace(dataCorr$foodEgg, dataCorr$foodEgg==TRUE, 1)
dataCorr$foodEgg<-replace(dataCorr$foodEgg, dataCorr$foodEgg==FALSE, 0)
unique(dataCorr$foodEgg)
dataCorr$foodEgg <- sapply(dataCorr$foodEgg, as.numeric)
str(dataCorr$foodEgg)

    #If the child had fish yesterday
dataCorr$foodFish<-replace(dataCorr$foodFish, dataCorr$foodFish==TRUE, 1)
dataCorr$foodFish<-replace(dataCorr$foodFish, dataCorr$foodFish==FALSE, 0)
unique(dataCorr$foodFish)
dataCorr$foodFish <- sapply(dataCorr$foodFish, as.numeric)
str(dataCorr$foodFish)

    #If the child had beans yesterday
dataCorr$foodBean<-replace(dataCorr$foodBean, dataCorr$foodBean==TRUE, 1)
dataCorr$foodBean<-replace(dataCorr$foodBean, dataCorr$foodBean==FALSE, 0)
unique(dataCorr$foodBean)
dataCorr$foodBean <- sapply(dataCorr$foodBean, as.numeric)
str(dataCorr$foodBean)

    #If the child had dairy yesterday
dataCorr$foodDairy<-replace(dataCorr$foodDairy, dataCorr$foodDairy==TRUE, 1)
dataCorr$foodDairy<-replace(dataCorr$foodDairy, dataCorr$foodDairy==FALSE, 0)
unique(dataCorr$foodDairy)
dataCorr$foodDairy <- sapply(dataCorr$foodDairy, as.numeric)
str(dataCorr$foodDairy)

    #If the child had fats yesterday
dataCorr$foodFats<-replace(dataCorr$foodFats, dataCorr$foodFats==TRUE, 1)
dataCorr$foodFats<-replace(dataCorr$foodFats, dataCorr$foodFats==FALSE, 0)
unique(dataCorr$foodFats)
dataCorr$foodFats <- sapply(dataCorr$foodFats, as.numeric)
str(dataCorr$foodFats)

    #If the child had sugar yesterday
dataCorr$foodSugar<-replace(dataCorr$foodSugar, dataCorr$foodSugar==TRUE, 1)
dataCorr$foodSugar<-replace(dataCorr$foodSugar, dataCorr$foodSugar==FALSE, 0)
unique(dataCorr$foodSugar)
dataCorr$foodSugar <- sapply(dataCorr$foodSugar, as.numeric)
str(dataCorr$foodSugar)

    #If the child had condiments yesterday
dataCorr$foodCondiment<-replace(dataCorr$foodCondiment, dataCorr$foodCondiment==TRUE, 1)
dataCorr$foodCondiment<-replace(dataCorr$foodCondiment, dataCorr$foodCondiment==FALSE, 0)
unique(dataCorr$foodCondiment)
dataCorr$foodCondiment <- sapply(dataCorr$foodCondiment, as.numeric)
str(dataCorr$foodCondiment)

    #If the child had insects yesterday
dataCorr$foodInsect<-replace(dataCorr$foodInsect, dataCorr$foodInsect==TRUE, 1)
dataCorr$foodInsect<-replace(dataCorr$foodInsect, dataCorr$foodInsect==FALSE, 0)
unique(dataCorr$foodInsect)
dataCorr$foodInsect <- sapply(dataCorr$foodInsect, as.numeric)
str(dataCorr$foodInsect)

    #If the child had red palm yesterday
dataCorr$foodRedPalm<-replace(dataCorr$foodRedPalm, dataCorr$foodRedPalm==TRUE, 1)
dataCorr$foodRedPalm<-replace(dataCorr$foodRedPalm, dataCorr$foodRedPalm==FALSE, 0)
unique(dataCorr$foodRedPalm)
dataCorr$foodRedPalm <- sapply(dataCorr$foodRedPalm, as.numeric)
str(dataCorr$foodRedPalm)

    #If the child had any other food yesterday
dataCorr$foodOther<-replace(dataCorr$foodOther, dataCorr$foodOther==TRUE, 1)
dataCorr$foodOther<-replace(dataCorr$foodOther, dataCorr$foodOther==FALSE, 0)
unique(dataCorr$foodOther)
dataCorr$foodOther <- sapply(dataCorr$foodOther, as.numeric)
str(dataCorr$foodOther)

    #If the child had salt yesterday
dataCorr$foodSalt<-replace(dataCorr$foodSalt, dataCorr$foodSalt==TRUE, 1)
dataCorr$foodSalt<-replace(dataCorr$foodSalt, dataCorr$foodSalt==FALSE, 0)
unique(dataCorr$foodSalt)
dataCorr$foodSalt <- sapply(dataCorr$foodSalt, as.numeric)
str(dataCorr$foodSalt)

    #How the child was measured
dataCorr$howMeasure<-replace(dataCorr$howMeasure, dataCorr$howMeasure=='L', 1)
dataCorr$howMeasure<-replace(dataCorr$howMeasure, dataCorr$howMeasure=='H', 2)
unique(dataCorr$howMeasure)
dataCorr$howMeasure <- sapply(dataCorr$howMeasure, as.numeric)
str(dataCorr$howMeasure)

    #Oedema status
dataCorr$oedema<-replace(dataCorr$oedema, dataCorr$oedema=='y', 1)
dataCorr$oedema<-replace(dataCorr$oedema, dataCorr$oedema=='n', 0)
unique(dataCorr$oedema)
dataCorr$oedema <- sapply(dataCorr$oedema, as.numeric)
str(dataCorr$oedema)

    #The results of the measurement
unique(dataCorr$resultsMeasure)
dataCorr$resultsMeasure<-replace(dataCorr$resultsMeasure, dataCorr$resultsMeasure=='Measured only wt', 1)
dataCorr$resultsMeasure<-replace(dataCorr$resultsMeasure, dataCorr$resultsMeasure=='Measured only ht', 2)
dataCorr$resultsMeasure<-replace(dataCorr$resultsMeasure, dataCorr$resultsMeasure=='Measured both ht & wt', 3)
unique(dataCorr$resultsMeasure)
dataCorr$resultsMeasure <- sapply(dataCorr$resultsMeasure, as.numeric)
str(dataCorr$resultsMeasure)

    #The age group
unique(dataCorr$ageGroup)
dataCorr$ageGroup<-replace(dataCorr$ageGroup, dataCorr$ageGroup=='00-05 mo', 1)
dataCorr$ageGroup<-replace(dataCorr$ageGroup, dataCorr$ageGroup=='06-11 mo', 2)
dataCorr$ageGroup<-replace(dataCorr$ageGroup, dataCorr$ageGroup=='12-23 mo', 3)
dataCorr$ageGroup<-replace(dataCorr$ageGroup, dataCorr$ageGroup=='24-35 mo', 4)
dataCorr$ageGroup<-replace(dataCorr$ageGroup, dataCorr$ageGroup=='36-47 mo', 5)
dataCorr$ageGroup<-replace(dataCorr$ageGroup, dataCorr$ageGroup=='48-59 mo', 6)
unique(dataCorr$ageGroup)
dataCorr$ageGroup <- sapply(dataCorr$ageGroup, as.numeric)
str(dataCorr$ageGroup)

    #Dependent variable - stunted
dataCorr$stunted<-replace(dataCorr$stunted, dataCorr$stunted==TRUE, 1)
dataCorr$stunted<-replace(dataCorr$stunted, dataCorr$stunted==FALSE, 0)
unique(dataCorr$stunted)
dataCorr$stunted <- sapply(dataCorr$stunted, as.numeric)
str(dataCorr$stunted)

    #Dependent variable - wasting
dataCorr$wasting<-replace(dataCorr$wasting, dataCorr$wasting==TRUE, 1)
dataCorr$wasting<-replace(dataCorr$wasting, dataCorr$wasting==FALSE, 0)
unique(dataCorr$wasting)
dataCorr$wasting <- sapply(dataCorr$wasting, as.numeric)
str(dataCorr$wasting)

    #Dependent variable - underweight
dataCorr$underweight<-replace(dataCorr$underweight, dataCorr$underweight==TRUE, 1)
dataCorr$underweight<-replace(dataCorr$underweight, dataCorr$underweight==FALSE, 0)
unique(dataCorr$underweight)
dataCorr$underweight <- sapply(dataCorr$underweight, as.numeric)
str(dataCorr$underweight)

    #Dependent variable - malnourished
dataCorr$malnourished<-replace(dataCorr$malnourished, dataCorr$malnourished==TRUE, 1)
dataCorr$malnourished<-replace(dataCorr$malnourished, dataCorr$malnourished==FALSE, 0)
unique(dataCorr$malnourished)
dataCorr$malnourished <- sapply(dataCorr$malnourished, as.numeric)
str(dataCorr$malnourished)

#Kendall's Correlation ----
myCorr <- round(cor(dataCorr, use = "pairwise.complete.obs", method = 'kendall'), 2)
warnings()
    #warning about SD being zero is probably due to very imbalanced variables

    #Exploring variables for low variability
format(sapply(dataCorr, var, na.rm=TRUE), scientific=FALSE)
    #As expected given the imbalanceed nature of the dataset the variance of most of the variables is quite low (below 1)
summary(dataCorr)

lowTriFun <- function(myCorr){
  myCorr[upper.tri(myCorr)] <- NA
  return(myCorr)
}
lowTriangle <- lowTriFun(myCorr)

meltedCorr <- melt(lowTriangle, na.rm = TRUE)
head(meltedCorr)

ggplot(data = meltedCorr, aes(Var1, Var2, fill=value)) + 
  geom_raster() + 
  scale_fill_gradient2(low = "#0067BC", high = "#FF8A61", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = 'Kendall Correlation') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed()

    #Seperating out correlations by strength so I can actually SEE what is going on
    #Very strong
vStrongCorr <- subset(meltedCorr, value >= 0.8 & value %notin% 1.00 | value <= -0.8 & value %notin% -1.00)
ggplot(data = vStrongCorr, aes(Var1, Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "#0067BC", high = "#FF8A61", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = 'Kendall Correlation') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed() +
  geom_text(aes(Var1, Var2, label=value), color = 'black', size = 4) + labs(title= 'Very Strong Correlations')

    #Strong
strongCorr1 <- subset(meltedCorr, value >= 0.7 & value < 0.8 | value <= -0.7 & value > -0.8)
ggplot(data = strongCorr1, aes(Var1, Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "#0067BC", high = "#FF8A61", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = 'Kendall Correlation') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed() +
  geom_text(aes(Var1, Var2, label=value), color = 'black', size = 4) + labs(title= 'Strong Correlations - Plot 1')

strongCorr2 <- subset(meltedCorr, value >= 0.6 & value < 0.7 | value <= -0.6 & value > -0.7)
ggplot(data = strongCorr2, aes(Var1, Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "#0067BC", high = "#FF8A61", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = 'Kendall Correlation') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed() +
  geom_text(aes(Var1, Var2, label=value), color = 'black', size = 4) + labs(title= 'Strong Correlations - Plot 2')
    #Moderate
modCorr1 <- subset(meltedCorr, value >= 0.5 & value < 0.6 | value <= -0.5 & value > -0.6)
ggplot(data = modCorr1, aes(Var1, Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "#0067BC", high = "#FF8A61", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = 'Kendall Correlation') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed() +
  geom_text(aes(Var1, Var2, label=value), color = 'black', size = 4) + labs(title= 'Moderate Correlations - Plot 1')

modCorr2 <- subset(meltedCorr, value >= 0.45 & value < 0.5 | value <= -0.45 & value > -0.5)
ggplot(data = modCorr2, aes(Var1, Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "#0067BC", high = "#FF8A61", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = 'Kendall Correlation') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed() +
  geom_text(aes(Var1, Var2, label=value), color = 'black', size = 4) + labs(title= 'Moderate Correlations - Plot 2')

modCorr3 <- subset(meltedCorr, value >= 0.4 & value < 0.45 | value <= -0.4 & value > -0.45)
ggplot(data = modCorr3, aes(Var1, Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "#0067BC", high = "#FF8A61", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = 'Kendall Correlation') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed() +
  geom_text(aes(Var1, Var2, label=value), color = 'black', size = 4) + labs(title= 'Moderate Correlations - Plot 3')
    #Weak
weakCorr1 <- subset(meltedCorr, value >= 0.3 & value < 0.4 | value <= -0.3 & value > -0.4)
ggplot(data = weakCorr1, aes(Var1, Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "#0067BC", high = "#FF8A61", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = 'Kendall Correlation') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed() +
  geom_text(aes(Var1, Var2, label=value), color = 'black', size = 4) + labs(title= 'Weak Correlations - Plot 1')

weakCorr2 <- subset(meltedCorr, value >= 0.2 & value < 0.3 | value <= -0.2 & value > -0.3)
ggplot(data = weakCorr2, aes(Var1, Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "#0067BC", high = "#FF8A61", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = 'Kendall Correlation') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed() +
  geom_text(aes(Var1, Var2, label=value), color = 'black', size = 4) + labs(title= 'Weak Correlations - Plot 2')


#Removing highly correlated variables
    #Removing columns from original dataset and recoded dataset so I can run the Kendall Correlation
    #a second time without having to recode categorical and logical variables again in either direction (work smart, not hard)

    #Very strong correlations

    #For very strong correlations, stunted and malnourished connected, but are dependent variables and won't be in
    #a model together, so leave
    #StillBreastfed and howMeasure correlated, makes sense, both age related. Remove howMeasure as that was more for
    #the Anthro Analyzer and just provides additional info on age
    #Breastfed yesterday and still breastfed are strongly correlated, makes sense. Can leave number and remove other two, 
    #as they don't add any additional details
    #ageGroup is correlated with ageMonth and birthYear. Will leave ageGroup and remove others, as ageGroup will likely be
    #more useful for the intended users of the model
AllDataWALA2 = subset(AllDataWALA, select = -c(howMeasure, ageMonth, birthYear, stillBreastfed, breastfedYday))
dataCorr2 = subset(dataCorr, select = -c(howMeasure, ageMonth, birthYear, stillBreastfed, breastfedYday))

    #Strong correlations

    #Strong correlations, many of the variables involved in calculating the z scores and dependent variables are correlated.
    #The information is already included in the dependent variable, so will remove most. Especially since workers are already
    #tracking things like height and weight, so there would be no additional change to practice
    #ifInfant and stillBreastfed are correlated. Logical, more likely to still be breastfed if under a year. Remove
    #ifInfant as it only provides additional details on age
AllDataWALA2 = subset(AllDataWALA2, select = -c(BMI, oedema, comboHtLgth, weight, lengthZ, weightZ, wt4LgthZ, ifInfant))
dataCorr2 = subset(dataCorr2, select = -c(BMI, oedema, comboHtLgth, weight, lengthZ, weightZ, wt4LgthZ, ifInfant))


    #Combining overlapping variables (two records of weigh ins, combined into one record of if the child was weighed 1+ times in the 
    #last three months)
summary(AllDataWALA2$childWtMonth)
summary(AllDataWALA2$childWt3Month)
AllDataWALA2$childWeighIn <- AllDataWALA2$childWtMonth
AllDataWALA2[AllDataWALA2$childWt3Month %in% TRUE,]$childWeighIn <- TRUE
summary(AllDataWALA2$childWeighIn)
    #Logic: If childWtMonth was true, we know they were measured once in the last month so childWeighIn is true (created variable using column)
    #Where if childWtMonth is false or NA and childWt3Month is true, then that was a survey error and childWeighIn should
    #be true (update to column)
AllDataWALA2 = subset(AllDataWALA2, select = -c(childWtMonth, childWt3Month))

    #Repeated for correlation data
dataCorr2$childWeighIn <- dataCorr2$childWtMonth
dataCorr2[dataCorr2$childWt3Month==1 & dataCorr2$childWt3Month %notin% NA ,]$childWeighIn <- 1
summary(dataCorr2$childWeighIn)
dataCorr2 = subset(dataCorr2, select = -c(childWtMonth, childWt3Month))

    #Combining drinking yogurt and eating dairy
summary(AllDataWALA2$liquidYogurt)
summary(AllDataWALA2$foodDairy)
AllDataWALA2$anyDairy <- AllDataWALA2$liquidYogurt
AllDataWALA2[AllDataWALA2$foodDairy %in% TRUE,]$anyDairy <- TRUE
summary(AllDataWALA2$anyDairy)
AllDataWALA2 = subset(AllDataWALA2, select = -c(liquidYogurt, foodDairy))

    #Repeated for correlation data
dataCorr2$anyDairy <- dataCorr2$liquidYogurt
dataCorr2[dataCorr2$foodDairy==1 & dataCorr2$foodDairy %notin% NA ,]$anyDairy <- 1
summary(dataCorr2$anyDairy)
dataCorr2 = subset(dataCorr2, select = -c(liquidYogurt, foodDairy))

    #For the strong correlations between salt/water consumption and the results of the measurements (wt + ht/lgth,
    #wt only, ht/lgth only), this is likely due to the very imbalanced nature of the dataset.
    #90% drank water, 89% had salt, and 90% had both weight and height/length measured
    #So these may not be causal relationships, but appear correlated because of the numbers
    #Will remove the results of the measurement (does not add any additional information)
    #But will leave the water and salt variables in. If they are included in the final models, I will reevaluate
AllDataWALA2 = subset(AllDataWALA2, select = -c(resultsMeasure))
dataCorr2 = subset(dataCorr2, select = -c(resultsMeasure))

    #For the strong negative association between washing hands after using the toilet and never washing hands, again
    #this is logical. The two are mutually exclusive (if they never wash their hands, they don't after using the toilet,
    #and if they wash their hands after using the toilet, they don't never wash their hands). Will leave as is and 
    #reevaluate if they are included in the final models


#Second Kendall's Correlation with further cleaned dataset
myCorr2 <- round(cor(dataCorr2, use = "pairwise.complete.obs", method = 'kendall'), 2)
    #significant drop in the number of warnings, good sign

summary(dataCorr2)

lowTriFun <- function(myCorr2){
  myCorr2[upper.tri(myCorr2)] <- NA
  return(myCorr2)
}
lowTriangle2 <- lowTriFun(myCorr2)

meltedCorr2 <- melt(lowTriangle2, na.rm = TRUE)
head(meltedCorr2)

ggplot(data = meltedCorr2, aes(Var1, Var2, fill=value)) + 
  geom_raster() + 
  scale_fill_gradient2(low = "#0067BC", high = "#FF8A61", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = 'Kendall Correlation Take 2') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed()

    #repeating splitting out very strong and strong to make sure correct changes were made
    #Very strong
vStrongCorr2 <- subset(meltedCorr2, value >= 0.8 & value %notin% 1.00 | value <= -0.8 & value %notin% -1.00)

ggplot(data = vStrongCorr2, aes(Var1, Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "#0067BC", high = "#FF8A61", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = 'Kendall Correlation') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed() +
  geom_text(aes(Var1, Var2, label=value), color = 'black', size = 4) + labs(title= 'Very Strong Correlations Take 2')
    #Will keep the any dairy and liquidYogurt number in mind if they are in the final model

    #Strong
strongCorr <- subset(meltedCorr2, value >= 0.6 & value < 0.8 | value <= -0.6 & value > -0.8)

ggplot(data = strongCorr, aes(Var1, Var2, fill=value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "#0067BC", high = "#FF8A61", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = 'Kendall Correlation') +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + coord_fixed() +
  geom_text(aes(Var1, Var2, label=value), color = 'black', size = 4) + labs(title= 'Strong Correlations Take 2')
    #As expected. Difficult to seperate out breastfeeding and age data

#updating data dictionary again, further cleaning
table(AllDataWALA2$childWeighIn)
sum(is.na(AllDataWALA2$childWeighIn))

table(AllDataWALA2$Treatment)
str(AllDataWALA2$Treatment)


#EXPLORATORY DATA ANALYSIS - multivariate analysis ----

    #Will begin with some exploratory statistical testing to investigate if there are differences between malnutrition status in
    #different groups of children
    #As the data is not normally distributed, I will be using either the Wilcoxon Rank Sum (for two groups) or the Kruskal Wallis 
    #with the post hoc Dunn's test to confirm where the difference lies (if significant)

    #Age group
kruskal.test(malnourished ~ ageGroup, data = AllDataWALA2)
    #Significant difference, so go to Dunn's test using already recoded data from correlations
attach(dataCorr2)
dunn.test(malnourished, ageGroup, kw = TRUE, label=TRUE)
detach(dataCorr2)
    #There is a significant difference between the malnutrition rates across all age groups except 3 (12-23 mo)
    #and 5 (36-47 mo)

ggplot(data=AllDataWALA, aes(x=ageGroup, fill = malnourished)) + geom_bar(colour = 'black', position = "dodge") + 
  scale_fill_manual(values=c('#DCEFFF', '#D6B4FC'))+  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, hjust = 0.5) +
  labs(x='Age Group', y= "Count", title= 'Number of Malnourished Children by Age Group') +  theme_bw() + 
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))

    #Sex
wilcox.test(malnourished ~ childSex, data = dataCorr2)
    #The p-value is smaller than 0.05, which suggests that there is a significant difference in malnutrition rates between female 
    #and male children

    #Breastfeeding number
kruskal.test(malnourished ~ breastfedYdayNum, data = AllDataWALA2)
    #The p-value is larger than 0.05, which suggests that there is no significant difference in malnutrition rates by breastfeeding number

    #Number of times they ate yesterday
kruskal.test(malnourished ~ foodNum, data = AllDataWALA2)
    #Significant difference, so go to Dunn's test
attach(dataCorr2)
dunn.test(malnourished, foodNum, kw = TRUE, label=TRUE)
detach(dataCorr2)
    #Interestingly, there are only a few pairs that have a significant difference in this test. All significant differences are with
    #0 meals (significantly different from groups that ate 1-6 times yesterday)

    #Treatment group (to confirm results from WALA report)
wilcox.test(malnourished ~ Treatment, data = dataCorr2)
    #The p-value is larger than 0.05, which suggests that there is no significant difference in malnutrition rates between WALA and
    #non WALA communities (matches WALA report)

    #District
kruskal.test(malnourished ~ districtName, data = AllDataWALA2)
    #Significant difference, so go to Dunn's test
attach(dataCorr2)
dunn.test(malnourished, districtName, kw = TRUE, label=TRUE)
detach(dataCorr2)
    #There is a significant difference between malnutrition rates in different districts, will have to take this into consideration during modeling


    #Next, develop a decision tree to further explore the data and the associated decision rules

    #Remove other three dependent variables and only leave malnourished status
decisionData = subset(AllDataWALA2, select = -c(stunted, wasting, underweight))
decisionData$malnourished<-replace(decisionData$malnourished, decisionData$malnourished==TRUE, "malnourished")
decisionData$malnourished<-replace(decisionData$malnourished, decisionData$malnourished==FALSE, "healthy")
unique(decisionData$malnourished)
decisionData$malnourished <- sapply(decisionData$malnourished, as.factor)
str(decisionData$malnourished)

decisionData[sapply(decisionData, is.character)] <- lapply(decisionData[sapply(decisionData, is.character)], as.factor)
decisionData[sapply(decisionData, is.logical)] <- lapply(decisionData[sapply(decisionData, is.logical)], as.numeric)

ctreeWALA <- ctree(malnourished ~ ., data=decisionData, na.action=na.pass)
print(ctreeWALA)
plot(ctreeWALA, type="simple")
    #As expected, there is a bias towards the majority class (healthy), so we can see
    #higher error rates and not great splitting. This highlights the importance of using something like
    #a stratified k-fold cross validation split for modeling

    #seeing the before feeding a child hand washing variable pop up is surprising, and might
    #be signalling a need for further cleaning/dimensionality reduction (for example, combining
    #all hand washing practices into one or two overarching variables).

    #Signifiance of attributes in ctree
sctest(ctreeWALA, node=1)
    #A lot of the variables have a p-value of 1, so no difference between malnourished and healthy children outside of what is by chance
    #Next step will be to continue cleaning, removing variables that provide very little information and combining others.
    #Will use feature selection to do this, then repeat decision tree to explore decision rules

    #Feature Selection
featData <- decisionData[complete.cases(decisionData),]
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(malnourished~., data=featData, 
               method="lvq", preProcess="scale", 
               trControl=control)
importance <- varImp(model)
print(importance)
plot(importance)
    #Will remove the least important 27 variables (where the graph really levels out), and rerun decision tree
    #Pretty much all of these had lower variance, unsurprising that they are not important features

AllDataWALA3 = subset(AllDataWALA2, select = -c(aftToiletHW, bedNet, foodRedPalm, liquidJuice, liquidYogurtNum, 
                                                bfCleanChildHW, anyDairy, foodCondiment, bfFoodPrepHW, aftFeedChildHW,
                                                liquidFormulaNum, liquidMilkNum, liquidMilk, foodYellow, foodProduce, 
                                                foodFats, foodEgg, foodOrgan, weighIn, otherHW, foodInsect, liquidBroth,
                                                startBreastfeed, liquidORS, childDoc, drankBreastmilk))

    #Ran through again (not shown), and no changes to the decision tree. Remove more unimportant variables and try again
AllDataWALA3 = subset(AllDataWALA3, select = -c(bfFeedChildHW, foodFish, liquidOther, liquidSuppl, liquidTea, bfFoodHW, 
                                                liquidPorridge, childCode, childWeighIn, aftFoodPrepHW, aftChildDefHW, 
                                                other6Month, neverHW, aftFoodHW, other3Day, bioMom, foodMeat))


decisionData2 = subset(AllDataWALA3, select = -c(stunted, wasting, underweight))
decisionData2$malnourished<-replace(decisionData2$malnourished, decisionData2$malnourished==TRUE, "malnourished")
decisionData2$malnourished<-replace(decisionData2$malnourished, decisionData2$malnourished==FALSE, "healthy")
unique(decisionData2$malnourished)
decisionData2$malnourished <- sapply(decisionData2$malnourished, as.factor)
str(decisionData2$malnourished)

decisionData2[sapply(decisionData2, is.character)] <- lapply(decisionData2[sapply(decisionData2, is.character)], as.factor)
decisionData2[sapply(decisionData2, is.logical)] <- lapply(decisionData2[sapply(decisionData2, is.logical)], as.numeric)

ctreeWALA2 <- ctree(malnourished ~ ., data=decisionData2, na.action=na.pass)
print(ctreeWALA2)
plot(ctreeWALA2, type="simple")
sctest(ctreeWALA2, node=1)
    #A much larger proportion of the variables are now significant in the decision tree, however there is still the issue of the 
    #malnourished incidences being completely masked by the healthy (bias towards the majority class). This highlights the need for
    #some sort of cross validation at the predictive modeling stage

    #Explore if this pattern continues for other dependent variables
    #stunted
decisionData3 = subset(AllDataWALA2, select = -c(malnourished, wasting, underweight))
decisionData3 = decisionData3[!(decisionData3$stunted %in% NA),]
decisionData3$stunted<-replace(decisionData3$stunted, decisionData3$stunted==TRUE, "stunted")
decisionData3$stunted<-replace(decisionData3$stunted, decisionData3$stunted==FALSE, "healthy")
unique(decisionData3$stunted)
decisionData3$stunted <- sapply(decisionData3$stunted, as.factor)
str(decisionData3$stunted)

decisionData3[sapply(decisionData3, is.character)] <- lapply(decisionData3[sapply(decisionData3, is.character)], as.factor)
decisionData3[sapply(decisionData3, is.logical)] <- lapply(decisionData3[sapply(decisionData3, is.logical)], as.numeric)

ctreeWALA3 <- ctree(stunted ~ ., data=decisionData3, na.action=na.pass)
print(ctreeWALA3)
plot(ctreeWALA3, type="simple")
    #As before, bias towards majority class, real bad decision tree

    #Signifiance of attributes in ctree
sctest(ctreeWALA3, node=1)

    #Feature Selection
featData3 <- decisionData3[complete.cases(decisionData3),]
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model3 <- train(stunted~., data=featData3, 
               method="lvq", preProcess="scale", 
               trControl=control)
importance3 <- varImp(model3)
print(importance3)
plot(importance3)
    #I do see differences here on what variables are important compared to with malnutrition in general. For the final dataset before
    #modelling I will make sure the most important variables of the four different dependent variables are all included (not removed
    #after a different decision tree)

    #wasting
decisionData4 = subset(AllDataWALA2, select = -c(malnourished, stunted, underweight))
decisionData4 = decisionData4[!(decisionData4$wasting %in% NA),]
decisionData4$wasting<-replace(decisionData4$wasting, decisionData4$wasting==TRUE, "wasting")
decisionData4$wasting<-replace(decisionData4$wasting, decisionData4$wasting==FALSE, "healthy")
unique(decisionData4$wasting)
decisionData4$wasting <- sapply(decisionData4$wasting, as.factor)
str(decisionData4$wasting)

decisionData4[sapply(decisionData4, is.character)] <- lapply(decisionData4[sapply(decisionData4, is.character)], as.factor)
decisionData4[sapply(decisionData4, is.logical)] <- lapply(decisionData4[sapply(decisionData4, is.logical)], as.numeric)

ctreeWALA4 <- ctree(wasting ~ ., data=decisionData4, na.action=na.pass)
print(ctreeWALA4)
plot(ctreeWALA4, type="simple")
    #As before, bias towards majority class

    #Signifiance of attributes in ctree
sctest(ctreeWALA4, node=1)

#Feature Selection
featData4 <- decisionData4[complete.cases(decisionData4),]
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model4 <- train(wasting~., data=featData4, 
                method="lvq", preProcess="scale", 
                trControl=control)
importance4 <- varImp(model4)
print(importance4)
plot(importance4)
    #Less of a dramatic drop in importance, more of a smooth decline. Chose the first level out as the cut off, added a 
    #few variables back into the mix

    #Underweight
decisionData5 = subset(AllDataWALA2, select = -c(malnourished, stunted, wasting))
decisionData5 = decisionData5[!(decisionData5$underweight %in% NA),]
decisionData5$underweight<-replace(decisionData5$underweight, decisionData5$underweight==TRUE, "underweight")
decisionData5$underweight<-replace(decisionData5$underweight, decisionData5$underweight==FALSE, "healthy")
unique(decisionData5$underweight)
decisionData5$underweight <- sapply(decisionData5$underweight, as.factor)
str(decisionData5$underweight)

decisionData5[sapply(decisionData5, is.character)] <- lapply(decisionData5[sapply(decisionData5, is.character)], as.factor)
decisionData5[sapply(decisionData5, is.logical)] <- lapply(decisionData5[sapply(decisionData5, is.logical)], as.numeric)

ctreeWALA5 <- ctree(underweight ~ ., data=decisionData5, na.action=na.pass)
print(ctreeWALA5)
plot(ctreeWALA5, type="simple")
    #As before, bias towards majority class

    #Signifiance of attributes in ctree
sctest(ctreeWALA5, node=1)

    #Feature Selection
featData5 <- decisionData5[complete.cases(decisionData5),]
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model5 <- train(underweight~., data=featData5, 
                method="lvq", preProcess="scale", 
                trControl=control)
importance5 <- varImp(model5)
print(importance5)
plot(importance5)
    #A couple additional important factors

    #Will also remove liquidFormula and liquidMilk, because the count variable of those provide the same information now
AllDataWALA2 = subset(AllDataWALA2, select = -c(liquidMilk, liquidFormula))



    #Final dataset in advance of modeling
modelDataWALA = subset(AllDataWALA2, select = -c(foodRedPalm, liquidJuice, liquidYogurtNum, 
                                                bfCleanChildHW, anyDairy, foodCondiment, bfFoodPrepHW, aftFeedChildHW,
                                                liquidFormulaNum, liquidMilkNum, foodYellow, 
                                                foodEgg, foodOrgan, weighIn, otherHW, foodInsect, liquidBroth,
                                                startBreastfeed, liquidORS, childDoc, drankBreastmilk, bfFoodHW, 
                                                liquidPorridge, aftFoodHW, bioMom, foodMeat, childCode, childQNum))

#write.csv(modelDataWALA, "C:/Users/leahg/Documents/CIND820/WALAmodeldata.csv", row.names=TRUE)






#MODELING ----

    #MODEL REQUIREMENTS/DETAILS:
    #Naive Bayes does not require feature scaling or normalization, as it uses probabilities and not distances
    #I will need to take the mixed data types into consideration for Naive Bayes, and choose packages/codes to ensure this works

    #While some resources say logistic regression requires normalization to avoid the "vanishing gradient" problem 
    #during training (because it uses gradient descent), others say it doesn't respond well.
    #Source: https://www.turing.com/kb/effects-of-normalization-techniques-on-logistic-regression-in-data-science
    #Important consideration is interpretability with normalization, difficult to interpret coefficients when normalized
    #So I'll start by not normalizing, and normalize if I need to
    #If I do normalize, can use standardization (between -1 and 1), works well for models that suffer from gradient vanishing problems

    #Decision trees generally don't need normalizing/scaling, because splits don't change with any monotonic transformation

#Dividing into train and test set for each dependent variable
    #Since there are mild (malnourished and stunted) and moderate (wasting and underweight) imbalances in the dependent variable, I won't
    #start with any extreme measures to balance, but will take it into consideration when splitting the data set into train and test sets
    #to make sure the proportions are the same

dataMal <- subset(modelDataWALA, malnourished %notin% NA)
dataMal = subset(dataMal, select = -c(wasting, underweight, stunted))
dataMal <- dataMal[complete.cases(dataMal),] #breastfeeding and weight in variables have higher NA counts, does bring down this number
    #May remove them later if they aren't important for model and rerun
dataMal$malnourished <- as.factor(dataMal$malnourished)

dataWast <- subset(modelDataWALA, wasting %notin% NA)
dataWast = subset(dataWast, select = -c(malnourished, underweight, stunted))
dataWast <- dataWast[complete.cases(dataWast),] 
dataWast$wasting <- as.factor(dataWast$wasting)

dataUnder <- subset(modelDataWALA, underweight %notin% NA)
dataUnder = subset(dataUnder, select = -c(wasting, malnourished, stunted))
dataUnder <- dataUnder[complete.cases(dataUnder),] 
dataUnder$underweight <- as.factor(dataUnder$underweight)

dataStunt <- subset(modelDataWALA, stunted %notin% NA)
dataStunt = subset(dataStunt, select = -c(wasting, underweight, malnourished))
dataStunt <- dataStunt[complete.cases(dataStunt),] 
dataStunt$stunted <- as.factor(dataStunt$stunted)

    #One concern about doing it this way is that the data sets the different models are built on are different right away (by using all instances
    #that don't have an NA in one specific dependent variable).
    #Another option would be to remove any row instances where there was an NA for any of the four dependent variables, HOWEVER I also
    #know that many of the children under 6 months were only weighed and not measured, so they would have an NA for stunted and wasting.
    #The drop in instances of children under 6 months could also negatively affect the models' predictive ability, so will stick with this
    #option till it looks like it was the wrong move
set.seed(27)

    #Malnourished
dataMalT <- subset(dataMal, malnourished==TRUE)
dataMalF <- subset(dataMal, malnourished==FALSE)

indexMalF <- sample(1:nrow(dataMalF), 0.70 *nrow(dataMalF))
indexMalT <- sample(1:nrow(dataMalT), 0.70 *nrow(dataMalT))

dataMalTrain <- rbind((dataMalF[indexMalF,]), (dataMalT[indexMalT,]))
dataMalTest <- rbind((dataMalF[-indexMalF,]), (dataMalT[-indexMalT,]))
summary(dataMalTest$malnourished)
summary(dataMalTrain$malnourished)

dataMalTestY = dataMalTest$malnourished
dataMalTest <- dataMalTest[,-31]

    #Wasting
dataWastT <- subset(dataWast, wasting==TRUE)
dataWastF <- subset(dataWast, wasting==FALSE)

indexWastT <- sample(1:nrow(dataWastT), 0.70 *nrow(dataWastT))
indexWastF <- sample(1:nrow(dataWastF), 0.70 *nrow(dataWastF))

dataWastTrain <- rbind((dataWastF[indexWastF,]), (dataWastT[indexWastT,]))
dataWastTest <- rbind((dataWastF[-indexWastF,]), (dataWastT[-indexWastT,]))
summary(dataWastTest$wasting)
summary(dataWastTrain$wasting)

dataWastTestY = dataWastTest$wasting
dataWastTest <- dataWastTest[,-31]

    #Stunted
dataStuntT <- subset(dataStunt, stunted==TRUE)
dataStuntF <- subset(dataStunt, stunted==FALSE)

indexStuntT <- sample(1:nrow(dataStuntT), 0.70 *nrow(dataStuntT))
indexStuntF <- sample(1:nrow(dataStuntF), 0.70 *nrow(dataStuntF))

dataStuntTrain <- rbind((dataStuntF[indexStuntF,]), (dataStuntT[indexStuntT,]))
dataStuntTest <- rbind((dataStuntF[-indexStuntF,]), (dataStuntT[-indexStuntT,]))
summary(dataStuntTest$stunted)
summary(dataStuntTrain$stunted)

dataStuntTestY = dataStuntTest$stunted
dataStuntTest <- dataStuntTest[,-31]

    #Underweight
dataUnderT <- subset(dataUnder, underweight==TRUE)
dataUnderF <- subset(dataUnder, underweight==FALSE)

indexUnderT <- sample(1:nrow(dataUnderT), 0.70 *nrow(dataUnderT))
indexUnderF <- sample(1:nrow(dataUnderF), 0.70 *nrow(dataUnderF))

dataUnderTrain <- rbind((dataUnderF[indexUnderF,]), (dataUnderT[indexUnderT,]))
dataUnderTest <- rbind((dataUnderF[-indexUnderF,]), (dataUnderT[-indexUnderT,]))
summary(dataUnderTest$underweight)
summary(dataUnderTrain$underweight)

dataUnderTestY = dataUnderTest$underweight
dataUnderTest <- dataUnderTest[,-31]

#Naive Bayes ----
    #While Naive Bayes can handle missing data, I also want the models to be as comparable as possible so will use the same data set
    #One potential concern is that Naive Bayes isn't great when there are significant differences
    #in attribute distribution between the train and test datasets. Important to keep in mind with this
    #dataset as many of the attributes were very imbalanced


    #Malnourished - normal split with e1071 Naive Bayes
malNBModel <- naiveBayes(malnourished ~ ., data = dataMalTrain)  
summary(malNBModel)

predMalNB <- predict(malNBModel, dataMalTest, type="class") 

cmMalNB <- confusionMatrix(predMalNB, reference = dataMalTestY, positive = 'TRUE')
cmMalNB


    #Stunted - normal split with e1071 Naive Bayes
stuntNBModel <- naiveBayes(stunted ~ ., data = dataStuntTrain)  
summary(stuntNBModel)

predStuntNB <- predict(stuntNBModel, dataStuntTest, type="class") 

cmStuntNB <- confusionMatrix(predStuntNB, reference = dataStuntTestY, positive = 'TRUE')
cmStuntNB

    #Wasting - normal split with e1071 Naive Bayes
wastNBModel <- naiveBayes(wasting ~ ., data = dataWastTrain)  
summary(wastNBModel)

predWastNB <- predict(wastNBModel, dataWastTest, type="class") 

cmWastNB <- confusionMatrix(as.factor(predWastNB), reference = as.factor(dataWastTestY), positive = 'TRUE')
    #Not sure why I need to convert it to a factor here and no where else, I do not question R too much...
cmWastNB

    #Underweight - normal split with e1071 Naive Bayes
underNBModel <- naiveBayes(underweight ~ ., data = dataUnderTrain)  
summary(underNBModel)

predUnderNB <- predict(underNBModel, dataUnderTest, type="class") 

cmUnderNB <- confusionMatrix(predUnderNB, reference = dataUnderTestY, positive = 'TRUE')
cmUnderNB

#Stratified K-fold cross validation
    #Given suboptimal performance metrics, will do stratified cross fold validation
    #Starting with 10 since that's a pretty common starting point
set.seed(27)
    #Malnourished
folds <- 10
cvIndexMal <- createFolds(factor(dataMalTrain$malnourished), folds, returnTrain = T)
tcMal <- trainControl(index = cvIndexMal,
                      method = 'cv', 
                      number = folds)

    #Stunted
cvIndexStunt <- createFolds(factor(dataStuntTrain$stunted), folds, returnTrain = T)
tcStunt <- trainControl(index = cvIndexStunt,
                        method = 'cv', 
                        number = folds)

    #Wasting
cvIndexWast <- createFolds(factor(dataWastTrain$wasting), folds, returnTrain = T)
tcWast <- trainControl(index = cvIndexWast,
                       method = 'cv', 
                       number = folds)

    #Underweight
cvIndexUnder <- createFolds(factor(dataUnderTrain$underweight), folds, returnTrain = T)
tcUnder <- trainControl(index = cvIndexUnder,
                        method = 'cv', 
                        number = folds)

    #Malnourished - k fold cross validation with caret
malNBModel2 = train(x=dataMalTrain[,-31], y=dataMalTrain$malnourished, method='nb', trControl = tcMal)
malNBModel2

malNBModel2$resample
    #warnings about 0 probability, fold 10 failed

predMalNB2 <- predict(malNBModel2$finalModel, dataMalTest)$class

cmMalNB2 <- confusionMatrix(predMalNB2, reference = dataMalTestY, positive = 'TRUE')
cmMalNB2

    #Stunted - k fold cross validation with caret
stuntNBModel2 = train(x=dataStuntTrain[,-31], y=dataStuntTrain$stunted, method='nb', trControl = tcStunt)
stuntNBModel2

stuntNBModel2$resample
    #Fold 9 failed

predStuntNB2 <- predict(stuntNBModel2$finalModel, dataStuntTest)$class
    #warnings about 0 probability

cmStuntNB2 <- confusionMatrix(predStuntNB2, reference = dataStuntTestY, positive = 'TRUE')
cmStuntNB2

    #Wasting - k fold cross validation with caret
wastNBModel2 = train(x=dataWastTrain[,-31], y=dataWastTrain$wasting, method='nb', trControl = tcWast)
wastNBModel2

wastNBModel2$resample

predWastNB2 <- predict(wastNBModel2$finalModel, dataWastTest)$class
    #warnings about 0 probability, issue with test vs train split and distribution

cmWastNB2 <- confusionMatrix(predWastNB2, reference = dataWastTestY, positive = 'TRUE')
cmWastNB2

    #Underweight - k fold cross validation with caret
underNBModel2 = train(x=dataUnderTrain[,-31], y=dataUnderTrain$underweight, method='nb', trControl = tcUnder)
underNBModel2

underNBModel2$resample
    #warnings about 0 probability, fold 10 failed

predUnderNB2 <- predict(underNBModel2$finalModel, dataUnderTest)$class

cmUnderNB2 <- confusionMatrix(predUnderNB2, reference = dataUnderTestY, positive = 'TRUE')
cmUnderNB2

    #Some real bad models
    #Feasible options for improving Naive Bayes at this point include using a log probability
    #for calculations, eliminate the zero observations (major issue with this data set), retrain, 
    #and manage continuous variables

    #Will start by using laplace smoothing (adding a parameter to the numerator and denominator
    #to ensure there isn't a zero probability), then training the models with a larger datasets
    #Could also adjust continuous variables (number of times breastfed and number of meals) so
    #they are represented as categories instead. But I know they are not normally distributed to
    #begin with, so would need to use a non-parametric method

#Malnourished - k fold cross validation with caret + laplace smoothing
myGrid <- data.frame(fL=c(0,0.5,1.0), usekernel = TRUE, adjust=c(0,0.5,1.0))

malNBModel3 = train(x=dataMalTrain[,-31], y=dataMalTrain$malnourished, method='nb', trControl = tcMal, tuneGrid = myGrid)
    #warnings about 0 probability rolled over with 0.0 fL
malNBModel3
malNBModel3$finalModel$tuneValue
plot(malNBModel3)
malNBModel3$resample
    #Fold 10 failed

predMalNB3 <- predict(malNBModel3$finalModel, dataMalTest)$class

cmMalNB3 <- confusionMatrix(predMalNB3, reference = dataMalTestY, positive = 'TRUE')
cmMalNB3

    #Stunted - k fold cross validation with caret + laplace smoothing
stuntNBModel3 = train(x=dataStuntTrain[,-31], y=dataStuntTrain$stunted, method='nb', trControl = tcStunt, tuneGrid = myGrid)
stuntNBModel3
stuntNBModel3$resample
    #Fold 9 failed

malNBModel3$finalModel$tuneValue

predStuntNB3 <- predict(stuntNBModel3$finalModel, dataStuntTest)$class

cmStuntNB3 <- confusionMatrix(predStuntNB3, reference = dataStuntTestY, positive = 'TRUE')
cmStuntNB3

    #Wasting - k fold cross validation with caret + laplace smoothing
wastNBModel3 = train(x=dataWastTrain[,-31], y=dataWastTrain$wasting, method='nb', trControl = tcWast, tuneGrid = myGrid)
wastNBModel3
wastNBModel3$resample

malNBModel3$finalModel$tuneValue

predWastNB3 <- predict(wastNBModel3$finalModel, dataWastTest)$class

cmWastNB3 <- confusionMatrix(predWastNB3, reference = dataWastTestY, positive = 'TRUE')
cmWastNB3

    #Underweight - k fold cross validation with caret + laplace smoothing
underNBModel3 = train(x=dataUnderTrain[,-31], y=dataUnderTrain$underweight, method='nb', trControl = tcUnder, tuneGrid = myGrid)
underNBModel3
underNBModel3$resample
    #Fold 10 failed

predUnderNB3 <- predict(underNBModel2$finalModel, dataUnderTest)$class

cmUnderNB3 <- confusionMatrix(predUnderNB3, reference = dataUnderTestY, positive = 'TRUE')
cmUnderNB3

    #Laplace smoothing helped a little for the malnourished model, and not at all for the other three
    #Now will try using a larger amount of data to train and test

    #Since it involves redoing all train/test splits and the fold metrics, will do the whole process for malnourished only first
    #so see if it makes a large change

dataMal2 <- subset(modelDataWALA, malnourished %notin% NA)
dataMal2 = subset(dataMal2, select = -c(wasting, underweight, stunted))
dataMal2 = subset(dataMal2, select = -c(breastfedYdayNum, childWeighIn)) #both these variables have high NA counts, removing
dataMal2 <- dataMal2[complete.cases(dataMal2),]
dataMal2$malnourished <- as.factor(dataMal2$malnourished)

set.seed(27)
dataMalT2 <- subset(dataMal2, malnourished==TRUE)
dataMalF2 <- subset(dataMal2, malnourished==FALSE)

indexMalF2 <- sample(1:nrow(dataMalF2), 0.70 *nrow(dataMalF2))
indexMalT2 <- sample(1:nrow(dataMalT2), 0.70 *nrow(dataMalT2))

dataMalTrain2 <- rbind((dataMalF2[indexMalF2,]), (dataMalT2[indexMalT2,]))
dataMalTest2 <- rbind((dataMalF2[-indexMalF2,]), (dataMalT2[-indexMalT2,]))
summary(dataMalTest2$malnourished)
summary(dataMalTrain2$malnourished)

dataMalTestY2 = dataMalTest2$malnourished
dataMalTest2 <- dataMalTest2[,-30]

set.seed(27)
cvIndexMal2 <- createFolds(factor(dataMalTrain2$malnourished), folds, returnTrain = T)
tcMal2 <- trainControl(index = cvIndexMal2, method = 'cv', number = folds)

malNBModel4 = train(x=dataMalTrain2[,-30], y=dataMalTrain2$malnourished, method='nb', trControl = tcMal2, tuneGrid = myGrid)
warnings()
    #Same number of warnings
malNBModel4
malNBModel4$resample

malNBModel4$finalModel$tuneValue
plot(malNBModel4)

predMalNB4 <- predict(malNBModel4$finalModel, dataMalTest2)$class

cmMalNB4 <- confusionMatrix(predMalNB4, reference = dataMalTestY2, positive = 'TRUE')
cmMalNB4
    #helped marginally, but not enough to justify changing all the models





#Logistic Regression ----
    #Chose glm as it does not need to be normally distributed, allows for binomial models,
    #and it is able to deal with categorical predictors
    #Other benefits include easy interpretation and it's less susceptible to overfitting

    #Malnourished - glm+binomial with normal split
malLRModel <- glm(malnourished~., dataMalTrain, family = "binomial") 
summary(malLRModel)

predMalLR <- predict(malLRModel, dataMalTest, type="response")

predMalLR <- ifelse(predMalLR>=0.5, 1, 0) #rounding p to 1 and 0.
predMalLR <- as.logical(predMalLR)

cmMalLR <- confusionMatrix(as.factor(predMalLR), reference = as.factor(dataMalTestY), positive = 'TRUE')
cmMalLR

    #Stunted - glm+binomial with normal split
stuntLRModel <- glm(stunted~., dataStuntTrain, family = "binomial") 
summary(stuntLRModel)

predStuntLR <- predict(stuntLRModel, dataStuntTest, type="response")

predStuntLR <- ifelse(predStuntLR>=0.5, 1, 0) #rounding p to 1 and 0.
predStuntLR <- as.logical(predStuntLR)

cmStuntLR <- confusionMatrix(as.factor(predStuntLR), reference = dataStuntTestY, positive = 'TRUE')
cmStuntLR

    #Wasting - glm+binomial with normal split
wastLRModel <- glm(wasting~., dataWastTrain, family = "binomial") 
summary(wastLRModel)

predWastLR <- predict(wastLRModel, dataWastTest, type="response")

predWastLR <- ifelse(predWastLR>=0.5, 1, 0) #rounding p to 1 and 0. 
predWastLR <- as.logical(predWastLR)

cmWastLR <- confusionMatrix(as.factor(predWastLR), reference = dataWastTestY, positive = 'TRUE')
cmWastLR

    #Underweight - glm+binomial with normal split
underLRModel <- glm(underweight~., dataUnderTrain, family = "binomial") 
summary(underLRModel)

predUnderLR <- predict(underLRModel, dataUnderTest, type="response")

predUnderLR <- ifelse(predUnderLR>=0.5, 1, 0) #rounding p to 1 and 0. 
predUnderLR <- as.logical(predUnderLR)

cmUnderLR <- confusionMatrix(as.factor(predUnderLR), reference = dataUnderTestY, positive = 'TRUE')
cmUnderLR

    #10-fold cross validation

    #Malnourished - k fold cross validation with caret
malLRModel2 <- train(x=dataMalTrain[,-31], y=dataMalTrain$malnourished, method="glm", 
                     trControl = tcMal, family = "binomial")
malLRModel2$resample
    #Fold 10 failed

summary(malLRModel2)
varImp(malLRModel2)
    #got warning message about a fold failing due to new values (under dispose faeces, one of the very imbalanced answers)

predMalLR2 <- predict(malLRModel2, dataMalTest)

cmMalLR2 <- confusionMatrix(predMalLR2, reference = dataMalTestY, positive = 'TRUE')
cmMalLR2

    #Stunted - k fold cross validation with caret
stuntLRModel2 <- train(x=dataStuntTrain[,-31], y=dataStuntTrain$stunted, method="glm", 
                       trControl = tcStunt, family = "binomial")

stuntLRModel2$resample
varImp(stuntLRModel2)

summary(stuntLRModel2)
    #more warning messages about a fold failing due to new values (age group, under 6 months, due to most children under
    #6 months being weighed and not measured). Was in fold 9
predStuntLR2 <- predict(stuntLRModel2, dataStuntTest)

cmStuntLR2 <- confusionMatrix(predStuntLR2, reference = dataStuntTestY, positive = 'TRUE')
cmStuntLR2

    #Wasting - k fold cross validation with caret
wastLRModel2 <- train(x=dataWastTrain[,-31], y=dataWastTrain$wasting, method="glm", 
                      trControl = tcWast, family = "binomial")

summary(wastLRModel2)
varImp(wastLRModel2)
wastLRModel2$resample

    #more warning messages about fitted probabilities being 0 or 1, though no folds failed
predWastLR2 <- predict(wastLRModel2, dataWastTest)

cmWastLR2 <- confusionMatrix(predWastLR2, reference = dataWastTestY, positive = 'TRUE')
cmWastLR2

    #Underweight - k fold cross validation with caret
underLRModel2 <- train(x=dataUnderTrain[,-31], y=dataUnderTrain$underweight, method="glm", 
                       trControl = tcUnder, family = "binomial")

summary(underLRModel2)
underLRModel2$resample
    #more warning messages about disposing of faeces, fold 10 failed

predUnderLR2 <- predict(underLRModel2, dataUnderTest)

cmUnderLR2 <- confusionMatrix(predUnderLR2, reference = dataUnderTestY, positive = 'TRUE')
cmUnderLR2

    #Trying the larger dataset (without breastfeeding number and weigh in variables) for 
    #malnourished again to see if that makes a difference
malLRModel3 <- train(x=dataMalTrain2[,-30], y=dataMalTrain2$malnourished, method="glm", 
                     trControl = tcMal2, family = "binomial")
summary(malLRModel3)
malLRModel3$resample
    #disposed faeces messing up a fold again, fold 6 failed

predMalLR3 <- predict(malLRModel3, dataMalTest2)

cmMalLR3 <- confusionMatrix(predMalLR3, reference = dataMalTestY2, positive = 'TRUE')
cmMalLR3

    #Trying a data set with fewer variables, removing ones that were not identified as important or
    #significant previously
    #Using cut off of 30 from glm variable importance
dataMal3 <- subset(modelDataWALA, malnourished %notin% NA)
dataMal3 = subset(dataMal3, select = -c(wasting, underweight, stunted))
dataMal3 = subset(dataMal3, select = c(malnourished, ageGroup, childSex, foodSalt, other3Day, foodFats, 
                                       aftChildDefHW, bfFeedChildHW, foodWhite, foodProduce, foodGrain, neverHW))
dataMal3$malnourished <- as.factor(dataMal3$malnourished)
dataMal3 <- dataMal3[complete.cases(dataMal3),]

set.seed(27)
dataMalT3 <- subset(dataMal3, malnourished==TRUE)
dataMalF3 <- subset(dataMal3, malnourished==FALSE)

indexMalF3 <- sample(1:nrow(dataMalF3), 0.70 *nrow(dataMalF3))
indexMalT3 <- sample(1:nrow(dataMalT3), 0.70 *nrow(dataMalT3))

dataMalTrain3 <- rbind((dataMalF3[indexMalF3,]), (dataMalT3[indexMalT3,]))
dataMalTest3 <- rbind((dataMalF3[-indexMalF3,]), (dataMalT3[-indexMalT3,]))
summary(dataMalTest3$malnourished)
summary(dataMalTrain3$malnourished)

dataMalTestY3 = dataMalTest3$malnourished
dataMalTest3 <- dataMalTest3[,-1]

set.seed(27)
cvIndexMal3 <- createFolds(factor(dataMalTrain3$malnourished), folds, returnTrain = T)
tcMal3 <- trainControl(index = cvIndexMal3, method = 'cv', number = folds)

    #model
malLRModel4 <- train(x=dataMalTrain3[,-1], y=dataMalTrain3$malnourished, method="glm", 
                     trControl = tcMal3, family = "binomial")
summary(malLRModel4)
malLRModel4$resample

predMalLR4 <- predict(malLRModel4, dataMalTest3)

cmMalLR4 <- confusionMatrix(predMalLR4, reference = dataMalTestY3, positive = 'TRUE')
cmMalLR4
    #Now even worse for identifying malnourished...





#Decision Tree ----
    #rpart apparently not as good as c50 (which uses winnowing and boosting), but it is less prone to overfitting,
    #which is why it was used here
    #rpart uses gini impurity to select splits (measure of the likelihood of random data being misclassified
    #if it was given a random class label, based on the current class distributions. 0=all cases of the node are in
    #one category)

    #Malnourished - rpart with standard split
malDTModel<-rpart(malnourished~., data=dataMalTrain, method = "class")
prp(malDTModel, extra=1)
printcp(malDTModel)

malTable <- data.frame(value = dataMalTestY, pred = predict(malDTModel, dataMalTest, type = "class"))

cmMalDT <- confusionMatrix(malTable$pred, reference=malTable$value, positive = 'TRUE')
cmMalDT

    #Stunted - rpart with standard split
stuntDTModel<-rpart(stunted~., data=dataStuntTrain, method = "class")

prp(stuntDTModel, extra=1)
printcp(stuntDTModel)

stuntTable <- data.frame(value = dataStuntTestY, pred = predict(stuntDTModel, dataStuntTest, type = "class"))

cmStuntDT <- confusionMatrix(stuntTable$pred, reference=stuntTable$value, positive = 'TRUE')
cmStuntDT

    #Wasting - rpart with standard split
wastDTModel<-rpart(wasting~., data=dataWastTrain, method = "class")

prp(wastDTModel, extra=1)
printcp(wastDTModel)

wastTable <- data.frame(value = dataWastTestY, pred = predict(wastDTModel, dataWastTest, type = "class"))

cmWastDT <- confusionMatrix(wastTable$pred, reference=wastTable$value, positive = 'TRUE')
cmWastDT

    #Underweight - rpart with standard split
underDTModel<-rpart(underweight~., data=dataUnderTrain, method = "class")

prp(underDTModel, extra=1)
printcp(underDTModel)
    #useless decision tree, just one node

underTable <- data.frame(value = dataUnderTestY, pred = predict(underDTModel, dataUnderTest, type = "class"))

cmUnderDT <- confusionMatrix(underTable$pred, reference=underTable$value, positive = 'TRUE')
cmUnderDT

    #Stratified 10-fold cross validation

    #Malnourished - caret/rpart with stratified k fold cross validation
malDTModel2 <- train(x=dataMalTrain[,-31], y=dataMalTrain$malnourished, method="rpart", 
                     trControl = tcMal)

malDTModel2
malDTModel2$resample
    #fold warning, dispose faeces, fold 10

plot(malDTModel2$finalModel, uniform = TRUE, main = "Malnourished Decision Tree")
text(malDTModel2$finalModel, use.n = TRUE, all=TRUE)

predMalDT2 <- predict(malDTModel2, dataMalTest, type="raw")

cmMalDT2 <- confusionMatrix(predMalDT2, reference = dataMalTestY, positive = 'TRUE')
cmMalDT2

    #Stunted - caret/rpart with stratified k fold cross validation
stuntDTModel2 <- train(x=dataStuntTrain[,-31], y=dataStuntTrain$stunted, method="rpart", 
                       trControl = tcStunt)

stuntDTModel2
stuntDTModel2$resample
    #fold warning, age, fold 9 failed

plot(stuntDTModel2$finalModel, uniform = TRUE, main = "Stunted Decision Tree")
text(stuntDTModel2$finalModel, use.n = TRUE, all=TRUE)

predStuntDT2 <- predict(stuntDTModel2, dataStuntTest, type="raw")

cmStuntDT2 <- confusionMatrix(predStuntDT2, reference = dataStuntTestY, positive = 'TRUE')
cmStuntDT2

    #Wasting - caret/rpart with stratified k fold cross validation
wastDTModel2 <- train(x=dataWastTrain[,-31], y=dataWastTrain$wasting, method="rpart", 
                      trControl = tcWast)

wastDTModel2
wastDTModel2$resample

plot(wastDTModel2$finalModel, uniform = TRUE, main = "Wasting Decision Tree")
text(wastDTModel2$finalModel, use.n = TRUE, all=TRUE)

predWastDT2 <- predict(wastDTModel2, dataWastTest, type="raw")

cmWastDT2 <- confusionMatrix(predWastDT2, reference = dataWastTestY, positive = 'TRUE')
cmWastDT2

    #Underweight - caret/rpart with stratified k fold cross validation
underDTModel2 <- train(x=dataUnderTrain[,-31], y=dataUnderTrain$underweight, method="rpart", 
                       trControl = tcUnder)

underDTModel2
underDTModel2$resample
    #fold fail warning, dispose of faeces, fold 10
    #Just a root like simple split, so won't plot

predUnderDT2 <- predict(underDTModel2, dataUnderTest, type="raw")

cmUnderDT2 <- confusionMatrix(predUnderDT2, reference = dataUnderTestY, positive = 'TRUE')
cmUnderDT2


    #All the options for imrpoving the tree/avoiding overfitting don't really work here
    #Things like minimum samples for leaf splits, max depth, pruning aren't really
    #applicable because I'm not getting huge tree. It's an issue of prediction




#STATISTICAL ANALYSIS ----
malType <- c('malnourished', 'stunted', 'wasting', 'underweight', 'malnourished', 'stunted', 'wasting', 'underweight', 
               'malnourished', 'stunted', 'wasting', 'underweight', 'malnourished', 'malnourished', 'stunted', 
               'wasting', 'underweight', 'malnourished', 'stunted', 'wasting', 'underweight', 'malnourished', 
               'malnourished', 'malnourished', 'stunted', 'wasting', 'underweight', 'malnourished', 'stunted', 'wasting', 'underweight')
modelType <- c('Naive', 'Naive', 'Naive', 'Naive', 'Naive', 'Naive', 'Naive', 'Naive', 'Naive', 'Naive', 'Naive', 'Naive', 'Naive',
               'logistic', 'logistic', 'logistic', 'logistic', 'logistic', 'logistic', 'logistic', 'logistic', 'logistic', 'logistic', 'tree', 
               'tree', 'tree', 'tree', 'tree', 'tree', 'tree', 'tree')
splitType <- c('normal', 'normal', 'normal', 'normal', 'stratified', 'stratified', 'stratified', 'stratified', 
               'stratified', 'stratified', 'stratified', 'stratified', 'stratified', 'normal', 'normal', 
               'normal', 'normal', 'stratified', 'stratified', 'stratified', 'stratified', 'stratified', 
               'stratified', 'normal', 'normal', 'normal', 'normal', 'stratified', 'stratified', 'stratified', 'stratified')
modelName <- c('malNBModel', 'stuntNBModel', 'wastNBModel', 'underNBModel', 'malNBModel2', 'stuntNBModel2', 'wastNBModel2', 'underNBModel2', 
               'malNBModel3', 'stuntNBModel3', 'wastNBModel3', 'underNBModel3', 'malNBModel4', 'malLRModel', 'stuntLRModel', 
               'wastLRModel', 'underLRModel', 'malLRModel2', 'stuntLRModel2', 'wastLRModel2', 'underLRModel2', 'malLRModel3', 
               'malLRModel4', 'malDTModel', 'stuntDTModel', 'wastDTModel', 'underDTModel', 'malDTModel2', 'stuntDTModel2', 'wastDTModel2', 'underDTModel2')
recallNum <- c(0.54, 0.15, 0.05, 0.00, 0.44, 0.15, 0.03, 0.00, 0.45, 0.05, 0.03, 0.00, 0.46, 0.30, 0.14, 
               0.00, 0.00, 0.30, 0.14, 0.00, 0.00, 0.26, 0.17, 0.23, 0.05, 0.03, 0.00, 0.23, 0.19, 0.03, 0.00) 
kappaNum <- c(0.11, 0.08, 0.05, -0.007, 0.10, 0.08, 0.03, 0.00, 0.10, 0.01, 0.03, 0.00, 0.14, 0.15, 0.07, 
              -0.004, -0.004, 0.15, 0.07, -0.004, -0.004, 0.12, 0.10, 0.12, 0.04, 0.03, 0.00, 0.12, 0.13, 0.03, 0.00) 

modelAnalysis <- data.frame(malType, modelType, splitType, modelName, recallNum, kappaNum)


    #Kruskal Wallis test for recall between different malnutrition forms
kruskal.test(recallNum ~ malType, data = modelAnalysis)
attach(modelAnalysis)
dunn.test(recallNum, malType, kw = TRUE, label=TRUE)
detach(modelAnalysis)
    #Significant difference between the recall of the different malnutrition models

    #Kruskal Wallis test for kappa between different malnutrition forms
kruskal.test(kappaNum ~ malType, data = modelAnalysis)
attach(modelAnalysis)
dunn.test(kappaNum, malType, kw = TRUE, label=TRUE)
detach(modelAnalysis)
    #Significant difference between the recall of the different malnutrition models

    #Kruskal Wallis test for recall between different model types
kruskal.test(recallNum ~ modelType, data = modelAnalysis)
    #No significant difference between the recall of the different model types

    #Kruskal Wallis test for kappa between different model types
kruskal.test(kappaNum ~ modelType, data = modelAnalysis)
    #No significant difference between the kappa of the different model types

    #Wilcoxon rank sum test for recall between different splits
wilcox.test(recallNum ~ splitType, data = modelAnalysis)
    #No significant difference between the recall of the different types of splits

    #Wilcoxon rank sum test for kappa between different splits
wilcox.test(kappaNum ~ splitType, data = modelAnalysis)
    #No significant difference between the kappa of the different types of splits