#DATA CLEANING
library(tidyverse)
Sys.which('pdflatex')
    #general survey codes:
    #95 = no other answer
    #96 = other
    #97 = none
    #98 = don't know
    #99 = refused

#upload dataset, check structure
dataWALAorig<-read.csv(file='C:/Users/leahg/Documents/CIND820/WALA documents/WALA_2018_Follow-up_Child_Dataset_20240116.csv',header=T,sep=",")
head(dataWALAorig)
is.data.frame(dataWALAorig)
summary(dataWALAorig)
str(dataWALAorig)

dataWALA <- dataWALAorig

#DATA CLEANING: variable management ----

#Change Variable Names
colnames(dataWALA) <- c("districtName", "childSex", "refMomLine", "bioMom", 
                        "birthYear", "birthYearSource", "ageMonth", "ifInfant", "birthAssist1",
                        "birthAssist2", "birthAssist3", "birthAssist4", "birthLoc", "confirmBioMom1",
                        "momVitA", "vitAWhere", "disposeFaeces", "disposeWater", "whenWashHand1", 
                        "whenWashHand2", "whenWashHand3", "whenWashHand4", "houseBedNet", "childBedNet",
                        "weighInMoH", "weighInCHAM", "childDoc", "childWtMonth", "childWt3Month",
                        "childWtRec1", "childWtRec2", "childWtRec3", "childWtTrend", "wasBreastfed",
                        "hadBreastmilk", "startBreastfeed", "waitBreastfeed", "ridFirstMilk", "other3Day",
                        "other6Month", "otherMonth", "stillBreastfed", "confirmBioMom2", "breastfedYday",
                        "breastfedYdayNum", "liquidSuppl", "liquidORS", "liquidWater", "liquidFormula",
                        "liquidFormulaNum", "liquidMilk", "liquidMilkNum", "liquidJuice", "liquidBroth",
                        "liquidYogurt", "liquidYogurtNum", "liquidPorridge", "liquidTea", "liquidOther",
                        "foodGrain", "foodYellow", "foodWhite", "foodGreen", "foodMangoPap", "foodProduce",
                        "foodOrgan", "foodMeat", "foodEgg", "foodFish", "foodBean", "foodDairy", "foodFats",
                        "foodSugar", "foodCondiment", "foodInsect", "foodRedPalm", "foodOther", "foodSalt",
                        "foodNum", "confirmAgeMonth", "weight", "length", "height", "oedema", "resultsMeasure",
                        "childCode", "childQNum", "Treatment")

#Convert variables from characters to logical
dataWALA$ifInfant <- sapply(dataWALA$ifInfant, as.logical)
dataWALA$houseBedNet <- sapply(dataWALA$houseBedNet, as.logical)
dataWALA$weighInMoH <- sapply(dataWALA$weighInMoH, as.logical)
dataWALA$weighInCHAM <- sapply(dataWALA$weighInCHAM, as.logical)
dataWALA$childWtMonth <- sapply(dataWALA$childWtMonth, as.logical)
dataWALA$childWt3Month <- sapply(dataWALA$childWt3Month, as.logical)
dataWALA$wasBreastfed <- sapply(dataWALA$wasBreastfed, as.logical)
dataWALA$hadBreastmilk <- sapply(dataWALA$hadBreastmilk, as.logical)
dataWALA$stillBreastfed <- sapply(dataWALA$stillBreastfed, as.logical)
dataWALA$breastfedYday <- sapply(dataWALA$breastfedYday, as.logical)

str(dataWALA)


#Cleaning and converting additional variables from character to logical

    #Vitamin A shot
unique(dataWALA$momVitA)
dataWALA$momVitA<-replace(dataWALA$momVitA, dataWALA$momVitA=='dk', NA)
dataWALA$momVitA<-replace(dataWALA$momVitA, dataWALA$momVitA=='yes', TRUE)
dataWALA$momVitA<-replace(dataWALA$momVitA, dataWALA$momVitA=='no', FALSE)
dataWALA$momVitA<-replace(dataWALA$momVitA, dataWALA$momVitA=='', NA)
unique(dataWALA$momVitA)
dataWALA$momVitA <- sapply(dataWALA$momVitA, as.logical)
str(dataWALA$momVitA)

    #If the child slept under a bed net last night
unique(dataWALA$childBedNet)
dataWALA$childBedNet<-replace(dataWALA$childBedNet, dataWALA$childBedNet==98, NA)
dataWALA$childBedNet<-replace(dataWALA$childBedNet, dataWALA$childBedNet=='yes', TRUE)
dataWALA$childBedNet<-replace(dataWALA$childBedNet, dataWALA$childBedNet=='no', FALSE)
dataWALA$childBedNet<-replace(dataWALA$childBedNet, dataWALA$childBedNet=='', NA)
unique(dataWALA$childBedNet)
dataWALA$childBedNet <- sapply(dataWALA$childBedNet, as.logical)
str(dataWALA$childBedNet)

    #Anything other than breast milk in first three days
unique(dataWALA$other3Day)
dataWALA$other3Day<-replace(dataWALA$other3Day, dataWALA$other3Day=='dont know', NA)
dataWALA$other3Day<-replace(dataWALA$other3Day, dataWALA$other3Day=='', NA)
dataWALA$other3Day<-replace(dataWALA$other3Day, dataWALA$other3Day=='yes', TRUE)
dataWALA$other3Day<-replace(dataWALA$other3Day, dataWALA$other3Day=='no', FALSE)
unique(dataWALA$other3Day)
dataWALA$other3Day <- sapply(dataWALA$other3Day, as.logical)
str(dataWALA$other3Day)

    #Anything other than breast milk in first six months
unique(dataWALA$other6Month)
dataWALA$other6Month<-replace(dataWALA$other6Month, dataWALA$other6Month=='dont know', NA)
dataWALA$other6Month<-replace(dataWALA$other6Month, dataWALA$other6Month=='', NA)
dataWALA$other6Month<-replace(dataWALA$other6Month, dataWALA$other6Month==6, NA)
dataWALA$other6Month<-replace(dataWALA$other6Month, dataWALA$other6Month==96, NA)
dataWALA$other6Month<-replace(dataWALA$other6Month, dataWALA$other6Month==97, 'no')
dataWALA$other6Month<-replace(dataWALA$other6Month, dataWALA$other6Month=='yes', TRUE)
dataWALA$other6Month<-replace(dataWALA$other6Month, dataWALA$other6Month=='no', FALSE)
unique(dataWALA$other6Month)
dataWALA$other6Month <- sapply(dataWALA$other6Month, as.logical)
str(dataWALA$other6Month)

    #If they consumed vitamin or mineral supplements or medicine
unique(dataWALA$liquidSuppl)
dataWALA$liquidSuppl<-replace(dataWALA$liquidSuppl, dataWALA$liquidSuppl==98, NA)
dataWALA$liquidSuppl<-replace(dataWALA$liquidSuppl, dataWALA$liquidSuppl=='', NA)
dataWALA$liquidSuppl<-replace(dataWALA$liquidSuppl, dataWALA$liquidSuppl=='yes', TRUE)
dataWALA$liquidSuppl<-replace(dataWALA$liquidSuppl, dataWALA$liquidSuppl=='no', FALSE)
unique(dataWALA$liquidSuppl)
dataWALA$liquidSuppl <- sapply(dataWALA$liquidSuppl, as.logical)
str(dataWALA$liquidSuppl)

    #If they consumed ORS yesterday
unique(dataWALA$liquidORS)
dataWALA$liquidORS<-replace(dataWALA$liquidORS, dataWALA$liquidORS==98, NA)
dataWALA$liquidORS<-replace(dataWALA$liquidORS, dataWALA$liquidORS==0, NA)
dataWALA$liquidORS<-replace(dataWALA$liquidORS, dataWALA$liquidORS=='', NA)
dataWALA$liquidORS<-replace(dataWALA$liquidORS, dataWALA$liquidORS=='yes', TRUE)
dataWALA$liquidORS<-replace(dataWALA$liquidORS, dataWALA$liquidORS=='no', FALSE)
unique(dataWALA$liquidORS)
dataWALA$liquidORS <- sapply(dataWALA$liquidORS, as.logical)
str(dataWALA$liquidORS)

    #If they consumed water yesterday
unique(dataWALA$liquidWater)
dataWALA$liquidWater<-replace(dataWALA$liquidWater, dataWALA$liquidWater==98, NA)
dataWALA$liquidWater<-replace(dataWALA$liquidWater, dataWALA$liquidWater=='', NA)
dataWALA$liquidWater<-replace(dataWALA$liquidWater, dataWALA$liquidWater=='yes', TRUE)
dataWALA$liquidWater<-replace(dataWALA$liquidWater, dataWALA$liquidWater=='no', FALSE)
unique(dataWALA$liquidWater)
dataWALA$liquidWater <- sapply(dataWALA$liquidWater, as.logical)
str(dataWALA$liquidWater)

    #If they consumed infant formula yesterday
unique(dataWALA$liquidFormula)
dataWALA$liquidFormula<-replace(dataWALA$liquidFormula, dataWALA$liquidFormula==98, NA)
dataWALA$liquidFormula<-replace(dataWALA$liquidFormula, dataWALA$liquidFormula=='', NA)
dataWALA$liquidFormula<-replace(dataWALA$liquidFormula, dataWALA$liquidFormula=='yes', TRUE)
dataWALA$liquidFormula<-replace(dataWALA$liquidFormula, dataWALA$liquidFormula=='no', FALSE)
unique(dataWALA$liquidFormula)
dataWALA$liquidFormula <- sapply(dataWALA$liquidFormula, as.logical)
str(dataWALA$liquidFormula)

    #If they consumed animal milk yesterday
unique(dataWALA$liquidMilk)
dataWALA$liquidMilk<-replace(dataWALA$liquidMilk, dataWALA$liquidMilk==98, NA)
dataWALA$liquidMilk<-replace(dataWALA$liquidMilk, dataWALA$liquidMilk=='', NA)
dataWALA$liquidMilk<-replace(dataWALA$liquidMilk, dataWALA$liquidMilk=='yes', TRUE)
dataWALA$liquidMilk<-replace(dataWALA$liquidMilk, dataWALA$liquidMilk=='no', FALSE)
unique(dataWALA$liquidMilk)
dataWALA$liquidMilk <- sapply(dataWALA$liquidMilk, as.logical)
str(dataWALA$liquidMilk)

    #If they consumed juice yesterday
unique(dataWALA$liquidJuice)
dataWALA$liquidJuice<-replace(dataWALA$liquidJuice, dataWALA$liquidJuice==98, NA)
dataWALA$liquidJuice<-replace(dataWALA$liquidJuice, dataWALA$liquidJuice=='', NA)
dataWALA$liquidJuice<-replace(dataWALA$liquidJuice, dataWALA$liquidJuice=='yes', TRUE)
dataWALA$liquidJuice<-replace(dataWALA$liquidJuice, dataWALA$liquidJuice=='no', FALSE)
unique(dataWALA$liquidJuice)
dataWALA$liquidJuice <- sapply(dataWALA$liquidJuice, as.logical)
str(dataWALA$liquidJuice)

    #If they consumed clear broth yesterday
unique(dataWALA$liquidBroth)
dataWALA$liquidBroth<-replace(dataWALA$liquidBroth, dataWALA$liquidBroth==98, NA)
dataWALA$liquidBroth<-replace(dataWALA$liquidBroth, dataWALA$liquidBroth=='', NA)
dataWALA$liquidBroth<-replace(dataWALA$liquidBroth, dataWALA$liquidBroth=='yes', TRUE)
dataWALA$liquidBroth<-replace(dataWALA$liquidBroth, dataWALA$liquidBroth=='no', FALSE)
unique(dataWALA$liquidBroth)
dataWALA$liquidBroth <- sapply(dataWALA$liquidBroth, as.logical)
str(dataWALA$liquidBroth)

    #If they consumed yogurt yesterday
unique(dataWALA$liquidYogurt)
dataWALA$liquidYogurt<-replace(dataWALA$liquidYogurt, dataWALA$liquidYogurt==98, NA)
dataWALA$liquidYogurt<-replace(dataWALA$liquidYogurt, dataWALA$liquidYogurt=='', NA)
dataWALA$liquidYogurt<-replace(dataWALA$liquidYogurt, dataWALA$liquidYogurt=='yes', TRUE)
dataWALA$liquidYogurt<-replace(dataWALA$liquidYogurt, dataWALA$liquidYogurt=='no', FALSE)
unique(dataWALA$liquidYogurt)
dataWALA$liquidYogurt <- sapply(dataWALA$liquidYogurt, as.logical)
str(dataWALA$liquidYogurt)

    #If they consumed thin porridge yesterday
unique(dataWALA$liquidPorridge)
dataWALA$liquidPorridge<-replace(dataWALA$liquidPorridge, dataWALA$liquidPorridge==98, NA)
dataWALA$liquidPorridge<-replace(dataWALA$liquidPorridge, dataWALA$liquidPorridge=='', NA)
dataWALA$liquidPorridge<-replace(dataWALA$liquidPorridge, dataWALA$liquidPorridge=='yes', TRUE)
dataWALA$liquidPorridge<-replace(dataWALA$liquidPorridge, dataWALA$liquidPorridge=='no', FALSE)
unique(dataWALA$liquidPorridge)
dataWALA$liquidPorridge <- sapply(dataWALA$liquidPorridge, as.logical)
str(dataWALA$liquidPorridge)

    #If they consumed tea yesterday
unique(dataWALA$liquidTea)
dataWALA$liquidTea<-replace(dataWALA$liquidTea, dataWALA$liquidTea==98, NA)
dataWALA$liquidTea<-replace(dataWALA$liquidTea, dataWALA$liquidTea=='', NA)
dataWALA$liquidTea<-replace(dataWALA$liquidTea, dataWALA$liquidTea=='yes', TRUE)
dataWALA$liquidTea<-replace(dataWALA$liquidTea, dataWALA$liquidTea=='no', FALSE)
unique(dataWALA$liquidTea)
dataWALA$liquidTea <- sapply(dataWALA$liquidTea, as.logical)
str(dataWALA$liquidTea)

    #If they consumed other liquids yesterday
unique(dataWALA$liquidOther)
dataWALA$liquidOther<-replace(dataWALA$liquidOther, dataWALA$liquidOther==98, NA)
dataWALA$liquidOther<-replace(dataWALA$liquidOther, dataWALA$liquidOther=='', NA)
dataWALA$liquidOther<-replace(dataWALA$liquidOther, dataWALA$liquidOther=='yes', TRUE)
dataWALA$liquidOther<-replace(dataWALA$liquidOther, dataWALA$liquidOther=='no', FALSE)
unique(dataWALA$liquidOther)
dataWALA$liquidOther <- sapply(dataWALA$liquidOther, as.logical)
str(dataWALA$liquidOther)

    #If they consumed grains yesterday
unique(dataWALA$foodGrain)
dataWALA$foodGrain<-replace(dataWALA$foodGrain, dataWALA$foodGrain==98, NA)
dataWALA$foodGrain<-replace(dataWALA$foodGrain, dataWALA$foodGrain=='', NA)
dataWALA$foodGrain<-replace(dataWALA$foodGrain, dataWALA$foodGrain=='yes', TRUE)
dataWALA$foodGrain<-replace(dataWALA$foodGrain, dataWALA$foodGrain=='no', FALSE)
unique(dataWALA$foodGrain)
dataWALA$foodGrain <- sapply(dataWALA$foodGrain, as.logical)
str(dataWALA$foodGrain)

    #If they consumed orange or yellow produce yesterday
unique(dataWALA$foodYellow)
dataWALA$foodYellow<-replace(dataWALA$foodYellow, dataWALA$foodYellow==98, NA)
dataWALA$foodYellow<-replace(dataWALA$foodYellow, dataWALA$foodYellow=='', NA)
dataWALA$foodYellow<-replace(dataWALA$foodYellow, dataWALA$foodYellow=='yes', TRUE)
dataWALA$foodYellow<-replace(dataWALA$foodYellow, dataWALA$foodYellow=='no', FALSE)
unique(dataWALA$foodYellow)
dataWALA$foodYellow <- sapply(dataWALA$foodYellow, as.logical)
str(dataWALA$foodYellow)

    #If they consumed white produce yesterday
unique(dataWALA$foodWhite)
dataWALA$foodWhite<-replace(dataWALA$foodWhite, dataWALA$foodWhite==98, NA)
dataWALA$foodWhite<-replace(dataWALA$foodWhite, dataWALA$foodWhite=='', NA)
dataWALA$foodWhite<-replace(dataWALA$foodWhite, dataWALA$foodWhite=='yes', TRUE)
dataWALA$foodWhite<-replace(dataWALA$foodWhite, dataWALA$foodWhite=='no', FALSE)
unique(dataWALA$foodWhite)
dataWALA$foodWhite <- sapply(dataWALA$foodWhite, as.logical)
str(dataWALA$foodWhite)

    #If they consumed dark leafy greens yesterday
unique(dataWALA$foodGreen)
dataWALA$foodGreen<-replace(dataWALA$foodGreen, dataWALA$foodGreen==98, NA)
dataWALA$foodGreen<-replace(dataWALA$foodGreen, dataWALA$foodGreen=='', NA)
dataWALA$foodGreen<-replace(dataWALA$foodGreen, dataWALA$foodGreen=='yes', TRUE)
dataWALA$foodGreen<-replace(dataWALA$foodGreen, dataWALA$foodGreen=='no', FALSE)
unique(dataWALA$foodGreen)
dataWALA$foodGreen <- sapply(dataWALA$foodGreen, as.logical)
str(dataWALA$foodGreen)

    #If they consumed mango or papaya yesterday
unique(dataWALA$foodMangoPap)
dataWALA$foodMangoPap<-replace(dataWALA$foodMangoPap, dataWALA$foodMangoPap==98, NA)
dataWALA$foodMangoPap<-replace(dataWALA$foodMangoPap, dataWALA$foodMangoPap=='', NA)
dataWALA$foodMangoPap<-replace(dataWALA$foodMangoPap, dataWALA$foodMangoPap=='yes', TRUE)
dataWALA$foodMangoPap<-replace(dataWALA$foodMangoPap, dataWALA$foodMangoPap=='no', FALSE)
unique(dataWALA$foodMangoPap)
dataWALA$foodMangoPap <- sapply(dataWALA$foodMangoPap, as.logical)
str(dataWALA$foodMangoPap)

    #If they consumed other produce yesterday
unique(dataWALA$foodProduce)
dataWALA$foodProduce<-replace(dataWALA$foodProduce, dataWALA$foodProduce==98, NA)
dataWALA$foodProduce<-replace(dataWALA$foodProduce, dataWALA$foodProduce=='', NA)
dataWALA$foodProduce<-replace(dataWALA$foodProduce, dataWALA$foodProduce=='yes', TRUE)
dataWALA$foodProduce<-replace(dataWALA$foodProduce, dataWALA$foodProduce=='no', FALSE)
unique(dataWALA$foodProduce)
dataWALA$foodProduce <- sapply(dataWALA$foodProduce, as.logical)
str(dataWALA$foodProduce)

    #If they consumed organ meat yesterday
unique(dataWALA$foodOrgan)
dataWALA$foodOrgan<-replace(dataWALA$foodOrgan, dataWALA$foodOrgan==98, NA)
dataWALA$foodOrgan<-replace(dataWALA$foodOrgan, dataWALA$foodOrgan=='', NA)
dataWALA$foodOrgan<-replace(dataWALA$foodOrgan, dataWALA$foodOrgan=='yes', TRUE)
dataWALA$foodOrgan<-replace(dataWALA$foodOrgan, dataWALA$foodOrgan=='no', FALSE)
unique(dataWALA$foodOrgan)
dataWALA$foodOrgan <- sapply(dataWALA$foodOrgan, as.logical)
str(dataWALA$foodOrgan)

    #If they consumed meat yesterday
unique(dataWALA$foodMeat)
dataWALA$foodMeat<-replace(dataWALA$foodMeat, dataWALA$foodMeat==98, NA)
dataWALA$foodMeat<-replace(dataWALA$foodMeat, dataWALA$foodMeat=='', NA)
dataWALA$foodMeat<-replace(dataWALA$foodMeat, dataWALA$foodMeat=='yes', TRUE)
dataWALA$foodMeat<-replace(dataWALA$foodMeat, dataWALA$foodMeat=='no', FALSE)
unique(dataWALA$foodMeat)
dataWALA$foodMeat <- sapply(dataWALA$foodMeat, as.logical)
str(dataWALA$foodMeat)

    #If they consumed eggs yesterday
unique(dataWALA$foodEgg)
dataWALA$foodEgg<-replace(dataWALA$foodEgg, dataWALA$foodEgg==98, NA)
dataWALA$foodEgg<-replace(dataWALA$foodEgg, dataWALA$foodEgg=='', NA)
dataWALA$foodEgg<-replace(dataWALA$foodEgg, dataWALA$foodEgg=='yes', TRUE)
dataWALA$foodEgg<-replace(dataWALA$foodEgg, dataWALA$foodEgg=='no', FALSE)
unique(dataWALA$foodEgg)
dataWALA$foodEgg <- sapply(dataWALA$foodEgg, as.logical)
str(dataWALA$foodEgg)

    #If they consumed fish yesterday
unique(dataWALA$foodFish)
dataWALA$foodFish<-replace(dataWALA$foodFish, dataWALA$foodFish==98, NA)
dataWALA$foodFish<-replace(dataWALA$foodFish, dataWALA$foodFish=='', NA)
dataWALA$foodFish<-replace(dataWALA$foodFish, dataWALA$foodFish=='yes', TRUE)
dataWALA$foodFish<-replace(dataWALA$foodFish, dataWALA$foodFish=='no', FALSE)
unique(dataWALA$foodFish)
dataWALA$foodFish <- sapply(dataWALA$foodFish, as.logical)
str(dataWALA$foodFish)

    #If they consumed beans etc yesterday
unique(dataWALA$foodBean)
dataWALA$foodBean<-replace(dataWALA$foodBean, dataWALA$foodBean==98, NA)
dataWALA$foodBean<-replace(dataWALA$foodBean, dataWALA$foodBean=='', NA)
dataWALA$foodBean<-replace(dataWALA$foodBean, dataWALA$foodBean=='yes', TRUE)
dataWALA$foodBean<-replace(dataWALA$foodBean, dataWALA$foodBean=='no', FALSE)
unique(dataWALA$foodBean)
dataWALA$foodBean <- sapply(dataWALA$foodBean, as.logical)
str(dataWALA$foodBean)

    #If they consumed dairy yesterday
unique(dataWALA$foodDairy)
dataWALA$foodDairy<-replace(dataWALA$foodDairy, dataWALA$foodDairy==98, NA)
dataWALA$foodDairy<-replace(dataWALA$foodDairy, dataWALA$foodDairy=='', NA)
dataWALA$foodDairy<-replace(dataWALA$foodDairy, dataWALA$foodDairy=='yes', TRUE)
dataWALA$foodDairy<-replace(dataWALA$foodDairy, dataWALA$foodDairy=='no', FALSE)
unique(dataWALA$foodDairy)
dataWALA$foodDairy <- sapply(dataWALA$foodDairy, as.logical)
str(dataWALA$foodDairy)

    #If they consumed fats yesterday
unique(dataWALA$foodFats)
dataWALA$foodFats<-replace(dataWALA$foodFats, dataWALA$foodFats==98, NA)
dataWALA$foodFats<-replace(dataWALA$foodFats, dataWALA$foodFats=='', NA)
dataWALA$foodFats<-replace(dataWALA$foodFats, dataWALA$foodFats=='yes', TRUE)
dataWALA$foodFats<-replace(dataWALA$foodFats, dataWALA$foodFats=='no', FALSE)
unique(dataWALA$foodFats)
dataWALA$foodFats <- sapply(dataWALA$foodFats, as.logical)
str(dataWALA$foodFats)

    #If they consumed sugar yesterday
unique(dataWALA$foodSugar)
dataWALA$foodSugar<-replace(dataWALA$foodSugar, dataWALA$foodSugar==98, NA)
dataWALA$foodSugar<-replace(dataWALA$foodSugar, dataWALA$foodSugar=='', NA)
dataWALA$foodSugar<-replace(dataWALA$foodSugar, dataWALA$foodSugar=='yes', TRUE)
dataWALA$foodSugar<-replace(dataWALA$foodSugar, dataWALA$foodSugar=='no', FALSE)
unique(dataWALA$foodSugar)
dataWALA$foodSugar <- sapply(dataWALA$foodSugar, as.logical)
str(dataWALA$foodSugar)

    #If they consumed condiments yesterday
unique(dataWALA$foodCondiment)
dataWALA$foodCondiment<-replace(dataWALA$foodCondiment, dataWALA$foodCondiment==98, NA)
dataWALA$foodCondiment<-replace(dataWALA$foodCondiment, dataWALA$foodCondiment=='', NA)
dataWALA$foodCondiment<-replace(dataWALA$foodCondiment, dataWALA$foodCondiment=='yes', TRUE)
dataWALA$foodCondiment<-replace(dataWALA$foodCondiment, dataWALA$foodCondiment=='no', FALSE)
unique(dataWALA$foodCondiment)
dataWALA$foodCondiment <- sapply(dataWALA$foodCondiment, as.logical)
str(dataWALA$foodCondiment)

    #If they consumed insects yesterday
unique(dataWALA$foodInsect)
dataWALA$foodInsect<-replace(dataWALA$foodInsect, dataWALA$foodInsect==98, NA)
dataWALA$foodInsect<-replace(dataWALA$foodInsect, dataWALA$foodInsect=='', NA)
dataWALA$foodInsect<-replace(dataWALA$foodInsect, dataWALA$foodInsect=='yes', TRUE)
dataWALA$foodInsect<-replace(dataWALA$foodInsect, dataWALA$foodInsect=='no', FALSE)
unique(dataWALA$foodInsect)
dataWALA$foodInsect <- sapply(dataWALA$foodInsect, as.logical)
str(dataWALA$foodInsect)

    #If they consumed red palm yesterday
unique(dataWALA$foodRedPalm)
dataWALA$foodRedPalm<-replace(dataWALA$foodRedPalm, dataWALA$foodRedPalm==98, NA)
dataWALA$foodRedPalm<-replace(dataWALA$foodRedPalm, dataWALA$foodRedPalm=='', NA)
dataWALA$foodRedPalm<-replace(dataWALA$foodRedPalm, dataWALA$foodRedPalm=='yes', TRUE)
dataWALA$foodRedPalm<-replace(dataWALA$foodRedPalm, dataWALA$foodRedPalm=='no', FALSE)
unique(dataWALA$foodRedPalm)
dataWALA$foodRedPalm <- sapply(dataWALA$foodRedPalm, as.logical)
str(dataWALA$foodRedPalm)

    #If they consumed any other food yesterday
unique(dataWALA$foodOther)
dataWALA$foodOther<-replace(dataWALA$foodOther, dataWALA$foodOther==98, NA)
dataWALA$foodOther<-replace(dataWALA$foodOther, dataWALA$foodOther=='', NA)
dataWALA$foodOther<-replace(dataWALA$foodOther, dataWALA$foodOther=='yes', TRUE)
dataWALA$foodOther<-replace(dataWALA$foodOther, dataWALA$foodOther=='no', FALSE)
unique(dataWALA$foodOther)
dataWALA$foodOther <- sapply(dataWALA$foodOther, as.logical)
str(dataWALA$foodOther)

    #If they consumed salt yesterday
unique(dataWALA$foodSalt)
dataWALA$foodSalt<-replace(dataWALA$foodSalt, dataWALA$foodSalt==98, NA)
dataWALA$foodSalt<-replace(dataWALA$foodSalt, dataWALA$foodSalt=='', NA)
dataWALA$foodSalt<-replace(dataWALA$foodSalt, dataWALA$foodSalt=='yes', TRUE)
dataWALA$foodSalt<-replace(dataWALA$foodSalt, dataWALA$foodSalt=='no', FALSE)
unique(dataWALA$foodSalt)
dataWALA$foodSalt <- sapply(dataWALA$foodSalt, as.logical)
str(dataWALA$foodSalt)


#Remove duplicate or unneeded variables
    #Includes mother/caregiver's line number, if the respondent is 
    #the biological mother (x2), why they wanted to get rid of first milk (very few responses).
    #copy of child's age will be removed later, after cleaning age column
dataWALA = subset(dataWALA, select = -c(refMomLine, confirmBioMom1, ridFirstMilk, confirmBioMom2))

    #source of birthday year, what month the child received anything other than breast milk
dataWALA = subset(dataWALA, select = -c(birthYearSource, otherMonth))
summary(dataWALA)




#DATA CLEANING: initial review ----

#Standardizing responses
unique(dataWALA$birthYear)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$birthYear==16, 2016)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$birthYear==15, 2015)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$birthYear==14, 2014)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$birthYear==13, 2013)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$birthYear==17, 2017)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$birthYear==18, 2018)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$birthYear==98, NA)

    #checking age in months (29 months or 2.4 years) to confirm birth year is 2016 and not 2006
dataWALA[dataWALA$birthYear==206,]
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$birthYear==206, 2016)
unique(dataWALA$birthYear)

    #same with 2004 and 2008
dataWALA[dataWALA$birthYear == 2004,]
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$birthYear==2004, 2014)
dataWALA[dataWALA$birthYear == 2008,]
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$birthYear==2008, 2018)
unique(dataWALA$birthYear)


#combining related/overlapping variables
    #weigh ins at Ministry of Health or CMAM facility GMP sessions
summary(dataWALA$weighInMoH)
summary(dataWALA$weighInCHAM)
dataWALA$weighIn <- dataWALA$weighInMoH
summary(dataWALA$weighIn)
    #Questionare asked if the child was weighed at a CHAM clinic if they were not weighed at a Ministry of 
    #Health clinic. So if they were weighed at a CMAM clinic, overall weigh in variable should be true
dataWALA[dataWALA$weighInMoH %in% FALSE & dataWALA$weighInCHAM %in% TRUE,]$weighIn <- TRUE
summary(dataWALA$weighIn)
    #Logic: only 8 instances where weighInMoH was FALSE and weighInCHAM was NA, assuming these
    #are errors in survey completion/data entry and will treat them overall as
    #no weigh in (FALSE). So can take the value of weighInMoH, and only change it if
    #weighInCHAM is TRUE
dataWALA = subset(dataWALA, select = -c(weighInMoH, weighInCHAM))


    #Breastfed and fed breast milk in other ways
summary(dataWALA$wasBreastfed)
summary(dataWALA$hadBreastmilk)

dataWALA$drankBreastmilk <- dataWALA$wasBreastfed
summary(dataWALA$drankBreastmilk)

dataWALA[dataWALA$wasBreastfed %in% FALSE & dataWALA$hadBreastmilk %in% TRUE,]$drankBreastmilk <- TRUE
summary(dataWALA$drankBreastmilk)
    #Logic: same as weigh ins, only change where first is false and second is true

dataWALA = subset(dataWALA, select = -c(wasBreastfed, hadBreastmilk))

    #if they had a bed net and if the child slept under it
summary(dataWALA$houseBedNet)
summary(dataWALA$childBedNet)

dataWALA$bedNet <- dataWALA$childBedNet
summary(dataWALA$bedNet)

dataWALA$bedNet<-replace(dataWALA$bedNet, dataWALA$houseBedNet==FALSE, FALSE)
    #Logic: if the child was reported as sleeping under a bed net last night OR NA, then
    #those are the answers. However, if the family does not have a bed net, then the only
    #possible answer about if the child slept under a bed net is FALSE
str(dataWALA$bedNet)
summary(dataWALA$bedNet)
dataWALA = subset(dataWALA, select = -c(houseBedNet, childBedNet))

    #Height and length
    #Create column with record of if the child was measured laying down (under 24 months) or
    #standing up (24+ months) as this is adjusted for in the Anthro Analyzer
`%notin%` <- Negate(`%in%`)
dataWALA$howMeasure = NA
dataWALA[dataWALA$length %notin% 0 & dataWALA$length %notin% NA & dataWALA$length %notin% '',]$howMeasure <- 'L'
dataWALA[dataWALA$height %notin% 0 & dataWALA$height %notin% NA & dataWALA$height %notin% '',]$howMeasure <- 'H'
table(dataWALA$howMeasure)
sum(is.na(dataWALA$howMeasure))
measureNA <- subset(dataWALA, howMeasure %in% NA)
    #confirming that any instance of NA were for rows where height/length was not taken, and not that there was
    #an error in the code
    
    #From a review of the raw data, two 24 month old children were measured laying down (length) instead of standing
    #up (height). Since the Anthro Analyzer considers and adjusts for the difference in height when laying down and 
    #standing, I will assume that the data was placed in the correct column and the children were measured laying down.
    #It is likely there was a misunderstanding about when children should begin being measured standing up (>24 months 
    #compared to >= 24 months)
    
    #Now to combine height and length column into one
dataWALA$comboHtLgth <- coalesce(dataWALA$length,dataWALA$height)
summary(dataWALA$length)
summary(dataWALA$height)
summary(dataWALA$comboHtLgth)

dataWALA = subset(dataWALA, select = -c(length, height))
summary(dataWALA)

#Standardizing categorical variables
    #Checking remaining character/categorical data to ensure inputted values/categories are consistent
    #Then correcting any inconsistencies found above
    #Any place where response is "other specify" changed to just "other", as the follow up question was removed
unique(dataWALA$districtName)
unique(dataWALA$bioMom)

    #updating child sex to format usable by WHO anthro package
unique(dataWALA$childSex)
dataWALA$childSex<-replace(dataWALA$childSex, dataWALA$childSex=='male', 'M')
dataWALA$childSex<-replace(dataWALA$childSex, dataWALA$childSex=='female', 'F')
unique(dataWALA$childSex)

    #Same with oedema
unique(dataWALA$oedema)
dataWALA$oedema<-replace(dataWALA$oedema, dataWALA$oedema=='false', 'N')
dataWALA$oedema<-replace(dataWALA$oedema, dataWALA$oedema=='true', 'Y')
dataWALA$oedema<-replace(dataWALA$oedema, dataWALA$oedema=='', NA)
unique(dataWALA$oedema)

    #who assisted with child's birth
unique(dataWALA$birthAssist1)
dataWALA$birthAssist1<-replace(dataWALA$birthAssist1, dataWALA$birthAssist1=='', NA)
dataWALA$birthAssist1<-replace(dataWALA$birthAssist1, dataWALA$birthAssist1=='other specify', 'other')
unique(dataWALA$birthAssist1)

unique(dataWALA$birthAssist2)
dataWALA$birthAssist2<-replace(dataWALA$birthAssist2, dataWALA$birthAssist2==95, NA)
dataWALA$birthAssist2<-replace(dataWALA$birthAssist2, dataWALA$birthAssist2=='', NA)
dataWALA$birthAssist2<-replace(dataWALA$birthAssist2, dataWALA$birthAssist2=='other specify', 'other')
unique(dataWALA$birthAssist2)

unique(dataWALA$birthAssist3)
unique(dataWALA$birthAssist4)

    #birthAssist3 and birthAssist4 have no other answers (95 and NA), so can be removed
dataWALA = subset(dataWALA, select = -c(birthAssist3, birthAssist4))

    #where the birth took place
unique(dataWALA$birthLoc)
dataWALA$birthLoc<-replace(dataWALA$birthLoc, dataWALA$birthLoc=='', NA)
dataWALA$birthLoc<-replace(dataWALA$birthLoc, dataWALA$birthLoc=='other specify', 'other')
unique(dataWALA$birthLoc)

    #Vitamin A shot location
unique(dataWALA$vitAWhere)
dataWALA$vitAWhere<-replace(dataWALA$vitAWhere, dataWALA$vitAWhere=='', NA)
dataWALA$vitAWhere<-replace(dataWALA$vitAWhere, dataWALA$vitAWhere=='other (specify)', 'other')
unique(dataWALA$vitAWhere)

    #disposing of faeces
unique(dataWALA$disposeFaeces)
dataWALA$disposeFaeces<-replace(dataWALA$disposeFaeces, dataWALA$disposeFaeces=='', NA)
dataWALA$disposeFaeces<-replace(dataWALA$disposeFaeces, dataWALA$disposeFaeces=='other (specify)', 'other')
unique(dataWALA$disposeFaeces)

    #disposing of waste water, same logic as vitamin A shot above
unique(dataWALA$disposeWater)
dataWALA$disposeWater<-replace(dataWALA$disposeWater, dataWALA$disposeWater=='', NA)
dataWALA$disposeWater<-replace(dataWALA$disposeWater, dataWALA$disposeWater=='other specify', 'other')
unique(dataWALA$disposeWater)

    #wash hands, same logic as vitamin A shot above
unique(dataWALA$whenWashHand1)
dataWALA$whenWashHand1<-replace(dataWALA$whenWashHand1, dataWALA$whenWashHand1=='', NA)
dataWALA$whenWashHand1<-replace(dataWALA$whenWashHand1, dataWALA$whenWashHand1=='other (specify)', 'other')
dataWALA$whenWashHand1<-replace(dataWALA$whenWashHand1, dataWALA$whenWashHand1==97, 'never')
unique(dataWALA$whenWashHand1)

unique(dataWALA$whenWashHand2)
dataWALA$whenWashHand2<-replace(dataWALA$whenWashHand2, dataWALA$whenWashHand2=='', NA)
dataWALA$whenWashHand2<-replace(dataWALA$whenWashHand2, dataWALA$whenWashHand2=='other (specify)', 'other')
dataWALA$whenWashHand2<-replace(dataWALA$whenWashHand2, dataWALA$whenWashHand2==95, NA)
unique(dataWALA$whenWashHand2)

unique(dataWALA$whenWashHand3)
dataWALA$whenWashHand3<-replace(dataWALA$whenWashHand3, dataWALA$whenWashHand3=='', NA)
dataWALA$whenWashHand3<-replace(dataWALA$whenWashHand3, dataWALA$whenWashHand3=='other (specify)', 'other')
dataWALA$whenWashHand3<-replace(dataWALA$whenWashHand3, dataWALA$whenWashHand3==95, NA)
unique(dataWALA$whenWashHand3)

unique(dataWALA$whenWashHand4)
dataWALA$whenWashHand4<-replace(dataWALA$whenWashHand4, dataWALA$whenWashHand4=='', NA)
dataWALA$whenWashHand4<-replace(dataWALA$whenWashHand4, dataWALA$whenWashHand4=='other (specify)', 'other')
dataWALA$whenWashHand4<-replace(dataWALA$whenWashHand4, dataWALA$whenWashHand4==95, NA)
unique(dataWALA$whenWashHand4)

    #If they have a health passport or GMP card
unique(dataWALA$childDoc)
dataWALA$childDoc<-replace(dataWALA$childDoc, dataWALA$childDoc=='', NA)
unique(dataWALA$childDoc)

    #The trend in weight
unique(dataWALA$childWtTrend)
dataWALA$childWtTrend<-replace(dataWALA$childWtTrend, dataWALA$childWtTrend=='', NA)
unique(dataWALA$childWtTrend)

    #how long after birth breastfeeding started
unique(dataWALA$startBreastfeed)
dataWALA$startBreastfeed<-replace(dataWALA$startBreastfeed, dataWALA$startBreastfeed=='', NA)
dataWALA$startBreastfeed<-replace(dataWALA$startBreastfeed, dataWALA$startBreastfeed=='dk', NA)
unique(dataWALA$startBreastfeed)

    #Why the wait to breastfeed
unique(dataWALA$waitBreastfeed)
dataWALA$waitBreastfeed<-replace(dataWALA$waitBreastfeed, dataWALA$waitBreastfeed=='', NA)
dataWALA$waitBreastfeed<-replace(dataWALA$waitBreastfeed, dataWALA$waitBreastfeed=='other specify', 'other')
dataWALA$waitBreastfeed<-replace(dataWALA$waitBreastfeed, dataWALA$waitBreastfeed==98, NA)
unique(dataWALA$waitBreastfeed)

    #results of measurements
unique(dataWALA$resultsMeasure)
dataWALA$resultsMeasure<-replace(dataWALA$resultsMeasure, dataWALA$resultsMeasure=='', NA)
dataWALA$resultsMeasure<-replace(dataWALA$resultsMeasure, dataWALA$resultsMeasure=='other (specify)', 'other')
unique(dataWALA$resultsMeasure)





#DATA CLEANING: numeric and integer variables ----

    #age in months
table(dataWALA$ageMonth)
    #assume 86 is data entry error and not age (would be over 7 years old and outside of survey age)
    #when duplicate age column checked (confirmAgeMonth), can see there are different answers, 
    #so error in ageMonth. Replace values in ageMonth with confirmAgeMonth, then will clean
subset(dataWALA, ageMonth == 86)
    #correcting ageMonth based on data from confirmAgeMonth
dataWALA$ageMonth<-replace(dataWALA$ageMonth, dataWALA$childCode== 2 & dataWALA$childQNum==2760 & dataWALA$ageMonth == 86, 9)
dataWALA$ageMonth<-replace(dataWALA$ageMonth, dataWALA$childCode== 2 & dataWALA$childQNum==1982 & dataWALA$ageMonth == 86, NA)
dataWALA$ageMonth<-replace(dataWALA$ageMonth, dataWALA$childCode== 3 & dataWALA$childQNum==1884 & dataWALA$ageMonth == 86, NA)
dataWALA$ageMonth<-replace(dataWALA$ageMonth, dataWALA$childCode== 17 & dataWALA$childQNum==1733 & dataWALA$ageMonth == 86, 59)
table(dataWALA$ageMonth)

    #then can delete duplicate variable (confirmAgeMonth)
dataWALA = subset(dataWALA, select = -c(confirmAgeMonth))

    #Record of weight (correct decimal places first)
dataWALA$childWtRec1 <- round(dataWALA$childWtRec1, 1)
table(dataWALA$childWtRec1)
    #assume 85 (error as above, would be over 180 lb) and 98 are survey 
    #codes, change to NA
    #based on entries, two 0s should be NA (reported weights in other columns)
dataWALA$childWtRec1<-replace(dataWALA$childWtRec1, dataWALA$childWtRec1==85, NA)
dataWALA$childWtRec1<-replace(dataWALA$childWtRec1, dataWALA$childWtRec1==98, NA)
dataWALA$childWtRec1<-replace(dataWALA$childWtRec1, dataWALA$childWtRec1==98.1, NA)
dataWALA$childWtRec1<-replace(dataWALA$childWtRec1, dataWALA$childWtRec1==98.8, NA)
dataWALA$childWtRec1<-replace(dataWALA$childWtRec1, dataWALA$childWtRec1==0, NA)
table(dataWALA$childWtRec1)

dataWALA$childWtRec2 <- round(dataWALA$childWtRec2, 1)
table(dataWALA$childWtRec2)

    #assume 80+ are survey codes and errors, change to NA. Same with 0s, as above
dataWALA$childWtRec2<-replace(dataWALA$childWtRec2, dataWALA$childWtRec2==80, NA)
dataWALA$childWtRec2<-replace(dataWALA$childWtRec2, dataWALA$childWtRec2==98, NA)
dataWALA$childWtRec2<-replace(dataWALA$childWtRec2, dataWALA$childWtRec2==98.1, NA)
dataWALA$childWtRec2<-replace(dataWALA$childWtRec2, dataWALA$childWtRec2==98.6, NA)
dataWALA$childWtRec2<-replace(dataWALA$childWtRec2, dataWALA$childWtRec2==98.8, NA)
dataWALA$childWtRec2<-replace(dataWALA$childWtRec2, dataWALA$childWtRec2==0, NA)
table(dataWALA$childWtRec2)

dataWALA$childWtRec3 <- round(dataWALA$childWtRec3, 1)
table(dataWALA$childWtRec3)

    #As above, 75+ (would be over 165 lb) are survey codes and errors, 
    #change to NA. Same with 0s, as above
dataWALA$childWtRec3<-replace(dataWALA$childWtRec3, dataWALA$childWtRec3==75, NA)
dataWALA$childWtRec3<-replace(dataWALA$childWtRec3, dataWALA$childWtRec3==95, NA)
dataWALA$childWtRec3<-replace(dataWALA$childWtRec3, dataWALA$childWtRec3==98, NA)
dataWALA$childWtRec3<-replace(dataWALA$childWtRec3, dataWALA$childWtRec3==98.1, NA)
dataWALA$childWtRec3<-replace(dataWALA$childWtRec3, dataWALA$childWtRec3==98.8, NA)
dataWALA$childWtRec3<-replace(dataWALA$childWtRec3, dataWALA$childWtRec3==0, NA)
table(dataWALA$childWtRec3)

    #number of times breastfeeding yesterday
table(dataWALA$breastfedYdayNum)

    #change 0 and 98 (don't know) to NA
dataWALA$breastfedYdayNum<-replace(dataWALA$breastfedYdayNum, dataWALA$breastfedYdayNum==0, NA)
dataWALA$breastfedYdayNum<-replace(dataWALA$breastfedYdayNum, dataWALA$breastfedYdayNum==98, NA)
table(dataWALA$breastfedYdayNum)
    #Logic: while some of the numbers seem high (25-50 times), there are enough instances that
    #it may not be a survey error. Especially if the child is constantly held and has
    #ready access to breastfeeding, it is possible they "snacked" through the day

table(dataWALA$breastfedYday)
    #when if they breastfed yesterday and number of times being breastfed is compared in the data set,
    #can see there are a number of entries where it is listed that they did not breastfeed,
    #but then list a number of times. Take this as an error for breastfedYday (if they were breastfed)
    #and breastfedYdayNum is correct, fixed here
dataWALA[dataWALA$breastfedYdayNum %notin% 0 & dataWALA$breastfedYdayNum %notin% NA & dataWALA$breastfedYdayNum %notin% '',]$breastfedYday <- TRUE
table(dataWALA$breastfedYday)

    #Also done for other variables involving counts of liquids
dataWALA[dataWALA$liquidFormulaNum %notin% 0 & dataWALA$liquidFormulaNum %notin% NA & dataWALA$liquidFormulaNum %notin% '',]$liquidFormula <- TRUE
dataWALA[dataWALA$liquidMilkNum %notin% 0 & dataWALA$liquidMilkNum %notin% NA & dataWALA$liquidMilkNum %notin% '',]$liquidMilk <- TRUE
dataWALA[dataWALA$liquidYogurtNum %notin% 0 & dataWALA$liquidYogurtNum %notin% NA & dataWALA$liquidYogurtNum %notin% '',]$liquidYogurt <- TRUE

    #number of times the child consumed infant formula
table(dataWALA$liquidFormulaNum)
    #22 times appears to be an outlier (only three instances, and much higher than
    #next highest answer of 3), assumed to be an error in entry and changed to 2
    #especially since when investigated, the three children were all also breastfed yesterday
dataWALA$liquidFormulaNum<-replace(dataWALA$liquidFormulaNum, dataWALA$liquidFormulaNum==22, 2)
table(dataWALA$liquidFormulaNum)

    #number of times the child consumed animal milk
table(dataWALA$liquidMilkNum)
    #Same assumption RE 22 as above
dataWALA$liquidMilkNum<-replace(dataWALA$liquidMilkNum, dataWALA$liquidMilkNum==22, 2)
table(dataWALA$liquidMilkNum)

    #number of times the child at food yesterday
table(dataWALA$foodNum)
    #97 is none, change to 0, 98 is don't know, changed to NA
dataWALA$foodNum<-replace(dataWALA$foodNum, dataWALA$foodNum==97, 0)
dataWALA$foodNum<-replace(dataWALA$foodNum, dataWALA$foodNum==98, NA)
table(dataWALA$foodNum)

    #Weigh in during survey (correct decimal places first)
dataWALA$weight <- round(dataWALA$weight, 1)
table(dataWALA$weight)
    #99 is refused, changed to NA
dataWALA$weight<-replace(dataWALA$weight, dataWALA$weight==99, NA)
    #number of errors. 69.4 and 80.3 are the exact height listed so error, changed to NA
dataWALA$weight<-replace(dataWALA$weight, dataWALA$weight==69.4, NA)
dataWALA$weight<-replace(dataWALA$weight, dataWALA$weight==80.3, NA)
    #71.6, 81.5, 88.7 appears to be error with decimal, as adjusted age would track with weights listed
    #in previous months and recorded trends, changed to 7.2, 8.2, 8.9.
dataWALA$weight<-replace(dataWALA$weight, dataWALA$weight==71.6, 7.2)
dataWALA$weight<-replace(dataWALA$weight, dataWALA$weight==81.5, 8.2)
dataWALA$weight<-replace(dataWALA$weight, dataWALA$weight==88.7, 8.9)
    #98.2, when height checked, appears the two were switched (lists 14.5 cm tall),
    #switched two
dataWALA$weight<-replace(dataWALA$weight, dataWALA$weight==98.2, 14.5)
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==14.5, 98.2)
table(dataWALA$weight)

    #height/length during survey (correct decimal places first)
dataWALA$comboHtLgth <- round(dataWALA$comboHtLgth, 1)
table(dataWALA$comboHtLgth)
    #-77 is NA in survey codes, was filled in for length (child under 2) when the child was measured
    #for height (2 and over). Corrected with height from raw data
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==-77 & dataWALA$weight==11.3, 84.2)
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==-77 & dataWALA$weight==10.6, 84)
    #5.4 error, recorded as only measuring on one, removed
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==5.4, NA)
    #0.6, 8.4, 7.9, 9.9, 0.8, 8, 8.3, 9.4, 9.7, 9.8 and one 9.5 appear to be decimal issues, changed to 60, 
    #84, 79, 99, 80, 80, 83, 94, 97, 98, 95 cm
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==0.6, 60)
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==8.4, 84)
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==7.9, 79)
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==9.9, 99)
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==0.8, 80)
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==8, 80)
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==8.3, 83)
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==9.4, 94)
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==9.7, 97)
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==9.8, 98)
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==9.5 & dataWALA$weight==12.9, 95)
    #13.3 error, exact weight listed, removed. Same with one 9.5
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==13.3, NA)
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==9.5 & dataWALA$weight==9.5, NA)
    #17.9, can't determine logic behind it, removed
dataWALA$comboHtLgth<-replace(dataWALA$comboHtLgth, dataWALA$comboHtLgth==17.9, NA)

table(dataWALA$comboHtLgth)


    #Correcting/validating results of measurement column
dataWALA[dataWALA$weight %notin% NA & dataWALA$comboHtLgth %notin% NA,]$resultsMeasure <- 'Measured both ht & wt'
dataWALA[dataWALA$weight %notin% NA & dataWALA$comboHtLgth %in% NA,]$resultsMeasure <- 'Measured only wt'
dataWALA[dataWALA$weight %in% NA & dataWALA$comboHtLgth %notin% NA,]$resultsMeasure <- 'Measured only ht'
    #no changes needed for refused, child not present, and other. These rows will be deleted later as this info
    #is needed to calculate the dependent variable
table(dataWALA$resultsMeasure)

    #correcting/validating hand washing answers
    #"Never" listed in a number of responses where they also listed times they do wash their hands, survey error
    #corrected here
subset(dataWALA, whenWashHand2 == 'never')
    #shifting answers for each over
dataWALA$whenWashHand2<-replace(dataWALA$whenWashHand2, dataWALA$childCode== 3 & dataWALA$childQNum==1367 & dataWALA$whenWashHand2 == 'never', 'after attending to a child who has defecated')
dataWALA$whenWashHand3<-replace(dataWALA$whenWashHand3, dataWALA$childCode== 3 & dataWALA$childQNum==1367 & dataWALA$whenWashHand2 == 'never', NA)

dataWALA$whenWashHand2<-replace(dataWALA$whenWashHand2, dataWALA$childCode== 2 & dataWALA$childQNum==1170 & dataWALA$whenWashHand2 == 'never', NA)

dataWALA$whenWashHand2<-replace(dataWALA$whenWashHand2, dataWALA$childCode== 17 & dataWALA$childQNum==1273 & dataWALA$whenWashHand2 == 'never', 'before eating food')
dataWALA$whenWashHand3<-replace(dataWALA$whenWashHand3, dataWALA$childCode== 17 & dataWALA$childQNum==1273 & dataWALA$whenWashHand2 == 'never', NA)

dataWALA$whenWashHand2<-replace(dataWALA$whenWashHand2, dataWALA$childCode== 17 & dataWALA$childQNum==1105 & dataWALA$whenWashHand2 == 'never', 'before eating food')
dataWALA$whenWashHand3<-replace(dataWALA$whenWashHand3, dataWALA$childCode== 17 & dataWALA$childQNum==1105 & dataWALA$whenWashHand2 == 'never', 'after visiting the toilet')
dataWALA$whenWashHand4<-replace(dataWALA$whenWashHand4, dataWALA$childCode== 17 & dataWALA$childQNum==1105 & dataWALA$whenWashHand2 == 'never', NA)

subset(dataWALA, whenWashHand3 == 'never')
dataWALA$whenWashHand3<-replace(dataWALA$whenWashHand3, dataWALA$childCode== 2 & dataWALA$childQNum==330 & dataWALA$whenWashHand3 == 'never', 'after visiting the toilet')
dataWALA$whenWashHand4<-replace(dataWALA$whenWashHand4, dataWALA$childCode== 2 & dataWALA$childQNum==330 & dataWALA$whenWashHand3 == 'never', NA)

subset(dataWALA, whenWashHand4 == 'never')
dataWALA$whenWashHand4<-replace(dataWALA$whenWashHand4, dataWALA$childCode== 3 & dataWALA$childQNum==1669 & dataWALA$whenWashHand4 == 'never', NA)
dataWALA$whenWashHand4<-replace(dataWALA$whenWashHand4, dataWALA$childCode== 17 & dataWALA$childQNum==1373 & dataWALA$whenWashHand4 == 'never', NA)




#DATA CLEANING - visual inspection ----

    #Plot weight and height by age to visually confirm no outliers or unexpected trends
plot(dataWALA$ageMonth, dataWALA$comboHtLgth, main='Height by age', xlab='Age in months', ylab='Height/length')
    #One visible potential outlier
subset(dataWALA, comboHtLgth>90 & ageMonth<10)
    #unlikely that a 2 month old is 94.5 cm tall. Went back to data, and height was originally recorded (so child is two or over).
    #Error in age in months, and unable to verify age in months. As it is directly related to the dependent variable calculation, 
    #and I have a large amount of data already, will delete the row instead of using an average
dataWALA <- dataWALA[-c(2714),]

plot(dataWALA$ageMonth, dataWALA$weight, main='Weight by age', xlab='Age in months', ylab='Weight')
    #five potential outliers
subset(dataWALA, weight >28 | weight <0.5)
    #weight 31.3, while high, is for an older child. Will leave as is
    #weight 47 is for a 2 month old (validated with birth year), appears to be decimal error. In line with previous weigh ins, change to 4.7
dataWALA$weight<-replace(dataWALA$weight, dataWALA$childCode== 17 & dataWALA$childQNum==732 & dataWALA$weight == 47.0, 4.7)
subset(dataWALA, weight <5 & ageMonth>40)
    #Older children, both appear to be a decimal issue with weight. However, moving decimal one place sets weight suspiciously high
    #Will remove both
dataWALA <- dataWALA[-c(914),]
dataWALA <- dataWALA[-c(1112),]


    #Plot age by year of birth
plot(dataWALA$birthYear, dataWALA$ageMonth, main='Age in months by birth year', xlab='Birth year', ylab='Age in months')
    #A number of outliers apparent, explore
    #While the raw data provides information on where birth years were collected from (parent/guardian, passport, etc), I have seen literature that these
    #documents are not always a reliable source of information. They can get lost, information recorded incorrectly, not brought to appointments. It
    #is possible that passports that were lost and replacement had the wrong birth year recorded

subset(dataWALA, birthYear == 2013 & ageMonth<50)
    #45 months old, height and weight are in line with other children their age, and month age is correctly listed in the two duplicate columns. 
    #assumption is that the birth year was incorrectly, listed as 2013 instead of 2014
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$childCode== 17 & dataWALA$childQNum==1100 & dataWALA$birthYear == 2013, 2014)

subset(dataWALA, birthYear == 2014 & ageMonth<40)
    #For the 35 and 30 month old, height and weight are in line with children their age. Assume error in birth year, corrected to 2015
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$childCode== 17 & dataWALA$childQNum==337 & dataWALA$birthYear == 2014, 2015)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$childCode== 17 & dataWALA$childQNum==985 & dataWALA$birthYear == 2014, 2015)

subset(dataWALA, birthYear == 2015 & ageMonth<30)
    #at 28 months (2.3 years), could have been born in Dec 2015 and would match age in months in early 2018
    #Since I don't know when the interviews were taken in 2018, leave as is

subset(dataWALA, birthYear == 2016 & ageMonth>38)
    #For both 40 and 48 month old, height and weight are in line with other children their age. 48 month old is very small in comparison,
    #however not so dramatically that I can confidently say they should be removed. Both years corrected to 2014
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$childCode== 17 & dataWALA$childQNum==556 & dataWALA$birthYear == 2016, 2014)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$childCode== 16 & dataWALA$childQNum==414 & dataWALA$birthYear == 2016, 2014)

subset(dataWALA, birthYear == 2017 & ageMonth<5)
    #checking the raw data, the age was later listed as 20 months. Corrected here
dataWALA$ageMonth<-replace(dataWALA$ageMonth, dataWALA$childCode== 17 & dataWALA$childQNum==934 & dataWALA$birthYear == 2017, 20)

subset(dataWALA, birthYear == 2017 & ageMonth>28)
    #33 month old, height is in line with other 33 month olds. Weight is low, but presence of oedema, so is automatically considered wasting Two 
    #34 month olds and 33 month old, height and weight are in line with other children of that age. Correct all here
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$childCode== 17 & dataWALA$childQNum==1237 & dataWALA$birthYear == 2017, 2015)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$childCode== 17 & dataWALA$childQNum==2011 & dataWALA$birthYear == 2017, 2015)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$childCode== 17 & dataWALA$childQNum==2645 & dataWALA$birthYear == 2017, 2015)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$childCode== 16 & dataWALA$childQNum==2673 & dataWALA$birthYear == 2017, 2016)

subset(dataWALA, birthYear == 2018 & ageMonth>15)
    #18, 20, and 23 month old, height and weight appears on track for age, will leave as is. Corrected to 2016 here
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$childCode== 3 & dataWALA$childQNum==2263 & dataWALA$birthYear == 2018, 2016)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$childCode== 17 & dataWALA$childQNum==2433 & dataWALA$birthYear == 2018, 2016)
dataWALA$birthYear<-replace(dataWALA$birthYear, dataWALA$childCode== 16 & dataWALA$childQNum==1281 & dataWALA$birthYear == 2018, 2016)

    #41 month old, age in months incorrectly listed and was actually 4 (from duplicate row in raw data), corrected here
dataWALA$ageMonth<-replace(dataWALA$ageMonth, dataWALA$childCode== 17 & dataWALA$childQNum==2303 & dataWALA$birthYear == 2018, 4)

    #Recoded ifInfant now that ageMonth is corrected
dataWALA$ifInfant<-replace(dataWALA$ifInfant, dataWALA$ageMonth<= 11, TRUE)
dataWALA$ifInfant<-replace(dataWALA$ifInfant, dataWALA$ageMonth>= 12, FALSE)
summary(dataWALA$ifInfant)
dataWALA[dataWALA$ifInfant %in% NA, ]
    #Row with NA for ifInfant is also missing the majority of the other data, will remove
dataWALA <- dataWALA[-c(355),]




#DATA CLEANING: Managing NA's ----

    #Checking rows with high NA counts
sum(rowSums(is.na(dataWALA))>20)
    #At this time, 168 rows have NA for over 20 variables. Will explore this again after removing variables with high NA counts

    #Explore proportion of NA by variable
apply(X = is.na(dataWALA), MARGIN = 2, FUN = sum)

    #Variables that should be removed because of high NA counts include who assisted with the birth 
    #(birthAssist1 and birthAssist2), where the birth took place (birthLoc), if the mother received
    #a vitamin A shot and from where (momVitA and vitAWhere), where waste water was disposed
    #of (disposeWater), record of weights from cards (childWtRec1 through childWtRec3), weight trend (childWtTrend) and
    #why the mother waited to breastfeed (waitBreastfeed)
    
    #should convert hand washing practices (whenWashHand1 through whenWashHand4) to a different structure 
    #such as separating out options into individual logical variables
    
    #should explore how to manage breastfedYday (if they breastfed yesterday) and breastfedYdayNum 
    #(how many times they breastfed). Only applicable to those who are still breastfeeding, so high NA.
    #But may need information in subset of data
    #Same with liquidFormulaNum (number of times breastfed), liquidMilkNum (number of times drinking 
    #animal milk), liquidYogurtNum (number of times consuming yogurt).
    #But will leave as is for exploratory data analysis

    #removed columns
dataWALA = subset(dataWALA, select = -c(birthAssist1, birthAssist2, birthLoc, momVitA, vitAWhere, disposeWater, 
                                        childWtRec1, childWtRec2, childWtRec3, childWtTrend, waitBreastfeed))

    #converting hand washing practices into more functional form
dataWALA$neverHW<-replace(dataWALA$neverHW, dataWALA$whenWashHand1== "never", TRUE)
dataWALA$neverHW<-replace(dataWALA$neverHW, dataWALA$neverHW %in% NA, FALSE)

    #cleaned data earlier, any reference to never is only in the first column
dataWALA$bfFoodPrepHW<-replace(dataWALA$bfFoodPrepHW, dataWALA$whenWashHand1== "before food preparation" | dataWALA$whenWashHand2 == "before food preparation" | dataWALA$whenWashHand3 == "before food preparation" | dataWALA$whenWashHand4== "before food preparation", TRUE)
dataWALA$bfFoodPrepHW<-replace(dataWALA$bfFoodPrepHW, dataWALA$bfFoodPrepHW %in% NA, FALSE)

dataWALA$aftFoodPrepHW<-replace(dataWALA$aftFoodPrepHW, dataWALA$whenWashHand1== "after food preparation" | dataWALA$whenWashHand2 == "after food preparation" | dataWALA$whenWashHand3 == "after food preparation" | dataWALA$whenWashHand4== "after food preparation", TRUE)
dataWALA$aftFoodPrepHW<-replace(dataWALA$aftFoodPrepHW, dataWALA$aftFoodPrepHW %in% NA, FALSE)

dataWALA$bfFeedChildHW<-replace(dataWALA$bfFeedChildHW, dataWALA$whenWashHand1== "before feeding the child" | dataWALA$whenWashHand2 == "before feeding the child" | dataWALA$whenWashHand3 == "before feeding the child" | dataWALA$whenWashHand4== "before feeding the child", TRUE)
dataWALA$bfFeedChildHW<-replace(dataWALA$bfFeedChildHW, dataWALA$bfFeedChildHW %in% NA, FALSE)

dataWALA$aftFeedChildHW<-replace(dataWALA$aftFeedChildHW, dataWALA$whenWashHand1== "after feeding the child" | dataWALA$whenWashHand2 == "after feeding the child" | dataWALA$whenWashHand3 == "after feeding the child" | dataWALA$whenWashHand4== "after feeding the child", TRUE)
dataWALA$aftFeedChildHW<-replace(dataWALA$aftFeedChildHW, dataWALA$aftFeedChildHW %in% NA, FALSE)

dataWALA$bfCleanChildHW<-replace(dataWALA$bfCleanChildHW, dataWALA$whenWashHand1== "before cleaning children" | dataWALA$whenWashHand2 == "before cleaning children" | dataWALA$whenWashHand3 == "before cleaning children" | dataWALA$whenWashHand4== "before cleaning children", TRUE)
dataWALA$bfCleanChildHW<-replace(dataWALA$bfCleanChildHW, dataWALA$bfCleanChildHW %in% NA, FALSE)

dataWALA$aftToiletHW<-replace(dataWALA$aftToiletHW, dataWALA$whenWashHand1== "after visiting the toilet" | dataWALA$whenWashHand2 == "after visiting the toilet" | dataWALA$whenWashHand3 == "after visiting the toilet" | dataWALA$whenWashHand4== "after visiting the toilet", TRUE)
dataWALA$aftToiletHW<-replace(dataWALA$aftToiletHW, dataWALA$aftToiletHW %in% NA, FALSE)

dataWALA$aftChildDefHW<-replace(dataWALA$aftChildDefHW, dataWALA$whenWashHand1== "after attending to a child who has defecated" | dataWALA$whenWashHand2 == "after attending to a child who has defecated" | dataWALA$whenWashHand3 == "after attending to a child who has defecated" | dataWALA$whenWashHand4== "after attending to a child who has defecated", TRUE)
dataWALA$aftChildDefHW<-replace(dataWALA$aftChildDefHW, dataWALA$aftChildDefHW %in% NA, FALSE)

dataWALA$bfFoodHW<-replace(dataWALA$bfFoodHW, dataWALA$whenWashHand1== "before eating food" | dataWALA$whenWashHand2 == "before eating food" | dataWALA$whenWashHand3 == "before eating food" | dataWALA$whenWashHand4== "before eating food", TRUE)
dataWALA$bfFoodHW<-replace(dataWALA$bfFoodHW, dataWALA$bfFoodHW %in% NA, FALSE)

dataWALA$aftFoodHW<-replace(dataWALA$aftFoodHW, dataWALA$whenWashHand1== "after eating food" | dataWALA$whenWashHand2 == "after eating food" | dataWALA$whenWashHand3 == "after eating food" | dataWALA$whenWashHand4== "after eating food", TRUE)
dataWALA$aftFoodHW<-replace(dataWALA$aftFoodHW, dataWALA$aftFoodHW %in% NA, FALSE)

dataWALA$otherHW<-replace(dataWALA$otherHW, dataWALA$whenWashHand1== "other" | dataWALA$whenWashHand2 == "other" | dataWALA$whenWashHand3 == "other" | dataWALA$whenWashHand4== "other", TRUE)
dataWALA$otherHW<-replace(dataWALA$otherHW, dataWALA$otherHW %in% NA, FALSE)

    #to confirm everything converted properly
freqDataHWOld <- apply(dataWALA[,8:11], 2, table)
freqDataHWOld

freqDataHWNew <- apply(dataWALA[,65:75], 2, table)
freqDataHWNew  

    #remove original columns
dataWALA = subset(dataWALA, select = -c(whenWashHand1, whenWashHand2, whenWashHand3, whenWashHand4))




#DATA CLEANING: Row management ----

    #Checking for duplicate rows
dupeRows <- dataWALA[duplicated(dataWALA),]
dupeRows
    #there are no duplicate rows in the database

dataWALAAnon <- subset(dataWALA, select=c(childCode,childQNum, Treatment))
dupeRowsAnon <- dataWALAAnon[duplicated(dataWALAAnon),]
count(dupeRowsAnon)
dupeRowsAnonSort <- dupeRowsAnon[order(dupeRowsAnon$childQNum),]
dupeRowsAnonSort
    #While there are no duplicate rows in the database, there are duplicate anon code combinations
    #Will have to be careful making edits to the data using these specifics (went back in code above and corrected), 
    #and if any future research involves joining databases, this will have to be managed, as these columns are what 
    #the data providers suggest using to join (information has been passed on to data manager already)


    #Removing rows where dependent variable cannot be calculated
    #I will be using the weight/height and age in months to calculate if the child is malnourished for their age
    #therefore, any age in months or (weight OR height) with an NA is not usable information, to be deleted
    
    #age
sum(is.na(dataWALA$ageMonth))
dataWALA = dataWALA[!(is.na(dataWALA$ageMonth)),]
sum(is.na(dataWALA$ageMonth))

    #any entries that are missing both weight and height
sum(is.na(dataWALA$weight))
sum(is.na(dataWALA$comboHtLgth))

dataWALA = dataWALA[!(dataWALA$weight %in% NA & dataWALA$comboHtLgth %in% NA),]
dataWALA[which(dataWALA$weight %in% NA & dataWALA$comboHtLgth %in% NA),]


unique(dataWALA$resultsMeasure)
    #to confirm that any answer such as refused measurements or child wasn't there have also been removed
    #(checks this portion of code, and confirms cleaning in other steps was effective)
    
    #Rechecking rows with high NA counts
sum(rowSums(is.na(dataWALA))>20)
    #Earlier in cleaning there were a large number of rows with high (>20) NA counts, but previous cleaning steps were effective 
    #and the number has dropped to only one
dataHighNA = dataWALA[rowSums(is.na(dataWALA))>20,]
dataHighNA
    #The majority of the survey was not completed, delete this row
dataWALA <- dataWALA[-c(822),]

    #Reordering columns (personal preference)
dataWALA <- dataWALA[, c(1:7, 62:72, 59, 57, 8:10, 58, 11:51, 61, 60, 52:56)]
head(dataWALA)

#EXPORTING DATAFRAME FOR ANALYSIS/DEPENDENT VARIABLE CREATION THROUGH WHO'S ANTHRO PACKAGE
write.csv(dataWALA, "C:/Users/leahg/Documents/CIND820/WALAclean.csv", row.names=TRUE)
