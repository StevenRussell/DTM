
setwd('C:/DTM/All Nigeria Site Assessment datasets/All Nigeria Site Assessment datasets')

library(dplyr)
library(Amelia)
library(gee)
library(ICC)

# Reading in CSV files
temp = list.files(pattern="*.csv")
for (i in 1:9) assign(substr(temp[i],1,3), read.csv(temp[i]))
for (i in 10:16) assign(substr(temp[i],1,2), read.csv(temp[i]))

# Standardizing columns in each dataset
R4 <- R4[,c(1:76)]
R4$STATUS <- NA
R4 <- R4[,c(1:6, 77, 7:20, 22:76)]

R5 <- R5[,c(1:76)]
R12$SURVEY.ROUND <- 12
R12 <- R12[,c(76, 1:75)]

R14 <- R14[,c(1:76)]

# Getting rid of blank rows
R5 <- R5[c(1:59),]
R6 <- R6[c(1:76),]
R9 <- R9[c(1:97),]

# Fixing R15
  # Getting rid of extra columns
  R15 <- R15[,-c(4, 6, 79:81)]

  # Reordering Lat, Long
  R15 <- R15[,c(1:7, 9, 8, 10:76)]
  
  # Changing variable names
  names(R15) <- names(R14)

# Fixing R16
  
  # Getting rid of extra columns
  R16 <- R16[,-c(4, 6, 79:81)]
  
  # Reordering Lat, Long
  R16 <- R16[,c(1:7, 9, 8, 10:76)]
  
  # Changing variable names
  names(R16) <- names(R14)
  
#Converting all variables to character
R4[] <- lapply(R4, as.character)
R5[] <- lapply(R5, as.character)
R6[] <- lapply(R6, as.character)
R7[] <- lapply(R7, as.character)
R8[] <- lapply(R8, as.character)
R9[] <- lapply(R9, as.character)
R10[] <- lapply(R10, as.character)
R11[] <- lapply(R11, as.character)
R12[] <- lapply(R12, as.character)
R13[] <- lapply(R13, as.character)
R14[] <- lapply(R14, as.character)
R15[] <- lapply(R15, as.character)
R16[] <- lapply(R16, as.character)

# Combinging the datasets
df <- rbind(R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16)

# Creating a missingness map
#missmap(df, main = "Missing values vs observed")

# Fixing the roman numeral
df$SURVEY.ROUND[df$SURVEY.ROUND == "IV"] <- "4"

# Converting all character variables to upper case
character.vars <- sapply(df, is.character)
df[character.vars] <- lapply(df[character.vars], toupper)

# Cleaning data

clean_data <- function(x) {
  
  x[x == 'NIL'] <- 'NO'
  x[x == 'UNKNOWN'] <- NA
  x[x == ""] <- NA
  x[x == "< 25 %"] <- '<25%'
  x[x == "< 50 %"] <- '<50%'
  x[x == "< 75 %"] <- '<75%'
  x[x == "> 75 %"] <- '>75%'
  x[x == "< 25%"] <- '<25%'
  x[x == "< 50%"] <- '<50%'
  x[x == "< 75%"] <- '<75%'
  x[x == "> 75%"] <- '>75%'
  x
}

df[] <- lapply(df, clean_data)

# Recoding SMA variable

df$SMA.TYPE[df$SMA.TYPE == "INDIVIDUAL PRIVATE"] <- "INDIVIDUAL/PRIVATE"
df$SMA.TYPE[df$SMA.TYPE %in% c("LOCAL NGO", "INGO")] <- "NGO"
df$SMA.TYPE[df$SMA.TYPE %in% c("NO", "NONE", "NULL", "N/A")] <- "NONE"
df$SMA.TYPE[is.na(df$SMA.TYPE) == T] <- "NOT ANSWERED"

# Recoding hand washing

df$HAND.WASHING.STATIONS[df$HAND.WASHING.STATIONS == "YES NO SOAP WATER"] <- "YES BUT NO SOAP/WATER INSIDE"
df$HAND.WASHING.STATIONS[df$HAND.WASHING.STATIONS == "NONE"] <- "NO"

# Recoding Latrine condition

df$LATRINE.CONDITION[df$LATRINE.CONDITION == "GOOD"] <- "GOOD (HYGIENIC)"
df$LATRINE.CONDITION[df$LATRINE.CONDITION == "NO"] <- "NOT SO GOOD (NOT HYGIENIC)"
df$LATRINE.CONDITION[df$LATRINE.CONDITION == "NOT SO GOOD"] <- "NOT SO GOOD (NOT HYGIENIC)"

# Recoding Access to food

df$ACCESS.TO.FOOD[df$ACCESS.TO.FOOD == "YES OFFSITE"] <- "YES, OFF SITE"
df$ACCESS.TO.FOOD[df$ACCESS.TO.FOOD == "YES ONSITE"] <- "YES, ON SITE"

# Recoding INDOORS, TENTS, MAKESHIFT, NO.SHELTER

df$INDOORS[df$INDOORS == "NO"] <- "NONE"
df$TENTS[df$TENTS == "NO"] <- "NONE"
df$NO.SHELTER[df$NO.SHELTER == "NO"] <- "NONE"
df$MAKESHIFT[df$MAKESHIFT == "NO"] <- "NONE"

# Cleaning SITE.ID

R4$SITE.ID <- gsub(" ", "_", R4$SITE.ID)

# Creating STATE.RISK variable (risk based on states)

df$STATE.CASES[df$STATE == "ADAMAWA"] <- 6380
df$STATE.CASES[df$STATE == "BAUCHI"] <-  33430
df$STATE.CASES[df$STATE == "BENUE"] <- 1000
df$STATE.CASES[df$STATE == "BORNO"] <- 12510
df$STATE.CASES[df$STATE == "FCT"] <- 1000
df$STATE.CASES[df$STATE == "KADUNA"] <- 9520
df$STATE.CASES[df$STATE == "KANO"] <- 13430
df$STATE.CASES[df$STATE == "NASARAWA"] <- 2550
df$STATE.CASES[df$STATE == "PLATEAU"] <- 1000
df$STATE.CASES[df$STATE == "TARABA"] <- 5590
df$STATE.CASES[df$STATE == "YOBE"] <- 3180
  
# 1000 used as baseline when no data was available ^
# http://www.plateformecholera.info/attachments/article/236/UNICEF-Factsheet-Nigeria-EN-FINAL.pdf

# Adding state populations

pops <- read.csv("Populations.csv")
pops$Pop <- as.numeric(gsub(",", "", pops$Pop))
names(pops)[1] <- "STATE"
df <- merge(df, pops, by = "STATE", all.x = T)

# Adding probability of disease 2002-2014

df <- mutate(df, PROB.CASE = STATE.CASES / Pop)

# Converting numeric variables
numeric.vars <- c("SURVEY.ROUND", "LATITUDE", "LONGITUDE", "NO.OF.HOUSEHOLDS", "INFANTS.MALE", "INFANTS.FEMALE", "CHILDREN.MALE", "CHILDREN.FEMALE", "YOUTH.MALE", "YOUTH.FEMALE", 
                  "ADULT.MALE", "ADULT.FEMALE", "ELDERLY.MALE", "ELDERLY.FEMALE", "TOTAL.NUMBER.OF.IDPS", "FUNCTIONING.TOILET")
df[,numeric.vars] <- lapply(df[,numeric.vars], as.numeric)

# Creating gender proportion variable

df <- mutate(df, PERC.FEMALE = 100 * (INFANTS.FEMALE + CHILDREN.FEMALE + YOUTH.FEMALE + ADULT.FEMALE + ELDERLY.FEMALE) / TOTAL.NUMBER.OF.IDPS)

# Creating age proportion variable

df <- mutate(df, PERC.UNDER5 = 100 * (INFANTS.FEMALE + INFANTS.MALE + CHILDREN.MALE + CHILDREN.FEMALE) / TOTAL.NUMBER.OF.IDPS)

# Creating household crowdedness variable

df <- mutate(df, HH.DENSITY = (TOTAL.NUMBER.OF.IDPS / NO.OF.HOUSEHOLDS)/ max(TOTAL.NUMBER.OF.IDPS / NO.OF.HOUSEHOLDS))

# Cleaning response variables
df$OPEN.DEFECATION2[df$OPEN.DEFECATION == "YES"] <- 1
df$OPEN.DEFECATION2[df$OPEN.DEFECATION == "NO"] <- 0
df$OPEN.DEFECATION2[df$OPEN.DEFECATION == "NO ANSWER"] <- 0.5
df$OPEN.DEFECATION2[is.na(df$OPEN.DEFECATION) == T] <- 0.5

df$SOLID.WASTE.PROBLEM2[df$SOLID.WASTE.PROBLEM == "YES"] <- 1
df$SOLID.WASTE.PROBLEM2[df$SOLID.WASTE.PROBLEM == "NO"] <- 0
df$SOLID.WASTE.PROBLEM2[df$SOLID.WASTE.PROBLEM == "NO ANSWER"] <- 0.5
df$SOLID.WASTE.PROBLEM2[is.na(df$SOLID.WASTE.PROBLEM) == T] <- 0.5

df$DRINKING.WATER.POTABLE2[df$DRINKING.WATER.POTABLE == "YES"] <- 0
df$DRINKING.WATER.POTABLE2[df$DRINKING.WATER.POTABLE == "NO"] <- 1
df$DRINKING.WATER.POTABLE2[df$DRINKING.WATER.POTABLE == "NO ANSWER"] <- 0.5
df$DRINKING.WATER.POTABLE2[is.na(df$DRINKING.WATER.POTABLE) == T] <- 0.5

df$ACCESS.TO.HEALTH.FACILITY2[df$ACCESS.TO.HEALTH.FACILITY == "YES"] <- 0
df$ACCESS.TO.HEALTH.FACILITY2[df$ACCESS.TO.HEALTH.FACILITY == "NO"] <- 1
df$ACCESS.TO.HEALTH.FACILITY2[df$ACCESS.TO.HEALTH.FACILITY == "NO ANSWER"] <- 0.5
df$ACCESS.TO.HEALTH.FACILITY2[is.na(df$ACCESS.TO.HEALTH.FACILITY) == T] <- 0.5

df$ACCESS.TO.MEDICINE2[df$ACCESS.TO.MEDICINE == "YES"] <- 0
df$ACCESS.TO.MEDICINE2[df$ACCESS.TO.MEDICINE == "NO"] <- 1
df$ACCESS.TO.MEDICINE2[df$ACCESS.TO.MEDICINE == "NO ANSWER"] <- 0.5
df$ACCESS.TO.MEDICINE2[is.na(df$ACCESS.TO.MEDICINE) == T] <- 0.5

df$HYGIENE.PROMOTION.CAMPAIGN2[df$HYGIENE.PROMOTION.CAMPAIGN == "YES"] <- 0
df$HYGIENE.PROMOTION.CAMPAIGN2[df$HYGIENE.PROMOTION.CAMPAIGN == "NO"] <- 1
df$HYGIENE.PROMOTION.CAMPAIGN2[df$HYGIENE.PROMOTION.CAMPAIGN == "NO ANSWER"] <- 0.5
df$HYGIENE.PROMOTION.CAMPAIGN2[is.na(df$HYGIENE.PROMOTION.CAMPAIGN) == T] <- 0.5

df$DRINKING.WATER.QUALITY.COMPLAINTS2[df$DRINKING.WATER.QUALITY.COMPLAINTS == "YES"] <- 1
df$DRINKING.WATER.QUALITY.COMPLAINTS2[df$DRINKING.WATER.QUALITY.COMPLAINTS == "NO"] <- 0
df$DRINKING.WATER.QUALITY.COMPLAINTS2[df$DRINKING.WATER.QUALITY.COMPLAINTS == "NO ANSWER"] <- 0.5
df$DRINKING.WATER.QUALITY.COMPLAINTS2[is.na(df$DRINKING.WATER.QUALITY.COMPLAINTS) == T] <- 0.5

df$SUPPORT.WASH2[df$SUPPORT.WASH == "YES"] <- 1
df$SUPPORT.WASH2[df$SUPPORT.WASH == "NO"] <- 0
df$SUPPORT.WASH2[df$SUPPORT.WASH == "NO ANSWER"] <- 0.5
df$SUPPORT.WASH2[is.na(df$SUPPORT.WASH) == T] <- 0.5

df$MOST.PREVALENT.HEALTH.PROBLEM2[df$MOST.PREVALENT.HEALTH.PROBLEM == "DIARRHEA"] <- 2
df$MOST.PREVALENT.HEALTH.PROBLEM2[df$MOST.PREVALENT.HEALTH.PROBLEM != "DIARRHEA"] <- 0

df$HAND.WASHING.STATIONS2[df$HAND.WASHING.STATIONS == "YES"] <- 0
df$HAND.WASHING.STATIONS2[df$HAND.WASHING.STATIONS == "YES BUT NO SOAP/WATER INSIDE"] <- 0.5
df$HAND.WASHING.STATIONS2[df$HAND.WASHING.STATIONS == "NO"] <- 1
df$HAND.WASHING.STATIONS2[is.na(df$HAND.WASHING.STATIONS) == T] <- 0.5

df$LATRINE.CONDITION2[df$LATRINE.CONDITION == "GOOD (HYGIENIC)"] <- 0
df$LATRINE.CONDITION2[df$LATRINE.CONDITION == "NOT SO GOOD (NOT HYGIENIC)"] <- 0.5
df$LATRINE.CONDITION2[df$LATRINE.CONDITION == "NON USABLE"] <- 1
df$LATRINE.CONDITION2[df$LATRINE.CONDITION == "NO ANSWER"] <- 0.5
df$LATRINE.CONDITION2[is.na(df$LATRINE.CONDITION) == T] <- 0.5

df$ACCESS.TO.FOOD2[df$ACCESS.TO.FOOD == "YES, OFF SITE"] <- 0
df$ACCESS.TO.FOOD2[df$ACCESS.TO.FOOD == "YES, ON SITE"] <- 0
df$ACCESS.TO.FOOD2[df$ACCESS.TO.FOOD == "NO"] <- 1

#df$CHOLERA.RISK <- OPEN.DEFECATION + SOLID.WASTE.PROBLEM + DRINKING.WATER.POTABLE + ACCESS.TO.HEALTH.FACILITY)
df <- df %>% mutate( CHOLERA.RISK = OPEN.DEFECATION2 + SOLID.WASTE.PROBLEM2 + DRINKING.WATER.POTABLE2 + ACCESS.TO.HEALTH.FACILITY2 +
                       ACCESS.TO.MEDICINE2 + HYGIENE.PROMOTION.CAMPAIGN2 + DRINKING.WATER.QUALITY.COMPLAINTS2 + SUPPORT.WASH2 +
                       MOST.PREVALENT.HEALTH.PROBLEM2 + HAND.WASHING.STATIONS2 + LATRINE.CONDITION2 + ACCESS.TO.FOOD2 + HH.DENSITY)

# dat1 <- df[,c("CHOLERA.RISK", "OPEN.DEFECATION2", "SOLID.WASTE.PROBLEM2" , "DRINKING.WATER.POTABLE2" , "ACCESS.TO.HEALTH.FACILITY2" ,
#              "ACCESS.TO.MEDICINE2" , "HYGIENE.PROMOTION.CAMPAIGN2" , "DRINKING.WATER.QUALITY.COMPLAINTS2" , "SUPPORT.WASH2" ,
#              "MOST.PREVALENT.HEALTH.PROBLEM2" , "HAND.WASHING.STATIONS2" , "LATRINE.CONDITION2")]

# Converting character variables to factors

factor.vars <- c("STATUS", "SITE.TYPE", "SMA.TYPE", "ACCESS.ELECTRICITY",
                 "NO.SHELTER", "TENTS", "MAKESHIFT", "INDOORS", "COMMON.SHELTER.TYPE")
df[,factor.vars] <- lapply(df[,factor.vars], as.factor)

# Specifying reference groups

df$INDOORS <- relevel(df$INDOORS, ref = "NONE")
df$MAKESHIFT <- relevel(df$MAKESHIFT, ref = "NONE")
df$NO.SHELTER <- relevel(df$NO.SHELTER, ref = "NONE")
df$TENTS <- relevel(df$TENTS, ref = "NONE")

# Risk * Size

#df$POP.ADJUSTED.RISK <- df$CHOLERA.RISK * df$TOTAL.NUMBER.OF.IDPS
#risk.table <- df[,c("POP.ADJUSTED.RISK", "CHOLERA.RISK", "TOTAL.NUMBER.OF.IDPS", "SITE.NAME")]
#risk.table <- arrange(risk.table,desc(POP.ADJUSTED.RISK))

# Risk * Size * PROB.CASE

#df$TOTAL.ADJUSTED.RISK <- df$CHOLERA.RISK * df$TOTAL.NUMBER.OF.IDPS * df$PROB.CASE
#risk.table <- df[,c("TOTAL.ADJUSTED.RISK", "CHOLERA.RISK", 
#                  "TOTAL.NUMBER.OF.IDPS", "SITE.NAME", 
#                    "STATE", "PROB.CASE")]
#risk.table <- arrange(risk.table,desc(TOTAL.ADJUSTED.RISK))

# Risk * PROB.CASE

df$PROB.ADJUSTED.RISK <- df$CHOLERA.RISK * df$PROB.CASE * 1000
risk.table <- df[,c("PROB.ADJUSTED.RISK", "CHOLERA.RISK", 
                    "TOTAL.NUMBER.OF.IDPS", "SITE.NAME", 
                    "STATE", "PROB.CASE")]


risk.table <- arrange(risk.table,desc(PROB.ADJUSTED.RISK))

# State by prob.case

r <- df[,c("STATE", "PROB.CASE")]
r <- arrange(r,desc(PROB.CASE))
r <- r[duplicated(r) == F,]

fit9 <- lm(CHOLERA.RISK ~ STATE, data=df)
summary(fit9)


aggregate(df[,c("CHOLERA.RISK", "SURVEY.ROUND")], list(df$SURVEY.ROUND), mean)

#-------------------------------------------------------------------------------------------------------------------------#
# Fitting models

# 1 

fit1 <- lm(CHOLERA.RISK ~ STATUS, data=df)
summary(fit1)

fit.g1 <- gee(CHOLERA.RISK ~ STATUS, id = SURVEY.ROUND, corstr = "independence", data=df)

# 2 

df$SMA.TYPE <- relevel(df$SMA.TYPE, ref = "GOVERNMENT")

fit2 <- lm(CHOLERA.RISK ~ SMA.TYPE, data=df)
summary(fit2) #NGOs are associated with an 0.66 unit increase in our risk score

df2 <- df[,c("CHOLERA.RISK", "SMA.TYPE")]
df2 <- df2[df2$SMA.TYPE == "GOVERNMENT" | df2$SMA.TYPE == "NGO",]
df2$SMA.TYPE <- as.numeric(df2$SMA.TYPE)
aggregate(df2, by = list(df2$SMA.TYPE), mean, na.rm=T)

fit.g2 <- gee(CHOLERA.RISK ~ SMA.TYPE, id = SURVEY.ROUND, corstr = "independence", data=df)

# 3

fit3 <- lm(CHOLERA.RISK ~ TOTAL.NUMBER.OF.IDPS, data=df)
summary(fit)

fit.g3 <- gee(CHOLERA.RISK ~ TOTAL.NUMBER.OF.IDPS, id = SURVEY.ROUND, corstr = "independence", data=df)
summary(fit.g3)

summary(df$TOTAL.NUMBER.OF.IDPS)

# 4

fit4 <- lm(CHOLERA.RISK ~ SITE.CLASSIFICATION, data=df)
summary(fit4) # Reference group = "PLANNED"

fit.g4 <- gee(CHOLERA.RISK ~ SITE.CLASSIFICATION, id = SURVEY.ROUND, corstr = "independence", data=df)
summary(fit.g4)

# 5

fit5 <- lm(CHOLERA.RISK ~ INDOORS, data=df)
summary(fit5) # Reference group = "PLANNED"

fit.g5 <- gee(CHOLERA.RISK ~ INDOORS, id = SURVEY.ROUND, corstr = "independence", data=df)
summary(fit.g5)

# 6

fit6 <- lm(CHOLERA.RISK ~ TENTS, data=df)
summary(fit6) # Reference group = "PLANNED"

fit.g6 <- gee(CHOLERA.RISK ~ TENTS, id = SURVEY.ROUND, corstr = "independence", data=df)
summary(fit.g6)

# 7

fit7 <- lm(CHOLERA.RISK ~ NO.SHELTER, data=df)
summary(fit7) # Reference group = "PLANNED"

fit.g7 <- gee(CHOLERA.RISK ~ NO.SHELTER, id = SURVEY.ROUND, corstr = "independence", data=df)
summary(fit.g7)

# 8

fit8 <- lm(CHOLERA.RISK ~ PERC.FEMALE, data=df)
summary(fit8) # Reference group = "PLANNED"

fit.g8 <- gee(CHOLERA.RISK ~ PERC.FEMALE, id = SURVEY.ROUND, corstr = "independence", data=df)
summary(fit.g8)

# 9

fit9 <- lm(CHOLERA.RISK ~ PERC.UNDER5, data=df)
summary(fit9) # Reference group = "PLANNED"

fit.g9 <- gee(CHOLERA.RISK ~ PERC.UNDER5, id = SURVEY.ROUND, corstr = "independence", data=df)
summary(fit.g9)

# Multivariate modeling

fit9 <- lm(CHOLERA.RISK ~ PERC.UNDER5 + NO.OF.HOUSEHOLDS + , data=df)
summary(fit9) # Reference group = "PLANNED"

fit9 <- lm(CHOLERA.RISK ~ PERC.UNDER5, data=df)
summary(fit9)



#------------------------------------------------------------------------------------------#

#http://www-bcf.usc.edu/~gareth/ISL/ISLR%20First%20Printing.pdf

library(tree)
library (ISLR)
library(randomForest)
library (gbm)

# Converting all character variables to factor
character.vars <- sapply(df, is.character)
df[character.vars] <- lapply(df[character.vars], as.factor)

tree1 =tree(CHOLERA.RISK ~ .-SITE.ID -SITE.NAME -WARD -ACCESS.TO.INCOME -SITE.MANAGEMENT.AGENCY..SMA. -LGA -PROB.ADJUSTED.RISK,df)
summary(tree1)
plot(tree1)
text(tree1)

# random forest

#varImpPlot(rf)

#set.seed(1)
bag = randomForest(CHOLERA.RISK ~ .-SITE.ID -SITE.NAME -WARD -ACCESS.TO.INCOME -SITE.MANAGEMENT.AGENCY..SMA. -LGA -PROB.ADJUSTED.RISK,data=df, #subset=train,
                        mtry=13,importance=TRUE)

#sapply(df$CHOLERA.RISK, function(x) any(is.na(x)))
#str(df)
                                            
#boosting

boost = gbm(CHOLERA.RISK~.-SITE.ID -SITE.NAME -WARD -ACCESS.TO.INCOME -SITE.MANAGEMENT.AGENCY..SMA. -LGA -PROB.ADJUSTED.RISK,data=df,distribution= "gaussian",n.trees=5000,interaction.depth=4)
summary(boost)
        
par(mfrow=c(1,2))
plot(boost,i="ACCESS.TO.MEDICINE")
plot(boost,i="DRINKING.WATER.POTABLE")

#---------------------------------------------------------------------------------------------------------------------#

# Intraclass correlation

ICCbare(df$LGA, df$CHOLERA.RISK)
ICCbare(df$LGA, df$PROB.ADJUSTED.RISK)

#---------------------------------------------------------------------------------------------------------------#

library(ggmap)

df$`Cholera Risk Score` <- df$CHOLERA.RISK

Map <- qmap('Nigeria', zoom = 6)
Map + geom_point(aes(x = LONGITUDE, y = LATITUDE, color=`Cholera Risk Score`),   #group=DataID, 
                 size = 5,
                 pch= 20,
                 data=df) + scale_colour_gradient2(low = "seagreen3", mid = "goldenrod1", high = "firebrick1", midpoint = 5,
                                                  space = "Lab", na.value = "grey50", guide = "colourbar")

display.brewer.all() 

#---------------------------------------------------------------------------------------------------------------#

library(plotly)

plot_ly(x = ~df$CHOLERA.RISK, type = "histogram")

ggplot(df, aes(CHOLERA.RISK)) +
  geom_density(adjust = 5) 



# Show case frequencies by survey round
risk.t <- table(df$CHOLERA.RISK, df$SURVEY.ROUND, df$SITE.ID)

mean(df$CHOLERA.RISK, df$SURVEY.ROUND, df$SITE.ID)


c <- function (site) {
colMeans(risk.t[,,site])
}

t <- sapply(1:dim(risk.t)[3], c)
  
`AD S001` <- t[,1]
`AD S002` <- t[,2]
`AD S003` <- t[,3]
`AD S004` <- t[,4]
`AD S005` <- t[,5]

dat <- data.frame(SURVEY.ROUND = 4:14, `AD S001`, `AD S002`, `AD S003`, `AD S004`, `AD S005`)

plot_ly(dat, x = ~SURVEY.ROUND, y = ~`AD S001`, name = 'AD S001', type = 'scatter', mode = 'lines',
        line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
  add_trace(y = ~`AD S002`, name = 'Male', line = list(color = 'rgb(22, 96, 167)', width = 4)) 

