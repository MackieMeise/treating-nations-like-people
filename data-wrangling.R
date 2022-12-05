#### LOAD NECESSARY PACKAGES ####

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(grid)
library(gridExtra)
library(gmodels)
library(sjPlot)
library(texreg)
library(lme4)
library(RColorBrewer)
library(jtools)
library(e1071)
library(lattice)
library(stargazer)
library(giscoR)
library(sf)
library(mice)
library(miceadds)
library(haven)
library(tidyr)
library(car)
library(broom.mixed)
library(eurostat)
library(sandwich)
library(lmtest)
library(bucky)
library(OECD)
library(ggeffects)
library(extrafont)
library(showtext)
library(interplot)
library(marginaleffects)
library(DescTools)

#### LOAD THE DATA FILE ####
rawdata <- get_dataset("IDD", start_time="2015") # takes a lot of time, remove the hastag at the beginning to load once only
df <- read.csv("data/REScEU_2019_Clasen.csv")

#### RENAME COLUMN LABELS ####

names(df)[names(df) == "q0_5"] <- "edu_y"
names(df)[names(df) == "education"] <- "edu"
names(df)[names(df) == "q1_3_1"] <- "fiscsol" #To what extent do you agree or disagree with the following statement: All EU Member States, including (COUNTRY), should contribute to a common EU fund to help any other Member State facing potential severe economic and financial difficulties in times of crisis.
names(df)[names(df) == "q1_3_3"] <- "fiscsol_t" #Would you personally be willing to support this fund with a 1% increase in your inc tax?
names(df)[names(df) == "q2_6_1_scale"] <- "crisman" #During the crisis some member states have done better than others (e.g. in terms of unemployment, poverty or growth rates). Please indicate to what extent do you agree with the following statements: The weaker member states have mismanaged their economy and public finances.
names(df)[names(df) == "q4_1"] <- "polpos" #In political matters people talk of "the left" and "the right". What is your position? Please use a scale from 0 to 10, where '0' means "left" and '10' means "right". Which number best describes your position?
names(df)[names(df) == "q4_2_4"] <- "imm" #Would you say that [COUNTRY]'s cultural life is generally enriched or undermined by people coming to live here from other countries?
names(df)[names(df) == "q6_2_4_scale"] <- "eucit" #To what extent do you agree or disagree with the following statements? I am proud to be a European citizen.
names(df)[names(df) == "q6_3"] <- "conoth" #The European Union does various things to support citizenÃÂÃÂ¢ÃÂÃÂÃÂÃÂs rights, but some say that it could do more.Which of the following things would enhance your feeling of being a European citizen?
names(df)[names(df) == "q8_14"] <- "inc" 
names(df)[names(df) == "q2_4_1_scale"] <- "trstnat" 
names(df)[names(df) == "q2_4_2_scale"] <- "trsteu" 
names(df)[names(df) == "q2_5_2_scale"] <- "trstms"

#### REMOVE UNNCESSARY COLUMNS ####

df$id_respondent <- NULL
df$currentday <- NULL
df$currentmonth <- NULL
df$currentyear <- NULL
df$sexbycleta <- NULL
df$X <- NULL
df[5:14] <- NULL #drops the country-specific encoding of edu
df[7:16] <- NULL #drops superfluous columns on regions
df$trstms <- NULL
df$X_merge <- NULL

#### CHANGE "ITALIA" TO "ITALY" ####

df["country"][df["country"] == "Italia"] <- "Italy"

#### CONVERT DATA INTO APPROPRIATE FORMAT ####

df$country <- as.factor(df$country)
df$age_class <- factor(df$age_class, levels = c("18-34", "35-54", "55-70"))
df$fiscsol <- factor(df$fiscsol, levels = c("Strongly agree", "Somewhat agree", 
                                            "Somewhat disagree", "Strongly disagree", 
                                            "Don't know"))
df$fiscsol_t <- factor(df$fiscsol_t, levels = c("Yes", "No", "Don't know"))
df$crisman <- factor(df$crisman, levels = c("Strongly agree", "Somewhat agree", 
                                            "Somewhat disagree", "Strongly disagree", 
                                            "Don't know"))
df$eucit <- factor(df$eucit, levels = c("Strongly agree", "Somewhat agree", 
                                        "Somewhat disagree", "Strongly disagree", 
                                        "Don't know"))
df$inc <- factor(df$inc, levels = c("Living comfortably on present income", 
                                          "Coping on present income", 
                                          "Finding it difficult on present income", 
                                          "Finding it very difficult on present income"))
df$edu <- factor(df$edu, levels = c("tertiary", "upper secondary", 
                                    "up to lower secondary"))

## some more extensive coding for gender ----
df["gender"][df["gender"] == "Female"] <- 1
df["gender"][df["gender"] == "Male"] <- 0
df$gender <- as.factor(df$gender)

## some more extensive coding required for "political position" ----
df["polpos"][df["polpos"] == "0 â€“ Left"] <- 0
df["polpos"][df["polpos"] == "10 â€“ Right"] <- 10
df["polpos"][df["polpos"] == "I prefer not to say"] <- NA
df["polpos"][df["polpos"] == "Don't know"] <- NA

df$polpos <- as.integer(df$polpos)

# some more extensive coding required for "immigration stance"

df["imm"][df["imm"] == "0 â€“ {#Qcountry} Cultural life enriched"] <- 0
df["imm"][df["imm"] == "10 â€“ {#Qcountry} Cultural life undermined"] <- 10
df["imm"][df["imm"] == "I prefer not to say"] <- NA
df["imm"][df["imm"] == "Don't know"] <- NA

df$imm <- as.integer(df$imm)

# some more extensive coding required for "trust in EU institutions"

df["trsteu"][df["trsteu"] == "0-no trust at all"] <- 0
df["trsteu"][df["trsteu"] == "10-complete trust"] <- 10
df["trsteu"][df["trsteu"] == "Don't know"] <- NA

df$trsteu <- as.integer(df$trsteu)

# some more extensive coding required for "trust in national government"

df["trstnat"][df["trstnat"] == "0-no trust at all"] <- 0
df["trstnat"][df["trstnat"] == "10-complete trust"] <- 10
df["trstnat"][df["trstnat"] == "Don't know"] <- NA

df$trstnat <- as.integer(df$trstnat)

# some more extensive coding required for "concern for others"

# creating the conoth_dk variable before transform the conoth variable
df$conoth_dk <- df$conoth
df["conoth_dk"][df["conoth_dk"] == "Don't know"] <- 1
df["conoth_dk"][df["conoth_dk"] != 1] <- 0
df$conoth_dk <- as.integer(df$conoth_dk)

# transforming the conoth variable
df["conoth"][df["conoth"] == "A more harmonised European social welfare system, based on common principles and"] <- 1
df["conoth"][df["conoth"] != 1] <- 0
df$conoth <- as.factor(df$conoth)

### create numerical copies of certain columns ====
# education
df$edu_n <- df$edu
df$edu_n <- as.character(df$edu_n)

df["edu_n"][df["edu_n"] == "up to lower secondary"] <- 1
df["edu_n"][df["edu_n"] == "upper secondary"] <- 2
df["edu_n"][df["edu_n"] == "tertiary"] <- 3

df$edu_n <- as.integer(df$edu_n)

# fiscal solidarity
recodevar <- function(df, column, name) {
  df$new_column <- as.character(column)
  df <- df %>% 
    mutate(new_column=replace(new_column, new_column=="Strongly agree",4)) %>%
    mutate(new_column=replace(new_column, new_column=="Somewhat agree",3)) %>%
    mutate(new_column=replace(new_column, new_column=="Somewhat disagree",2)) %>%
    mutate(new_column=replace(new_column, new_column=="Strongly disagree",1)) %>%
    mutate(new_column=replace(new_column, new_column=="Don't know",NA))
  df$new_column <- as.integer(df$new_column)
  names(df)[names(df) == "new_column"] <- paste(name, "_n", sep="")
  return(df)
}

df <- recodevar(df, df$fiscsol, "fiscsol")
df <- recodevar(df, df$crisman, "crisman")
df <- recodevar(df, df$eucit, "eucit")

### create dichotomous columns of certain columns ====

recodevardi <- function(df, column, name) {
  df$new_column <- as.integer(column)
  df <- df %>% 
    mutate(new_column=replace(new_column, new_column==1,0)) %>%
    mutate(new_column=replace(new_column, new_column==2,0)) %>%
    mutate(new_column=replace(new_column, new_column==3,1)) %>%
    mutate(new_column=replace(new_column, new_column==4,1)) 
  names(df)[names(df) == "new_column"] <- paste(name, "_d", sep="")
  return(df)
}

df <- recodevardi(df, df$fiscsol_n, "fiscsol")
df <- recodevardi(df, df$crisman_n, "crisman")
df <- recodevardi(df, df$eucit_n, "eucit")

df$fiscsol_d <- as.factor(df$fiscsol_d)
df$crisman_d <- as.factor(df$crisman_d)

### create dummy-variables for the DK-answers to the dependent variable and others ====
recodevardk <- function(df, column, name) {
  df$new_column <- as.character(column)
  df <- df %>% 
    mutate(new_column=replace(new_column, new_column!="Don't know",0)) %>%
    mutate(new_column=replace(new_column, new_column=="Don't know",1))
  df$new_column <- as.integer(df$new_column)
    names(df)[names(df) == "new_column"] <- paste(name, "_dk", sep="")
  return(df)
}

recodevardk2 <- function(df, column, name) {
  df$new_column <- as.character(column)
  df <- df %>% 
    mutate(new_column=replace(new_column, !is.na(new_column),0)) %>%
    mutate(new_column=replace(new_column, is.na(new_column),1))
  df$new_column <- as.integer(df$new_column)
  names(df)[names(df) == "new_column"] <- paste(name, "_dk", sep="")
  return(df)
}

df <- recodevardk(df, df$fiscsol, "fiscsol")
df <- recodevardk(df, df$crisman, "crisman")
df <- recodevardk(df, df$eucit, "eucit")
df <- recodevardk(df, df$inc, "inc")

df <- recodevardk2(df, df$polpos, "polpos")
df <- recodevardk2(df, df$imm, "imm")

### combine fiscal solidarity and fiscal solidarity tax into alternative dependent variable ====
df <- df %>% mutate(fiscsol_a = ifelse(fiscsol_d == 1 & fiscsol_t == "Yes", 1, 0))
df$fiscsol_a <- as.factor(df$fiscsol_a)

#### ADD COUNTRY LEVEL VARIABLES ####

macrodata <- read.csv("C:/Users/pclas/Desktop/Phd Stuff/20210920_country level data.csv", sep=";")
df <- merge(macrodata, df)
df <- df %>% mutate(gni19=gni19/1000)
df$bailoutcntry <- as.factor(df$bailoutcntry)
df$euromemb <- as.factor(df$euromemb)

range <- function(x){(x-min(x))/(max(x)-min(x))}
df$gni <- range(df$gni19)

wsdata <- filter(rawdata, Time=="2018" & AGE=="TOT")
wsdata <- filter(wsdata, MEASURE=="PVT5A" | MEASURE=="PVT5B")

wsdata[5:6] <- NULL
wsdata[2] <- NULL
wsdata[1] <- NULL
wsdata[4:7] <- NULL
wsdata <- pivot_wider(wsdata,names_from= MEASURE, values_from=ObsValue)

wsdata$LOCATION <- as.character(wsdata$LOCATION)
wsdata["LOCATION"][wsdata["LOCATION"] == "DEU"] <- "Germany"
wsdata["LOCATION"][wsdata["LOCATION"] == "SWE"] <- "Sweden"
wsdata["LOCATION"][wsdata["LOCATION"] == "FIN"] <- "Finland"
wsdata["LOCATION"][wsdata["LOCATION"] == "FRA"] <- "France"
wsdata["LOCATION"][wsdata["LOCATION"] == "GRC"] <- "Greece"
wsdata["LOCATION"][wsdata["LOCATION"] == "HUN"] <- "Hungary"
wsdata["LOCATION"][wsdata["LOCATION"] == "ITA"] <- "Italy"
wsdata["LOCATION"][wsdata["LOCATION"] == "NLD"] <- "Netherlands"
wsdata["LOCATION"][wsdata["LOCATION"] == "POL"] <- "Poland"
wsdata["LOCATION"][wsdata["LOCATION"] == "ESP"] <- "Spain"
wsdata$LOCATION <- as.factor(wsdata$LOCATION)

names(wsdata)[names(wsdata) == "LOCATION"] <- "country"
df <- left_join(df, wsdata)
df$PVT5A <- as.numeric(df$PVT5A)
df$PVT5B <- as.numeric(df$PVT5B)
df <- df %>% mutate(wsdiff=100*(PVT5B-PVT5A)/PVT5B)

#### CREATE AS CLEAN AS POSSIBLE GEOGRAPHICAL DATA ####

# because the data is different for each country, I need to split the dataframe in country level dataframes, before rejoing them again
# For each country, I need the NUTS3 level data and assign them a qualitative value of the character of the region (eg border, no border)

## France ----
#loading the csv file with the NUTS3 data for France
regfr <- read.csv("C:/Users/pclas/Desktop/Phd Stuff/20211007_GrenzregionenFR.csv", sep=";")
regfr <- rename(regfr, qmktsize_5_1=ISNEE) # a bit of cleaning is necessary
regfr$qmktsize_5_1 <- substr(regfr$qmktsize_5_1,3, nchar(regfr$qmktsize_5_1)) #because of the specific format of these regional codes I had to come up with this
regfr$Nuts_alt <- NULL

# in a next step, I need to create a country-level copy of the df for France
dffr <- filter(df,country=="France")

#now I can merge the 2
mergfr <- left_join(dffr, regfr, by="qmktsize_5_1")

## Italy ----
#loading the csv file with the NUTS3 data for Italy
regit <- read.csv("C:/Users/pclas/Desktop/Phd Stuff/20210930_GrenzregionenIT.csv", sep=";")
regit <- rename(regit, qmktsize_7_1=ï..ISTAT)
regit <- rename(regit, NUTS_ID=NUTS3)
regit$qmktsize_7_1 <- substr(regit$qmktsize_7_1,2, nchar(regit$qmktsize_7_1)) #because of the specific format of these regional codes I had to come up with this

# in a next step, I need to create a country-level copy of the df for France
dfit <- filter(df,country=="Italy")

#now I can merge the 2
mergit <- left_join(dfit, regit, by="qmktsize_7_1")

## Finland ----
#loading csv file, adjusting the data a bit
regfi <- read.csv("C:/Users/pclas/Desktop/Phd Stuff/20210930_GrenzregionenFI.csv", sep=";")
regfi <- rename(regfi, NUTS_ID=ï..NUTS_ID)

# country-level copy of df for Finland
dffi <- filter(df, country=="Finland")

# now I merge
mergfi <- left_join(dffi, regfi, by="qmktsize_7_1")

## Hungary ----
#loading csv file, adjusting the data a bit
reghu <- read.csv("C:/Users/pclas/Desktop/Phd Stuff/20210930_GrenzregionenHU.csv", sep=";")
reghu <- rename(reghu, NUTS_ID=ï..NUTS_ID)

#country-level copy of df for Hungary
dfhu <- filter(df, country=="Hungary")
dfhu$qmktsize_7_1 <- paste("HU", dfhu$qmktsize_7_1, sep="") #adjust the data in the original df to fit the nuts data

# now I merge
merghu <- left_join(dfhu, reghu, by="qmktsize_7_1")

## Greece ----
reggr <- read.csv("C:/Users/pclas/Desktop/Phd Stuff/20210929_GrenzregionenGR.csv", sep=";")
reggr <- rename(reggr, qmktsize_7_1 = ï..qmktsize_7_1)

# country-level copy of df for Greece
dfgr <- filter(df, country=="Greece")

# now I merge
merggr <- left_join(dfgr, reggr, by="qmktsize_7_1")

## Sweden ----
#load csv
regse <- read.csv("C:/Users/pclas/Desktop/Phd Stuff/20210930_GrenzregionenSE.csv", sep=";")
regse <- rename(regse, qmktsize_6_1 = ï..qmktsize_6_1)

#country-level copy
dfse <- filter(df, country=="Sweden")

# now I merge
mergse <- left_join(dfse, regse, by="qmktsize_6_1")

## Netherlands ----
#load csv
regnl <- read.csv("C:/Users/pclas/Desktop/Phd Stuff/20211007_GrenzregionenNL.csv", sep=";")
regnl <- rename(regnl, qmktsize_7_1 = ï..qmktsize_7_1)
regnl$qmktsize_7_1 <- as.character(regnl$qmktsize_7_1)

#country-level copy
dfnl <- filter(df, country=="Netherlands")

# now I merge
mergnl <- left_join(dfnl,regnl, by="qmktsize_7_1")

## Poland ----
#load csv
regpl <- read.csv("C:/Users/pclas/Desktop/Phd Stuff/20211007_GrenzregionenPL.csv", sep=";")
regpl <- rename(regpl, NUTS_ID = ï..NUTS_ID)
regpl$qmktsize_7_1 <- as.character(regpl$qmktsize_7_1)
regpl$qmktsize_7_1 <- substr(regpl$qmktsize_7_1,3, nchar(regpl$qmktsize_7_1)) #because of the specific format of these regional codes I had to come up with this

#country-level copy
dfpl <- filter(df, country=="Poland")

#now I merge
mergpl <- left_join(dfpl, regpl, by="qmktsize_7_1")

## Germany ----
#load csv
regde <- read.csv("C:/Users/pclas/Desktop/Phd Stuff/20211010_GrenzregionenDE.csv", sep=";")
regde <- rename(regde, NUTS_ID = ï..NUTS_ID)

#country-level copy
dfde <- filter(df, country=="Germany")

#now I merge
mergde <- left_join(dfde, regde, by="qmktsize_7_1")

## Spain ----
#load csv
reges <- read.csv("C:/Users/pclas/Desktop/Phd Stuff/20211010_GrenzregionenES.csv", sep=";")
reges <- rename(reges, NUTS_ID = ï..NUTS_ID)
reges$qmktsize_7_1 <- as.character(reges$qmktsize_7_1)

#country-level copy
dfes <- filter(df, country=="Spain")

#now I merge
merges <- left_join(dfes, reges, by="qmktsize_7_1")

### Now I can put them all together again ====
df <- bind_rows(mergit, mergfr, mergfi, merghu, merggr, mergse, mergpl, mergnl, mergde, merges)

# remove datasets not needed from this point
rm(dfde, dfes,dffi,dffr,dfgr,dfhu,dfit,dfnl,dfpl,dfse)
rm(mergde, merges,mergfi,mergfr,merggr,merghu,mergit,mergnl,mergpl,mergse)
rm(regde, reges,regfi,regfr,reggr,reghu,regit,regnl,regpl,regse)
rm(macrodata)

#### DROP EVERYTHING I DONT NEED FOR THE PAPER ####
df["netbalance"] <- NULL
df["euromemb"] <- NULL
df["bailoutcntry"] <- NULL
df["welfaretypeEA"] <- NULL
df["welfaretypeALT"] <- NULL
df["avggni1019"] <- NULL
df["area_tot"] <- NULL
df["qmktsize_3_1"] <- NULL
df["qmktsize_4_1"] <- NULL
df["qmktsize_5_1"] <- NULL
df["qmktsize_6_1"] <- NULL
df["qmktsize_7_1"] <- NULL
df["qmktsize_8_1"] <- NULL
df["trstnat"] <- NULL
df["trsteu"] <- NULL
df["PVT5B"] <- NULL
df["PVT5A"] <- NULL
df["fiscsol_dk"] <- NULL
df["crisman_dk"] <- NULL
df["inc_dk"] <- NULL
df["polpos_dk"] <- NULL
df["imm_dk"] <- NULL
df["edu_y"] <- NULL
df["age"] <- NULL

save(df1, file = "data/data.rda")


