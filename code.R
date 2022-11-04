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
df <- read.csv("C:/Users/pclas/Desktop/Phd Stuff/REScEU_2019_Clasen.csv")

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

#### EXPLORATORY DATA ANALYSIS ####
### Set The font For GGplot ====

theme_set(theme_gray(base_family = "serif"))

### Univariate Analysis ====

## do bar charts for the distribution of the dependent variable fiscal solidarity ----

p1 <- df %>%
  group_by(country,fiscsol) %>%
  summarise(count=n()) %>%
  mutate(prop=count/sum(count)) %>%
  filter(fiscsol!="Don't know")
p2 <- p1 %>%
  group_by(country) %>%
  summarise(order=sum(prop[fiscsol=="Strongly agree" | fiscsol=="Somewhat agree"]))
p <- left_join(p1, p2)
p$fiscsol <- factor(p$fiscsol, levels=c("Strongly agree", "Somewhat agree","Strongly disagree", "Somewhat disagree"))
p <- p %>% mutate(prop=ifelse(fiscsol=="Somewhat disagree" | fiscsol=="Strongly disagree",-1*prop,prop))

pl1 <- ggplot(data = p,aes(x=reorder(country, -order), y=prop, fill=fiscsol, label=paste(as.character(abs(round(100*prop,0))), "%", sep=""))) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(type = "div", palette = "Greys", direction= -1) +
  geom_text(position=position_stack(vjust=0.5), aes(colour=ifelse(fiscsol=="Strongly disagree" | fiscsol=="Somewhat disagree","black","white")), family="serif") +
  scale_colour_manual(values=c("black", "lightgrey"), guide="none") + 
  labs(subtitle="All EU Member States, including (COUNTRY), should contribute to a common EU\nfund to help any other Member State facing potential severe economic and financial\ndifficulties in times of crisis.", 
       y="Share of respondents           ", 
       x="") +
  geom_hline(yintercept=0, colour="grey") +
  theme_pubclean() +
  theme(text = element_text(family="serif"), legend.title = element_blank(), legend.key.width = unit(0.25, "cm"), 
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(),
        panel.grid.major.y = element_blank())

p3 <- df %>%
  group_by(country,fiscsol) %>%
  summarise(count=n()) %>%
  mutate(prop=count/sum(count)) %>%
  filter(fiscsol=="Don't know")
py <- left_join(p3, p2)
pl2 <- ggplot(data = py,aes(x=reorder(country, -order), y=prop, fill=fiscsol, label=paste(as.character(round(100*prop,0)), "%", sep=""))) + 
  geom_bar(stat = "identity", fill="grey") +
  geom_text(position=position_stack(vjust=0.5), family="serif") +
  labs(subtitle="Share of Don't know responses",
       y="", 
       x="") +
  geom_hline(yintercept=0, colour="grey") +
  theme_pubclean() +
  theme(text = element_text(family="serif"), legend.title = element_blank(), legend.key.width = unit(0.25, "cm"), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y = element_blank())

finalp <- grid.arrange(arrangeGrob(pl1, nrow=1, ncol=1),
             arrangeGrob(pl2, nrow=1, ncol=1), heights=(c(5,1)))
ggsave("fiscsoleu3_bw.png", finalp, width=160, height=110, units="mm")

# calculate means, medians and standard deviation for the whole sample and by country
df_f <- filter(df, fiscsol_n<5) 
sd(df_f$fiscsol_n)
mean(df_f$fiscsol_n)
median(df_f$fiscsol_n)

means <- list()
medians <- list()
stdv <- list()
countries = unique(df$country)

for (country in countries) {
  df_f <- df[which(df$country == country), ]
  df_f <- df_f[ which(df_f$fiscsol_n < 5), ]
  means[[country]] <- mean(df_f$fiscsol_n)
  medians[[country]] <- median(df_f$fiscsol_n)
  stdv[[country]] <-sd(df_f$fiscsol_n)
}

## do bar charts for the distribution of the follow-up question fiscal solidarity, if individuals are willing to contribute personally to such a fund ----

p1 <- filter(df, !is.na(fiscsol_t)) %>%
  group_by(country,fiscsol_t) %>%
  summarise(count=n()) %>%
  mutate(prop=count/sum(count))
p2 <- filter(df, !is.na(fiscsol_t)) %>%
  group_by(country) %>%
  summarise(order=n())
p <- left_join(p1, p2)
p$fiscsol <- factor(p$fiscsol_t, levels=c("Yes", "Don't know", "No"))

ggplot(data = p,aes(x=reorder(country, -order), y=prop, fill=fiscsol_t, label=paste(as.character(round(100*prop,1)), "%", sep=""))) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(type = "div", palette = "RdBu") +
  geom_text(position=position_stack(vjust=0.5)) +
  labs(subtitle="Would you personally be willing to support this fund with a 1% increase in your income tax?", 
       y="Share of respondents", 
       x="") +
  theme_pubclean() +
  theme(text = element_text(family="serif"), legend.title = element_blank())
ggsave("fiscsoleu_tax.png", width=160, height=110, units="mm")

## independent variables: perception of crisis mismanagement ----
p1 <- df %>%
  group_by(country,crisman) %>%
  summarise(count=n()) %>%
  mutate(prop=count/sum(count))
p2 <- p1 %>%
  group_by(country) %>%
  summarise(order=sum(prop[crisman=="Strongly agree" | crisman=="Somewhat agree"]))
p <- left_join(p1, p2)
p$crisman <- factor(p$crisman, levels=c("Strongly agree", "Somewhat agree","Strongly disagree", "Somewhat disagree"))
p <- p %>% mutate(prop=ifelse(crisman=="Somewhat disagree" | crisman=="Strongly disagree",-1*prop,prop))

pl1 <- ggplot(data = p,aes(x=reorder(country, -order), y=prop, fill=crisman, label=paste(as.character(abs(round(100*prop,0))), "%", sep=""))) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(type = "div", palette = "Greys", direction= -1) +
  geom_text(position=position_stack(vjust=0.5), aes(colour=ifelse(crisman=="Strongly disagree" | crisman=="Somewhat disagree","black","white")), family="serif") +
  scale_colour_manual(values=c("black", "lightgrey"), guide="none") + 
  labs(subtitle="During the crisis some member states have done better than others (e.g. in terms of\nunemployment, poverty or growth rates). Please indicate to what extent do you\nagree with the following statements: Weaker member states have mismanaged\ntheir economy andpublic finances.", 
       y="Share of respondents", 
       x="") +
  geom_hline(yintercept=0, colour="grey") +
  theme_pubclean() +
  theme(text = element_text(family="serif"), legend.title = element_blank(), legend.key.width = unit(0.25, "cm"), 
        axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.ticks.x=element_blank(),
        panel.grid.major.y = element_blank())

p3 <- df %>%
  group_by(country,crisman) %>%
  summarise(count=n()) %>%
  mutate(prop=count/sum(count)) %>%
  filter(crisman=="Don't know")
py <- left_join(p3, p2)
pl2 <- ggplot(data = py,aes(x=reorder(country, -order), y=prop, fill=crisman, label=paste(as.character(round(100*prop,0)), "%", sep=""))) + 
  geom_bar(stat = "identity", fill="grey") +
  geom_text(position=position_stack(vjust=0.5), family="serif") +
  labs(y="% DK", 
       x="") +
  geom_hline(yintercept=0, colour="grey") +
  theme_pubclean() +
  theme(text = element_text(family="serif"), legend.title = element_blank(), legend.key.width = unit(0.25, "cm"), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.y = element_blank())

finalp <- grid.arrange(arrangeGrob(pl1, nrow=1, ncol=1),
                       arrangeGrob(pl2, nrow=1, ncol=1), heights=(c(5,1)))
ggsave("crisman_bw.png", finalp, width=160, height=110, units="mm")

# calculate means, medians and standard deviation for the whole sample and by country
df_f <- filter(df, crisman_n<5) 
sd(df_f$crisman_n)
mean(df_f$crisman_n)
median(df_f$crisman_n)

means <- list()
medians <- list()
stdv <- list()
countries = unique(df$country)

for (country in countries) {
  df_f <- df[which(df$country == country), ]
  df_f <- df_f[which(df_f$crisman_n < 5), ]
  means[[country]] <- mean(df_f$crisman_n)
  medians[[country]] <- median(df_f$crisman_n)
  stdv[[country]] <-sd(df_f$crisman_n)
}

## independent variables: living in a border region ----
# load geographical data
geodata3 <- gisco_get_nuts(nuts_level="3")
geodata3 <- filter(geodata3, CNTR_CODE=="FR"|CNTR_CODE=="FI"|CNTR_CODE=="SE"|CNTR_CODE=="PL"|CNTR_CODE=="HU"|CNTR_CODE=="IT"|CNTR_CODE=="EL")
geodata2 <- gisco_get_nuts(nuts_level="2")
geodata2 <- filter(geodata2, CNTR_CODE=="DE"|CNTR_CODE=="NL"|CNTR_CODE=="ES")
geodata0 <- gisco_get_countries()

# create grouped datafame
reggrp <- filter(df, fiscsol_n<5) %>%
  group_by(NUTS_ID) %>%
  summarize(fiscsol = mean(fiscsol_d), count=n(), landb=mean(landb), lang = mean (lang), country= country)

# join the two
mapdata3 <- left_join(geodata3, reggrp, by="NUTS_ID")
mapdata2 <- left_join(geodata2, reggrp, by="NUTS_ID")

ggplot() +
  geom_sf(data=geodata0, size=0.3, colour="darkgrey") +
  geom_sf(data=mapdata3, size=0.3, colour="darkgrey", aes(fill=fiscsol)) +
  geom_sf(data=mapdata2, size=0.3, colour="darkgrey", aes(fill=fiscsol)) +
  scale_fill_steps2(high="#ca0020", low="#0571b0", mid="#f7f7f7",midpoint=0.5, n.breaks=9, name="European\nfiscal\nsolidarity") +
  coord_sf(xlim = c(-14, 35), ylim=c(34, 70), expand=FALSE) +
  theme_pubclean() +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),legend.position = "right")
ggsave("map.png", width=160, height=160, units="mm")

## control variables ----

#political position
df_f <- filter(df, polpos<11) 
sd(df_f$polpos)
mean(df_f$polpos)
median(df_f$polpos)

#age
sd(df_f$age)
mean(df_f$age)
median(df_f$age)

#income
df_f <- filter(df, inc_n<5)
sd(df_f$inc_n)
mean(df_f$inc_n)
median(df_f$inc_n)

#education
sd(df_f$edu_n)
mean(df_f$edu_n)
median(df_f$edu_n)

#gender
df$gender <- as.numeric(df$gender)
sd(df$gender)
mean(df$gender)
median(df$gender)
df$gender <- factor(df$gender)

### Bivariate Analysis ====
## relation between mismanagement perception and solidarity ----

# plot crosstable with full scale
sjt.xtab(df$crisman_n,df$fiscsol_n, show.row.prc = TRUE, file = "fiscsolcrisman.doc")

# plot crosstable with dichotomously recoded variables
sjt.xtab(df$crisman_d,df$fiscsol_d, show.row.prc = TRUE, file = "fiscsolcrisman_d.doc")

# mosaic plot
ggplot(data = df) +
  geom_mosaic(aes(x=product(crisman_d), fill=fiscsol_d)) + 
  geom_mosaic_text(aes(x=(product(crisman_d, fiscsol_d)))) +
  theme_mosaic()

# interaction effect attributed responsibility and GNI
dfgr <- filter(df, country=="Greece")
dfnl <- filter(df, country=="Netherlands")
dffi <- filter(df, country=="Finland")

sjt.xtab(dfgr$crisman_d,dfgr$fiscsol_d, show.row.prc = TRUE, file = "fiscsolcrisman_dgr.doc")
sjt.xtab(dfnl$crisman_d,dfnl$fiscsol_d, show.row.prc = TRUE, file = "fiscsolcrisman_dnl.doc")
sjt.xtab(dffi$crisman_d,dffi$fiscsol_d, show.row.prc = TRUE, file = "fiscsolcrisman_dfi.doc")

# test to see if the relation is in all of the countries
countries <- unique(df$country)
for (country in countries) {
  df_woDK <- df[which(df$fiscsol != "Don't know"), ]
  df_woDK <- df_woDK[which(df$crisman != "Don't know"), ]
  df_woDK$fiscsol <- factor(df_woDK$fiscsol, levels = c("Strongly agree", "Somewhat agree", 
                                                        "Somewhat disagree", "Strongly disagree"))
  df_woDK$crisman <- factor(df_woDK$crisman, levels = c("Strongly agree", "Somewhat agree", 
                                                        "Somewhat disagree", "Strongly disagree"))
  df_woDK <- df_woDK[which(df_woDK$country == country), ]
  CrossTable(df_woDK$crisman_d, df_woDK$fiscsol_d, chisq = TRUE)
}

# mosaic graph for deservingness and solidarity
ggplot(df) +
  geom_mosaic(aes(x=product(fiscsol, crisman), fill=fiscsol)) + facet_wrap(~country)

## relation between cosmopolitanism and fiscal solidarity ----
ggplot(df, aes(y=cosmopolitanism, x=fiscsol, fill=fiscsol_n)) +
  geom_boxplot() +
  scale_fill_distiller(type = "div", palette = "RdBu") +
  labs(x="European fiscal solidarity", y="Cosmopolitan European identity")+
  theme_pubclean() +
  theme(legend.position="")
  ggsave("cosmosoli.png", width=160, height=110, units="mm")


#### MULTIPLE LOGISTIC REGRESSION ####

# regression model with the standard dependent variable
model1 <- glm.cluster(fiscsol_d ~ crisman_d + gender + age_class + edu + inc + polpos, 
              weights = df$PESO_TOT, 
              data=df, 
              family="binomial",
              cluster=df$country) 

model2 <- glm.cluster(fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos, 
              weights = df$PESO_TOT, 
              data=df, 
              family="binomial",
              cluster=df$country) 

model3 <- glm.cluster(fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff, 
              weights = df$PESO_TOT, 
              data=df, 
              family="binomial",
              cluster=df$country)

model4 <- glm.cluster(fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*conoth, 
                      weights = df$PESO_TOT, 
                      data=df, 
                      family="binomial",
                      cluster=df$country) 

model5 <- glm.cluster(fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*gni, 
                      weights = df$PESO_TOT, 
                      data=df, 
                      family="binomial",
                      cluster=df$country) 

model6 <- glm.cluster(fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*wsdiff, 
              weights = df$PESO_TOT, 
              data=df, 
              family="binomial",
              cluster=df$country) 

model7 <- glm.cluster(fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*conoth + crisman_d*gni + crisman_d*wsdiff, 
                      weights = df$PESO_TOT, 
                      data=df, 
                      family="binomial",
                      cluster=df$country) 

screenreg(list(model1,model2, model3, model4, model5, model6, model7))  
htmlreg(list(model1, model2, model3, model4, model5, model6, model7), file="regtable_p2.doc", single.row=T, stars=0.01)

# calculating R²
r2 <- with(summary(model1$glm_res), 1 - deviance/null.deviance)
r2 <- with(summary(model2$glm_res), 1 - deviance/null.deviance)
r2 <- with(summary(model3$glm_res), 1 - deviance/null.deviance)
r2 <- with(summary(model4$glm_res), 1 - deviance/null.deviance)
r2 <- with(summary(model5$glm_res), 1 - deviance/null.deviance)
r2 <- with(summary(model6$glm_res), 1 - deviance/null.deviance)
r2 <- with(summary(model7$glm_res), 1 - deviance/null.deviance)

# LRT for the interaction effects
anova(model7$glm_res, model6$glm_res, test="LRT")
anova(model7$glm_res, model5$glm_res, test="LRT")
anova(model7$glm_res, model4$glm_res, test="LRT")

# regression model with the alternative dependent variable
model1a <- glm.cluster(fiscsol_a ~ crisman_d + gender + age_class + edu + inc + polpos, 
                      weights = df$PESO_TOT, 
                      data=df, 
                      family="binomial",
                      cluster=df$country) 
model2a <- glm.cluster(fiscsol_a ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos, 
                      weights = df$PESO_TOT, 
                      data=df, 
                      family="binomial",
                      cluster=df$country) 
model3a <- glm.cluster(fiscsol_a ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff, 
                      weights = df$PESO_TOT, 
                      data=df, 
                      family="binomial",
                      cluster=df$country) 

model4a <- glm.cluster(fiscsol_a ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*conoth, 
                      weights = df$PESO_TOT, 
                      data=df, 
                      family="binomial",
                      cluster=df$country) 

model5a <- glm.cluster(fiscsol_a ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*gni, 
                      weights = df$PESO_TOT, 
                      data=df, 
                      family="binomial",
                      cluster=df$country) 

model6a <- glm.cluster(fiscsol_a ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*wsdiff, 
                      weights = df$PESO_TOT, 
                      data=df, 
                      family="binomial",
                      cluster=df$country) 

model7a <- glm.cluster(fiscsol_a ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*conoth + crisman_d*gni + crisman_d*wsdiff, 
                      weights = df$PESO_TOT, 
                      data=df, 
                      family="binomial",
                      cluster=df$country) 
coefnames <- c("Intercept", "Attributed responsibility of recipients", "Gender (1: Woman)", "Age (35-54)", "Age (55-70)", "Education (upper secondary)", "Education (up to lower secondary)", "Income (Coping)", "Income (difficult on present income)", "Income (very difficult on present income)", "Political self-placement", "Immigration stance", "Proud to be European citizen", "Concern for others", "GNI", "Welfare state effectiveness", "Attributed responsibility x Concern for others", "Attributed responsibility x GNI", "Attributed responsibility x Welfare state effectiveness")
screenreg(list(model1a,model2a, model3a, model4a, model5a, model6a, model7a))  
htmlreg(list(model1a, model2a, model3a, model4a, model5a, model6a, model7a), file="regtable_a_p2.doc", single.row=T, stars=0.01, custom.coef.names = coefnames)

# calculating R²
r2 <- with(summary(model1a$glm_res), 1 - deviance/null.deviance)
r2 <- with(summary(model2a$glm_res), 1 - deviance/null.deviance)
r2 <- with(summary(model3a$glm_res), 1 - deviance/null.deviance)
r2 <- with(summary(model4a$glm_res), 1 - deviance/null.deviance)
r2 <- with(summary(model5a$glm_res), 1 - deviance/null.deviance)
r2 <- with(summary(model6a$glm_res), 1 - deviance/null.deviance)
r2 <- with(summary(model7a$glm_res), 1 - deviance/null.deviance)


## start imputing missing values ----
countries <- unique(df$country)
df_list <- list()
imp <- seq(1:5)
dfi <- list()

for (i in imp) {
 for (countri in countries) {
   df_list[[countri]] <- filter(df, country==countri)
  
   ry1 <- ifelse(is.na(df_list[[countri]]$crisman_d), FALSE, TRUE)
   crisman_i <- mice.impute.sample(df_list[[countri]]$crisman_d, ry1)
  
   ind <- which(is.na(df_list[[countri]]$crisman_d))
   df_list[[countri]][ind, "crisman_d"] <- crisman_i
   df_list[[countri]][ind, "crisman_i"] <- 1
   df_list[[countri]] <- df_list[[countri]] %>% mutate(crisman_i=ifelse(is.na(crisman_i),0,1))

   ry2 <- ifelse(is.na(df_list[[countri]]$fiscsol_d), FALSE, TRUE)
   fiscsol_i <- mice.impute.sample(df_list[[countri]]$fiscsol_d, ry2)
  
   ind <- which(is.na(df_list[[countri]]$fiscsol_d))
   df_list[[countri]][ind, "fiscsol_d"] <- fiscsol_i
   df_list[[countri]][ind, "fiscsol_i"] <- 1
   df_list[[countri]] <- df_list[[countri]] %>% mutate(fiscsol_i=ifelse(is.na(fiscsol_i),0,1))
  
   ry3 <- ifelse(is.na(df_list[[countri]]$eucit_d), FALSE, TRUE)
   eucit_i <- mice.impute.sample(df_list[[countri]]$eucit_d, ry3)
  
   ind <- which(is.na(df_list[[countri]]$eucit_d))
   df_list[[countri]][ind, "eucit_d"] <- eucit_i
   df_list[[countri]][ind, "eucit_i"] <- 1
   df_list[[countri]] <- df_list[[countri]] %>% mutate(eucit_i=ifelse(is.na(eucit_i),0,1))
 }
  
dfi[[i]] <- bind_rows(df_list, .id = "column_label")
dfi[[i]] <- filter(dfi[[i]], !is.na(imm))
dfi[[i]] <- filter(dfi[[i]], !is.na(polpos))
dfi[[i]] <- filter(dfi[[i]], !is.na(inc))

}

# regression model with the standard dependent variable, using imputed data

custom_pooling <- function(formula) {
  b <- se <- NULL
  imp <- seq(1:5)
  for (i in imp) {
    model <- glm.cluster(formula=formula, 
                         data=dfi[[i]], 
                         family="binomial", 
                         cluster=dfi[[i]]$country,
                         weights="PESO_TOT")
    b <- cbind(b, coef(model$glm_res))
    se <- cbind(se, summary(model)[,2])
    }
  
  #now pool the results
  b.pool <- apply(b, 1, mean)
  between.var <- apply(b, 1, var)
  within.var <- apply(se^2, 1, mean)
  se.pool <- sqrt(within.var+between.var+between.var/5) 
  t.pool <- b.pool/se.pool 
  pvalue.pool <- (1-pnorm(abs(t.pool)))*2 
  coefficients <- data.frame(b.pool, se.pool, t.pool, pvalue.pool)
  
  aic <- extractAIC(model$glm_res)[2]
  LL <- logLik(model$glm_res)
  r2 <- with(summary(model$glm_res), 1 - deviance/null.deviance)
  n <- nobs(model$glm_res)
  imputations <- "5"
  
  return(list(coef=coefficients,
              aic=aic,
              LL=LL,
              r2=r2,
              n=n,
              imputations=imputations)) 
}

convertModel <- function(model) {
  tr <- createTexreg(
    coef.names = rownames(model$coef), 
    coef = model$coef$b.pool, 
    se = model$coef$se.pool, 
    pvalues = model$coef$pvalue.pool,
    gof.names = c("AIC", "Log Likelihood", "McFadden R²", "N", "No. imputations"), 
    gof = c(model$aic, model$LL, model$r2, model$n, as.numeric(model$imputations)), 
    gof.decimal = c(T,T,T,F,F)
  )
}

m1 <- custom_pooling("fiscsol_d ~ crisman_d + gender + age_class + edu + inc + polpos")
m2 <- custom_pooling("fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos")
m3 <- custom_pooling("fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff")
m4 <- custom_pooling("fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*conoth")
m5 <- custom_pooling("fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*gni")
m6 <- custom_pooling("fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*wsdiff")
m7 <- custom_pooling("fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*wsdiff + crisman_d*gni + crisman_d*conoth")

mo1 <- convertModel(m1)
mo2 <- convertModel(m2)
mo3 <- convertModel(m3)
mo4 <- convertModel(m4)
mo5 <- convertModel(m5)
mo6 <- convertModel(m6)
mo7 <- convertModel(m7)
screenreg(list(mo1, mo2, mo3, mo4, mo5, mo6, mo7))
coefnames <- c("Intercept", "Attributed responsibility of recipients", "Gender (1: Woman)", "Age (35-54)", "Age (55-70)", "Education (upper secondary)", "Education (up to lower secondary)", "Income (Coping)", "Income (difficult on present income)", "Income (very difficult on present income)", "Political self-placement", "Immigration stance", "Proud to be European citizen", "Concern for others", "GNI", "Welfare state effectiveness", "Attributed responsibility x Concern for others", "Attributed responsibility x GNI", "Attributed responsibility x Welfare state effectiveness")
htmlreg(list(mo1, mo2, mo3, mo4, mo5, mo6, mo7), file="regtable_i3.doc", single.row=T, stars=0.01, custom.coef.names = coefnames)

## runs diagnostic tests ----
# linearity assumption

modelX <- glm(fiscsol_d ~ crisman_d + cosmopolitanism + polpos + 
                  age + gender + edu_n + inc_n + trsteu + trstnat + landb + lang + country, family="binomial", 
                data=filter(df, fiscsol_n <5, crisman_n<5, eucit_n<5, imm<11, inc_n<5, polpos<11, trstnat <11, trsteu<11))

logodds <- modelX$linear.predictors

probabilities <- predict(modelX, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "yes", "no")

mydata <- filter(df, fiscsol_n <5, crisman_n<5, eucit_n<5, imm<11, inc_n<5, polpos<11, trstnat <11, trsteu<11, !is.na(landb))
mydata <- mydata[c("age", "trstnat")]

predictors <- colnames(mydata)

# Bind the logit and tidy the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", colour="#242424") +
  facet_wrap(~predictors, scales = "free_y") +
  theme_pubclean()
ggsave("linearityinspect.png", width=150, height=100, units="mm")

# LRT for the interaction effects
anova(model7i$glm_res, model6i$glm_res, test="LRT")
anova(model7i$glm_res, model5i$glm_res, test="LRT")
anova(model7i$glm_res, model4i$glm_res, test="LRT")

# estimate the accuracy of the model
data <- filter(dfi, !is.na(polpos) & !is.na(crisman_d) & !is.na(edu) & !is.na(inc) & !is.na(fiscsol_d))
probabilities <- predict(model1i$glm_res, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "yes", "no")
CrossTable(predicted.classes,data$fiscsol_d)

probabilities <- predict(model2i$glm_res, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "yes", "no")
CrossTable(predicted.classes,data$fiscsol_d)

probabilities <- predict(model3i$glm_res, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "yes", "no")
CrossTable(predicted.classes,data$fiscsol_d)

probabilities <- predict(model4i$glm_res, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "yes", "no")
CrossTable(predicted.classes,data$fiscsol_d)

# comparing models
anova(model2,model3,test="Chisq")

# check for multicollinearity
vif(modelt)

### plots to visualize effect of variables ====
## plotting interaction effect ----

modelt <- glm(formula=fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*wsdiff + crisman_d*gni + crisman_d*conoth, 
              weights=dfi[[2]]$PESO_TOT, 
              data=dfi[[2]], 
              family="binomial")

p1 <- plot_cap(modelt, condition=c("gni", "crisman_d"), conf_level=0.99, vcov= ~country) + 
  labs(title="", y="Predicted probability of European fiscal solidarity", x="GNI per capita\n") + 
  scale_colour_manual(values=c("black", "darkgray"), name="Responsibility\nattribution") +
  scale_fill_manual(values=c("darkgray", "gray"), name="Responsibility\nattribution") +
  scale_y_continuous(limits=c(0.5,0.9))+
  theme_pubclean() +
  theme(text = element_text(family="serif"))
p2 <- plot_cap(modelt, condition=c("wsdiff", "crisman_d"), conf_level=0.99, vcov= ~country) + 
  labs(title="", y="", x="Welfare state\neffectiveness") + 
  scale_colour_manual(values=c("black", "darkgray"), name="Responsibility\nattribution") +
  scale_fill_manual(values=c("darkgray", "gray"), name="Responsibility\nattribution") +
  scale_y_continuous(limits=c(0.5,0.9))+
  theme_pubclean() +
  theme(text = element_text(family="serif"))
p3 <- plot_cap(modelt, condition=c("conoth", "crisman_d"), conf_level=0.99, vcov= ~country) + 
  labs(title="", y="", x="Concern for others\noutside community") + 
  scale_colour_manual(values=c("black", "darkgray"), name="Responsibility\nattribution") +
  scale_fill_manual(values=c("darkgray", "gray"), name="Responsibility\nattribution") +
  scale_y_continuous(limits=c(0.5,0.9))+
  theme_pubclean() +
  theme(text = element_text(family="serif"))

ggarrange(p1,p2,p3, ncol=3, labels="AUTO", common.legend=T)
ggsave("interaction_bw.png", width=190, height=130, unit="mm")

#### TESTING AND TOYING AROUND ####

# redistribution index
red <- df %>%
  group_by(country) %>%
  summarise(red=mean(wsdiff))

#bivariate relationship between education and DK answers for fiscal solidarity
sjt.xtab(df$edu,df$fiscsol_dk, show.row.prc = TRUE, file = "fiscsoldkedu.doc")

df_num <- filter(df, country == "Netherlands") %>% select(trsteu, trstnat)
print(round(cor(df_num, use="pair"), 2))

countries <- unique(df$country)
for (land in countries){
  df_num <- filter(df, country == land) %>% select(trsteu, trstnat)
  print(round(cor(df_num, use="pair"), 2))
  print(land)
}

imp.data <- data.frame(model1b)
test <- predict(imp.data)

imputeddf <- imp$imp
augmenteddata <- augment(model3, data=data, type.predict="response")
augmenteddata <- augmenteddata %>% mutate(predfiscsol=ifelse(.fitted >0.5, 1, 0))
CrossTable(augmenteddata$fiscsol_d,augmenteddata$predfiscsol)

augmenteddata2 <- augment(model3b, data=filter(ldt,.imp=0), type.predict="response")

## graphs for PPT ----
theme_set(theme_gray(base_family = "sans"))

p1 <- df %>%
  group_by(country,fiscsol) %>%
  summarise(count=n()) %>%
  mutate(prop=count/sum(count))
p2 <- df %>%
  group_by(country) %>%
  summarise(order=mean(fiscsol_n, na.rm=T))
p <- left_join(p1, p2)
p$fiscsol <- factor(p$fiscsol, levels=c("Strongly agree", "Somewhat agree", "Don't know", "Somewhat disagree", "Strongly disagree"))

ggplot(data = p,aes(x=reorder(country, -order), y=prop, fill=fiscsol, label=paste(as.character(round(100*prop,1)), "%", sep=""))) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(type = "div", palette = "RdBu") +
  geom_text(position=position_stack(vjust=0.5), family="sans") +
  labs(y="Share of respondents", 
       x="") +
  theme_pubclean() +
  theme(text = element_text(family="sans"), legend.title = element_blank(), legend.key.width = unit(0.25, "cm"))
ggsave("fiscsol.png", width=180, height=120, units="mm")

p1 <- df %>%
  group_by(country,crisman) %>%
  summarise(count=n()) %>%
  mutate(prop=count/sum(count))
p2 <- df %>%
  group_by(country) %>%
  summarise(order=mean(crisman_n, na.rm=T))
p <- left_join(p1, p2)
p$crisman <- factor(p$crisman, levels=c("Strongly agree", "Somewhat agree", "Don't know", "Somewhat disagree", "Strongly disagree"))

ggplot(data = p,aes(x=reorder(country, -order), y=prop, fill=crisman, label=paste(as.character(round(100*prop,1)), "%", sep=""))) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(type = "div", palette = "RdBu") +
  geom_text(position=position_stack(vjust=0.5), family="sans") +
  labs(y="Share of respondents", 
       x="") +
  theme_pubclean() +
  theme(text = element_text(family="sans"), legend.title = element_blank(), legend.key.width = unit(0.25, "cm"))
ggsave("crisman.png", width=180, height=120, units="mm")


ggplot(data = filter(df, fiscsol!="Don't know" & crisman!="Don't know")) +
  geom_mosaic(aes(x=product(crisman_d), fill=fiscsol_d),show.legend=F) + 
  scale_fill_manual(values=c("#67a9cf","#ef8a62"), na.value="#f7f7f7") +
  labs(y="European solidarity", 
       x="Responsibility attribution") +
  facet_wrap(~country, nrow=2) +
  theme_mosaic()
ggsave("mosaic.png", width=240, height=120, units="mm")

p1 <- plot_model(model4i$glm_res, type="emm", ci.lvl=.99, terms=c("gni[0.2,0.5,0.8]","crisman_d")) + 
  labs(title="", y="Predicted probability of European fiscal solidarity", x="national GNI per capita,\nrelative to EU average") + 
  scale_colour_manual(values=c("#ca0020", "#0571b0"), name="Responsibility attribution") +
  scale_fill_manual(values=c("#ca0020", "#0571b0"), name="Responsibility attribution") +
  theme_pubclean() +
  theme(text = element_text(family="sans"))
p2 <- plot_model(model4i$glm_res, type="emm", ci.lvl=.99, terms=c("wsdiff","crisman_d")) + 
  labs(title="", y="", x="Welfare state effectiveness") + 
  scale_colour_manual(values=c("#ca0020", "#0571b0"), name="Responsibility attribution") +
  scale_fill_manual(values=c("#ca0020", "#0571b0"), name="Responsibility attribution") +
  theme_pubclean() +
  theme(text = element_text(family="sans"))

ggarrange(p1,p2, ncol=2, labels="AUTO", common.legend=T)
ggsave("interactionGNIdeservingness.png", width=180, height=120, unit="mm")


#### TEMPLATES ####
### Template for Controlling the Correct Assignment of Regions ====

#inspect coding for regions
geodata <- gisco_get_nuts(nuts_level="3")
mapdata <- left_join(geodata, "[regions dataset]", by="NUTS_ID")

ggplot(data=mapdata) +
  geom_sf(colour="darkgrey", aes(fill=landb)) +
  coord_sf(xlim = c(coordinates), ylim=c(coordinates), expand=FALSE) +
  geom_sf_label(aes(label=ifelse(CNTR_CODE=="[COUNTRY]",NUTS_ID, NA)))

ggplot(data=mapdata) +
  geom_sf(colour="darkgrey", aes(fill=ireg)) +
  coord_sf(xlim = c(coordinates), ylim=c(coordinates), expand=FALSE) +
  geom_sf_label(aes(label=ifelse(CNTR_CODE=="[COUNTRY]",NUTS_ID, NA)))

cor(as.numeric(df$crisman_d), as.numeric(df$fiscsol_d), use="complete.obs")

#nuts3 data

df$NUTS2 <- substr(df$NUTS_ID, 1, 4)
df$NUTS1 <- substr(df$NUTS_ID, 1, 3)

nuts3 <- get_eurostat("nama_10r_3gdp")
nuts3 <- filter(nuts3, time=="2019-01-01")
nuts3 <- filter(nuts3, unit=="PPS_EU27_2020_HAB")
nuts3$NUTS_ID <- nuts3$geo
nuts3$NUTS2 <- nuts3$geo
nuts3$NUTS1 <- nuts3$geo
nuts3$unit <- NULL
nuts3$time <- NULL
nuts3$geo <- NULL
nuts3$NUTS_ID <- NULL
nuts3$NUTS1 <- NULL
dfx <- left_join(df, nuts3)

ggplot(data=dfx) +
  geom_jitter(aes(x=country, y=values))

model1 <- glmer(fiscsol_d ~ crisman_d + gender + age_class + edu + inc + polpos + (1|NUTS2) + (1|country), 
                      weights = df$PESO_TOT, 
                      data=dfx, 
                      family="binomial",
                      nAGQ = 0) 

model2 <- glmer(fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos+ (1|NUTS2) + (1|country), 
                      weights = df$PESO_TOT, 
                      data=dfx, 
                      family="binomial",
                       nAGQ = 0)  

model3 <- glmer(fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + values + wsdiff+ (1|NUTS2) + (1|country), 
                      weights = df$PESO_TOT, 
                      data=dfx, 
                      family="binomial",
                       nAGQ = 0) 

model4 <- glmer(fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni +values + wsdiff + crisman_d*conoth+ (1|NUTS2) + (1|country), 
                      weights = df$PESO_TOT, 
                      data=dfx, 
                      family="binomial",
                      nAGQ = 0)  
    
model5 <- glmer(fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni +values + wsdiff + crisman_d*values+ (1|NUTS2) + (1|country), 
                      weights = df$PESO_TOT, 
                      data=dfx, 
                      family="binomial",
                      nAGQ = 0)  

model6 <- glmer(fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni +values + wsdiff + crisman_d*wsdiff+ (1|NUTS2) + (1|country), 
                      weights = df$PESO_TOT, 
                      data=dfx, 
                      family="binomial",
                      nAGQ = 0)  

model7 <- glmer(fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni +values + wsdiff + crisman_d*conoth + crisman_d*values + crisman_d*wsdiff+ (1|NUTS2) + (1|country), 
                      weights = df$PESO_TOT, 
                      data=dfx, 
                      family="binomial",
                       nAGQ = 0)  

screenreg(list(model1,model2, model3, model4, model5, model6, model7))  
htmlreg(list(model1, model2, model3, model4, model5, model6, model7), file="regtable_p2.doc", single.row=T, stars=0.01)


#### TRYING OUT IMPUTATION BY LOOPING ####

## start imputing missing values ----

custom_pooling <- function(formula, datframe) {
 data <- datframe
 b <- se <- NULL
 imp <- seq(1:5)
 for (i in imp) {
   model <- glm.cluster(formula = formula, 
                 weights = data[[i]]$PESO_TOT, 
                 data=data[[i]], 
                 family="binomial",
                 cluster=data[[i]]$country)
   
   b <- cbind(b, coef(model$glm_res))
   se <- cbind(se, summary(model)[,2])
 }
 
 #now pool the results
 b.pool <- apply(b, 1, mean)
 between.var <- apply(b, 1, var)
 within.var <- apply(se^2, 1, mean)
 se.pool <- sqrt(within.var+between.var+between.var/5) 
 t.pool <- b.pool/se.pool 
 pvalue.pool <- (1-pnorm(abs(t.pool)))*2 
 coefficients <- data.frame(b.pool, se.pool, t.pool, pvalue.pool)
 
 n <- nobs(model$glm_res)

 return(list(coef=coefficients,
             n=n)) 
}

m1 <- custom_pooling(datframe=dfi, formula="fiscsol_d ~ crisman_d + gender + age_class + edu + inc + polpos")
m2 <- custom_pooling("fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos")
m3 <- custom_pooling("fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff")

convertModel <- function(model) {
  tr <- createTexreg(
    coef.names = rownames(model$coef), 
    coef = model$coef$b.pool, 
    se = model$coef$se.pool, 
    pvalues = model$coef$pvalue.pool,
    gof.names = c("N"), 
    gof = c(model$n), 
    gof.decimal = c(F)
  )
}

glm.cluster(formula="fiscsol_d ~ crisman_d + gender + age_class + edu + inc + polpos", 
            data=dfi[[2]], 
            weights = dfi[[2]]$PESO_TOT, 
            family="binomial",
            cluster=dfi[[2]]$country)

mo1 <- convertModel(m1)
mo2 <- convertModel(m2)
mo3 <- convertModel(m3)
screenreg(list(mo1, mo2, mo3))
          
model <- glm.cluster(formula=fiscsol_d ~ crisman_d + gender + age_class + edu + inc + polpos, 
                     weights=df$PESO_TOT, 
                     data=df, 
                     family="binomial",
                     cluster=df$country)
modelt <- glm(formula=fiscsol_d ~ crisman_d + imm + eucit_d + conoth + gender + age_class + edu + inc + polpos + gni + wsdiff + crisman_d*wsdiff + crisman_d*gni + crisman_d*conoth, 
                     weights=dfi[[1]]$PESO_TOT, 
                     data=dfi[[1]], 
                     family="binomial")

p1 <- plot_cap(modelt, condition=c("gni", "crisman_d"), conf_level=0.99, vcov= ~country) + 
  labs(title="", y="Predicted probability of European fiscal solidarity", x="GNI per capita\n") + 
  scale_colour_manual(values=c("black", "darkgray"), name="Responsibility attribution") +
  scale_fill_manual(values=c("darkgray", "gray"), name="Responsibility attribution") +
  scale_y_continuous(limits=c(0.5,0.9))+
  theme_pubclean() +
  theme(text = element_text(family="serif"))
p2 <- plot_cap(modelt, condition=c("wsdiff", "crisman_d"), conf_level=0.99, vcov= ~country) + 
  labs(title="", y="", x="Welfare state effectiveness\n") + 
  scale_colour_manual(values=c("black", "darkgray"), name="Responsibility attribution") +
  scale_fill_manual(values=c("darkgray", "gray"), name="Responsibility attribution") +
  scale_y_continuous(limits=c(0.5,0.9))+
  theme_pubclean() +
  theme(text = element_text(family="serif"))
p3 <- plot_cap(modelt, condition=c("conoth", "crisman_d"), conf_level=0.99, vcov= ~country) + 
  labs(title="", y="", x="Concern for others\noutside community") + 
  scale_colour_manual(values=c("black", "darkgray"), name="Responsibility attribution") +
  scale_fill_manual(values=c("darkgray", "gray"), name="Responsibility attribution") +
  scale_y_continuous(limits=c(0.5,0.95))+
  theme_pubclean() +
  theme(text = element_text(family="serif"))

ggarrange(p1,p2,p3, ncol=3, labels="AUTO", common.legend=T)

