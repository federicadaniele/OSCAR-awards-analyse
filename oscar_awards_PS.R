# setting the directory ----
setwd('D:/Dati/Profili/m031988/Downloads/')

library(readr)
library(data.table)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(rvest)
library(jpeg)
library(randomForest)
require(caTools)

# loading datasets ----

title_prin <- read_csv("title_prin.csv",col_names = FALSE)
colnames(title_prin) <- paste(c("tconst","ordering","nconst","category","job","characters"))

name_basic <- read_csv("name_basics.csv",col_names = FALSE)
colnames(name_basic) <- paste(c("nconst","primaryName","birthYear","deathYear","primaryProfession","knownForTitles"))

title_basic <- read_csv("title_basics.csv",col_names = FALSE)
colnames(title_basic) <- paste(c("tconst","titleType","primaryTitle","originalTitle","isAdult","startYear","endYear","runtimeMinutes","genres"))
                        
title_rating <- read_tsv("title.ratings.tsv.gz") 

oscar <- read.csv("firstoscars.csv")
nominees <- read.csv("the_oscar_award.csv")

# view first and last year of oscars
min(oscar$oscar_yr,na.rm=T)
max(oscar$oscar_yr,na.rm=T)

# data manipulation ----

# subset the data
nominees <- nominees[nominees$category=="ACTOR IN A LEADING ROLE"|nominees$category=="ACTRESS IN A LEADING ROLE"|nominees$category=="ACTOR"|nominees$category=="ACTRESS",]
title_prin <- title_prin[title_prin$category=="actor"|title_prin$category=="actress",]
# replace a given value
oscar$name[oscar$name=="Leonardo Di Caprio"] <- "Leonardo DiCaprio"
oscar$name[oscar$name=="Elisabeth Taylor"] <- "Elizabeth Taylor"
nominees$name[nominees$name=="Leonardo Di Caprio"] <- "Leonardo DiCaprio"
nominees$name[nominees$name=="Elisabeth Taylor"] <- "Elizabeth Taylor"
names(nominees)
nominees <- nominees %>% rename(primaryTitle = names(.)[6])
colnames <- paste(c("name","winner","primaryTitle"))
nominees <- subset(nominees,select=colnames)

# merge info on actors/actresses to dataset with cast per each movie
combo <- merge(title_prin,name_basic,all=FALSE)

# rename variable
names(combo)
combo <- combo %>% rename_(name = names(.)[7])

save(combo,file = "combo.RData")

load("combo.RData")

# carry out the analysis on oscar winners, keep only matching records
combo_oscar <- merge(combo,oscar,all=FALSE)

# check how many actors/actresses
nrow(distinct(oscar["name"]))
nrow(distinct(combo_oscar["name"]))

combo_oscar <- merge(combo_oscar,title_basic,all=FALSE)

# inspect data type
sapply(combo_oscar, typeof)

# create variable after convert to numeric format
combo_oscar$startYear <- as.numeric(combo_oscar$startYear)
sapply(combo_oscar, typeof)
combo_oscar$age <-  combo_oscar$startYear - combo_oscar$birth_y
combo_oscar$age_oscar <-  combo_oscar$oscar_yr - combo_oscar$birth_y
sum(is.na(combo_oscar$age)==TRUE)

# tabulate categorical variable
table(combo_oscar$titleType)

# remove duplicates
cols <- paste(c("tconst","name","nconst","category","birthYear","deathYear","knownForTitles","oscar_no","oscar_yr","award","movie","age","birth_pl","birth_y","titleType","primaryTitle","originalTitle","startYear","genres","age_oscar"))
combo_oscar <- combo_oscar[,cols]
combo_oscar <- distinct(combo_oscar)
combo_oscar <- combo_oscar %>% group_by(name,primaryTitle) %>% mutate(count = n())
table(combo_oscar$count)
combo_oscar$aux <- 1
combo_oscar <- combo_oscar %>% group_by(name,primaryTitle) %>% mutate(ticker = cumsum(aux))
combo_oscar <- combo_oscar[combo_oscar$ticker==1,]
nrow(distinct(combo_oscar["name"]))
combo_oscar <- subset(combo_oscar,select=-c(count,aux,ticker))

# merge movies file with nomination file
combo_oscar_nominees <- merge(combo_oscar,nominees,all.x=TRUE)
nrow(distinct(combo_oscar_nominees["name"]))

# create dummy for nomination
combo_oscar_nominees <- combo_oscar_nominees %>% mutate(nominated = ifelse(is.na(winner)==F,1,0))
# create dummy for award
combo_oscar_nominees <- combo_oscar_nominees %>% mutate(awarded = ifelse(winner=="True",1,0))
table(combo_oscar_nominees$nominated)
table(combo_oscar_nominees$awarded)

# create experience variable
combo_oscar_nominees <- combo_oscar_nominees %>% group_by(name) %>% mutate(firstmovie = min(startYear,na.rm =TRUE))
combo_oscar_nominees$exp <-  combo_oscar_nominees$startYear - combo_oscar_nominees$firstmovie
combo_oscar_nominees$exp_oscar <-  combo_oscar_nominees$oscar_yr - combo_oscar_nominees$firstmovie

# peer effects
combo_oscar_nominees$alreadywinner <- 0
combo_oscar_nominees$alreadywinner[combo_oscar_nominees$startYear>combo_oscar_nominees$oscar_yr] <- 1
combo_oscar_nominees$sex <- c("Uomini")
combo_oscar_nominees$sex[combo_oscar_nominees$award=="Best actress"] <- c("Donne")
combo_oscar_nominees$aux <- 0
combo_oscar_nominees$aux[combo_oscar_nominees$sex==c("Donne") & combo_oscar_nominees$alreadywinner==1] <- 1
combo_oscar_nominees <- combo_oscar_nominees %>% group_by(primaryTitle) %>% mutate(womanalreadywinner = max(aux))
combo_oscar_nominees$aux <- 0
combo_oscar_nominees$aux[combo_oscar_nominees$sex==c("Uomini") & combo_oscar_nominees$alreadywinner==1] <- 1
combo_oscar_nominees <- combo_oscar_nominees %>% group_by(primaryTitle) %>% mutate(manalreadywinner = max(aux))
combo_oscar_nominees$sexnum <- 1
combo_oscar_nominees$sexnum[combo_oscar_nominees$award=="Best actress"] <- 2

# data visualisation ----

# 1. age and experience analysis
cols <- paste(c("name","sex","age_oscar","exp_oscar","birth_y","birth_pl"))
onlyactors <- combo_oscar_nominees[,cols]
onlyactors <- distinct(onlyactors)

# a. histogram of age at which won the oscar by gender 
hist(as.numeric(unlist(onlyactors[onlyactors$sex=="Donne","age_oscar"])), col=rgb(1,0,0,0.5), xlab="Età", 
     ylab="N (numero di attori/attrici)", main="Età al momento del primo Oscar" )
hist(as.numeric(unlist(onlyactors[onlyactors$sex=="Uomini","age_oscar"])), col=rgb(0,0,1,0.5), add=T)
legend("topright", legend=c("Donne","Uomini"), col=c(rgb(1,0,0,0.5), 
                                                     rgb(0,0,1,0.5)), pt.cex=2, pch=15 )

# b. average age and experience by gender
onlyactors %>% group_by(sex) %>% summarise(average = mean(age_oscar))
onlyactors %>% group_by(sex) %>% summarise(youngest = min(age_oscar))

# c. youngest winner by gender
onlyactors <- onlyactors %>% group_by(sex) %>% mutate(youngest = min(age_oscar))
print(onlyactors[onlyactors$youngest==onlyactors$age_oscar,"name"])
print(distinct(combo_oscar_nominees[combo_oscar_nominees$name=="Adrien Brody"|combo_oscar_nominees$name=="Marlee Matlin","movie"]))

# d. average age by cohort 
onlyactors$cohort <- c("1860-1900")
onlyactors$cohort[onlyactors$birth_y>=1900 & onlyactors$birth_y<1920 & is.na(onlyactors$birth_y)==F] <- c("1900-1920")
onlyactors$cohort[onlyactors$birth_y>=1920 & onlyactors$birth_y<1940 & is.na(onlyactors$birth_y)==F] <- c("1920-1940")
onlyactors$cohort[onlyactors$birth_y>=1940 & onlyactors$birth_y<1960 & is.na(onlyactors$birth_y)==F] <- c("1940-1960")
onlyactors$cohort[onlyactors$birth_y>=1960 & onlyactors$birth_y<1980 & is.na(onlyactors$birth_y)==F] <- c("1960-1980")
onlyactors$cohort[onlyactors$birth_y>=1980 & onlyactors$birth_y<2000 & is.na(onlyactors$birth_y)==F] <- c("1980-2000")

df <- onlyactors %>% group_by(sex,cohort) %>% summarise(average_age_oscar = mean(age_oscar))
ggplot(data=df, aes(x=cohort, y=average_age_oscar, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge())+
  xlab("Anno di nascita")+ 
  ylab("Età al momento del primo Oscar")

# e. tabulate nationalities that have won more oscars
df <- onlyactors %>% group_by(birth_pl) %>% summarise(count = n())
df <- df[order(-df$count),]
head(df,10)

# 2. career analysis
# a. number of movies by age
df <- combo_oscar_nominees %>% group_by(sex,age) %>% summarise(count_movie = n())
df <- df[df$age>15 & df$age<85 & is.na(df$age)==F,]
ggplot(data=df, aes(x=titleType, y=count_movie_40, fill=name)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Età")+ 
  ylab("Numero di produzioni")

# b. actor/actress with most movies
df <- combo_oscar_nominees %>% group_by(name) %>% summarise(count_movie = n())
onlyactors <- merge(onlyactors,df)
setorder(setDT(onlyactors %>%  group_by(sex) %>% top_n(1, count_movie)), -count_movie)[, head(.SD, 5L), keyby = sex]

img1<-readJPEG("susiesarandon.jpg")
img2<-readJPEG("jamesstewart.jpg")
plot(1, type="n", xlim=c(100, 200), ylim=c(300, 350),xlab="",ylab="")
par(xaxt='n')
par(yaxt='n')
image <- as.raster(matrix(0:1, ncol = 2, nrow = 1))
rasterImage(img1,100, 300, 150, 350)
rasterImage(img2,150, 300, 200, 350)

# c. actor/actress with most movies by the age of 40s
df <- combo_oscar_nominees %>% group_by(name) %>% summarise(count_movie_40 = sum(age<40,na.rm=T))
onlyactors <- merge(onlyactors,df)
setorder(setDT(onlyactors %>%  group_by(sex) %>% top_n(1, count_movie_40)), -count_movie_40)[, head(.SD, 5L), keyby = sex]

# d. distribution of production by movie types
df <- combo_oscar_nominees %>% group_by(name,titleType) %>% summarise(count_movie_40 = sum(age<40,na.rm=T))
df <- df[df$name=="Susan Sarandon"|df$name=="Jean Dujardin",]
ggplot(data=df, aes(x=titleType, y=count_movie_40, fill=name)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# e. actor/actress with most nominations
df <- combo_oscar_nominees %>% group_by(name) %>% summarise(count_nomin = sum(nominated))
onlyactors <- merge(onlyactors,df)
setorder(setDT(onlyactors %>%  group_by(sex) %>% top_n(1, count_nomin)), -count_nomin)[, head(.SD, 5L), keyby = sex]

# f. activity post-oscar winning (counterintuitive)
df <- combo_oscar_nominees %>% group_by(name,startYear) %>% summarise(count_movie = n(),count_nomin = sum(nominated), sexnum= mean(sexnum), oscar_yr = mean(oscar_yr))
df$newtime <- df$startYear - df$oscar_yr
df <- df[df$newtime >-10 & df$newtime<10 & is.na(df$newtime)==F,] 
df <- df %>%  group_by(sexnum,newtime) %>% summarise(count_movie = mean(count_movie),count_nomin = mean(count_nomin))
df$sex <- c("Uomini")
df$sex[df$sexnum==2] <- c("Donne")
ggplot(data=df, aes(x=newtime, y=count_movie, fill=sex)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_continuous(name="Anni rispetto al (primo) Oscar", limits=c(-10, 10))

# 3. rating analysis
combo_oscar_nominees_rat <- merge(combo_oscar_nominees,title_rating,all.x=TRUE)

# a. average rating by movie type
combo_oscar_nominees_rat %>% group_by(titleType) %>% summarise(averagereview = mean(averageRating,na.rm=T),countreview=mean(numVotes,na.rm=T))
df <- combo_oscar_nominees_rat %>% group_by(titleType,startYear) %>% summarise(averagereview = mean(averageRating,na.rm=T),countreview=mean(numVotes,na.rm=T))
df <- ts(df[df$titleType=="movie" & df$startYear<=2020,paste(c("averagereview","countreview"))],start=1905)

# b. average rating and count of reviews over time
autoplot(df)

# c. rating of nominated/awarded movies
combo_oscar_nominees_rat$winner[is.na(combo_oscar_nominees_rat$winner)==T] <- c("Not nominated")
combo_oscar_nominees_rat$nominatedlabel <- c("Not nominated")
combo_oscar_nominees_rat$nominatedlabel[combo_oscar_nominees_rat$nominated==1] <- c("Nominated")
combo_oscar_nominees_rat %>% group_by(sex,nominatedlabel) %>% summarise(averagereview = mean(averageRating,na.rm=T),countreview=mean(numVotes,na.rm=T))
combo_oscar_nominees_rat %>% group_by(sex,winner) %>% summarise(averagereview = mean(averageRating,na.rm=T),countreview=mean(numVotes,na.rm=T))

# 4. prediction with random forest: the goal is to check 1) how accurate is the model and 2) importance of different features at predicting oscar winning given nomination ----

# a. prepare variables for RF
combo_oscar_nominees_rat <- merge(combo_oscar_nominees,title_rating,all.x=TRUE)

combo_oscar_nominees_rat <- combo_oscar_nominees_rat[order(combo_oscar_nominees_rat$name,combo_oscar_nominees_rat$startYear),]

combo_oscar_nominees_rat$aux <- 1
combo_oscar_nominees_rat <- combo_oscar_nominees_rat %>% group_by(name) %>% mutate(counter = cumsum(aux))
combo_oscar_nominees_rat$movie <- 0
combo_oscar_nominees_rat$movie[combo_oscar_nominees_rat$titleType=="movie"] <- 1
combo_oscar_nominees_rat <- combo_oscar_nominees_rat %>% group_by(name) %>% mutate(counter_movie = cumsum(movie))

combo_oscar_nominees_rat <- combo_oscar_nominees_rat %>% group_by(name) %>% mutate(counter_nomin = cumsum(nominated))

combo_oscar_nominees_rat$productivity <- combo_oscar_nominees_rat$counter/combo_oscar_nominees_rat$age

combo_oscar_nominees_rat$cohort <- c("1860-1900")
combo_oscar_nominees_rat$cohort[combo_oscar_nominees_rat$startYear>=1900 & combo_oscar_nominees_rat$startYear<1920 & is.na(combo_oscar_nominees_rat$startYear)==F] <- c("1900-1920")
combo_oscar_nominees_rat$cohort[combo_oscar_nominees_rat$startYear>=1920 & combo_oscar_nominees_rat$startYear<1940 & is.na(combo_oscar_nominees_rat$startYear)==F] <- c("1920-1940")
combo_oscar_nominees_rat$cohort[combo_oscar_nominees_rat$startYear>=1940 & combo_oscar_nominees_rat$startYear<1960 & is.na(combo_oscar_nominees_rat$startYear)==F] <- c("1940-1960")
combo_oscar_nominees_rat$cohort[combo_oscar_nominees_rat$startYear>=1960 & combo_oscar_nominees_rat$startYear<1980 & is.na(combo_oscar_nominees_rat$startYear)==F] <- c("1960-1980")
combo_oscar_nominees_rat$cohort[combo_oscar_nominees_rat$startYear>=1980 & combo_oscar_nominees_rat$startYear<2000 & is.na(combo_oscar_nominees_rat$startYear)==F] <- c("1980-2000")

df <- combo_oscar_nominees_rat
df <- subset(combo_oscar_nominees_rat,select=c("name","awarded","age","exp","sexnum","startYear","movie","genres","womanalreadywinner","manalreadywinner","cohort","birth_pl","counter","counter_movie","counter_nomin","productivity","averageRating"))

# keep only movies up to the oscar winning one:
df$awarded[df$awarded!=1|is.na(df$awarded)==T] <- 0
df$aux1 <- df$counter*df$awarded
df$aux1[df$aux1==0] <- 1000
df <- df %>% group_by(name) %>% mutate(aux2 = min(aux1))
df <- df[df$counter<=df$aux2,]
df <- subset(df,select=-c(aux1,aux2))

length(unique(df$genres))
length(unique(df$birth_pl))

df <- df %>% separate(genres, c("aux1", "aux2", "aux3"), ",")
df$action <- 0
df$action[df$aux1=="Action"|df$aux2=="Action"|df$aux3=="Action"] <- 1
df$comedy <- 0
df$comedy[df$aux1=="Comedy"|df$aux2=="Comedy"|df$aux3=="Comedy"] <- 1
df$drama <- 0
df$drama[df$aux1=="Drama"|df$aux2=="Drama"|df$aux3=="Drama"] <- 1
df$crime <- 0
df$crime[df$aux1=="Crime"|df$aux2=="Crime"|df$aux3=="Crime"] <- 1
df <- subset(df,select=-c(aux1,aux2,aux3,name))

df$country <- c("USA")
df$country[df$birth_pl==c("England")|df$birth_pl==c("Wales")] <- c("UK")
df$country[df$birth_pl==c("Italy")|df$birth_pl==c("Belgium")|df$birth_pl==c("France")|df$birth_pl==c("Switzerland")|df$birth_pl==c("Sweden")|df$birth_pl==c("Germany")|df$birth_pl==c("Austria")|df$birth_pl==c("Japan")|df$birth_pl==c("Israel")] <- c("OECD (non ex-USSR")
df$country[df$birth_pl==c("Australia")|df$birth_pl==c("New Zealand")|df$birth_pl==c("Canada")] <- c("AUS/NZL/CAN")
df$country[df$birth_pl==c("South Africa")|df$birth_pl==c("India")|df$birth_pl==c("Russia")|df$birth_pl==c("Hungary")|df$birth_pl==c("Poland")] <- c("Others")

# last step: remove NAs and select variables
df <- na.omit(df)
df$awarded <- as.factor(df$awarded)
vars <- c("awarded","age","exp","movie","womanalreadywinner","manalreadywinner","cohort","country","counter","counter_movie","counter_nomin","productivity","averageRating","drama","comedy","crime")
dfwomen <- df[df$sexnum==2,vars]
dfmen <- df[df$sexnum==1,vars]

sapply(dfwomen,typeof)

# c. run algorithm
sample <- sample.split(dfwomen$awarded, SplitRatio = .75)
train <- subset(dfwomen, sample == TRUE)
test  <- subset(dfwomen, sample == FALSE)

rfwomen <- randomForest(
  awarded ~ .,
  data=train,
  importance = TRUE
)

sample <- sample.split(dfmen$awarded, SplitRatio = .75)
train <- subset(dfmen, sample == TRUE)
test  <- subset(dfmen, sample == FALSE)

rfmen <- randomForest(
  awarded ~ .,
  data=train,
  importance = TRUE
)

print(rfwomen)
varImpPlot(rfwomen,n.var=15,type=1)

print(rfmen)
varImpPlot(rfmen,n.var=15,type=1)
