# STAT 380 Final Report
### By: Andrew Casey and Ciara Virgo

We analysed the amount of people in the states of Pennsylvania, Texas, and Washington who use Tobacco or have died from Heart Disease. 

### Markdown


```{r echo=TRUE, message=FALSE}
# always clean up R environment
rm(list = ls())

# loading packages
library(plyr)
library(mdsr)
library(tidyr)
library(data.table)
library(stringr)
library(ggplot2)
library(tibble)
library(klaR)
library(Rmixmod)

# loading in data
Heart_Raw <- fread("NCHS_-_Potentially_Excess_Deaths_from_the_Five_Leading_Causes_of_Death.csv")
Tobacco_Raw <- fread("Behavioral_Risk_Factor_Data__Tobacco_Use__2010_And_Prior_.csv")
```

#Data Wrangling

```{r}
#Filter the Smoking Data to narrow it down to 3 States and years common between the two data sets
#Also remove all, but the necessary columns
HeartData <-
  Heart_Raw %>%
  dplyr::select(Year, `Cause of Death`, State, `Age Range`, Locality, `Observed Deaths`, Benchmark) %>%
  filter(`Cause of Death` == "Heart Disease") %>%
  filter(State %in% c("Pennsylvania", "Texas", "Washington")) %>%
  filter(Year %in% c(2005, 2006, 2007, 2008, 2009, 2010)) %>%
  filter(Locality %in% c("Metropolitan", "Nonmetropolitan")) %>%
  filter(Benchmark %in% c("2005 Fixed"))

#Filter the Heart Disease Data to narrow it down to 3 states and years common between the two data sets
#Also remove all, but the necessary columns
TobaccoData <-
  Tobacco_Raw %>%
  dplyr::select(YEAR, LocationDesc, TopicType, TopicDesc, MeasureDesc, Sample_Size, Gender, Race, Age, Education) %>%
  setnames(old=c("YEAR","LocationDesc", "Age"), new=c("Year", "State", "Age Range")) %>%
  filter(State %in% c("Pennsylvania", "Texas", "Washington")) %>%
  filter(Year %in% c(2005, 2006, 2007, 2008, 2009, 2010))

#Remove the rows that contain Male and Female, so there are only rows with Overall.
Rows1 <- TobaccoData$Gender=="Overall"
TobaccoData <- TobaccoData[Rows1,]

#Remove the rows that contain All Ages for the age range
Rows2 <- TobaccoData$`Age Range`=="All Ages"
TobaccoData <- TobaccoData[Rows2==FALSE,]

#Remove the rows that have anything but All Grades as the education level
Rows3 <- TobaccoData$Education=="All Grades"
TobaccoData <- TobaccoData[Rows3,]

TobaccoData <-
  TobaccoData %>%
  dplyr::select(-c(7,8,10))

#Changes the Sample Size column to numeric variables
TobaccoData$Sample_Size <- as.numeric(gsub(",", "", TobaccoData$Sample_Size))
TobaccoData$Sample_Size <- as.numeric(as.character(TobaccoData$Sample_Size))
```

```{r}
#Changes the labels of the Age Ranges in both data sets so they are common
for(i in 1:length(HeartData$`Age Range`)){
  if(HeartData$`Age Range`[i] == "0-49"){
    HeartData$`Age Range`[i] <- c("44 Years and Below")
  }
  else if(HeartData$`Age Range`[i] == "0-54"){
    HeartData$`Age Range`[i] <- c("45 to 64 Years")
  }
  else if(HeartData$`Age Range`[i] == "0-59"){
    HeartData$`Age Range`[i] <- c("45 to 64 Years")
  }
  else if(HeartData$`Age Range`[i] == "0-64"){
    HeartData$`Age Range`[i] <- c("45 to 64 Years")
  }
  else if(HeartData$`Age Range`[i] == "0-69"){
    HeartData$`Age Range`[i] <- c("65 Years and Older")
  }
  else if(HeartData$`Age Range`[i] == "0-74"){
    HeartData$`Age Range`[i] <- c("65 Years and Older")
  }
  else if(HeartData$`Age Range`[i] == "0-79"){
    HeartData$`Age Range`[i] <- c("65 Years and Older")
  }
  else if(HeartData$`Age Range`[i] == "0-84"){
    HeartData$`Age Range`[i] <- c("65 Years and Older")
  }
}
for(i in 1:length(TobaccoData$`Age Range`)){
  if(TobaccoData$`Age Range`[i] == "18 to 24 Years"){
    TobaccoData$`Age Range`[i] <- c("44 Years and Below")
  }
  else if(TobaccoData$`Age Range`[i] == "25 to 44 Years"){
    TobaccoData$`Age Range`[i] <- c("44 Years and Below")
  }
}
```

```{r}
#Function that makes the data longer by duplicating the number of rows by the value in the sample size column and making the data set have more rows.
TobaccoCasestoRows <- function(){
  Year <- vector()
  State <- vector()
  TopicType <- vector()
  TopicDesc <- vector()
  MeasureDesc <- vector()
  AgeRange <- vector()
  for (i in 1:length(TobaccoData$Sample_Size)){
    Year <- c(Year, rep(TobaccoData$Year[i], TobaccoData$Sample_Size[i]))
    State <- c(State, rep(TobaccoData$State[i], TobaccoData$Sample_Size[i]))
    TopicType <- c(TopicType, rep(TobaccoData$TopicType[i], TobaccoData$Sample_Size[i]))
    TopicDesc <- c(TopicDesc, rep(TobaccoData$TopicDesc[i], TobaccoData$Sample_Size[i]))
    MeasureDesc <- c(MeasureDesc, rep(TobaccoData$MeasureDesc[i], TobaccoData$Sample_Size[i]))
    AgeRange <- c(AgeRange, rep(TobaccoData$`Age Range`[i], TobaccoData$Sample_Size[i]))
  }
  df <- data.frame(Year, State, TopicType, TopicDesc, MeasureDesc, AgeRange)
  return(df)
}
Tobacco <- TobaccoCasestoRows()
```

```{r}
HeartCasestoRows <- function(){
  Year <- vector()
  State <- vector()
  AgeRange <- vector()
  Locality <- vector()
  for (i in 1:length(HeartData$`Observed Deaths`)){
    Year <- c(Year, rep(HeartData$Year[i], HeartData$`Observed Deaths`[i]))
    State <- c(State, rep(HeartData$State[i], HeartData$`Observed Deaths`[i]))
    Locality <- c(Locality, rep(HeartData$Locality[i], HeartData$`Observed Deaths`[i]))
    AgeRange <- c(AgeRange, rep(HeartData$`Age Range`[i], HeartData$`Observed Deaths`[i]))
  }
  df <- data.frame(Year, State, Locality, AgeRange)
  return(df)
}
Heart <- HeartCasestoRows()
```

```{r}
Heart_age <- 
  HeartData %>%
  select(`Age Range`)

grep("[YEARS]", Heart_age, value = TRUE)
```


#Data Visualization

##Data Wrangling for plots A and B.
```{r}
#Select desired variables from Tobacco.
Tobacco_Measure <- Tobacco %>%
  select(Year, State, MeasureDesc, AgeRange)

#Adding a measure descriptor variable and selecting desired variables from Heart.
Heart_Measure <- Heart %>%
  add_column(MeasureDesc = "Heart Disease") %>%
  select(Year, State, MeasureDesc, AgeRange)
  
#Bind the two datasets together.
TobaccoHeart <- rbind(Tobacco_Measure, Heart_Measure)
```

```{r}
#Calculating percentages
#We did this so that our graphs aren't taken over by the large sample size of the Heart data set. If we don't use percentages, it would look like there were more individuals with heart disease in each state relative to their number of smokers. But in reality, the large number would just be because of the bigger sample. 

TobaccoHeartTable <- TobaccoHeart %>%
  #Count the number of unique rows.
  count(Year, State, MeasureDesc, AgeRange) %>%
  #Split momentarily into Tobacco and Heart again.
  group_by(MeasureDesc) %>%
  #Find the percentage of each unique "grouping" within each original dataset.
  mutate(Frequency = n / sum(n)) %>% 
  #Split into Tobacco and Heart but this time also split by state.
  group_by(MeasureDesc, State) %>%
  #Find the percentage of each grouping relative to the state it belongs to in the original dataset.
  mutate(FrequencyByState = n / sum(n))
```

###(A) Plot A uses both data sets.
```{r}
#Plot shows the count of people who currenlty smoke and have died from heart disease by age groups across Pennsylvania, Texas, and Washington.
ggplot(TobaccoHeartTable, aes(x = MeasureDesc, y = n, fill = MeasureDesc)) + 
  geom_boxplot() +
  facet_wrap(~ AgeRange, ncol = 3) +
  ggtitle("Number of people who currently smoke or have died from heart disease by age") +
  labs(fill = "Condition") +
  xlab("Condition") +
  ylab("Count of people")
```









```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/cvirgo/TobaccoVs.HeartDisease/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.
