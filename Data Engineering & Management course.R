library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

## Task 1
df = read.csv("/Users/sagitokishev/Downloads/appstore_games.csv")

## Task 2
df = subset(df, select = -c(URL, Name)) #Deleting two columns: "URL & Name"

df = df %>%
  mutate(Subtitle_binary = ifelse(nchar(Subtitle) > 0, 1, 0)) #Counting the number of 
#characters in each entry  and then assigning the binary format

df = select(df, -Subtitle) #Deleting the original Subtitle column

df = select(df, -Icon.URL) #Deleting the Icon.URL column

df$IAP = strsplit(df$In.app.Purchases, ",")

df$IAP_num = lengths(df$IAP)

df$IAP = sapply(df$IAP, function(x) as.numeric(ifelse(x == "", NA, x)))

df$IAP_min = sapply(df$IAP, min)
df$IAP_max = sapply(df$IAP, max)
df$IAP_sum = sapply(df$IAP, sum)
df$IAP_avg = sapply(df$IAP, mean)

df$IAP_min[is.infinite(df$IAP_min)] = NA
df$IAP_max[is.infinite(df$IAP_max)] = NA
df$IAP_sum[df$IAP_sum == 0] = NA
df$IAP_avg[is.nan(df$IAP_avg)] = NA

df = df %>%
  mutate(words_num = lengths(strsplit(Description, " ")))

df = select(df, -Description)

## Grouping by Developer column and ugrouping it so that new table is not initialised
df = df %>%
  group_by(Developer) %>%
  mutate(num_games = n()) %>%
  ungroup()

## Counting the number of games 
df = df %>%
  mutate(GameCount = ifelse(num_games > 3, "professional", "newbie"))

unique(df$Age.Rating) ## Checking the unique values of the column
df$age_rating = ifelse(df$Age.Rating == "4+", "4+", "9+")


for (i in 1:nrow(df)) {
  
  ## Check if the Languages column is empty
  if (df$Languages[i] == "") {
    df$num_languages[i] = NA
    df$Is_Available_in_English[i] = NA
  } else {
    
    ## Split the Languages string by comma and count the number of languages
    df$num_languages[i] <- ifelse(lengths(strsplit(df$Languages[i], ", ")) == 1, "single", "many")
    
    ## Check if English is available in the Languages
    df$Is_Available_in_English[i] <- ifelse(grepl("EN", df$Languages[i]), "Yes", "No")
  }
}


df = df %>%
  mutate(Genres_num = lengths(strsplit(Genres, ", ")))

## Getting unique attributes of Genres first to understand how many of them are there
## tolower is used to get genre types in lower case as strsplit didn't work before that
genres_all = unique(unlist(strsplit(tolower(df$Genres), ", ")))

## Looping through the list of unique genres and checking if the row contains one of them
## and thus assigning a number 1 or 0
for (genre in genres_all) {
  df[genre] = as.integer(str_detect(tolower(df$Genres), genre))
}

## Spliting by / symbol and selecting the second index as a month
df$Release_month = str_split(df$Original.Release.Date, "/") %>%
  sapply(function(x) x[2])

## Converting the columns to standard in R date format
df$Original.Release.Date = dmy(df$Original.Release.Date)
df$Current.Version.Release.Date = dmy(df$Current.Version.Release.Date)

## Variable to store the examined data 
examined_date = dmy("03/08/2019")

## Calculating the interval in a month format between release date and examined date
df$months_difference = sapply(df$Current.Version.Release.Date, function(x) {
  months_interval = interval(x, examined_date) %/% months(1)
})

## Checking if there a value inside In.app.Purchases column and assigning the value for Game_Free column accordingly
df = df %>%
  mutate(Game_Free = ifelse(nchar(In.app.Purchases) == 0, 1, 0))

## Variable for storing median number
med_rat_count = median(df$User.Rating.Count, na.rm = TRUE)

## Comparing rating count versus the median number and accordingly assigning the binary value
df$Categorical_Rating_Count = ifelse(df$User.Rating.Count < med_rat_count, "Low", "High")


