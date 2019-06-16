install.packages("tidyverse")
install.packages("plotly")
install.packages("RColorBrewer")
install.packages("lubridate")
install.packages("igraph")
install.packages("rattle")
install.packages("rpart")
install.packages("rpart.plot")

library("plotly") # interactive plots
library("tidyverse")  # for ggplot/dplyr
library(lubridate) #  lubridate to work with my dates/times
library("arules")   # association rules
library("arulesViz") # visualising association rules
library("RColorBrewer") 
library(igraph)
library("rattle") # Deicsion Trees
library(rpart) # Work with rattle on decision trees
library(rpart.plot) # plotting decision trees


#read the Kaggle dataset in 
crimedata <- read.csv("Crime_Data_2010_2017.csv", header=TRUE,sep=',', stringsAsFactors = FALSE)

mo_codes <- read.csv("MO_Codes.csv", header=TRUE, sep=",")

head(crimedata) #print out the first 6 of each column to see what columns are required

df <- crimedata[,c(3:4,6,9:12)] # subset the dataset to the columns required
df <- na.omit(df) #clears all columns with NA values. 
df<- df[!(is.na(df$MO.Codes) | df$MO.Codes == ""),] # clears MO if it has no MO code in the column for later use. 
df <- df[!(is.na(df$Victim.Sex) | df$Victim.Sex == ""),]

head(df) #print out the first 6 of the selected columns


       
#convert string to timestamp format

df$Date.Occurred <- mdy(df$Date.Occurred)

crime_quarter_data <- quarter(df$Date.Occurred, with_year = TRUE)
date_occ <- df$Date.Occurred

crime_dup <- table(crime_quarter_data) # duplicated crimes within the quarter into one value of the table

crime_amount <- length(crime_quarter_data)

crimedf <- data.frame(Ints=integer())

crimedf <- as.data.frame(crime_dup)

#Renaming Column names
colnames(crimedf)[1] <- "Year.Quarter"
colnames(crimedf)[2] <- "Amount"
head(crimedf)

#Standard R Barplot does not show each quarter. 
barplot( crime_dup, crime_quarter_data, main = "Reported Crimes comitted within the quarters of each year",
         xlab = " Quarters of the Year", ylab = "Amount of Crime comitted") 

# ggplot bar chart Displaying the amount of crimes committed between 2010-2017 between each quarter. 
gg_crime_qu_yr <- ggplot(crimedf, aes(y=Amount, x=Year.Quarter) ) +
   geom_bar(stat="identity") + 
  geom_label(aes(label=Amount), stat="identity", hjust=0.5 ,size=3, show.legend = FALSE) +
  ggtitle("Crimes Committed within the quarters of the years in Los Angeles between 2010-2017") +
  xlab("Quarters of the years") +
  ylab("Amount of crimes committed") 
  

# Ggplot 2 bar chart to show the chart normally before displaying it in the inateractive plot
gg_crime_qu_yr


# Plotly for interactivity plot therefore 
#can hover to show the amount of crimes commmitted between each time

ggplotly(gg_crime_qu_yr, width=1920, height = 1080) 

# This shows that every fourth quarter there is a drop in crime between Nov-Dec

# Next we wish to look at the most common type of crime

# Look at the frequency of how often the crimes are committed to see which crime is the most often
# Put this data against the amount of crimes commited 
df$Crime.Code.Description


crime_same <- table(df$Crime.Code.Description,  year(df$Date.Occurred))
#crime_year <- year(df$Date.Occurred)
head(crime_same)

#crime_type_against_frequency <- as.data.frame( crime_same)

crime_type_against_frequency <- as.tibble(crime_same)
#Renaming Column Names
colnames(crime_type_against_frequency)[1] <- "Type"
colnames(crime_type_against_frequency)[2] <- "Year"
colnames(crime_type_against_frequency)[3] <- "Amount"

crime_type_against_frequency


head(crime_type_against_frequency)


crime_type_against_frequency <- crime_type_against_frequency[-c(1),] # Clearing Missing Values 

crime_v_freq <- crime_type_against_frequency[crime_type_against_frequency$Amount > 200,  ]
crime_v_freq <- filter(crime_v_freq, Year < 2015)
tail(crime_v_freq)
#crime_v_freq$Year <- NULL

crime_v_freq <- arrange(crime_v_freq, desc(Amount)) 
# top 20 crimes 
crime_v_freq <-  head(crime_v_freq, 20)
crime_v_freq

# ggplot bar chart Displaying the amount of crime in order of the most decreasing  
gg_crime_freq_yq <- ggplot(crime_v_freq, aes(y=Amount, x=reorder(Type, Amount), fill=Year), width = 0.1 ) +
 #geom_point(stat = "identity") +
  geom_bar(stat="identity", width = 0.8, position="dodge") +
  geom_label(aes(label=Amount), stat="identity", position = position_dodge(width = 0.8), hjust=1 ,size=3, show.legend = FALSE) +
   scale_y_discrete(limit = c(0, 6250,  12500,18750 ,  25000))+
  ggtitle("Top type of crimes between 2010 - 2014 ") +
  xlab("Type of Crime") +
  ylab("Amount of crimes committed") +
  theme(plot.title = element_text(size=13),
        axis.title = element_text(size = 14, face="bold")) + coord_flip(ylim = c(0,25000))

gg_crime_freq_yq

#ggplotly(gg_crime_freq_yq, width=1920, height = 1080) 

sort_crimes <- crime_type_against_frequency[order(crime_type_against_frequency$Amount, decreasing = TRUE),]
sort_crimes 

sort_crimes <- filter(sort_crimes, Year < 2015)


# least amount of crimes

 least_crimes <- filter(sort_crimes, Amount >0)
least_crimes <- tail(least_crimes, 20)
least_crimes

# Bar plot for the Least amount of crimes so can compare to the top amount of crimes
gg_least_crimes <- ggplot(least_crimes, aes(y=Amount, x=reorder(Type, Amount), fill = Year), width = 0.5 ) +
  #geom_point(stat = "identity") +
  geom_bar(stat="identity", width = 0.5, position="dodge") +
#  geom_text(aes(label=Amount), stat="identity",hjust = -0.5, size=3 )+
  scale_y_discrete(limit = c(0, 1, 2))+
  coord_flip() + 
  ggtitle("20 Least Amount of Crimes 2010 - 2014") +
  xlab("Type of Crime") +
  ylab("Amount of crimes committed") +
  theme(plot.title = element_text(size=14),
        axis.title = element_text(size = 14, face="bold"))

gg_least_crimes





#Dataframe as factors so it can be used in association rules 

dfar <- df[,c(4,5)]  # For  on assciotion rules but putting in sep var. so it stays at default values

dfar$Crime.Code.Description <- as.factor(dfar$Crime.Code.Description)
dfar$MO.Codes <- as.factor(dfar$MO.Codes)


dfar <- separate(dfar, MO.Codes, into = c("MO.Codes_1", "MO.Codes_2", "MO.Codes_3",
                                          "MO.Codes_4", "MO.Codes_5", "MO.Codes_6"), sep = " ")

dfar[is.na(dfar)] <- " " # puts all the NA to blank 
head(dfar)

dfar$MO.Codes_1 <- as.factor(dfar$MO.Codes_1)
dfar$MO.Codes_2 <- as.factor(dfar$MO.Codes_2)
dfar$MO.Codes_3 <- as.factor(dfar$MO.Codes_3)
dfar$MO.Codes_4 <- as.factor(dfar$MO.Codes_4)
dfar$MO.Codes_5 <- as.factor(dfar$MO.Codes_5)
dfar$MO.Codes_6 <- as.factor(dfar$MO.Codes_6)



# Filtering only Vandalism so can look at the Modus Operandi codes to see how many were affected by the assault



modus_rules <- filter(dfar,Crime.Code.Description %in% 
                        c("VANDALISM - MISDEAMEANOR ($399 OR UNDER)",
                          "VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS) 0114" ))



head(modus_rules)  
  
set.seed(13)
tdata <- as(modus_rules, "transactions")
rules <- apriori(tdata, parameter=list(support=0.2, confidence=0.7))

inspect(head(rules))


rules_conf <- sort(rules, by="confidence", decreasing = TRUE)
inspect(head(rules_conf))


freqcrimeData <- as(dfar,"transactions")

frequentCrimes <- eclat(freqcrimeData, parameter = list( supp = 0.05, maxlen=10))
inspect(head(frequentCrimes))

plot(frequentCrimes)

plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading="confidence", control=list(main ="Two-key plot"))
subrules <- sample(rules_conf,25)

ig <- plot(subrules, method="graph", control=list(type="items"))

ig_df <- get.data.frame(ig, what = "both")
visNetwork::visNetwork(
  nodes = data.frame(
    id = ig_df$vertices$name
    ,value = ig_df$vertices$confidence
    ,title = ifelse(ig_df$vertices$label == "",ig_df$vertices$name, ig_df$vertices$label)
    ,ig_df$vertices
  )
  , edges = ig_df$edges
) %>%
  visNetwork::visEdges( arrows = "to" ) %>%
  visNetwork::visOptions( highlightNearest = T )


plot(rules_conf)
plot(rules_conf, measure = c("support", "lift"), shading = "confidence")






# Decision Trees
dt <- df[,c(4,6,7)]
head(dt)

# I was thinking of filtering to only do the vandalism for my decision tree. However, I then used an age filter instead. 

#dt_filter <- filter(dt,Crime.Code.Description %in%  
# c("VANDALISM - MISDEAMEANOR ($399 OR UNDER)", "VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS) 0114" ))

dt_age_filter <- filter(dt, Victim.Age > 18, Victim.Sex %in% c("F","M"))#  Filtering only victims above the age of 18 and male and females 

str(dt_age_filter)
#sub_tree <- sample_n(dt_age_filter, 790000)
index_dt <- sample(nrow(dt_age_filter), 0.7 * nrow(dt_age_filter)) # 70% for train
dt_train <- dt_age_filter[index_dt,]
dt_test <- dt_age_filter[-index_dt,]


tree <- rpart(Crime.Code.Description ~., data=dt_train, method="class", control=rpart.control(minsplit =3 , maxdepth=3,  cp = 0.0001) )
fancyRpartPlot(tree, type = 5, main = "Most common crimes per victim age groups and genders of male and female", sub = "")


dt_pred <- predict(tree, newdata=dt_test, type='class')
dt_table<- table(dt_pred, dt_test$Victim.Age, dnn = c("Crimes", "Ages"))
table(dt_pred, dt_test$Victim.Sex)
print(dt_table)
tree_test <- rpart(Crime.Code.Description ~., data=dt_test, method="class", control=rpart.control(minsplit =2, maxdepth= 3 , cp = 0.001) )
fancyRpartPlot(tree_test, type = 5, main = "Most common crimes per victim age groups and genders of male and female", sub = "")



