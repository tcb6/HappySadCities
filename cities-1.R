# Packages
# Packages
packages <- c("twitteR", "openssl", "rvest", "dplyr", "XML","magrittr","tidyr","stringr","rex","plyr","qdapRegex","SnowballC","tm","tidytext","rworldmap","maptools","maps","ggmap")

### checking if packages are already installed and installing if not
for(i in packages){
  if(!(i %in% installed.packages()[, "Package"])){
    install.packages(i)
  }
  library(i, character.only = TRUE) ## load packages
}

#import packages
lapply(packages, library, character.only = TRUE)

# Read in the cities using css selector.. 
states <- read_html("http://www.city-data.com/") %>% html_nodes(".cities_list") %>% html_text()
states
# Truncate to just one copy of each state.
states <- states[53:103]
states

# Setup a session using the website.
session <- html_session("http://www.city-data.com/")

# CLEAR PICKEDCITIES BEFORE LOOPING TO GET THE RIGHT NUMBER OF CITIES
pickedCities <- data.frame()
table <- ""

# Loop through each state and get the table.
for (i in states[1:length(states)]) {
  
  # THE I OF TOUBLESHOOTING
  # i <- "California"
  
  
  # Follow state page
  page <- session %>% follow_link(i) %>% read_html()
  
  # Get cities
  
  i <- gsub('\\s+', '-', i)
  
  nodes <- html_nodes(page, xpath=sprintf('//td/a[contains(@href,"-%s.html")]', i))
  nodesBold <- html_nodes(page, xpath=sprintf('//td/b/a[contains(@href,"-%s.html")]', i))
  nodes <- html_attr(nodes, "href")
  nodesBold <- html_attr(nodesBold, "href")
  
  # Combine nodes
  stateNodes <- append(nodes, nodesBold)
  stateNodes
  
  # Get rid of West Virginia during Virginia.
  if (i == "Virginia")
    stateNodes <- stateNodes[4:174]
  
  # Sort nodes
  stateNodesDF <- data.frame(stateNodes)
  stateNodesDF <- stateNodesDF[order(stateNodesDF$stateNodes),]
  stateNodesDF
  
  # Store the table.
  table <- page %>% html_node("#cityTAB") %>% html_table()
  table
  
  # Skip Urban Honolulu.
  if (i == "Hawaii"){
    table <- table[!(table$Name=="Urban Honolulu, HI"),]
    stateNodesDF[49] <- NA
  }
  
  stateNodesDF <- na.omit(stateNodesDF)
  stateNodesDF
  
  # Fix a few city links
  if (i == "Arizona"){
    tempNode <- stateNodesDF[88]
    stateNodesDF[88] <- stateNodesDF[89]
    stateNodesDF[89] <- tempNode
    
    stateNodesDF
    
    tempNode
    stateNodesDF[88]
    stateNodesDF[89]
  }
  else if (i == "California"){
    tempNode <- stateNodesDF[529]
    stateNodesDF[529] <- stateNodesDF[531]
    stateNodesDF[531] <- tempNode
    
    tempNode
    stateNodesDF[529]
    stateNodesDF[531]
  }
  
  # Get Rid of states with less than 6000 pop
  table$Population <- as.numeric(gsub(",","",table$Population))
  table<-table[!(table$Population<=6000),]
  
  # Link States with URLs
  table$link <- stateNodesDF
  table
  
  # Sort and store.
  table <- table[order(table$Population, decreasing = TRUE),]
  top3Table <- table[1:3,]
  pickedCities <- rbind( pickedCities,top3Table)
}

pickedCities

# remove the two NA from trying to get more than one city in dc
pickedCities <- pickedCities[-c(27,26), ]



#setting up session to loop through all the cities to pull their data

session <- html_session("http://www.city-data.com/")


unemploymentDF <- data.frame()
densityDF <- data.frame()
crimeDF <- data.frame()
LatLongDF<- data.frame(data.frame(matrix(ncol = 2)))
livingCostDF <- data.frame()
medianIncomeDF <- data.frame()
numHospitalsDF <- data.frame()
areaDF <- data.frame()
librariesDF <- data.frame()
stadiumsDf <- data.frame()
for(i in 1:151){
  
  session <- sprintf('http://www.city-data.com/city/%s', pickedCities[i,4])
  session
  
  pickedCities[i,4]
  
  #get nodes
  page <- session %>% read_html()
  nodes2 <-html_nodes(page, xpath='//*[@id="education-info"]/ul/li[4]')
  nodesDensity <- html_nodes(page, xpath='//*[@id="population-density"]/p[2]')
  nodesCrimeRateIndex <- html_nodes(page, xpath='//*[@id="crime"]/div[1]/table/tfoot/tr/td[13]')
  nodesCostOfLiving <- html_nodes(page, xpath='//*[@id="cost-of-living-index"]')
  nodesMedianIncome <-html_nodes(page, xpath='//*[@id="median-income"]')
  nodesHospitals <- html_nodes(page, xpath='//*[@id="hospitals"]')
  nodesArea <- html_nodes(page, xpath='//*[@id="population-density"]/p[1]')
  nodesLibraries <- html_nodes(page, xpath='//*[@id="libraries"]/div')
  nodesStadiums <- html_nodes(page, xpath='//*[@id="arenas"]/div')
  
  numLibraries <- length(xml_children(xml_children(nodesLibraries)))
  numHospitals <- length(xml_children(xml_children(nodesHospitals)))
  numStadiums <- length(xml_children(xml_children(nodesStadiums)))
  
  # Reformat
  nodes2 <-sub(".*</b> *(.*?) *</li>\n*", "\\1", nodes2)
  nodesDensity <- gsub(",", "", nodesDensity)
  nodesDensity <- gsub("*([0-9]+).*$", "\\1", nodesDensity)
  nodesDensity <- gsub("[^0-9]", "", nodesDensity)
  nodesCostOfLiving <- gsub("[^0-9.]", "", nodesCostOfLiving)
  nodesCostOfLiving <-sub (".*.2016 *(.*?) *..100*", "\\1", nodesCostOfLiving)
  income <- rm_between(nodesMedianIncome, ":</b> $", " (<b>it was</b>", extract=TRUE)
  income <- unlist(income)          #this for some reason returns a list of numbers, but the first number is the value we wanted so we need to unlist it to get that first value
  

  nodesCrimeRateIndex <-sub (".*<td> *(.*?) *</td>\n*", "\\1", nodesCrimeRateIndex)
  nodesArea <-sub(".*</b> *(.*?) *<b>square miles.</b> </p>*", "\\1", nodesArea)
  
  # If no crimerate index, set to US average
  if (length(nodesCrimeRateIndex) == 0)
    nodesCrimeRateIndex = 280.5
  
  
  
  #getting latitude and longitude
  nodesLoc <-html_nodes(page, xpath='//*[@id="coordinates"]/p')
  nodesLoc
  lat <- rm_between(nodesLoc, "<p><b>Latitude:</b> ", "N<b>,", extract=TRUE)
  long <- rm_between(nodesLoc, "Longitude:</b> ", " W</p>", extract=TRUE)
  LatLongDF[i,1] <- lat
  LatLongDF[i,2] <- as.numeric(long) * -1
  
  
  unemploymentDF[i,1] <- nodes2
  densityDF[i, 1] <- nodesDensity
  crimeDF[i,1] <- nodesCrimeRateIndex
  livingCostDF[i,1] <- nodesCostOfLiving
  medianIncomeDF[i,1] <- income[1]
  numHospitalsDF[i,1] <- numHospitals
  areaDF[i,1] <- nodesArea
  librariesDF[i,1] <- numLibraries
  stadiumsDf[i,1] <- numStadiums
  
  
  
  
 
  
  
  
  
  
  #not working for all cities
  
  #html_nodes(page, xpath='//*[@id="businesses-count-table"]') %> html_table()
  
  
  #
  # amentities <- htmltab(doc = session, which = '//*[@id="businesses-count-table"]', encoding = "UTF-8")
  # amentities <- cbind(c(amentities[,1],amentities[,3]), c(amentities[,2],amentities[,4]))
  # amentities[!rowSums(!is.finite(amentities)),]
  #
  #
  # specialtyCoffee <- 0
  #
  # for(j in 1:(nrow(amentities))){
  #   if(is.na(amentities[j,1])){
  #     break
  #   }
  #   if(amentities[j,1] == "Starbucks" || amentities[j,1] == "Dunkin Donuts"){
  #     specialtyCoffee <- as.numeric(specialtyCoffee) + as.numeric(amentities[j,2])
  #   }
  #
  # }
  # specialtyCoffeeDF[j,1] <- specialtyCoffee
  #
  
  
  
}

unemploymentDF
densityDF
crimeDF
LatLongDF
livingCostDF
numHospitalsDF
areaDF
librariesDF
stadiumsDf
medianIncomeDF

# Combine data.frames
pickedCities <- cbind(pickedCities, unemploymentDF)
pickedCities <- cbind(pickedCities, areaDF)
pickedCities <- cbind(pickedCities, densityDF)
pickedCities <- cbind(pickedCities, crimeDF)
pickedCities <- cbind(pickedCities, LatLongDF)
pickedCities <- cbind(pickedCities, livingCostDF)
pickedCities <- cbind(pickedCities, medianIncomeDF)
pickedCities <- cbind(pickedCities, numHospitalsDF)
pickedCities <- cbind(pickedCities, librariesDF)
pickedCities <- cbind(pickedCities, stadiumsDf)

pickedCities

names(pickedCities)[5] <- "Unemployment"
names(pickedCities)[6] <- "Area (Sq ft)"
names(pickedCities)[7] <- "Population Density"
names(pickedCities)[8] <- "Crime Rate Index"
names(pickedCities)[9] <- "Lat"
names(pickedCities)[10] <- "Lon"
names(pickedCities)[11] <- "Cost Of Living Index"
names(pickedCities)[12] <- "Median Income"
names(pickedCities)[13] <- "Number of Hospitals"
names(pickedCities)[14] <- "Number of Libraries"
names(pickedCities)[15] <- "Number of Stadiums"


pickedCities


# Prepare data for analysis
scoresDF <- data.frame(pickedCities$Name)

for(i in 1:151){
  scoresDF[i,2] <- as.numeric(gsub("[%]", "", pickedCities[i,5])) # Unemployment
  scoresDF[i,3] <- as.numeric(pickedCities[i,7]) # Population Density
  scoresDF[i,4] <- as.numeric(pickedCities[i,8]) # Crime Rate
  scoresDF[i,5] <- as.numeric(pickedCities[i,11]) # Cost of Living
  scoresDF[i,6] <- as.numeric(pickedCities[i,13]) # Hospitals
  scoresDF[i,7] <- as.numeric(pickedCities[i,14]) # Libraries
  scoresDF[i,8] <- as.numeric(pickedCities[i,15]) # Stadiums
  
  # Set to country average
  if (is.na(scoresDF[i,5]))
    scoresDF[i,5] <- 100
}
names(scoresDF)[1] <- "Name"
scoresDF

tail(scoresDF, 30)

averageUnemploy <- median(scoresDF[,2])
averageDensity <- median(scoresDF[,3])
averageCrime <- median(scoresDF[,4])
averageCost <- 100 # According to city-data.com, average is 100
averageUnemploy
averageDensity
averageCrime
averageCost

# Find % differences, negative numbers are unhappy.
for(i in 1:151){
  scoresDF[i,9] <- (averageUnemploy - scoresDF[i,2]) / averageUnemploy # Negative numbers are excess unemployment
  scoresDF[i,10] <- (averageDensity - scoresDF[i,3]) / averageDensity # Negative numbers are more dense
  scoresDF[i,11] <- (averageCrime - scoresDF[i,4]) / averageCrime # Negative numbers are more crime ridden
  scoresDF[i,12] <- (averageCost - scoresDF[i,5]) / averageCost # Negative numbers are more expensive.
  scoresDF[i,13] <- 10000 * scoresDF[i,6] / pickedCities[i,3] # Number of hospitals per 10000 people.
  scoresDF[i,14] <- 10000 * scoresDF[i,7] / pickedCities[i,3] # Number of libraries per 10000 people.
  scoresDF[i,15] <- 10000 * scoresDF[i,8] / pickedCities[i,3] # Number of stadiums per 10000 people.
}


averageHospitals <- median(scoresDF[,13])
averageLibs <- median(scoresDF[,14])
averageStads <- median(scoresDF[,15])
averageHospitals
averageLibs
averageStads

# Happiness means actual is higher than average
for (i in 1:151){
  scoresDF[i,13] <- (scoresDF[i,13] - averageHospitals) / averageHospitals
  scoresDF[i,14] <- (scoresDF[i,14] - averageLibs) / averageLibs
  scoresDF[i,15] <- (scoresDF[i,15] - averageStads) / averageStads
}

scoresDF <- scoresDF[-c(2, 3, 4, 5, 6, 7, 8)]
names(scoresDF)[2] <- "Unemployment"
names(scoresDF)[3] <- "Density"
names(scoresDF)[4] <- "Crime"
names(scoresDF)[5] <- "Cost"
names(scoresDF)[6] <- "Hospitals per 10000"
names(scoresDF)[7] <- "Libraries per 10000"
names(scoresDF)[8] <- "Stadiums per 10000"
scoresDF

tail(scoresDF, 50)

# Calclulate happiness and for each city by assigning weights and scoring them.
happinessDF <- data.frame(pickedCities$Name)
for (i in 1:151){
  happinessDF[i,2] <- .5 * scoresDF[i,2] + .1 * scoresDF[i,3] + .6 * scoresDF[i,4] + .2 * scoresDF[i,5] +
    .5 * scoresDF[i,6] + .2 * scoresDF[i,7] + .3 * scoresDF[i,8]
}

names(happinessDF)[2] <- "Happiness"
happinessDF

happySadTop20 <- 1:20

happinessDF <- happinessDF[order(happinessDF$Happiness, decreasing = TRUE),]
happySadTop20 <- cbind(happySadTop20, head(happinessDF[,1:2], 20))
happySadTop20

happinessDF <- happinessDF[order(happinessDF$Happiness),]
happySadTop20 <- cbind(happySadTop20, 1:20)
happySadTop20 <- cbind(happySadTop20, head(happinessDF[,c(1,2)], 20))
names(happySadTop20)[1] <- "Happiness Placement"
names(happySadTop20)[2] <- "Name"
names(happySadTop20)[3] <- "Happiness"
names(happySadTop20)[4] <- "Sadness Placement"
names(happySadTop20)[5] <- "Name"
names(happySadTop20)[6] <- "Sadness"
happySadTop20


# #TWITTER MINNING
# install.packages("base64enc")
# install.packages("twitteR")
# library(base64enc)
# library(twitteR)
# 
# 
# 
# consumer <- "bV94w1D7rENWyBApFDIccvIif"
# secret <- "NEkvY1sUdaGh3elixZv5DeINpBRnbr04kDGNk6WmNNZtiS4gxe"
# access <- "1465439760-sh6NK4cmTda7bXzCVaEk0Nb4E3cUCXxG8dLRFBu"
# access_secret <- "sh6s978hks99PwCKzs0nB94yt3CobKgZFGXXAYvQibAZP"
# 
# 
# packages <- c("twitteR", "openssl")
# ### checking if packages are already installed and installing if not
# for(i in packages){
#   if(!(i %in% installed.packages()[, "Package"])){
#     install.packages(i)
#   }
#   library(i, character.only = TRUE) ## load packages
# }
# 
# 
# 
# setup_twitter_oauth(consumer,secret,access,access_secret)
# 
# 
# dir.create("tweets")
# 
# 
# for(t in 1:151){
#   
#   tweets <- searchTwitter("", n= 1000, geocode = sprintf('%s,%s,10mi', pickedCities[t,"Latitude"], pickedCities[t,"Longitude"]))
#   name <- sub("\\.[[:alnum:]]+$", "", basename(as.character( pickedCities[t,4])))
#   
#   #make data frame
#   df <- do.call("rbind", lapply(tweets, as.data.frame))
#   
#   #write to csv file (or your RODBC code)
#   write.csv(df,file= sprintf('tweets/%s-Tweets.csv', name))
#   
#   Sys.sleep(120)
#   
#   
#   
# }



# Get map and plot points
coordsHappy <- data.frame()
coordsUnhappy <- data.frame()

# Get latitiude and longitude for happy and unhappy cities
for(i in 1:20){
  city <- happySadTop20[i,2]
  city <- filter(pickedCities, Name==as.character(city))
  coordsHappy[i,1] <- city[1,9]
  coordsHappy[i,2] <- city[1,10]
  
  city <- happySadTop20[i,5]
  city <- filter(pickedCities, Name==as.character(city))
  coordsUnhappy[i,1] <- city[1,9]
  coordsUnhappy[i,2] <- city[1,10]
}

map("world", fill=TRUE, col="white", bg="lightblue", xlim=c(-190,-40), ylim=c(0,90))
points(coordsHappy[,2], coordsHappy[,1], pch=21, bg=24, cex=happySadTop20$Happiness/3)
map("world", fill=TRUE, col="white", bg="lightblue", xlim=c(-190,-40), ylim=c(0,90))
points(coordsUnhappy[,2], coordsUnhappy[,1], pch=21, bg=24, cex=(happySadTop20$Sadness * -1)) # Works because saddest cities have negative value.
