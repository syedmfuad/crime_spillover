library(splm)
library(rgdal)
library(spdep)
library(plm)
library(lmtest)
library(fields)
library(tmap)

#Loading county shapefile
Output.Areas<- readOGR(".", "UScounties")

#If you want to see the counties in the shapefile, you can save it as a csv and view
write.csv(Output.Areas, file="County_names.csv")

#Since I want to merge the shapefile with a csv with demographic data, I need a unique county GEOID variable
#I cannot merge by the unique county FIPS code since the demographic csv file does not have the county FIPS code
#So I construct a variable by concatenating the state and county names and use that as a unique GEOID 

#Here I'm concatenating the county name ("NAME" variable in shapefile) and the state name ("STATE_NAME" in shapefile)
Output.Areas$state <- paste0(Output.Areas$NAME, Output.Areas$STATE_NAME)

#Removing Alaska and Hawaii
Output.Areas <- Output.Areas[!(Output.Areas$STATE_NAME=="Alaska" | Output.Areas$STATE_NAME=="Hawaii"),]

#Loading demographic csv file
fips <- read.csv("County_264(updated).csv")

#This file already has the unique county GEOID under variable name "StateID" so I dont have to construct it

#Now I only have to create a subset of the shapefile with only counties that are also in the demographic csv file
#In other words, I'm removing counties from shapefile that are not in the demographic csv file
shpfile <- Output.Areas %>% subset(state %in% fips$StateID)

#Since my demographic data is a panel dataset, I only select one year and then merge with shapefile
fips2009 <- subset(fips, Year=="2009")
data <- merge(shpfile, fips2009, by.x="state", by.y="StateID") #merging shapefile and demographic data

#You can now look at the merged file
write.csv(data, file="County_names2.csv")

#Getting the weight matrix according to your specifications 
#More details: https://www.rdocumentation.org/packages/spdep/versions/1.2-4/topics/nb2listw
ny_nb1 <- poly2nb(data)
ny_w <- nb2listw(ny_nb1, style = "W", zero.policy = T) #ny_w is the list containing the weight matrix

#This list is what is used as input in spatial models in R

#Alternatively, if you want to run spatial models in stata, you'd need the weight matrix in matrix form
#You can get it using the following way
ny_m <- nb2mat(ny_nb1, style = "W", zero.policy = T) #ny_m is the weight matrix

#Save the weight matrix and view
write.csv(ny_m, file="Weight_matrix.csv")

