### -------------crime-spillover-----------
# Authors        : Syed M. Fuad
# Data           : American Community Survey and FBI Uniform Crime Reporting Program
# New Techniques : Spatial econometric methods
# --------

#--- measuring crime spillover using spatial econometric methods ---
#--- crime-spillover ---


#--- part 1 getting demographic data from american community survey ---

#--- loading required libraries ---

library(magrittr)
library(tidyverse)
library(dplyr)
library(data.table)

#--- loading data ---

data1 <- read.csv("usa_00049.csv")
data1 <- read_csv("usa_00049.csv") #this is slower than the one below
data1 <- fread("usa_00049.csv")

data1 <- filter(data1, YEAR >= 2007)

#--- we take the msa-wise mean of some of the key demographic variables using the group_by function the dplyr package ---

data1 %>% group_by(YEAR, MET2013) %>%
  summarize(Age=mean(AGE, na.rm=TRUE),
            MultGen=sum(MULTGEN==3, na.rm=TRUE)/n(),
            Child=sum(NCHILD>0)/n(),
            Sex=sum(SEX==1)/n(),
            Married=sum(MARST==1 | MARST==2)/n(),
            Divorced=sum(MARST==3 | MARST==4)/n(),
            Single=sum(MARST==6)/n(),
            White=sum(RACE==1)/n(),
            Black=sum(RACE==2)/n(),
            Hispanic=sum(HISPAN>=1 & HISPAN<5)/n(),
            OtherRace=sum(RACE>2)/n(),
            Local=sum(BPL<100)/n(),
            Canada=sum(BPL==150)/n(),
            CentralAmerica=sum(BPL>=200 & BPL<=299)/n(),
            SouthAmerica=sum(BPL==300)/n(),
            Europe=sum(BPL>=400 & BPL<500)/n(),
            Africa=sum(BPL==600)/n(),
            Asia=sum(BPL>499 & BPL<600)/n(),
            Education=mean(EDUC, na.rm=TRUE),
            PublicSchl=sum(SCHLTYPE==2)/n(),
            #filter(BPL>100) %>% summarize(YearsUSA=mean(YRSUSA1, na.rm=TRUE))
            YearsUSA=mean(YRSUSA1[BPL>100], na.rm=TRUE)) -> data1_dplyr

#--- taking msa-wise mean using the data.table package ---

data1_dt <- as.data.table(data1)
data1_datatable <- data1_dt[,.
  (Age=mean(AGE, na.rm=TRUE),
  MultGen=sum(MULTGEN==3, na.rm=TRUE)/.N,
  Child=sum(NCHILD>0)/.N,
  Sex=sum(SEX==1)/.N,
  Married=sum(MARST==1 | MARST==2)/.N,
  Divorced=sum(MARST==3 | MARST==4)/.N,
  Single=sum(MARST==6)/.N,
  White=sum(RACE==1)/.N,
  Black=sum(RACE==2)/.N,
  Hispanic=sum(HISPAN>=1 & HISPAN<5)/.N,
  OtherRace=sum(RACE>2)/.N,
  Local=sum(BPL<100)/.N,
  Canada=sum(BPL==150)/.N,
  CentralAmerica=sum(BPL>=200 & BPL<=299)/.N,
  SouthAmerica=sum(BPL==300)/.N,
  Europe=sum(BPL>=400 & BPL<500)/.N,
  Africa=sum(BPL==600)/.N,
  Asia=sum(BPL>499 & BPL<600)/.N,
  Education=mean(EDUC, na.rm=TRUE),
  PublicSchl=sum(SCHLTYPE==2)/.N,
  YearsUSA=mean(YRSUSA1[BPL>100], na.rm=TRUE)),
  by=.(YEAR, MET2013)
]

#--- loading a second batch of data ---

data2 <- fread("usa_00051.csv")
data2 <- filter(data2, YEAR >= 2007)

#--- msa-wise means using dplyr package ---

data2 %>% group_by(YEAR, MET2013) %>%
  filter(!(INCWAGE==999999 | INCWAGE==999998)) %>%
  group_by(YEAR, MET2013) %>%
  summarize(Unemp=sum(EMPSTAT==2)/n(),
            HoursWorked=mean(UHRSWORK[EMPSTAT==1]),
            IncWage=mean(INCWAGE[EMPSTAT==1]),
            WorkedLast=sum(WORKEDYR==2)/n(),
            WorkedLast2=sum(WORKEDYR==1)/n(),
            VetStat=sum(VETSTAT==2)/n()) -> data2_dplyr

#--- msa-wise means using data.table package

data2_dt <- as.data.table(data2)
data2_dt[(INCWAGE!= 999999 | INCWAGE!= 999998),.
         (Unemp=sum(EMPSTAT==2)/n(),
           HoursWorked=mean(UHRSWORK[EMPSTAT==1]),
           IncWage=mean(INCWAGE[EMPSTAT==1]),
           WorkedLast=sum(WORKEDYR==2)/n(),
           WorkedLast2=sum(WORKEDYR==1)/n(),
           VetStat=sum(VETSTAT==2)/n()),
         by=.(YEAR, MET2013)
         ]

demo_data <- cbind(data1_dplyr, data2_dplyr)

#--- add the lapply functions as well ---

#part 2 getting crime data

#https://stackoverflow.com/questions/5413188/reading-csv-files-in-a-for-loop-and-assigning-dataframe-names
#https://stackoverflow.com/questions/45360423/subset-data-based-on-elements-in-list
#https://stackoverflow.com/questions/26682107/remove-the-letters-between-two-patterns-of-strings-in-r
#https://stackoverflow.com/questions/27153539/using-gsub-in-list-of-dataframes-with-r

library(readxl)
library(dplyr)
library(tidyverse)

filenames <- list.files(path=getwd()) 
numfiles <- length(filenames)

All <- lapply(filenames,function(i){
  i <- paste(".\\",i,sep="")
  read_excel(i)
})
filenames <- gsub("-","",filenames)
names(All) <- gsub(".xlsx","",filenames)

my_function <- function(data){
  data %>% `colnames<-` (c('MSA', 'County', 'Pop', 'VC', 'Murder', 'Rape', 'Robbery', 'AggAssault', 
                           'PC', 'Burglary', 'Larceny', 'MVT')) %>% 
                           fill(MSA) %>% filter(County=="Rate per 100,000 inhabitants") %>%
                           select(MSA) %>% filter(!grepl("M.D.", MSA))
}

#same function using piping function

All %>% lapply(my_function) %>% lapply(rapply(function(x)
  sub(",", "!,", x), how="list"), as.data.frame) %>% lapply(rapply(function(x)
  sub("-.*!", "", x), how="list"), as.data.frame) %>% lapply(function(x)
  sub("!", "", x), how="list"), as.data.frame) -> data_i_want4

#original function

data_i_want <- lapply(All, my_function)

data_i_want2 <- lapply(rapply(data_i_want, function(x)
  sub(",", "!,", x), how="list"), as.data.frame)

data_i_want3 <- lapply(rapply(data_i_want2, function(x)
  sub("-.*!", "", x), how="list"), as.data.frame)

data_i_want4 <- lapply(rapply(data_i_want3, function(x)
  sub("!", "", x), how="list"), as.data.frame)

my_list <- data_i_want4[1]

for (i in 1:11){
  list_2 <- data_i_want4[i+1]
  my_list <- merge(my_list, list_2, all=FALSE)
}

#new function with pipe

MSA_name <- read_excel("MSA Names.xlsx")
MSA_name %>% filter(Count==12) %>% select(CBSA, MSAname)

MSA_name$MSAname <- paste("M.S.A.", sep=" ") %>% sub(",", "!,") %>% sub("-.*!", "") %>% sub("!", "")

#old function

MSA_name <- read_excel("MSA Names.xlsx")
MSA_name <- filter(MSA_name, Count==12)
MSA_name <- select(MSA_name, CBSA, MSAname)
MSA_name$MSAname <- paste(MSA_name$MSAname, "M.S.A.", sep=" ")

MSA_name$MSAname <- sub(",", "!,", MSA_name$MSAname)
MSA_name$MSAname <- sub("-.*!", "", MSA_name$MSAname)
MSA_name$MSAname <- sub("!", "", MSA_name$MSAname)

my_vector <- unlist(my_list, use.names=FALSE)
Metname <- MSA_name %>% filter(MSAname %in% my_vector)

my_new_function <- function(data){
  data %>% `colnames<-` (c('MSA', 'County', 'Pop', 'VC', 'Murder', 'Rape', 'Robbery', 'AggAssault', 
                           'PC', 'Burglary', 'Larceny', 'MVT')) %>% 
    fill(MSA) %>% filter(County=="Rate per 100,000 inhabitants") %>%
    filter(!grepl("M.D.", MSA))
}

#new function with pipe

All %>% lapply(my_new_function) %>% do.call(rbind) -> finaldf
finaldf$MSA %>% sub(",", "!,") %>% sub("-.*!", "") %>% sub("!", "")

#old function

df_list <- lapply(All, my_new_function)
finaldf <- do.call(rbind, df_list)
finaldf$MSA <- sub(",", "!,", finaldf$MSA)
finaldf$MSA <- sub("-.*!", "", finaldf$MSA)
finaldf$MSA <- sub("!", "", finaldf$MSA)

Crimedata <- finaldf %>% filter(MSA %in% Metname$MSAname)

demo_data2 <- demo_data %>% filter(MET2013 %in% Metname$CBSA)

#part 3 getting weight matrix

#https://cran.r-project.org/web/packages/spdep/vignettes/nb_igraph.html

library(splm)
library(rgdal)
library(spdep)
library(plm)
library(lmtest)

#method 1

Output.Areas<- readOGR(".", "tl_2017_us_cbsa")
write.csv(Output.Areas, file="MSA shapefile 2017.csv")
shpfile <- Output.Areas %>% subset(GEOID %in% Metname$CBSA)
ny_nb1 <- poly2nb(shpfile)
ny_w1 <- nb2listw(ny_nb1, style = "W", zero.policy = T)

#method 2

id <- row.names(as(shpfile, "data.frame"))
coords <- coordinates(shpfile)
ny_nb <- knn2nb(knearneigh(coords, k = 6), row.names = id)
ny_w <- nb2listw(ny_nb, style = "W", zero.policy = T)

shp_mat <- listw2mat(ny_w1)
