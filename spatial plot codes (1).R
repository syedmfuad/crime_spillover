library(splm)
library(rgdal)
library(spdep)
library(plm)
library(lmtest)
library(fields)
library(tmap)

#loading shapefile
Output.Areas<- readOGR(".", "UScounties")
Output.Areas$state <- paste0(Output.Areas$NAME, Output.Areas$STATE_NAME)
Output.Areas <- Output.Areas[!(Output.Areas$STATE_NAME=="Alaska" | Output.Areas$STATE_NAME=="Hawaii"),] #removing Alaska and Hawaii

#loading demographic+crime data
fips <- read.csv("County_264(updated).csv")
shpfile <- Output.Areas %>% subset(state %in% fips$StateID) #selecting only shapefile counties that are in demographic+crime data

fips2009 <- subset(fips, Year=="2009")
data <- merge(Output.Areas, fips2009, by.x="state", by.y="StateID") #merging shapefile and demographic+crime data
data_shp <- st_as_sf(data) #reconverting to shapefile

#for all the plots below, I use the variable lnVC

#simple spatial plot of lnVC
tm_shape(data_shp) + 
  tm_fill("lnVC",
          palette = "Reds", 
          style = "quantile", 
          title = "Violent Crime") +
  tm_borders(alpha=.4) 

#loading weight matrix for plotting
#I'm using k=2 here

df <- read.csv("Weight matrix county 264 k2.csv")
mat <- as.matrix(df) 
mat <- mat[,-1] 
ny_w1 <- mat2listw(mat, style="W") 

local <- localmoran(as.vector(fips2009$lnVC), listw=ny_w1)

#plot local moran's I
data_shp_264 <- data_shp %>% subset(state %in% fips$StateID)
moran.map <- cbind(data_shp_264, local)

data_shp_rest <- subset(data_shp, !(state %in% fips$StateID))

moran.map <- bind_rows(moran.map, data_shp_rest)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "local moran statistic")

#lisa plot

data_shp_264$lnVC_2 <- scale(data_shp_264$lnVC) %>% as.vector()
data_shp_264$lag_lnVC <- lag.listw(ny_w1, data_shp_264$lnVC)

data_i_want <- st_as_sf(data_shp_264) %>% 
  mutate(quad_sig = ifelse(data_shp_264$lnVC > 0 & 
                             data_shp_264$lag_lnVC > 0 & 
                             local[,5] <= 0.05, 
                           "high-high",
                           ifelse(data_shp_264$lnVC <= 0 & 
                                    data_shp_264$lag_lnVC <= 0 & 
                                    local[,5] <= 0.05, 
                                  "low-low", 
                                  ifelse(data_shp_264$lnVC > 0 & 
                                           data_shp_264$lag_lnVC <= 0 & 
                                           local[,5] <= 0.05, 
                                         "high-low",
                                         ifelse(data_shp_264$lnVC <= 0 & 
                                                  data_shp_264$lag_lnVC > 0 & 
                                                  local[,5] <= 0.05,
                                                "low-high", 
                                                "non-significant")))))

lisa.map <- bind_rows(data_i_want, data_shp_rest)
qtm(lisa.map, fill="quad_sig", fill.title="LISA")

#cluster map

local_g <- localG(data_shp_264$lnVC, listw=ny_w1) 
local_g <- cbind(data_shp_264, as.matrix(local_g))
cluster.map <- bind_rows(local_g, data_shp_rest)
names(cluster.map)[85] <- "gstat"

tm_shape(cluster.map) + 
  tm_fill("gstat", 
          palette = "RdBu",
          style = "pretty") +
  tm_borders(alpha=.4)
