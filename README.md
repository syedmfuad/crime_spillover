# crime_spillover

This paper investigates the spilling characteristic of crime by analyzing county- and metropolitan statistical area-level violent and non-violent crime data. Since the tendency to engage in criminal activities is considered a human behavioral trait, behavioral spillover effects give rise to social multiplier effects. Therefore, if criminal actions face similar multiplier effects, one criminal activity can induce others to engage in similar behavior. 

There are research evidences which suggest it is possible to formulate economic network of criminal activities. For example, in neighborhoods that are connected to each other based on co-offending, there are evidences of intra-neighborhood gang  conflicts.

Based on such research evidences we analyze statistically the spilling characteristic of crime in United States using spatial econometric techniques (i.e. spatial auto regressive (SAR), spatial error (SE), spatial durbin (SD) models).

### script walkthrough

Throughout the first part of the script, which mainly involves loading and manipulating two large data sets (each >3GB), I write scripts using built-in r codes, and using the dplyr and data.table packages. The main purpose of this is to demonstrate the functional superiority of these packages in working with large data sets. 

### data sources

I use data from predominantly two sources: demographic data from annual American Community Survey and crime data from the FBI Uniform Crime Reporting Program. I also use a shapefile of metropolitan statistical areas for the spatial analysis.

Demographic data available through the Census API are aggregated over geographic levels or available at the micro (individual level). While we do use some summarized geographic level data (like mean income at geographic levels), most of what we need are not available in the pre-aggregated tables and thus have to be calculated (for example, proportion of residents in households with divorced or separated household heads, or proportion of residents born in South America). Therefore, it makes more sense for us to use microdata known as the Public Use Microdata Sample (PUMS). 

There is an R package tidycensus that uses Census API to get the data, however, the version that is published on CRAN only downloads the 5-year ACS and dicennial census. The authors XXX are currently working on another version where the ACS 1-year functionality is available. This version is available for download on GitHub and contains a thorough tutorial at this link. However, this was not available when I downloaded the data manually from the IPUMS website in two batches. 

The crime data are readily available aggregated at metropolitan statistical areas at this link.

### part 1: loading micro demographic data from acs and calculating msa-level aggregates

```{r}
#--- loading data using built-in code ---
system.time(
data1 <- read.csv("~")) 

#--- dplyr ---
system.time(
data1 <- read_csv("~")) 

#--- data.table ---
system.time(
data1 <- fread("~"))    
```

Once loaded into R session, we now proceed to calculate the MSA-level aggregates.

```{r}
#--- dplyr ---

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

#--- data.table ---

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
```

Because the second batch is a similar set of codes, I'm skipping embedding it here. The final step is combining the two batch of aggregate data by the cbind function.

### part 2: loading crime data


