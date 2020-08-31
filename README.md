# crime_spillover

This paper investigates the spilling characteristic of crime by analyzing county- and metropolitan statistical area-level violent and non-violent crime data. Since the tendency to engage in criminal activities is considered a human behavioral trait, behavioral spillover effects give rise to social multiplier effects. Therefore, if criminal actions face similar multiplier effects, one criminal activity can induce others to engage in similar behavior. 

There are research evidences which suggest it is possible to formulate economic network of criminal activities. For example, in neighborhoods that are connected to each other based on co-offending, there are evidences of intra-neighborhood gang  conflicts.

Based on such research evidences we analyze statistically the spilling characteristic of crime in United States using spatial econometric techniques (i.e. spatial auto regressive (SAR), spatial error
(SE), spatial durbin (SD) models).

## script walkthrough
### part 1: getting demographic data from american community survey

Throughout the first part, which mainly involves loading and manipulating a large data set (>3GB), I write scripts using built-in r codes, and using the dplyr and data.table packages. The main purpose of this is to demonstrate the functional superiority of these packages in working with large data sets. 