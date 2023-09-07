library(data.table)
library(plm)
library(splm)
library(lmtest)
library(rgdal)

data <- fread("E:/Desktop(Current)/Spatial Tutorial/County/County_264(updated).csv")

Weight <- fread ("E:/Desktop(Current)/Spatial Tutorial/County/Weight matrix county 264 k4.csv")

W <- as.matrix(Weight)

ny_w <- mat2listw(W)

dataX <- pdata.frame(data, index = c("count", "Year"))

fmpanel <- lnVC ~ lnPC + lnInc + lnage + lnMultgen + lnChild + lnSex + lnMarried + lnDivorced + lnWhite + lnBlack + lnHispanic + lnLocal + lnSouth + lnEuro + lnAfrica + lnAsia + lnYrUSA + lnEdu + lnUnemp + lnLast1 + lnWeekHrs + lnImmig + lnVet

data$lninc2 <- log((data$lnInc)*100000)
#OLS

ols <- plm(lnVC ~ lnPC + lnInc + lnage + lnChild + lnSex + lnMarried + lnDivorced + lnWhite + lnBlack + lnHispanic + lnLocal + lnSouth + lnEuro + lnAfrica + lnAsia + lnYrUSA + lnEdu + lnUnemp + lnLast1 + lnWeekHrs + lnImmig + lnVet, data=data, model = "random")

pooled_ols <-lm(lnVC ~ lnPC + lnInc + lnage + lnMultgen + lnChild + lnSex + lnMarried + lnDivorced + lnWhite + lnBlack + lnHispanic + lnLocal + lnSouth + lnEuro + lnAfrica + lnAsia + lnYrUSA + lnEdu + lnUnemp + lnLast1 + lnWeekHrs + lnImmig + lnVet, data=data)

summary(pooled_ols)

#SAR
SAR <- spml(fmpanel, data = data, index = c("count","Year"), listw = W, model = "within", effect = "individual", spatial.error = "none", lag = TRUE)


summary(SAR)

SAR_re <- spml(fmpanel, data = data, index = c("count","Year"), listw = W, model = "random", effect = "individual", spatial.error = "none", lag = TRUE)

summary(SAR_re)

# SAR Hausman

sphtest(SAR, SAR_re)

#SAR Impact

impact <- impacts(SAR, listw = mat2listw(W, style= "W"), time=17)

summary(impact, zstats = TRUE, short = TRUE) 


effects(SAR)

#SEM

SEM_re <- spreml(fmpanel, data = data, index = c("count","Year"), W, w2= W, errors = "sem2re", pvar = FALSE, hess= FALSE, quiet = TRUE,  initval = "zeros", x.tol = 1.5e-18, rel.tol = 1e-15, lag = FALSE)

summary(SEM_re)

SEM_fe1 <- spml(fmpanel, data = data, index = c("count","Year"), listw = W, model = "within", effect = "individual", spatial.error = "kkp", lag = FALSE)

summary(SEM_fe1)

summary(SEM_fe)

SEM_re1 <- spml(fmpanel, data = data, index = c("count","Year"), listw = W, model = "random", effect = "individual", spatial.error = "kkp", lag = FALSE)

summary(SEM_re1)

#SEM Hausman

sphtest(SEM_fe1, SEM_re1)

# SLX



panelSLX <- lnVC ~ lnPC + lnInc + lnage + lnMultgen+ lnChild + lnSex + lnMarried + lnDivorced + lnWhite + lnBlack + lnHispanic + lnLocal + lnSouth + lnEuro + lnAfrica + lnAsia + lnYrUSA + lnEdu + lnUnemp + lnLast1 + lnWeekHrs + lnImmig + lnVet+slag(lnPC, listw=ny_w) + slag(lnInc, listw = ny_w) + slag(lnage, listw=ny_w) + slag(lnMultgen, listw= ny_w) + slag(lnChild, listw= ny_w) + slag(lnSex, listw=ny_w) + slag(lnMarried, listw=ny_w)+ slag(lnDivorced, listw=ny_w)+ slag(lnWhite, listw=ny_w)+ slag(lnBlack, listw=ny_w)+ slag(lnHispanic, listw=ny_w)+slag(lnLocal, listw=ny_w)+slag(lnSouth, listw=ny_w) + slag(lnEuro, listw=ny_w) + slag(lnAfrica, listw=ny_w) + slag(lnAsia, listw= ny_w) + slag(lnYrUSA, listw=ny_w) + slag(lnEdu, listw=ny_w) + slag(lnUnemp, listw=ny_w) + slag(lnLast1, listw=ny_w) + slag(lnWeekHrs, listw=ny_w) + slag(lnImmig, listw=ny_w) + slag(lnVet, listw=ny_w)

SLX_fe <- plm(panelSLX, data= data, listw = ny_w, index= c("count", "Year"), model="within", effect="individual", spatial.error = "none", lag = FALSE)

SLX_fe1 <- plm(lnVC ~ lnPC + lnInc + lnage + lnMultgen + lnChild + lnSex + lnMarried + lnDivorced + lnWhite + lnBlack + lnHispanic + lnLocal + lnSouth + lnEuro + lnAfrica + lnAsia + lnYrUSA + lnEdu + lnUnemp + lnLast1 + lnWeekHrs + lnImmig + lnVet+slag(data2$lnPC, listw=ny_w) + slag(data2$lnInc, listw = ny_w) + slag(data2$lnage, listw=ny_w) + slag(data2$lnMultgen, listw=ny_w)+slag(data2$lnChild, listw= ny_w) + slag(data2$lnSex, listw=ny_w) + slag(data2$lnMarried, listw=ny_w)+ slag(data2$lnDivorced, listw=ny_w)+ slag(data2$lnWhite, listw=ny_w)+ slag(data2$lnBlack, listw=ny_w)+ slag(data2$lnHispanic, listw=ny_w)+slag(data2$lnLocal, listw=ny_w)+slag(data2$lnSouth, listw=ny_w) + slag(data2$lnEuro, listw=ny_w) + slag(data2$lnAfrica, listw=ny_w) + slag(data2$lnAsia, listw= ny_w) + slag(data2$lnYrUSA, listw=ny_w) + slag(data2$lnEdu, listw=ny_w) + slag(data2$lnUnemp, listw=ny_w) + slag(data2$lnLast1, listw=ny_w) + slag(data2$lnWeekHrs, listw=ny_w) + slag(data2$lnImmig, listw=ny_w) + slag(data2$lnVet, listw=ny_w)
               ,data= data, listw = ny_w,  model="within", effect="individual", spatial.error = "none", lag =FALSE, tol.solve = 1e-10) 

SLX_fe2 <- plm(lnVC ~ #lnPC + lnInc + lnage + lnMultgen+ lnChild + lnSex + lnMarried + lnDivorced + lnWhite + lnBlack + lnHispanic + lnLocal + lnSouth + lnEuro + lnAfrica + lnAsia + lnYrUSA + lnEdu + lnUnemp + lnLast1 + lnWeekHrs + lnImmig + lnVet,
               slag(lnPC, listw=ny_w) + slag(lnInc, listw = ny_w) + slag(lnage, listw=ny_w) + slag(lnMultgen, listw= ny_w) + slag(lnChild, listw= ny_w) + slag(lnSex, listw=ny_w) + slag(lnMarried, listw=ny_w)+ slag(lnDivorced, listw=ny_w)+ slag(lnWhite, listw=ny_w)+ slag(lnBlack, listw=ny_w)+ slag(lnHispanic, listw=ny_w)+slag(lnLocal, listw=ny_w)+slag(lnSouth, listw=ny_w) + slag(lnEuro, listw=ny_w) + slag(lnAfrica, listw=ny_w) + slag(lnAsia, listw= ny_w) + slag(lnYrUSA, listw=ny_w) + slag(lnEdu, listw=ny_w) + slag(lnUnemp, listw=ny_w) + slag(lnLast1, listw=ny_w) + slag(lnWeekHrs, listw=ny_w) + slag(lnImmig, listw=ny_w) + slag(lnVet, listw=ny_w),
               data= data, model="within", tol.solve = 1e-10) 

summary(SLX_fe2)

summary(SLX_fe)

SLX_re <- plm(panelSLX, data= data, listw = ny_w, index= c("count", "Year"), model="random", effect="individual", spatial.error = "none", lag = FALSE)

summary(SLX_re)

SLX_re2 <- plm(lnVC ~ #lnPC + lnInc + lnage + lnMultgen+ lnChild + lnSex + lnMarried + lnDivorced + lnWhite + lnBlack + lnHispanic + lnLocal + lnSouth + lnEuro + lnAfrica + lnAsia + lnYrUSA + lnEdu + lnUnemp + lnLast1 + lnWeekHrs + lnImmig + lnVet,
                 slag(lnPC, listw=ny_w) + slag(lnInc, listw = ny_w) + slag(lnage, listw=ny_w) + slag(lnMultgen, listw= ny_w) + slag(lnChild, listw= ny_w) + slag(lnSex, listw=ny_w) + slag(lnMarried, listw=ny_w)+ slag(lnDivorced, listw=ny_w)+ slag(lnWhite, listw=ny_w)+ slag(lnBlack, listw=ny_w)+ slag(lnHispanic, listw=ny_w)+slag(lnLocal, listw=ny_w)+slag(lnSouth, listw=ny_w) + slag(lnEuro, listw=ny_w) + slag(lnAfrica, listw=ny_w) + slag(lnAsia, listw= ny_w) + slag(lnYrUSA, listw=ny_w) + slag(lnEdu, listw=ny_w) + slag(lnUnemp, listw=ny_w) + slag(lnLast1, listw=ny_w) + slag(lnWeekHrs, listw=ny_w) + slag(lnImmig, listw=ny_w) + slag(lnVet, listw=ny_w),
               data= data, listw = ny_w, index= c("count", "Year"), model="random", effect="individual", spatial.error = "none", lag = FALSE) 

#SLX Hausman

phtest(SLX_fe, SLX_re)

# LM Test

# LM Lag test

rlml_model <- slmtest(lnVC ~ lnPC + lnInc + lnage + lnMultgen+ lnChild + lnSex + lnMarried + lnDivorced + lnWhite + lnBlack + lnHispanic + lnLocal + lnSouth + lnEuro + lnAfrica + lnAsia + lnYrUSA + lnEdu + lnUnemp + lnLast1 + lnWeekHrs + lnImmig + lnVet, data=data, listw= ny_w, test = "rlml")

print(rlml_model)

# LM Error test

rlme_model <- slmtest(lnVC ~ lnPC + lnInc + lnage +lnMultgen+ lnChild + lnSex + lnMarried + lnDivorced + lnWhite + lnBlack + lnHispanic + lnLocal + lnSouth + lnEuro + lnAfrica + lnAsia + lnYrUSA + lnEdu + lnUnemp + lnLast1 + lnWeekHrs + lnImmig + lnVet, data=data, listw= ny_w, test = "rlme")

print(rlme_model)

#SDM 
impact1 <- impacts(SDM_fe, listw = mat2listw(W, style= "W"), time=17)

summary(impact1, zstats = TRUE, short = TRUE) 

panelSDM <- lnVC ~ lnPC + lnInc + lnage + lnMultgen+ lnChild + lnSex + lnMarried + lnDivorced + lnWhite + lnBlack + lnHispanic + lnLocal + lnSouth + lnEuro + lnAfrica + lnAsia + lnYrUSA + lnEdu + lnUnemp + lnLast1 + lnWeekHrs + lnImmig + lnVet+slag(lnPC, listw=ny_w) + slag(lnInc, listw = ny_w) + slag(lnage, listw=ny_w) + slag(lnMultgen, listw=ny_w) + slag(lnChild, listw= ny_w) + slag(lnSex, listw=ny_w) + slag(lnMarried, listw=ny_w)+ slag(lnDivorced, listw=ny_w)+ slag(lnWhite, listw=ny_w)+ slag(lnBlack, listw=ny_w)+ slag(lnHispanic, listw=ny_w)+slag(lnLocal, listw=ny_w)+slag(lnSouth, listw=ny_w) + slag(lnEuro, listw=ny_w) + slag(lnAfrica, listw=ny_w) + slag(lnAsia, listw= ny_w) + slag(lnYrUSA, listw=ny_w) + slag(lnEdu, listw=ny_w) + slag(lnUnemp, listw=ny_w) + slag(lnLast1, listw=ny_w) + slag(lnWeekHrs, listw=ny_w) + slag(lnImmig, listw=ny_w) + slag(lnVet, listw=ny_w)

SDM_re <- spml(lnVC ~ lnPC + lnInc + lnage + lnMultgen + lnChild + lnSex + lnMarried + lnDivorced + lnWhite + lnBlack + lnHispanic + lnLocal + lnSouth + lnEuro + lnAfrica + lnAsia + lnYrUSA + lnEdu + lnUnemp + lnLast1 + lnWeekHrs + lnImmig + lnVet+slag(lnPC, listw=ny_w) + slag(lnInc, listw = ny_w) + slag(lnage, listw=ny_w) + slag(lnMultgen, listw=ny_w)+slag(lnChild, listw= ny_w) + slag(lnSex, listw=ny_w) + slag(lnMarried, listw=ny_w)+ slag(lnDivorced, listw=ny_w)+ slag(lnWhite, listw=ny_w)+ slag(lnBlack, listw=ny_w)+ slag(lnHispanic, listw=ny_w)+slag(lnLocal, listw=ny_w)+slag(lnSouth, listw=ny_w) + slag(lnEuro, listw=ny_w) + slag(lnAfrica, listw=ny_w) + slag(lnAsia, listw= ny_w) + slag(lnYrUSA, listw=ny_w) + slag(lnEdu, listw=ny_w) + slag(lnUnemp, listw=ny_w) + slag(lnLast1, listw=ny_w) + slag(lnWeekHrs, listw=ny_w) + slag(lnImmig, listw=ny_w) + slag(lnVet, listw=ny_w)
,data= data, listw = ny_w, index= c("count", "Year"), model="random", effect="individual", spatial.error = "none", lag = TRUE) 

summary(SDM_re)

data2 <- pdata.frame(data, index = c("count", "Year"), drop.index = TRUE)

SDM_fe <- spml(lnVC ~ lnPC + lnInc + lnage + lnMultgen + lnChild + lnSex + lnMarried + lnDivorced + lnWhite + lnBlack + lnHispanic + lnLocal + lnSouth + lnEuro + lnAfrica + lnAsia + lnYrUSA + lnEdu + lnUnemp + lnLast1 + lnWeekHrs + lnImmig + lnVet+slag(data2$lnPC, listw=ny_w) + slag(data2$lnInc, listw = ny_w) + slag(data2$lnage, listw=ny_w) + slag(data2$lnMultgen, listw=ny_w)+slag(data2$lnChild, listw= ny_w) + slag(data2$lnSex, listw=ny_w) + slag(data2$lnMarried, listw=ny_w)+ slag(data2$lnDivorced, listw=ny_w)+ slag(data2$lnWhite, listw=ny_w)+ slag(data2$lnBlack, listw=ny_w)+ slag(data2$lnHispanic, listw=ny_w)+slag(data2$lnLocal, listw=ny_w)+slag(data2$lnSouth, listw=ny_w) + slag(data2$lnEuro, listw=ny_w) + slag(data2$lnAfrica, listw=ny_w) + slag(data2$lnAsia, listw= ny_w) + slag(data2$lnYrUSA, listw=ny_w) + slag(data2$lnEdu, listw=ny_w) + slag(data2$lnUnemp, listw=ny_w) + slag(data2$lnLast1, listw=ny_w) + slag(data2$lnWeekHrs, listw=ny_w) + slag(data2$lnImmig, listw=ny_w) + slag(data2$lnVet, listw=ny_w)
               ,data= data, listw = ny_w,  model="within", effect="individual", spatial.error = "none", lag =TRUE, tol.solve = 1e-10) 


SDM_fe1 <- plm(lnVC ~ lnPC + lnInc + lnage + lnMultgen + lnChild + lnSex + lnMarried + lnDivorced + lnWhite + lnBlack + lnHispanic + lnLocal + lnSouth + lnEuro + lnAfrica + lnAsia + lnYrUSA + lnEdu + lnUnemp + lnLast1 + lnWeekHrs + lnImmig + lnVet+slag(data2$lnPC, listw=ny_w) + slag(data2$lnInc, listw = ny_w) + slag(data2$lnage, listw=ny_w) + slag(data2$lnMultgen, listw=ny_w)+slag(data2$lnChild, listw= ny_w) + slag(data2$lnSex, listw=ny_w) + slag(data2$lnMarried, listw=ny_w)+ slag(data2$lnDivorced, listw=ny_w)+ slag(data2$lnWhite, listw=ny_w)+ slag(data2$lnBlack, listw=ny_w)+ slag(data2$lnHispanic, listw=ny_w)+slag(data2$lnLocal, listw=ny_w)+slag(data2$lnSouth, listw=ny_w) + slag(data2$lnEuro, listw=ny_w) + slag(data2$lnAfrica, listw=ny_w) + slag(data2$lnAsia, listw= ny_w) + slag(data2$lnYrUSA, listw=ny_w) + slag(data2$lnEdu, listw=ny_w) + slag(data2$lnUnemp, listw=ny_w) + slag(data2$lnLast1, listw=ny_w) + slag(data2$lnWeekHrs, listw=ny_w) + slag(data2$lnImmig, listw=ny_w) + slag(data2$lnVet, listw=ny_w)
               ,data= data,  listw = ny_w,  model="within", effect="individual", spatial.error = "none", lag =TRUE, tol.solve = 1e-10) 

summary(SDM_fe)

#SDM Hausman

hausman_sdm<-sphtest(SDM_fe, SDM_re )

print(hausman_sdm)

#first test: SDM vs SAR
diff <- length(coef(SDM_fe))-length(coef(SAR))
teststat_lag <- 2*(SDM_fe$logLik-SAR$logLik)
pchisq(teststat_lag,df=diff,lower.tail=FALSE)

#second test: SDM vs SEM

# Here, Log-likelihood of SEM_fe1 = 1051.4326 (from STATA output when COntiguity Matrix is used)

# Here, Log-likelihood of SEM_fe1 = 1044.4639 (from STATA output when k=3 Matrix is used)

# Here, Log-likelihood of SEM_fe1 = 1042.8097 (from STATA output when k=4 Matrix is used)


diff1 <- length(coef(SDM_fe))-length(coef(SEM_fe1)) 
teststat_error1 <- 2*(SDM_fe$logLik- 1042.8097)
pchisq(teststat_error1,df=diff1,lower.tail=FALSE)


#diff2 <- length(coef(SDM_fe))-length(coef(SLX_fe))
#teststat_lag2 <- 2*(SDM_fe$logLik-SLX_fe$logLik)
#pchisq(teststat_lag2,df=diff2,lower.tail=FALSE)



#Wald Test

#SDM vs SLX

pwaldtest(SLX_fe1) # with explanatory variables and lagged explanatory variables

pwaldtest(SLX_fe2) # with lagged explanatory variables only

pwaldtest(SLX_re)

pwaldtest(SLX_re2)

