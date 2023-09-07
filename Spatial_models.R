library(splm)
library(rgdal)
library(spdep)
library(plm)
library(lmtest)
library(fields)
library(GISTools)

#loading data; this data should be the merged demographic and crime data
crimedata <-read.csv("Demographic data 2.csv", stringsAsFactors=FALSE, row.names=NULL)

#ols model
#change the model regressors according to what you have
ols <- plm(AvgInc ~ AvgAge+MultGen+Child+Sex+Married+Divorced+White+Black+Hispanic+SouthAmerica+Africa+Asia+
           Local+YearUSA+Education+EmpStat+WeekHrs+EmpLast1+EmpLast2+IntIn+VetStat+ViolentCrime, 
           data=crimedata, model="random")

#loading the appropriate weight matrix
weight <- read.csv("Weight matrix k1.csv")
weight$X <- NULL
weight <- as.matrix(weight)
ny_w <- mat2listw(weight)

#spatial regressions 

#spatial lag models 

#spatial lag fixed effects model
plag_modelfe <- spml(lnViolent ~ lnProp+lnAvgAge+MultGen+Child+Sex+Married+Divorced+White+Black+Hispanic+SouthAmerica+
                       Africa+Asia+Local+lnYearUSA+lnEdu+EmpStat+WeekHrs+EmpLast1+EmpLast2+LnIntin+VetStat+
                       lnAvginc, data=crimedata, index=NULL, listw=ny_w,
                   model="within", lag=TRUE, spatial.error="none", tol.solve=1.0e-20)

#spatial lag random effects model
plag_modelre <- spml(lnViolent ~ lnProp+lnAvgAge+MultGen+Child+Sex+Married+Divorced+White+Black+Hispanic+SouthAmerica+
                       Africa+Asia+Local+lnYearUSA+lnEdu+EmpStat+WeekHrs+EmpLast1+EmpLast2+LnIntin+VetStat+
                       lnAvginc, data=crimedata, index=NULL, listw=ny_w,
                    model="random", lag=TRUE, spatial.error="none", tol.solve=1.0e-20)

#Hausman test
sphtest(plag_modelfe, plag_modelre)

#slx models

#slx fixed effects model
pslx_modelfe <- plm(lnViolent ~ lnProp+lnAvgAge+MultGen+Child+Sex+Married+Divorced+White+Black+Hispanic+SouthAmerica+
                     Africa+Asia+Local+lnYearUSA+lnEdu+EmpStat+WeekHrs+EmpLast1+EmpLast2+LnIntin+VetStat+
                     lnAvginc, #+ 
                    #slag(lnProp,listw=ny_w)+slag(lnAvgAge,listw=ny_w)+slag(MultGen,listw=ny_w)+
                      #slag(Child,listw=ny_w)+slag(Sex,listw=ny_w)+slag(Married,listw=ny_w)+
                      #slag(Divorced,listw=ny_w)+slag(White,listw=ny_w)+slag(Black,listw=ny_w)+
                      #slag(Hispanic,listw=ny_w)+slag(SouthAmerica,listw=ny_w)+slag(Africa,listw=ny_w)+
                      #slag(Asia,listw=ny_w)+slag(Local,listw=ny_w)+slag(lnYearUSA,listw=ny_w)+
                      #slag(lnEdu,listw=ny_w)+slag(EmpStat,listw=ny_w)+slag(WeekHrs,listw=ny_w)+
                      #slag(EmpLast1,listw=ny_w)+slag(EmpLast2,listw=ny_w)+slag(LnIntin,listw=ny_w)+
                      #slag(VetStat,listw=ny_w)+slag(lnAvginc,listw=ny_w), 
                    data=crimedata,
                   model="within", index=c("MET2013", "Year"), tol.solve=1.0e-20)

#slx random effects model
pslx_modelre <- plm(lnViolent ~ lnProp+lnAvgAge+MultGen+Child+Sex+Married+Divorced+White+Black+Hispanic+SouthAmerica+
                      Africa+Asia+Local+lnYearUSA+lnEdu+EmpStat+WeekHrs+EmpLast1+EmpLast2+LnIntin+VetStat+
                      lnAvginc+ 
                      slag(lnProp,listw=ny_w)+slag(lnAvgAge,listw=ny_w)+slag(MultGen,listw=ny_w)+
                      slag(Child,listw=ny_w)+slag(Sex,listw=ny_w)+slag(Married,listw=ny_w)+
                      slag(Divorced,listw=ny_w)+slag(White,listw=ny_w)+slag(Black,listw=ny_w)+
                      slag(Hispanic,listw=ny_w)+slag(SouthAmerica,listw=ny_w)+slag(Africa,listw=ny_w)+
                      slag(Asia,listw=ny_w)+slag(Local,listw=ny_w)+slag(lnYearUSA,listw=ny_w)+
                      slag(lnEdu,listw=ny_w)+slag(EmpStat,listw=ny_w)+slag(WeekHrs,listw=ny_w)+
                      slag(EmpLast1,listw=ny_w)+slag(EmpLast2,listw=ny_w)+slag(LnIntin,listw=ny_w)+
                      slag(VetStat,listw=ny_w)+slag(lnAvginc,listw=ny_w), 
                    data=crimedata,
                    model="random", tol.solve=1.0e-20)

#Hausman test
phtest(pslx_modelfe, pslx_modelre)

#spatial error models

#spatial error fixed effects model
perror_modelfe <- spml(lnViolent ~ lnProp+lnAvgAge+MultGen+Child+Sex+Married+Divorced+White+Black+Hispanic+SouthAmerica+
                       Africa+Asia+Local+lnYearUSA+lnEdu+EmpStat+WeekHrs+EmpLast1+EmpLast2+LnIntin+VetStat+
                       lnAvginc, data=crimedata, index=NULL, listw=ny_w,
                     model="within", effect="individual", lag=FALSE, spatial.error="kkp", tol.solve=1.0e-20)

#spatial error random effects model
perror_modelre <- spml(lnViolent ~ lnProp+lnAvgAge+MultGen+Child+Sex+Married+Divorced+White+Black+Hispanic+SouthAmerica+
                       Africa+Asia+Local+lnYearUSA+lnEdu+EmpStat+WeekHrs+EmpLast1+EmpLast2+LnIntin+VetStat+
                       lnAvginc, data=crimedata, index=NULL, listw=ny_w,
                     model="random", lag=FALSE, spatial.error="kkp", tol.solve=1.0e-20)

#Hausman test
sphtest(perror_modelfe, perror_modelre)

#spatial durbin models

crimedata2 <- pdata.frame(crimedata, index=c("coun", "Year"), drop.index=TRUE)

#spatial durbin fixed effects model
pdurbin_modelfe <- spml(lnViolent ~ lnProp+lnAvgAge+MultGen+Child+Sex+Married+Divorced+White+Black+Hispanic+SouthAmerica+
                          Africa+Asia+Local+lnYearUSA+lnEdu+EmpStat+WeekHrs+EmpLast1+EmpLast2+LnIntin+VetStat+
                          lnAvginc+
                          slag(crimedata2$lnProp,listw=ny_w)+slag(crimedata2$lnAvgAge,listw=ny_w)+slag(crimedata2$MultGen,listw=ny_w)+
                          slag(crimedata2$Child,listw=ny_w)+slag(crimedata2$Sex,listw=ny_w)+slag(crimedata2$Married,listw=ny_w)+
                          slag(crimedata2$Divorced,listw=ny_w)+slag(crimedata2$White,listw=ny_w)+slag(crimedata2$Black,listw=ny_w)+
                          slag(crimedata2$Hispanic,listw=ny_w)+slag(crimedata2$SouthAmerica,listw=ny_w)+slag(crimedata2$Africa,listw=ny_w)+
                          slag(crimedata2$Asia,listw=ny_w)+slag(crimedata2$Local,listw=ny_w)+slag(crimedata2$lnYearUSA,listw=ny_w)+
                          slag(crimedata2$lnEdu,listw=ny_w)+slag(crimedata2$EmpStat,listw=ny_w)+slag(crimedata2$WeekHrs,listw=ny_w)+
                          slag(crimedata2$EmpLast1,listw=ny_w)+slag(crimedata2$EmpLast2,listw=ny_w)+slag(crimedata2$LnIntin,listw=ny_w)+
                          slag(crimedata2$VetStat,listw=ny_w)+slag(crimedata2$lnAvginc,listw=ny_w), 
                      data=crimedata, listw=ny_w, lag = TRUE, spatial.error = "none", 
                      model = "within", effect="individual", tol.solve = 1e-10)

#spatial durbin random effects model
pdurbin_modelre <- spml(lnViolent ~ lnProp+lnAvgAge+MultGen+Child+Sex+Married+Divorced+White+Black+Hispanic+SouthAmerica+
                        Africa+Asia+Local+lnYearUSA+lnEdu+EmpStat+WeekHrs+EmpLast1+EmpLast2+LnIntin+VetStat+
                        lnAvginc+
                        slag(lnProp,listw=ny_w)+slag(lnAvgAge,listw=ny_w)+slag(MultGen,listw=ny_w)+
                        slag(Child,listw=ny_w)+slag(Sex,listw=ny_w)+slag(Married,listw=ny_w)+
                        slag(Divorced,listw=ny_w)+slag(White,listw=ny_w)+slag(Black,listw=ny_w)+
                        slag(Hispanic,listw=ny_w)+slag(SouthAmerica,listw=ny_w)+slag(Africa,listw=ny_w)+
                        slag(Asia,listw=ny_w)+slag(Local,listw=ny_w)+slag(lnYearUSA,listw=ny_w)+
                        slag(lnEdu,listw=ny_w)+slag(EmpStat,listw=ny_w)+slag(WeekHrs,listw=ny_w)+
                        slag(EmpLast1,listw=ny_w)+slag(EmpLast2,listw=ny_w)+slag(LnIntin,listw=ny_w)+
                        slag(VetStat,listw=ny_w)+slag(lnAvginc,listw=ny_w), 
                        data=crimedata, index = NULL, listw=ny_w, lag = TRUE, spatial.error = "none", 
                        model = "random",tol.solve = 1e-10)

#Hausman test
sphtest(pdurbin_modelfe, pdurbin_modelre)

#spatial durbin error models

#spatial durbin error fixed effects model
pdurbinerror_modelfe <- spml(lnViolent ~ lnProp+lnAvgAge+MultGen+Child+Sex+Married+Divorced+White+Black+Hispanic+SouthAmerica+
                             Africa+Asia+Local+lnYearUSA+lnEdu+EmpStat+WeekHrs+EmpLast1+EmpLast2+LnIntin+VetStat+
                             lnAvginc+
                               slag(crimedata2$lnProp,listw=ny_w)+slag(crimedata2$lnAvgAge,listw=ny_w)+slag(crimedata2$MultGen,listw=ny_w)+
                               slag(crimedata2$Child,listw=ny_w)+slag(crimedata2$Sex,listw=ny_w)+slag(crimedata2$Married,listw=ny_w)+
                               slag(crimedata2$Divorced,listw=ny_w)+slag(crimedata2$White,listw=ny_w)+slag(crimedata2$Black,listw=ny_w)+
                               slag(crimedata2$Hispanic,listw=ny_w)+slag(crimedata2$SouthAmerica,listw=ny_w)+slag(crimedata2$Africa,listw=ny_w)+
                               slag(crimedata2$Asia,listw=ny_w)+slag(crimedata2$Local,listw=ny_w)+slag(crimedata2$lnYearUSA,listw=ny_w)+
                               slag(crimedata2$lnEdu,listw=ny_w)+slag(crimedata2$EmpStat,listw=ny_w)+slag(crimedata2$WeekHrs,listw=ny_w)+
                               slag(crimedata2$EmpLast1,listw=ny_w)+slag(crimedata2$EmpLast2,listw=ny_w)+slag(crimedata2$LnIntin,listw=ny_w)+
                               slag(crimedata2$VetStat,listw=ny_w)+slag(crimedata2$lnAvginc,listw=ny_w), 
                             data=crimedata, index = NULL,listw = ny_w, lag = TRUE, 
                             spatial.error = "kkp", model = "within", effect="individual", tol.solve = 1e-10)

#spatial durbin error random effects model
pdurbinerror_model <- spml(lnViolent ~ lnProp+lnAvgAge+MultGen+Child+Sex+Married+Divorced+White+Black+Hispanic+SouthAmerica+
                             Africa+Asia+Local+lnYearUSA+lnEdu+EmpStat+WeekHrs+EmpLast1+EmpLast2+LnIntin+VetStat+
                             lnAvginc+
                             slag(lnProp,listw=ny_w)+slag(lnAvgAge,listw=ny_w)+slag(MultGen,listw=ny_w)+
                             slag(Child,listw=ny_w)+slag(Sex,listw=ny_w)+slag(Married,listw=ny_w)+
                             slag(Divorced,listw=ny_w)+slag(White,listw=ny_w)+slag(Black,listw=ny_w)+
                             slag(Hispanic,listw=ny_w)+slag(SouthAmerica,listw=ny_w)+slag(Africa,listw=ny_w)+
                             slag(Asia,listw=ny_w)+slag(Local,listw=ny_w)+slag(lnYearUSA,listw=ny_w)+
                             slag(lnEdu,listw=ny_w)+slag(EmpStat,listw=ny_w)+slag(WeekHrs,listw=ny_w)+
                             slag(EmpLast1,listw=ny_w)+slag(EmpLast2,listw=ny_w)+slag(LnIntin,listw=ny_w)+
                             slag(VetStat,listw=ny_w)+slag(lnAvginc,listw=ny_w), 
                           data=crimedata, index = NULL,listw = ny_w, lag = TRUE, 
                           spatial.error = "kkp", model = "random",tol.solve = 1e-10)

#model selection 
#lagrange multiplier tests

#lagrange multiplier error test
rlme_model <- slmtest(lnViolent ~ lnProp+lnAvgAge+MultGen+Child+Sex+Married+Divorced+White+Black+Hispanic+SouthAmerica+
                        Africa+Asia+Local+lnYearUSA+lnEdu+EmpStat+WeekHrs+EmpLast1+EmpLast2+LnIntin+VetStat+
                        lnAvginc, data=crimedata, listw=ny_w, test="rlme")

#lagrange multiplier lag test
rlml_model <- slmtest(lnViolent ~ lnProp+lnAvgAge+MultGen+Child+Sex+Married+Divorced+White+Black+Hispanic+SouthAmerica+
                        Africa+Asia+Local+lnYearUSA+lnEdu+EmpStat+WeekHrs+EmpLast1+EmpLast2+LnIntin+VetStat+
                        lnAvginc, data=crimedata, listw=ny_w, test="rlml")

#likelihood ratio tests

#test with SDM

#first test: SDM vs SAR
diff <- length(coef(pdurbin_modelfe))-length(coef(plag_modelfe))
teststat_lag <- 2*(pdurbin_modelfe$logLik-plag_modelfe$logLik)
pchisq(teststat_lag,df=diff,lower.tail=FALSE)

#second test: SDM vs SEM
diff <- length(coef(pdurbin_modelfe))-length(coef(perror_modelfe))
teststat_error <- 2*(pdurbin_modelfe$logLik-perror_modelfe$logLik)
pchisq(teststat_error,df=diff,lower.tail=FALSE)

#test with SDEM

#first test: SDEM vs SAR
diff <- length(coef(pdurbinerror_modelfe))-length(coef(plag_modelfe))
teststat_lag <- 2*(pdurbinerror_modelfe$logLik-plag_modelfe$logLik)
pchisq(teststat_lag,df=diff,lower.tail=FALSE)

#second test: SDEM vs SEM
diff <- length(coef(pdurbinerror_modelfe))-length(coef(perror_modelfe))
teststat_error <- 2*(pdurbinerror_modelfe$logLik-perror_modelfe$logLik)
pchisq(teststat_error,df=diff,lower.tail=FALSE)

#wald test

#SDM vs SLX
waldtest(pdurbin_modelfe, pslx_modelfe)

#SDEM vs SLX
waldtest(pdurbinerror_modelfe, pslx_modelfe)

