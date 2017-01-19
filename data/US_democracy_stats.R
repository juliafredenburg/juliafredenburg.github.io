require("nortest")
library("nortest")

#Collect federal data from the american community survey
popchangedf = read.csv("/Users/dominicburkart/Desktop/democracy/PEP_2015_PEPTCOMP/PEP_2015_PEPTCOMP_with_ann.csv")
popdf = read.csv("/Users/dominicburkart/Desktop/democracy/PEP_2015_PEPANNRES/PEP_2015_PEPANNRES_with_ann.csv")
racedf = read.csv("/Users/dominicburkart/Desktop/democracy/ACS_15_5YR_B02001/ACS_15_5YR_B02001_with_ann.csv")
ginidf = read.csv("/Users/dominicburkart/Desktop/democracy/ACS_15_5YR_B19083/ACS_15_5YR_B19083_with_ann.csv")

#pare federal data down into a single dataframe
censusdf1 = merge(popchangedf, popdf, by="Geography")
censusdf2 = merge(racedf, ginidf, by="Geography")
feddf = merge(censusdf1,censusdf2, by="Geography")

#remove extraneous pointers
rm(censusdf1,censusdf2, ginidf, popchangedf, popdf, racedf)

#Collect electoral integrity project (EIP) data
load("/Users/dominicburkart/Desktop/democracy/PEI US 2016 state-level (PEI_US_1.0) 16-12-2018.RData")
pei <- `PEI_US_2016_state-level_(PEI_US_1.0)_16-12-2016`
pei$state[12] = "Hawaii" #to avoid merge issues
rm(`PEI_US_2016_state-level_(PEI_US_1.0)_16-12-2016`)

#Put it all together! We loose Hawaii and Puerto Rico, unfortunately, because EIP didn't collect data there.
all = merge(feddf, pei, by.x="Geography", by.y="state")
rm(feddf, pei)

#Test variables of interest for normality
ad.test(all$ratingstate_lci) #normal or bimodal A = 0.6111, p-value = 0.1061
ad.test(all$ratingstate_hci) #bimodal A = 1.2689, p-value = 0.002394
ad.test(all$Population.Estimate..as.of.July.1....2015) #logarithmic A = 3.9888, p-value = 4.44e-10
ad.test(all$Estimate..Total....Black.or.African.American.alone) #logarithmic (maybe) A = 3.6318, p-value = 3.342e-09
ad.test(all$Estimate..Total....Asian.alone) #logarithmic A = 9.777, p-value < 2.2e-16
ad.test(all$Cumulative.Estimates.of.the.Components.of.Population.Change...April.1..2010.to.July.1..2015...Net.Migration...International..2.) #logarithmic A = 7.3535, p-value < 2.2e-16
ad.test(all$PEIIndexi) #normal A = 0.46452, p-value = 0.2444

#Great! So we have our normally distributed dependent variable, PEI index, and some nifty IVs. Let's see if there's a linear relationship.
glm(scale(all$PEIIndexi)~ scale(all$Population.Estimate..as.of.July.1....2015)+scale(all$Cumulative.Estimates.of.the.Components.of.Population.Change...April.1..2010.to.July.1..2015...Net.Migration...International..2.)+scale(all$Estimate..Total....Black.or.African.American.alone))
glm(scale(all$PEIIndexi)~ scale(all$Estimate..Gini.Index)+scale(all$Cumulative.Estimates.of.the.Components.of.Population.Change...April.1..2010.to.July.1..2015...Net.Migration...International..2.))
glm(scale(all$PEIIndexi)~ scale(all$Cumulative.Estimates.of.the.Components.of.Population.Change...April.1..2010.to.July.1..2015...Total.Population.Change..1.))
x = glm(scale(all$PEIIndexi)~ scale(all$Estimate..Gini.Index))

cor.test(all$PEIIndexi,all$Estimate..Gini.Index)
plot(scale(all$PEIIndexi)~scale(all$Estimate..Gini.Index))
plot(x)
rm(x)

#evaluate the relative importance of each IV
fit = glm(scale(all$PEIIndexi) ~ scale(all$Population.Estimate..as.of.July.1....2015) + scale(all$Estimate..Gini.Index) + scale(all$Cumulative.Estimates.of.the.Components.of.Population.Change...April.1..2010.to.July.1..2015...Net.Migration...International..2.) + scale(all$Estimate..Total....White.alone))
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(fit, b = 10000, type = c("lmg", 
                                            "last", "first", "pratt"), rank = TRUE, 
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result


#this is the analysis we reported:
fit = lm(scale(all$PEIIndexi) ~ scale(all$Population.Estimate..as.of.July.1....2015) * scale(all$Estimate..Gini.Index) * scale(all$Cumulative.Estimates.of.the.Components.of.Population.Change...April.1..2010.to.July.1..2015...Net.Migration...International..2.) * scale(all$Estimate..Total....White.alone))
summary(fit)
#great! now let's add lgbt data. What we have now: adjusted R-squared is 0.287, p-value is 0.01905 F-statistic: 2.342 on 15 and 35 DF

#add LGBT population from http://www.lgbtmap.org/equality-maps/lgbt_populations
lgbt = read.csv("/Users/dominicburkart/Desktop/democracy/gallup-williamson_2013_LGBT_state_data - Feuille 1.csv")
lgbt$State = tolower(lgbt$State)
all$Geography = tolower(all$Geography)

all = merge(lgbt, all, by.x="State", by.y="Geography")
rm(lgbt)

fit = lm(scale(all$PEIIndexi) ~ scale(all$Population.Estimate..as.of.July.1....2015) * scale(all$Estimate..Gini.Index) * scale(all$Cumulative.Estimates.of.the.Components.of.Population.Change...April.1..2010.to.July.1..2015...Net.Migration...International..2.) * scale(all$Estimate..Total....White.alone) *scale(all$LGBT.Population.Density))
summary(fit)
# adding LGBT Population density decreases R-squared.
plot(fit)
# influential states: new york, DC, california
s47 = all %>% filter(!(all$State %in% c("district of columbia", "california", "new york")))
fit = lm(scale(s47$PEIIndexi) ~ scale(s47$Population.Estimate..as.of.July.1....2015) * scale(s47$Estimate..Gini.Index) * scale(s47$Cumulative.Estimates.of.the.Components.of.Population.Change...April.1..2010.to.July.1..2015...Net.Migration...International..2.) * scale(s47$Estimate..Total....White.alone) *scale(s47$LGBT.Population.Density))
summary(fit)
plot(fit)
# even in s47, this regression is insignificant. LGBT population density has no relationship with PEI index.
plot(s47$PEIIndexi, s47$LGBT.Population.Density)
# yup! i don't see any nonlinear relationships.


# #let's include hate crime data. 
# hate = read.csv("/Users/dominicburkart/Desktop/democracy/FBI_hate-crimes_2015.csv")
# hate$Participatingstate = tolower(hate$Participatingstate)
# all = merge(all, hate, by.x = "State", by.y ="Participatingstate")
# fit = lm(scale(all$PEIIndexi) ~ scale(all$Population.Estimate..as.of.July.1....2015) * scale(all$Estimate..Gini.Index) * scale(all$Cumulative.Estimates.of.the.Components.of.Population.Change...April.1..2010.to.July.1..2015...Net.Migration...International..2.) * scale(all$Estimate..Total....White.alone) *scale(all$Total.numberof.incidents.reported))
# summary(fit)
# # hate crime data lowers R squared and p value of model. Adjusted R-squared:  0.1925, p-value: 0.2401
# # again, new york cali and dc are the outliers
# rm(hate)

# s47 = all %>% filter(!(all$State %in% c("district of columbia", "california", "new york")))
# fit = lm(scale(s47$PEIIndexi) ~ scale(s47$Population.Estimate..as.of.July.1....2015) * scale(s47$Estimate..Gini.Index) * scale(s47$Cumulative.Estimates.of.the.Components.of.Population.Change...April.1..2010.to.July.1..2015...Net.Migration...International..2.) * scale(s47$Estimate..Total....White.alone) *scale(s47$Total.numberof.incidents.reported))
# summary(fit)
# removing the outliers actually makes the model less robust-- interesting! Adjusted R-squared:  0.1541, p-value: 0.3185

#s47 isn't very different than full 50 
fit = lm(scale(s47$PEIIndexi) ~ scale(s47$Population.Estimate..as.of.July.1....2015) * scale(s47$Estimate..Gini.Index) * scale(-s47$Cumulative.Estimates.of.the.Components.of.Population.Change...April.1..2010.to.July.1..2015...Net.Migration...International..2.) * scale(s47$Estimate..Total....White.alone))
summary(fit)


#WAIT LET'S SEE IF MEDIAN INCOME PLAYS A ROLE. That would make sense given our weird GINI interactions.

med = read.csv("/Users/dominicburkart/Desktop/democracy/median_income/ACS_15_5YR_S1903_with_ann.csv")
med$Geography = tolower(med$Geography)
all = merge(all, med, by.x = "State", by.y = "Geography")
rm(med)


fit = lm(scale(all$PEIIndexi) ~ scale(all$Population.Estimate..as.of.July.1....2015) * scale(all$Estimate..Gini.Index) * scale(all$Cumulative.Estimates.of.the.Components.of.Population.Change...April.1..2010.to.July.1..2015...Net.Migration...International..2.) * scale(all$Estimate..Total....White.alone) * scale(all$Median.income..dollars...Estimate..Households))
summary(fit)
#nope! this messes up our model. Adjusted R-squared:  0.2465, p-value: 0.1772

cor.test(all$Median.income..dollars...Estimate..Households, all$Estimate..Gini.Index)
# Pearson's product-moment
# 	correlation
# 
# data:  all$Median.income..dollars...Estimate..Households and all$Estimate..Gini.Index
# t = -0.30447, df = 48,
# p-value = 0.7621
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.3183612  0.2373442
# sample estimates:
# cor 
# -0.04390405 
