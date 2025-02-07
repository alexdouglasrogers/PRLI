#Final considerations value#

setwd("/Users/alexrogers/Desktop/NBSAP_project")
library(dplyr)
library(readr)
library(lme4)
library(lmerTest)
library(car)
library(robustlmm)
library(beepr)
library(boot)

source("functions/NBSAP_functions.R")

dat <- read_csv("Outputs/qog_ts_short.csv")

### Hierarchical model? ###

model_null <- lmer(Value ~ 1 + (1 | cname), data = dat)
ICC <- as.numeric(VarCorr(model_null)$cname[1]) / (as.numeric(VarCorr(model_null)$cname[1]) + attr(VarCorr(model_null), "sc")^2)
ICC

#High ICC justifies use of hierarchical model

m1 <- lmer(Value ~ 1
           +(1|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
m2 <- lm(Value ~ 1, data=dat)
anova(m1,m2)
#Better fit


### Mod_1: Base Model ###

m1 <- lmer(Value ~ year_ind
           +(year_ind|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m1)



m1r <- rlmer(Value ~ year_ind
           +(year_ind|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m1r)

set.seed(123)
results <- bootMer(m1, fixef, nsim = 5000)
Final <- bs_table(results, m1, m1r)
names(Final)[2:7] <- c("boot estimate", "boot CI", "boot sig", "robust estimate", "robust CI", "robust sig")
Final$parameter[2] <- c("year")
write.csv(Final, "Outputs/Model_Results/m1_time_mod.csv")
Final <- read.csv("Outputs/Model_Results/m1_time_mod.csv")[,2:8]
table_maker(Final, cap ="Model 1: Time Model", countries = 193, random_slopes = "year")


residuals <- resid(m1)
fitted_values <- fitted(m1)
plot(fitted_values, residuals)
abline(h = 0, col = "red")


### Mod_2: Time Model ###

m2 <- lmer(Value ~ year_ind*start_value
             +(year_ind|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m2)

new_data <- data.frame(year_ind = c(1, 1),
                       start_value = c(0.5,1))
fixed_effect_predictions <- predict(m2, newdata = new_data,re.form = NA)
print(fixed_effect_predictions)
3.482e-04-(0*8.520e-03)+0.5+(0*0.5*8.314e-03)


m2r <- rlmer(Value ~ year_ind*start_value
           +(year_ind|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m2r)

set.seed(123)
results <- bootMer(m2, fixef, nsim = 5000)
Final <- bs_table(results, m2, m2r)
names(Final)[2:7] <- c("boot estimate", "boot CI", "boot sig", "robust estimate", "robust CI", "robust sig")
Final$parameter[c(2,3,4)] <- c("year","base RLI", "year:base RLI")
#write.csv(Final, "Outputs/Model_Results/m2_time_base_mod.csv")
#Final <- read.csv("Outputs/Model_Results/m2_time_base_mod.csv")[,2:8]
table_maker(Final, cap ="Model 2: Time & Base RLI Model",countries = 193, random_slopes = "year")



m2.1 <- lmer(Value ~ year_ind*start_value+I(year_ind^2)*start_value
           +(year_ind|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m2.1) #this one would be good to model 1000 year trajectory possibly.

m2.1r <- rlmer(Value ~ year_ind*start_value+I(year_ind^2)*start_value
             +(year_ind|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m2.1r)

set.seed(123)
results <- bootMer(m2.1, fixef, nsim = 5000)
Final <- bs_table(results, m2.1, m2.1r)
names(Final)[2:7] <- c("boot estimate", "boot CI", "boot sig", "robust estimate", "robust CI", "robust sig")
Final$parameter[c(2,3,4,5,6)] <- c("year","base RLI","year^2", "year:base RLI", "year^2:base RLI")
write.csv(Final, "Outputs/Model_Results/m2_non-linear_time_base_mod.csv")
Final <- read.csv("Outputs/Model_Results/m2_non-linear_time_base_mod.csv")[,2:8]
table_maker(Final, cap ="Model 2: Non-linear Time & Base RLI Model", countries = 193, random_slopes = "year")


residuals <- resid(m2.1)
fitted_values <- fitted(m2.1)
plot(fitted_values, residuals)
abline(h = 0, col = "red")

### Mod_3: NBSAP Time Model ###

m3.1 <- lmer(Value ~ start_value*year_ind + NBSAP*year_ind+ I(year_ind^2)
             +(year_ind+NBSAP|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m3.1)

m3.1r <- rlmer(Value ~ start_value*year_ind + NBSAP*year_ind+ I(year_ind^2)
               +(year_ind+NBSAP|cname), data=dat,
               control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m3.1r)

set.seed(123)
#results <- bootMer(m3.1, fixef, nsim = 5000)
Final <- bs_table(results, m3.1, m3.1r)
names(Final)[2:7] <- c("boot estimate", "boot CI", "boot sig", "robust estimate", "robust CI", "robust sig")
Final$parameter[c(2:7)] <- c("base RLI", "year", "NBSAP","year^2", "year:base RLI", "year:NBSAP")
write.csv(Final, "Outputs/Model_Results/NBSAP_mod.csv")
Final <- read.csv("Outputs/Model_Results/NBSAP_mod.csv")[,2:8]
table_maker(Final, cap ="Model 3: NBSAP Model", countries = 193, obs = 3860, random_slopes = "NBSAP, year")


residuals <- resid(m3.1)
fitted_values <- fitted(m3.1)
plot(fitted_values, residuals)
abline(h = 0, col = "red")



m3.2 <- lmer(Value ~ start_value*year_ind + NBSAP_Lag_5*year_ind+ I(year_ind^2)
             +(year_ind+NBSAP_Lag_5|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m3.2)
m3.2r <- rlmer(Value ~ start_value*year_ind + NBSAP_Lag_5*year_ind+ I(year_ind^2)
               +(year_ind+NBSAP_Lag_5|cname), data=dat,
               control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))

set.seed(123)
#results <- bootMer(m3.2, fixef, nsim = 5000)
Final <- bs_table(results, m3.2, m3.2r)
names(Final)[2:7] <- c("boot estimate", "boot CI", "boot sig", "robust estimate", "robust CI", "robust sig")
Final$parameter[c(2:7)] <- c("base RLI", "year", "NBSAP 6 year lag", "year^2", "year:base RLI", "year:NBSAP 6 year lag")
write.csv(Final, "Outputs/Model_Results/NBSAP_mod_lag6.csv")
Final <- read.csv("Outputs/Model_Results/NBSAP_mod_lag6.csv")[,2:8]
table_maker(Final, cap ="Model 3.1: NBSAP 6 Year Lag Model", countries = 193, obs = 3860,random_slopes = "NBSAP 6 year lag, year")

m3.3 <- lmer(Value ~ start_value*year_ind + NBSAP_Lag_10*year_ind+ I(year_ind^2)
             +(year_ind+NBSAP_Lag_10|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m3.3)
m3.3r <- rlmer(Value ~ start_value*year_ind + NBSAP_Lag_10*year_ind+ I(year_ind^2)
               +(year_ind+NBSAP_Lag_10|cname), data=dat,
               control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))

set.seed(123)
#results <- bootMer(m3.3, fixef, nsim = 5000)
Final <- bs_table(results, m3.3, m3.3r)
names(Final)[2:7] <- c("boot estimate", "boot CI", "boot sig", "robust estimate", "robust CI", "robust sig")
Final$parameter[c(2:7)] <- c("base RLI", "year", "NBSAP 12 year lag", "year^2", "year:base RLI", "year:NBSAP 12 year lag")
write.csv(Final, "Outputs/Model_Results/NBSAP_mod_lag12.csv")
Final <- read.csv("Outputs/Model_Results/NBSAP_mod_lag12.csv")[,2:8]
table_maker(Final, cap ="Model 3.2: NBSAP 12 year lag Model", countries = 193, obs = 3860, random_slopes = "NBSAP 12 year lag, year")

### Mod_4: Region Model ###

m4 <- lmer(Value ~
             wb_region*year_ind
           +(year_ind|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m4)

m4r <- rlmer(Value ~
             wb_region*year_ind
           +(year_ind|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m4)

set.seed(123)
#results <- bootMer(m4, fixef, nsim = 5000)
Final <- bs_table(results, m4, m4r)
names(Final)[2:7] <- c("boot estimate", "boot CI", "boot sig", "robust estimate", "robust CI", "robust sig")
Final$parameter[c(2:14)] <- c("Europe & Central Asia", "Latin America & Caribbean", "Middle East & North Africa", 
                              "North America", "South Asia","Sub-Saharan Africa",
                              "year", "year:Europe & Central Asia", "year:Latin America & Caribbean", "year:Middle East & North Africa", 
                              "year:North America", "year:South Asia","year:Sub-Saharan Africa")
write.csv(Final, "Outputs/Model_Results/region_mod.csv")
Final <- read.csv("Outputs/Model_Results/region_mod.csv")[,2:8]
table_maker(Final, cap ="Model 4: Region Model", countries = 193, random_slopes = "year")

summary(lmer(Value ~
       wb_region*year_ind*island
     +(year_ind|cname), data=dat,
     control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5))))

residuals <- resid(m4)
fitted_values <- fitted(m4)
plot(fitted_values, residuals)
abline(h = 0, col = "red")

### Mod 5: GDP Model ###

summary(lmer(Value ~ 
               year_ind*wdi_gdpcapcur 
             +(year_ind+wdi_gdpcapcur|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5))))

summary(lmer(Value ~ 
               year_ind*wdi_gdpcapcur + start_value
             +(year_ind+wdi_gdpcapcur|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5))))

summary(lmer(Value ~ 
             year_ind*wdi_gdpcapcur+ year_ind*I(wdi_gdpcapcur^2)
             +(year_ind+wdi_gdpcapcur|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5))))
#Square term not significant
 
summary(lmer(Value ~ wdi_gdpcapcur*wb_region*year_ind 
             +(year_ind+wdi_gdpcapcur|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5))))
#Interacting with region is interesting but the effect size is small (three 
# regions would be significant if I made the GDP variable every $10,000)

contrasts(dat$wb_region) <- NULL

m5 <- lmer(Value ~wdi_gdpcapcur*wb_region*year_ind 
             +(year_ind+wdi_gdpcapcur|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m5) 

m5r <- rlmer(Value ~wdi_gdpcapcur*wb_region*year_ind 
             +(year_ind+wdi_gdpcapcur|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m5r)

set.seed(123)
#results <- bootMer(m5, fixef, nsim = 5000)
Final <- bs_table(results, m5, m5r)
names(Final)[2:7] <- c("boot estimate", "boot CI", "boot sig", "robust estimate", "robust CI", "robust sig")
Final$parameter[c(2:28)] <- c("GDP per capita","Europe & Central Asia", "Latin America & Caribbean", "Middle East & North Africa", 
                              "North America", "South Asia","Sub-Saharan Africa", "year", "GDP:Europe & Central Asia", 
                              "GDP:Latin America & Caribbean", "GDP:Middle East & North Africa", 
                              "GDP:North America", "GDP:South Asia","GDP:Sub-Saharan Africa", "GDP:year",
                              "year:Europe & Central Asia", "year:Latin America & Caribbean", "year:Middle East & North Africa", 
                              "year:North America", "year:South Asia","year:Sub-Saharan Africa",
                              "GDP:year:Europe & Central Asia", "GDP:year:Latin America & Caribbean", "GDP:year:Middle East & North Africa", 
                              "GDP:year:North America", "GDP:year:South Asia","GDP:year:Sub-Saharan Africa")
write.csv(Final, "Outputs/Model_Results/income_group.csv")
Final <- read.csv("Outputs/Model_Results/income_group.csv")[,2:8]
table_maker(Final, random_slopes = "year, GDP", cap ="Model 5: GDP Region Model", countries = 192, obs = 3778)


residuals <- resid(m5)
fitted_values <- fitted(m5)
plot(fitted_values, residuals)
abline(h = 0, col = "red")

### Mod 6: Island Model ###

m6 <- lmer(Value ~
             year_ind*island+
             (year_ind|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))

summary(m6)

m6r <- rlmer(Value ~
             year_ind*island+
             (year_ind|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))

set.seed(123)
#results <- bootMer(m6, fixef, nsim = 5000)
Final <- bs_table(results, m6, m6r)
names(Final)[2:7] <- c("boot estimate", "boot CI", "boot sig", "robust estimate", "robust CI", "robust sig")
Final$parameter[c(2:4)] <- c("year", "island", "year:island")
Final <- bs_table(results, m6, m6r)
write.csv(Final, "Outputs/Model_Results/island_mod.csv")
Final <- read.csv("Outputs/Model_Results/island_mod.csv")[,2:8]
table_maker(Final, cap ="Model 6: Island Model", random_slopes = "year", countries = 193, obs = 3860)


residuals <- resid(m6)
fitted_values <- fitted(m6)
plot(fitted_values, residuals)
abline(h = 0, col = "red")

### Model 7: Pop_density Model ###

summary(lmer(Value ~ I(wdi_popden/1000)*year_ind+
               (year_ind|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5))))
#candidate model but leaves out some important nuance 

m7 <- lmer(Value ~ I(wdi_popden/1000)*year_ind*start_value+
             (year_ind+I(wdi_popden/1000)|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m7)

m7r <- rlmer(Value ~ I(wdi_popden/1000)*year_ind*start_value+
             (year_ind+I(wdi_popden/1000)|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))

set.seed(123)
#results <- bootMer(m7, fixef, nsim = 5000)
Final <- bs_table(results, m7, m7r)
names(Final)[2:7] <- c("boot estimate", "boot CI", "boot sig", "robust estimate", "robust CI", "robust sig")
Final$parameter[c(2:8)] <- c("pop density", "year", "base RLI value",
                             "year:pop density", "base RLI value:pop density",
                             "year:base RLI vale", "year:base RLI value:pop density")
write.csv(Final, "Outputs/Model_Results/m7_pop_den_mod.csv")
Final <- read.csv("Outputs/Model_Results/m7_pop_den_mod.csv")[,2:8]
table_maker(Final, cap ="Model 7: Population Density Model", random_slopes = "year, pop density", countries = 193, obs = 3842)

length(unique(dat[dat$year_ind == 19 & dat$Value < 0.731,]$cname))
length(unique(dat[dat$year_ind == 19 & dat$Value < 0.731,]$cname))/length(unique(dat$cname))
#Shows what percent of countries have positive relations with globalization.


residuals <- resid(m7)
fitted_values <- fitted(m7)
plot(fitted_values, residuals)
abline(h = 0, col = "red")


### Mod 8: Globalization Mod ###

m8 <- lmer(Value ~ dr_ig*year_ind+
              (year_ind+dr_ig|cname), data=dat,
            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m8)

m8r <- rlmer(Value ~ dr_ig*year_ind+
             (year_ind+dr_ig|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m8r)

set.seed(123)
#results <- bootMer(m8, fixef, nsim = 5000)
Final <- bs_table(results, m8, m8r)
names(Final)[2:7] <- c("boot estimate", "boot CI", "boot sig", "robust estimate", "robust CI", "robust sig")
Final$parameter[c(2:4)] <- c("globalization", "year", "year:globalization")
write.csv(Final, "Outputs/Model_Results/m8_globalization_mod.csv")
Final <- read.csv("Outputs/Model_Results/m8_globalization_mod.csv")[,2:8]
table_maker(Final, cap ="Model 8: Globalization Model", random_slopes = "year, globalization", countries = 189, obs = 3772)


residuals <- resid(m8)
fitted_values <- fitted(m8)
plot(fitted_values, residuals)
abline(h = 0, col = "red")

m9.1 <- lmer(Value ~ dr_ig*island*year_ind+
                (year_ind+dr_ig|cname), data=dat,
              control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m9.1)
m9.1r <- rlmer(Value ~ dr_ig*island*year_ind+
               (year_ind+dr_ig|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))

set.seed(123)
#results <- bootMer(m9.1, fixef, nsim = 5000)
Final <- bs_table(results, m9.1, m9.1r)
names(Final)[2:7] <- c("boot estimate", "boot CI", "boot sig", "robust estimate", "robust CI", "robust sig")
Final$parameter[c(2:8)] <- c("globalization", "island", "year","island:globalization",
                             "year:globalization", "year:island", "year:island:globalization")
write.csv(Final, "Outputs/Model_Results/m8.1_globalization_island_mod.csv")
Final <- read.csv("Outputs/Model_Results/m8.1_globalization_island_mod.csv")[,2:8]
table_maker(Final, cap ="Model 8.1: Globalization & Island Model", random_slopes = "year, globalization", countries = 189, obs = 3772)

new_data <- data.frame(year_ind = c(0,1,0,1),
                       island = c(0,0,0,0),
                       dr_ig = c(0,0,1,1))
fixed_effect_predictions <- predict(m9.1, newdata = new_data,re.form = NA)
print(fixed_effect_predictions)

residuals <- resid(m9.1)
fitted_values <- fitted(m9.1)
plot(fitted_values, residuals)
abline(h = 0, col = "red")

### Mod 9: Indigenous mod ###
summary(lmer(Value ~ indig_pcnt* +
       (indig_pcnt|cname), data=dat,
     control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5))))


m9 <- lmer(Value ~  indig_pcnt*year_ind+
              (year_ind|cname), data=dat,
            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m9)

m9a <- lmer(Value ~  indig_pcnt*year_ind+
             (year_ind+ indig_pcnt|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m9)
anova(m9,m9a) #indigenous control in random effects not helpful so use m9

m9r <- rlmer(Value ~ indig_pcnt*year_ind +
              (year_ind|cname), data=dat,
            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))

set.seed(123)
#results <- bootMer(m9, fixef, nsim = 5000)
Final <- bs_table(results, m9, m9r)
names(Final)[2:7] <- c("boot estimate", "boot CI", "boot sig", "robust estimate", "robust CI", "robust sig")
Final$parameter[c(2:4)] <- c("indigenous control", "year", "year:indigenous control")
write.csv(Final, "Outputs/Model_Results/m9_indigenous_mod.csv")
Final <- read.csv("Outputs/Model_Results/m9_indigenous_mod.csv")[,2:8]
table_maker(Final, cap ="Model 9: Indigenous Land Control Model",random_slopes = "year", countries = 96, obs = 1920)

beep(2)

range(na.omit(dat$indig_pcnt))


residuals <- resid(m9)
fitted_values <- fitted(m9)
plot(fitted_values, residuals)
abline(h = 0, col = "red")


### NOT SIGNIFICANT AT ALL MODELS ##

### QOG ###

summary(lmer(Value ~ 
               fh_fog*year_ind+
               (year_ind+fh_fog|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5))))


m7.1 <- lmer(Value ~ start_value*year_ind+
               fh_fog*year_ind+
               (year_ind+fh_fog|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m7.1)

m7 <- lmer(Value ~ fh_fog + year_ind+
             (fh_fog+year_ind|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m7)



### Gini###

m10 <- lmer(Value ~ wdi_gini*year_ind+
              (year_ind+wdi_gini|cname), data=dat,
            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m10)

### Economic Growth Model ###

m11 <- lmer(Value ~ wdi_gdpcapgr*year_ind+
              (year_ind+wdi_gdpcapgr|cname), data=dat,
            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m11)

### park mod ###

m13 <- lmer(Value ~ pr_protected +
              (pr_protected|cname), data=dat,
            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m13)

m13.1 <- lmer(Value ~ pr_protected*year_ind + 
                (year_ind+pr_protected|cname), data=dat,
              control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m13.1)

m13.2 <- lmer(Value ~ pr_protected*start_value*year_ind+
                (year_ind+pr_protected|cname), data=dat,
              control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m13.2)

sqrt(0.0011534/0.0014597)


### Ag mod ###

m8 <- lmer(Value ~ year_ind*fao_luagr*start_value+
             (year_ind+fao_luagr|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m8)

#interact with QOG region and NBSAP.

summary(lmer(Value ~ fao_luagr*year_ind*fh_fog+
               (year_ind+fao_luagr+fh_fog|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5))))


a<- lmer(Value ~ fao_luagr*year_ind*NBSAP+
           (year_ind+NBSAP+fao_luagr|cname), data=dat,
         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(a)

b<- lmer(Value ~ fao_luagr*year_ind*wb_region+
           (year_ind+fao_luagr|cname), data=dat,
         control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(b)

### Mod 10: KBA mod ###

m10 <- lmer(Value ~ KBA_protected +
              (KBA_protected|cname), data=dat,
            control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m10)
m10r <- lmer(Value ~ KBA_protected +
               (KBA_protected|cname), data=dat,
             control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))

set.seed(123)
results <- bootMer(m10, fixef, nsim = 5000)
Final <- bs_table_mult(results, m10, m10r)
write.csv(Final, "Outputs/Model_Results/m10_KBA_mod.csv")
table_maker(Final, cap ="KBA Model")


m10.1 <- lmer(Value ~ KBA_protected*year_ind*start_value+
                +(year_ind+KBA_protected|cname), data=dat,
              control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
m10.1r <- rlmer(Value ~ KBA_protected*year_ind*start_value+
                  +(year_ind+KBA_protected|cname), data=dat,
                control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))

set.seed(123)
#results <- bootMer(m10.1, fixef, nsim = 5000)
Final <- bs_table_mult(results, m10.1, m10.1r)
write.csv(Final, "Outputs/Model_Results/m10.1_KBA_Base_RLI_mod.csv")
table_maker(Final, cap ="KBA Model")

library("splines")

m10.2 <- lmer(Value ~ KBA_protected+year_ind+start_value+
                KBA_protected:year_ind+
                KBA_protected*start_value+
                year_ind*start_value+
                KBA_protected:year_ind:ns(start_value, knots = c(0.65,0.8,0.9))
              +(year_ind+KBA_protected|cname), data=dat,
              control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))

new_data <- data.frame(year_ind = c(0,1,0,1,0,1),
                       KBA_protected = c(0.2,0.2,rep(mean(na.omit(dat[dat$year_ind==0,]$KBA_protected)),2),0.5,0.5),
                       start_value = rep(0.95,6))
fixed_effect_predictions <- predict(m10.2, newdata = new_data,re.form = NA)
print(fixed_effect_predictions)




m10.2r <- rlmer(Value ~ KBA_protected+year_ind+start_value+
                  KBA_protected:year_ind+
                  KBA_protected*start_value+
                  year_ind*start_value+
                  KBA_protected:year_ind:ns(start_value, knots = c(0.65,0.8,0.9))
                +(year_ind+KBA_protected|cname), data=dat,
                control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))

set.seed(123)
#results1 <- bootMer(m10.2, fixef, nsim = 5000)
Final <- bs_table_mult(results1, m10.2, m10.2r)
write.csv(Final, "Outputs/Model_Results/m10.2_KBA_spline_Base_RLI_mod.csv")
table_maker(Final, cap ="KBA Spline Model")

### NBSAP alternative####
summary(dat$total_change)
a <- lmer(Value ~ start_value*year_ind*NBSAP_Lag_10*year_ind+
     +(year_ind+NBSAP_Lag_10|cname), data=dat,
     control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
b <- lmer(Value ~ start_value*year_ind*NBSAP_Lag_10*year_ind+
            +(year_ind|cname), data=dat,
          control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
anova(a,b)

a <- lmer(Value ~ start_value*year_ind + NBSAP_Lag_10*year_ind+
            +(year_ind+NBSAP_Lag_10|cname), data=dat,
          control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
b <- lmer(Value ~ start_value*year_ind + NBSAP_Lag_10*year_ind+
            +(year_ind|cname), data=dat,
          control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
anova(a,b)

a <- lmer(Value ~ start_value*year_ind + NBSAP_Lag_10*year_ind+ I(year_ind^2)
            +(year_ind+NBSAP_Lag_10|cname), data=dat,
          control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
b <- lmer(Value ~ start_value*year_ind + NBSAP_Lag_10*year_ind+
            +(year_ind+NBSAP_Lag_10|cname), data=dat,
          control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
anova(a,b)

library(splines)

a <- lmer(Value ~ start_value*ns(year_ind, knots = c(6,12)) + NBSAP*ns(year_ind, knots = c(6,12))+ I(year_ind^2)
       +(year_ind+NBSAP_Lag_10|cname), data=dat,
     control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(a)


m <- lmer(Value ~year_ind*wdi_gdpcapcur*wb_region + year_ind*I(wdi_gdpcapcur^2)*wb_region
           +(year_ind+wdi_gdpcapcur|cname), data=dat,
           control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 1e5)))
summary(m) 
anova(m,n)
