library(haven)
library(dplyr)
library(here)
library(lmtest)
library(stats)
library(car)

#Read DHS Water Data
water.data <- haven::read_sas(here::here("Data","dhs_water.sas7bdat"))
class(water.data$CASEID)

#Remove NAs from DHS Water Data
water.data.noNA <- water.data %>% na.omit()

#Convert variables into factor variables
water.data.noNA$WaterDistance_factor <- factor(water.data.noNA$WaterDistance)
water.data.noNA$Breastfeeding_factor <- factor(water.data.noNA$Breastfeeding)
water.data.noNA$WaterSource_factor <- factor(water.data.noNA$WaterSource)

table(water.data.noNA$Breastfeeding_factor,water.data.noNA$WaterDistance_factor)
#Full logistic model
logistic_model <- glm(Breastfeeding_factor ~ WaterDistance_factor + WaterSource_factor, data = water.data.noNA, family = binomial (link = 'logit'))
summary(logistic_model)
logistic_model

#Get OR and 95% CIs on OR
cbind(exp(coef(logistic_model)),exp(confint.default(logistic_model)))


logistic_model_reduced <- glm(Breastfeeding_factor ~ WaterSource_factor, dat = water.data.noNA, family = binomial (link= 'logit'))
summary(logistic_model_reduced)
logistic_model_reduced

anova(logistic_model,logistic_model_reduced,test="Chisq")
car::Anova(logistic_model,test="Wald")

#Recode water distance
water.data.noNA.recode <- water.data.noNA %>% mutate(WaterDistance_factor_recode = ifelse(WaterDistance_factor == 1, 1, -1))

#Run model with recoded data
logistic_model_recoded <- glm(Breastfeeding_factor ~WaterDistance_factor_recode + WaterSource_factor, 
                           data = water.data.noNA.recode,family=binomial(link="logit"))
logistic_model_recoded
cbind(exp(coef(logistic_model_recoded)),exp(confint.default(logistic_model_recoded)))
