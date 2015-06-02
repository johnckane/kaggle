### This file simply re-fits the three models discussed specifically in the write-up.
setwd('/home/john/Kaggle/West Nile Virus Prediction/Data')

library(gam)
library(dplyr)


training_final <- read.csv("training_final.csv", stringsAsFactors = FALSE, header = TRUE)

training_final <- training_final %>%
    select(cut,
           species2,
           latitude,
           longitude,
           avg_may_temp,
           num_may_rain,
           avg_may_dew,
           avg_may_wet,
           avg_avg,
           avg_min,
           avg_max,
           avg_dew,
           avg_wet,
           wet_con,
           temp_range,
           temp30,
           rain14,
           dew30,
           wet30,
           range1,
           wnvpresent)

str(training_final)
training_final$species2 <- as.factor(training_final$species2)
training_final$cut <- as.factor(training_final$cut)

### Logistic regression, forward selection variables
forward_model <- glm(data=training_final,
                     wnvpresent ~ dew30 + avg_may_wet + longitude + species2 + avg_may_temp +
                         cut + avg_may_dew + avg_dew + range1,
                     family = 'binomial')
forward_model$coef
summary(forward_model)
### GAM, forward selection variables
gam_fit_fwd = gam(as.factor(wnvpresent)~ cut + species2 + s(longitude,df=5) + 
                  s(avg_may_dew,df=5) + s(avg_may_wet,df=5) + s(dew30, df=5) +
                  s(avg_may_temp,df=5) + s(avg_dew, df=5) + s(range1, df=5),
              family = binomial , 
              data = training_final)
gam_fit_fwd$coef
summary(gam_fit_fwd)
### GAM, lasso variables
gam_fit_lasso = gam(as.factor(wnvpresent)~ cut + species2 + s(longitude,df=5) + 
                  s(avg_may_dew,df=5) + s(avg_may_wet,df=5) + s(dew30, df=5),
              family = binomial , 
              data = training_final)
gam_fit_lasso$coef
summary(gam_fit_lasso)
plot(gam_fit_lasso)
