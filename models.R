
####################################################################################
# Basic visualisations of the data
####################################################################################

# Histogram of grant amounts
df %>% 
  ggplot(aes(x = Amount)) +
  geom_histogram()

# Scatterplot of TPP against Grants
df %>% 
  ggplot(aes(x = ALP_Percent, y = Amount)) +
  geom_point()

# Fitting a loess curve to see if there's anything there
df %>% 
  ggplot(aes(x = ALP_Percent, y = Amount)) +
  geom_point() +
  geom_smooth(span = 0.5)

####################################################################################
# Building a flexible regression model - GAM
####################################################################################
library(visreg)
library(mgcv)

mod <- gam(Amount ~ s(ALP_Percent) + s(Population) + s(MedianAge) + 
             s(MedianPersonalIncome) + s(HighSchool) + s(Unemployed) + s(Owned) + s(Swing),
           data = df %>% 
             mutate(Amount_per_Grant = ifelse(Number_Grants == 0, 0, Amount/Number_Grants)))
summary(mod)
visreg(mod, "ALP_Percent")

####################################################################################
# Diagnostics
####################################################################################

# Quantile-quantile plot - Check normality of residuals
ggplot(aes(sample = mod$residuals), data = NULL) + 
  geom_qq() + 
  geom_qq_line()

# Checking for influential points
library(broom)
aug <- augment(mod, data = df)
aug %>% 
  ggplot(aes(x = .hat, y = .cooksd)) + 
  geom_point() +
  geom_label(aes(label = Amount))

##### Robustness #####
# Two electorates have high leverage (hat values) and three have potentially high influence (cooksd > 0.05)
influential_electorates <- aug %>% 
  filter(.cooksd > 0.05) %>% 
  select(DivisionNm) %>% unlist %>% unname
# Removing the influential electorates
mod_robust1 <- gam(Amount ~ s(ALP_Percent) + s(Population) + s(MedianAge) + 
                     s(MedianPersonalIncome) + s(HighSchool) + s(Unemployed) + s(Owned) + s(Swing),
                   data = df %>% filter(!DivisionNm %in% influential_electorates))
visreg(mod_robust1, "ALP_Percent")
# Same effect is observed

# Alternatively remove the outliers in terms of their amount
ggplot(aes(y = Amount), data = df) +
  geom_boxplot()
mod_robust2 <- gam(Amount ~ s(ALP_Percent) + s(Population) + s(MedianAge) + 
                     s(MedianPersonalIncome) + s(HighSchool) + s(Unemployed) + s(Owned) + s(Swing),
                   data = df %>% filter(Amount < 1700000))
visreg(mod_robust2, "ALP_Percent")
# Same effect is observed 

####################################################################################
# Scatterplot of TPP against Grants
####################################################################################

df %>% 
  ggplot(aes(x = ALP_Percent, y = Number_Grants)) +
  geom_point() +
  geom_smooth()

df %>% 
  filter(Amount < 1600000) %>% 
  ggplot(aes(x = ALP_Percent, y = Amount)) +
  geom_point() +
  geom_smooth()

df %>% 
  ggplot(aes(x = ALP_Percent, y = Amount)) +
  geom_point() +
  geom_smooth(method = "lm")

df %>% 
  mutate(Amount_per_Grant = ifelse(Number_Grants == 0, 0, Amount/Number_Grants)) %>% 
  ggplot(aes(x = ALP_Percent, y = Amount_per_Grant)) +
  geom_point() +
  geom_smooth()

p1 <- df %>% 
  ggplot(aes(x = ALP_Percent, y = Amount)) +
  geom_point() +
  geom_smooth(span = 0.5) +
  ggtitle("0.5")
p2 <- df %>% 
  ggplot(aes(x = ALP_Percent, y = Amount)) +
  geom_point() +
  geom_smooth(span = 0.75) +
  ggtitle("0.75")
library(gridExtra)
grid.arrange(p1, p2, nrow =1)

library(visreg)
library(mgcv)
mod <- gam(Amount ~ s(ALP_Percent) + s(Population) + s(MedianAge) + 
    s(MedianPersonalIncome) + s(HighSchool) + s(Unemployed) + s(Owned) + s(Swing),
  data = df %>% 
    mutate(Amount_per_Grant = ifelse(Number_Grants == 0, 0, Amount/Number_Grants)))
visreg(mod, "ALP_Percent")

# Histogram
ggplot(aes(x = mod$residuals), data = NULL) + geom_histogram()

# Quantile-quantile plot
ggplot(aes(sample = mod$residuals), data = NULL) + 
  geom_qq() + 
  geom_qq_line()

# Checking for influential points
library(broom)
aug <- augment(mod, data = df)
aug %>% 
  ggplot(aes(x = .hat, y = .cooksd)) + 
  geom_point() +
  geom_label(aes(label = Amount))

# Two electorates have high leverage (hat values) and three have potentially high influence (cooksd > 0.05)
influential_electorates <- aug %>% 
  filter(.cooksd > 0.05) %>% 
  select(DivisionNm) %>% unlist %>% unname
# Removing the influential electorates
mod_robust1 <- gam(Amount ~ s(ALP_Percent) + s(Population) + s(MedianAge) + 
             s(MedianPersonalIncome) + s(HighSchool) + s(Unemployed) + s(Owned) + s(Swing),
           data = df %>% filter(!DivisionNm %in% influential_electorates))
visreg(mod_robust1, "ALP_Percent")
# Same effect is observed

# Alternatively remove the outliers in terms of their amount
ggplot(aes(y = Amount), data = df) +
  geom_boxplot()
mod_robust2 <- gam(Amount ~ s(ALP_Percent) + s(Population) + s(MedianAge) + 
                     s(MedianPersonalIncome) + s(HighSchool) + s(Unemployed) + s(Owned) + s(Swing),
                   data = df %>% filter(Amount < 1700000))
visreg(mod_robust2, "ALP_Percent")
# Same effect is observed