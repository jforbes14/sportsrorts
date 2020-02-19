
####################################################################################
# See distribution of grants across electorates
####################################################################################

# Histogram of number of grants
df %>% 
  ggplot(aes(x = Number_Grants)) +
  geom_histogram()

# Histogram of grant amounts
df %>% 
  ggplot(aes(x = Amount)) +
  geom_histogram()

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
library(gam)
library(mgcv)
mod <- gam(Amount_per_Grant ~ s(ALP_Percent) + s(Population) + s(MedianAge) + 
    s(MedianPersonalIncome) + s(HighSchool) + s(Unemployed) + s(Owned) + s(Swing),
  data = df %>% 
    mutate(Amount_per_Grant = ifelse(Number_Grants == 0, 0, Amount/Number_Grants)))
visreg(mod)

# Quantile-quantile plot
ggplot(aes(sample = mod$residuals), data = NULL) + geom_qq() + geom_qq_line()

