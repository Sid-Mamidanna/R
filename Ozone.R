# Loda libraries
library(ggplot2)
library(dplyr)
library(broom)

# Read in data
airquality <- read.csv('airquality.csv')

# Plot to compare temperature vs ozone
p <- ggplot(airquality) + geom_point(aes(x = Temp, y= Ozone))
print(p)

# Filter for only complete cases
aircomplete <- airquality %>% filter(complete.cases((airquality)))

# Create a plot comparing, Ozone levels with temperature
p <- ggplot(aircomplete) + geom_point(aes(x=Temp, y=Ozone))
p

ozone_model <- lm(Ozone ~ Temp, data = aircomplete)
ozone_model %>% summary()

two_model <- lm(Ozone ~ Temp + Solar.R, data = aircomplete)
two_model %>% summary

ozone_model %>% summary

ozone_intercept <- ozone_model %>% tidy %>% filter(term == "(Intercept)") %>% select(estimate) %>% unlist()
ozone_slope <- ozone_model %>% tidy %>% filter(term== "Temp") %>% select(estimate) %>% unlist()

p2 <- p + geom_abline(intercept = ozone_intercept, slope = ozone_slope)
p2

p3 <- p + geom_smooth(aes(Temp, Ozone), method = 'lm')
p3

aircomplete <- aircomplete %>% mutate(Temp2 = ifelse(Temp < 80, "cool", "warm" ))
ggplot(aircomplete) + geom_boxplot(aes(x=Temp2, y= Ozone))
