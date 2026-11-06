############ Data analysis with R - lectures ################
# week 6 - correlation and simple regression
# author: Tomas Diviak, tomas.diviak@manchester.ac.uk
options(scipen = 999)
library(ggplot2) # visualisations
library(tidyverse)
# data - Swiss French-speaking cantons from late 18th century
swiss <- read.csv("swiss.csv", header = TRUE)
View(swiss)

# descriptives of all the columns (variables)
sapply(swiss, summary)
sapply(swiss[,2:7], summary) # only columns 2 through 7 (numeric ones)

# scatterplot
ggplot(swiss, aes(x = Education, y = Fertility)) +     
  geom_point()

# size of points = % Catholics
ggplot(swiss, aes(x = Education, y =  Fertility, size = Catholic)) +     
  geom_point()

# labeled by the name of the canton
ggplot(swiss, aes(x = Education, y = Fertility)) +     
  geom_point() +
  geom_text(aes(label = Canton))

# size of the main title changed
ggplot(swiss, aes(x = Education, y = Fertility)) +     
  geom_point() +
  labs(title = "scatterplot") +
  theme(plot.title = element_text(size = 10)) 

# correlation coefficients - Pearson, Spearman, Kendall
cor(swiss$Fertility, swiss$Education, method = "pearson")
cor(swiss$Fertility, swiss$Education, method = "spearman")
cor(swiss$Fertility, swiss$Education, method = "kendall")

# testing the statistical significance of the coefficients + their CIs
cor.test(swiss$Fertility, swiss$Education, method = "pearson")
cor.test(swiss$Fertility, swiss$Education, method = "spearman")
cor.test(swiss$Fertility, swiss$Education, method = "kendall")

# simple linear regression
# a simple model
model1 <- lm(Fertility ~ Education, data = swiss)
summary(model1)
confint(model1)

# another model
model2 <- lm(Fertility ~ Agriculture, data = swiss)
summary(model2)
confint(model2)

# scatterplot with a regression line
ggplot(swiss, aes(x = Education, y = Fertility)) +     
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(swiss, aes(x = Education, y = Fertility)) +     
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# predicting with linear regression
newcantons <- data.frame(Education = c(10, 50, 90))
predict(model1, newdata = newcantons)
predict(model1, newdata = newcantons, interval = "prediction")
