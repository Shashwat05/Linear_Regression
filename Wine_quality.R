install.packages("rpart")
library(rpart)
library(RWeka)
wine <- read.csv("whitewines.csv")
str(wine)
wine$quality
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]
m.rpart <- rpart(quality ~ ., data = wine_train)
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)
p.rpart <- predict(m.rpart, wine_test)
cor(p.rpart, wine_test$quality)
MAE <- function(actual, predicted) {mean(abs(actual - predicted))}
MAE(p.rpart, wine_test$quality)

# New Model -M5's algorithm
m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p
p.m5p <- predict(m.m5p, wine_test)
MAE(p.m5p,wine_test$quality)
