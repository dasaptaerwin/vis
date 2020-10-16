### Loading libraries

library("tree")
library("randomForest")
library("pander")
library("rpart")


### Data Input
data <- as.data.frame(read.csv("0806alldata.csv", header = TRUE))
attach(data)
group1 <- data[,c("x", "y", "type", 
                  "ec", "elv", 
                  "ph", "hard", 
                  "tds", "temp",
                  "eh", "cumrain", 
                  "lag1")]

group2 <- data[,c("x", "y", "type", 
                  "ec", "elv",
                  "aq", "Ca", "Mg", 
                  "Fe", "Mn", "K", 
                  "Na", "cumrain", 
                  "lag1")]

group3 <- data[,c("x", "y", "type", 
                  "ec", "elv",
                  "aq", "CO3", 
                  "HCO3", 
                  "CO2", "Cl", 
                  "SO4", "NO2", 
                  "NO3", "SiO2",  
                  "cumrain", 
                  "lag1")]

Formula1 <- ec ~ x + y + elv + 
                  ph + hard + tds + 
                  temp + eh + cumrain + 
                  lag1

Formula2 <- ec ~ Ca + Mg + Fe +
                  Mn + K + Na + 
                  cumrain + lag1

Formula3 <- ec ~ CO3 + HCO3 + CO2 + 
                  Cl + SO4 + NO2 + 
                  NO3 + SiO2 + cumrain +
                  lag1

#### Tree package ####
### Fit Regression Tree
## Fit tree model
treeModel1 <- tree(Formula1, data = group1)
treeModel2 <- tree(Formula2, data = group2)
treeModel3 <- tree(Formula3, data = group3)

## Evaluate tree
summary(treeModel1)
summary(treeModel2)
summary(treeModel3)

deviance(treeModel1, detail = F)
deviance(treeModel2, detail = F)
deviance(treeModel3, detail = F)

plot(treeModel1)
text(treeModel1, cex = 0.5)

plot(treeModel2)
text(treeModel2, cex = 0.5)

plot(treeModel3)
text(treeModel3, cex = 0.5)

## Cross validate the model (where to stop prunning)
cvTree1 <- cv.tree(treeModel1, FUN = prune.tree)
cvTree2 <- cv.tree(treeModel2, FUN = prune.tree)
cvTree3 <- cv.tree(treeModel3, FUN = prune.tree)

plot.new()
par(mfrow=c(1,3))
mtext("Cross validation plots")
plot(cvTree1$size, cvTree1$dev, 
     type = "l", 
     col = "red", 
     main = "Group 1")
plot(cvTree2$size, cvTree2$dev, 
     type = "l", 
     col = "green", 
     main = "Group 2")
plot(cvTree3$size, cvTree3$dev, 
     type = "l", 
     col = "blue", 
     main = "Group 3")

## Use optimum number of nodes to prune the tree
treePrune1 <- prune.tree(treeModel1, best=2)
treePrune2 <- prune.tree(treeModel2, best=5)
treePrune3 <- prune.tree(treeModel3, best=10)

## Summary
summary(treePrune1)
summary(treePrune2)
summary(treePrune3)

## Plot pruned tree
plot.new()
par(mfrow=c(1,3))
plot(treePrune1)
text(treePrune1, cex = 0.8)
plot(treePrune2)
text(treePrune2, cex = 0.8)
plot(treePrune3)
text(treePrune3, cex = 0.8)

plot.new()
par(mfrow=c(1,3))
plot(prune.tree(treeModel1), order = c("decreasing"))
plot(prune.tree(treeModel2), order = c("decreasing"))
plot(prune.tree(treeModel3), order = c("decreasing"))


#### rpart package ####
## Make model
rpModel1 <- rpart(Formula1, 
                  method = "class", 
                  data = group1)
rpModel2 <- rpart(Formula2, 
                  method = "class", 
                  data = group1)
rpModel3 <- rpart(Formula3, 
                  method = "class", 
                  data = group1)

## Plot model
pander(printcp(rpModel1))
pander(printcp(rpModel2))
pander(printcp(rpModel3))

## Crossval model
plot.new()
par(mfrow=c(1,3))
plotcp(rpModel1, 
       main = "Group1") 
plotcp(rpModel2, 
       main = "Group2") 
plotcp(rpModel2, 
       main = "Group3") 

## Summary
summary(rpModel1)
summary(rpModel2)
summary(rpModel3)

## plot tree
plot(rpModel1, 
     uniform=TRUE, 
     main="Class tree group1")
text(rpModel1, 
     use.n=TRUE, 
     all=TRUE, 
     cex=.8)
plot(rpModel2, 
     uniform=TRUE, 
     main="Class tree group2")
text(rpModel2, 
     use.n=TRUE, 
     all=TRUE, 
     cex=.8)
plot(rpModel3, 
     uniform=TRUE, 
     main="Class tree group3")
text(rpModel3, 
     use.n=TRUE, 
     all=TRUE, 
     cex=.8)


#### evtree package ####
library("evtree")
evModel1 <- evtree(Formula1, data = group1)
plot(evModel1)
table(predict(evModel1), group1$ec)

evModel2 <- evtree(Formula2, data = group2)
plot(evModel2)
table(predict(evModel2), group2$ec)

evModel3 <- evtree(Formula3, data = group3)
plot(evModel3)
table(predict(evModel3), group3$ec)


#### RandomForest package ####
## Fit a RF model
# Impute missing values with mean
sum(!is.na(data))
group1Imp <- rfImpute(ec ~ ., data = group1)
rfModel1 <- randomForest(Formula1, data = group1Imp, 
                              ntree = 500, 
                              mtry = 2,
                              importance = TRUE,
                              do.trace = 100, 
                              proximity=TRUE)

rfModel2 <- randomForest(Formula2, 
                              data = group2,
                              ntree = 500, 
                              mtry = 2,
                              importance = TRUE,
                              do.trace = 100, 
                              proximity=TRUE)

rfModel3 <- randomForest(Formula3, 
                              data = group3,  
                              ntree = 500, 
                              mtry = 2,
                              importance = TRUE,
                              do.trace = 100, 
                              proximity=TRUE)

## Evaluate RF model
print(rfModel1)
print(rfModel2)
print(rfModel3)

plot.new()
par(mfrow=c(1,3))
plot(rfModel1)
plot(rfModel2)
plot(rfModel3)

pander(round(importance(rfModel1), 3))
pander(round(importance(rfModel2), 3))
pander(round(importance(rfModel3), 3))

varImpPlot(rfModel1)
varImpPlot(rfModel2)
varImpPlot(rfModel3)

