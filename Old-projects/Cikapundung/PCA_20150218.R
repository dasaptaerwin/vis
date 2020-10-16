# PCA of Cikapundung
# Load library 
# install biocLite script for package installation from 
# biocondictor (not CRAN) repo 
# omit the "#" in line 6 and 7, if you have installed it in your system
#source("http://bioconductor.org/biocLite.R") 
#biocLite("pcaMethods")
library(pcaMethods) # calling pcaMethods package

# Load data
data <- as.data.frame(read.csv("BandungData.csv",
                      header = TRUE))
attach(data)
head(data)

# Group data
group1 <- data[,c("distx", "type", "ec", "elv",  
                   "ph", "hard", "tds", "temp",
                   "eh", "cumrain", "lag1")]

group2 <- data[,c("distx", "type", "ec", "elv",
                  "aq", "Ca", "Mg", 
                  "Fe", "Mn", "K", 
                  "Na", "cumrain", "lag1")]

group3 <- data[,c("distx", "type", "ec", "elv",
                  "aq", "CO3", "HCO3", 
                  "CO2", "Cl", "SO4", 
                  "NO2", "NO3", "SiO2",  
                  "cumrain", "lag1")]

# Run PCA 
## using svdImpute = standard pca, with imputation, standardised, method univariate (uv)
pcaSvd1 <- pca(group1, 
               method = "svdImpute", 
               scale = "uv",
               center = T,
               nPcs = 3,
               evalPcs = 1:3)

pcaSvd2 <- pca(group2, 
               method = "svdImpute", 
               scale = "uv",
               center = T,
               nPcs = 3,
               evalPcs = 1:3)

pcaSvd3 <- pca(group3, 
               method = "svdImpute", 
               scale = "uv",
               center = T,
               nPcs = 3,
               evalPcs = 1:3)

# PCA result
## Summary
summary(pcaSvd1)
summary(pcaSvd2)
summary(pcaSvd3)

## Eigenvalues
sDev1 <- sDev(pcaSvd1) # Extract eigenvalues
sDev2 <- sDev(pcaSvd2)
sDev3 <- sDev(pcaSvd3)

matplot(sDev1, type = 'l', 
        xlab="Principal Component", 
        ylab="Variance", 
        lwd = 3) # plotting eigenvalues
plotPcs(pcaSvd1,
        Pcs = 1:3,
        col = group1$type)
legend("topright", 
       legend = c("groundwater", "river water"), 
       pch = "o",
       col = c("black", "red"))
dev.off()

matplot(sDev2, type = 'l', 
        xlab="Principal Component", 
        ylab="Variance", 
        lwd = 3) # plotting eigenvalues
plotPcs(pcaSvd2,
        Pcs = 1:3,
        col = group2$type)
legend("topright", 
       legend = c("groundwater", "river water"), 
       pch = "o",
       col = c("black", "red"))
dev.off()

matplot(sDev3, type = 'l', 
        xlab="Principal Component", 
        ylab="Variance", 
        lwd = 3) # plotting eigenvalues
plotPcs(pcaSvd3,
        Pcs = 1:3,
        col = group3$type)
legend("topright", 
       legend = c("groundwater", "river water"), 
       pch = "o",
       col = c("black", "red"))
dev.off()

## SLplots
slplot(pcaSvd1)
slplot(pcaSvd2) 
slplot(pcaSvd3) 

## Loadings
loadSvd1 <- loadings(pcaSvd1)  # Variable loadings to PCs
plot.new()
plot(loadings(pcaSvd1), 
     pch = 20,
     main = "Variable loadings",
     sub = "Svd on Group1")
text(loadings(pcaSvd1), 
     row.names(loadings(pcaSvd1)),
     cex=0.6, 
     pos=1, 
     col="red")

loadSvd2 <- loadings(pcaSvd2)  # Variable loadings to PCs
plot.new()
plot(loadings(pcaSvd2), 
     pch = 20,
     main = "Variable loadings",
     sub = "Svd on Group2")
text(loadings(pcaSvd2), 
     row.names(loadings(pcaSvd2)),
     cex=0.6, 
     pos=1, 
     col="red")

loadSvd3 <- loadings(pcaSvd3)  # Variable loadings to PCs
plot.new()
plot(loadings(pcaSvd3), 
     pch = 20,
     main = "Variable loadings",
     sub = "Svd on Group3")
text(loadings(pcaSvd3), 
     row.names(loadings(pcaSvd3)),
     cex=0.6, 
     pos=1, 
     col="red")

## Scores
scores(pcaSvd1)     # Scores cases to PCs
plot(scores(pcaSvd1), 
     pch = c(group1$type), 
     col = c(group1$type),
     main = "Case scores",
     sub = "Svd on Group1")
legend(x = "bottomright", 
       c("Groundwater", "River Water"),
       title = "Water type:",
       pch = c(1, 2), 
       col = c("black", "red"))
dev.off()

scores(pcaSvd2)     # Scores cases to PCs
plot(scores(pcaSvd2), 
     pch = c(group2$type), 
     col = c(group2$type),
     main = "Case scores",
     sub = "Svd on Group1")
legend(x = "bottomright", 
       c("Groundwater", "River Water"),
       title = "Water type:",
       pch = c(1, 2), 
       col = c("black", "red"))
dev.off()

scores(pcaSvd3)     # Scores cases to PCs
plot(scores(pcaSvd3), 
     pch = c(group3$type), 
     col = c(group3$type),
     main = "Case scores",
     sub = "Svd on Group1")
legend(x = "bottomright", 
       c("Groundwater", "River Water"),
       title = "Water type:",
       pch = c(1, 2), 
       col = c("black", "red"))
dev.off()
