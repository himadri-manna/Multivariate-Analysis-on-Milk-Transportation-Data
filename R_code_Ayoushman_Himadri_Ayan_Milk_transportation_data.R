# multivariate poject // Milk transportation data
# Ayoushman // Himadri // Ayan

library(readr)
library(readxl)
library(car)
library(heplots)
library(rgl)
library(mvShapiroTest)
library(MASS)
library(tidyverse)
library(ggplot2)
library(lattice)
library(latticeExtra)
library(tactile)
library(factoextra)
library(FactoMineR)
library(ade4)
library(MVN)
library(matlib)
library(reshape2)
library(biotools)
library(GGally)
library(class)
library(plotrix)
library(plotly)

# paste milk_transportation.txt file path
milk <- read_table2("D:/M stat sem 2/Multivariate/Milk data/milk_transportation.txt", 
                    col_names = FALSE, 
                    col_types = cols(X1 = col_factor(levels = c("1", "2")))) 
colnames(milk) <- c("Type", "F_cost", "R_cost", "C_cost")
n1 <- 36
n2 <- 23
p <- 3

# Scatter plot

data_sp <-  data.frame(milk)
levels(data_sp[,1])[levels(data_sp[,1]) == "1"] <- "Gasoline"
levels(data_sp[,1])[levels(data_sp[,1]) == "2"] <- "Diesel"
p_sp = 
  ggpairs(data_sp, aes(color = Type), diag = list(continuous = wrap("densityDiag", 
                                                                    alpha = 0.7))) + 
  ggtitle("Scatter Plot Matrix for Original Data") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

for (i in 1:p_sp$nrow) {
  for (j in 1:p_sp$ncol) {
    p_sp[i,j] = p_sp[i,j] + 
      scale_fill_manual(values = c("firebrick1","dodgerblue4")) + 
      scale_color_manual(values = c("firebrick1", "dodgerblue4"))
  }
}
p_sp

open3d()
plot3d(milk[2:4],col=c("red","blue4")[milk$Type], type = "s", size = 1)
legend3d("topright", legend = paste('Type', c('Gasoline', 'Diesel'))
         , pch = 16, col = c("red","blue4"), cex = 1, inset = c(0.02))

# Normality for Gasoline
qqmath(subset(milk, Type == "1")$F_cost, distribution = qnorm, 
       main ="QQ Plot for Fuel Cost for Gasoline",
       ylab = "Fuel cost",
       panel = function(x, ...) {
         panel.qqmath(subset(milk, Type == "1")$F_cost, grid = TRUE, pch = 19)
         panel.qqmathline(subset(milk, Type == "1")$F_cost, col = "red")
         panel.qqmathci(x, y = x, ci = 0.95)
       })
qqmath(subset(milk, Type == "1")$R_cost, distribution = qnorm, 
       main ="QQ Plot for Repair Cost for Gasoline",
       ylab = "Repair cost", 
       panel = function(x, ...) {
         panel.qqmath(subset(milk, Type == "1")$R_cost, grid = TRUE, pch = 19)
         panel.qqmathline(subset(milk, Type == "1")$R_cost, col = "red")
         panel.qqmathci(x, y = x, ci = 0.95)
       })
qqmath(subset(milk, Type == "1")$C_cost, distribution = qnorm, 
       main ="QQ Plot for Capital Cost for Gasoline",
       ylab = "Capital cost", 
       panel = function(x, ...) {
         panel.qqmath(subset(milk, Type == "1")$C_cost, grid = TRUE, pch = 19)
         panel.qqmathline(subset(milk, Type == "1")$C_cost, col = "red")
         panel.qqmathci(x, y = x, ci = 0.95)
       })

shapiro.test(subset(milk, Type == "1")$F_cost)
shapiro.test(subset(milk, Type == "1")$R_cost)
shapiro.test(subset(milk, Type == "1")$C_cost)
mvShapiro.Test(as.matrix(subset(milk, Type == "1")[,-1]))
mvn(subset(milk, Type == "1")[, -1])

# Normality for Diesel

qqmath(subset(milk, Type == "2")$F_cost, distribution = qnorm, 
       main ="QQ Plot for Fuel Cost for Diesel",
       ylab = "Fuel cost",
       panel = function(x, ...) {
         panel.qqmath(subset(milk, Type == "2")$F_cost, grid = TRUE, pch = 19)
         panel.qqmathline(subset(milk, Type == "2")$F_cost, col = "red")
         panel.qqmathci(x, y = x, ci = 0.95)
       })
qqmath(subset(milk, Type == "2")$R_cost, distribution = qnorm, 
       main ="QQ Plot for Repair Cost for Diesel",
       ylab = "Repair cost", 
       panel = function(x, ...) {
         panel.qqmath(subset(milk, Type == "2")$R_cost, grid = TRUE, pch = 19)
         panel.qqmathline(subset(milk, Type == "2")$R_cost, col = "red")
         panel.qqmathci(x, y = x, ci = 0.95)
       })
qqmath(subset(milk, Type == "2")$C_cost, distribution = qnorm, 
       main ="QQ Plot for Capital Cost for Diesel",
       ylab = "Capital cost", 
       panel = function(x, ...) {
         panel.qqmath(subset(milk, Type == "2")$C_cost, grid = TRUE, pch = 19)
         panel.qqmathline(subset(milk, Type == "2")$C_cost, col = "red")
         panel.qqmathci(x, y = x, ci = 0.95)
       })

shapiro.test(subset(milk, Type == "2")$F_cost)
shapiro.test(subset(milk, Type == "2")$R_cost)
shapiro.test(subset(milk, Type == "2")$C_cost)
mvShapiro.Test(as.matrix(subset(milk, Type == "2")[,-1]))
mvn(subset(milk, Type == "2")[, -1])

# Boxcox trans

powerTransform(subset(milk, Type == "1")[,-1], family = "bcPower")

milk1 <-  bcPower(as.matrix(milk[,-1]), c(0.5, 0.5, 0.5))
milk1 = cbind(milk[,1],milk1)
milk1 <- as_tibble(milk1)
colnames(milk1) <- c("Type", "F_cost", "R_cost", "C_cost")

# Normality for Gasoline
qqmath(subset(milk1, Type == "1")$F_cost, distribution = qnorm, 
       main ="QQ Plot for Fuel Cost for Gasoline",
       ylab = "Fuel cost",
       panel = function(x, ...) {
         panel.qqmath(subset(milk1, Type == "1")$F_cost, grid = TRUE, pch = 19)
         panel.qqmathline(subset(milk1, Type == "1")$F_cost, col = "red")
         panel.qqmathci(x, y = x, ci = 0.95)
       })
qqmath(subset(milk1, Type == "1")$R_cost, distribution = qnorm, 
       main ="QQ Plot for Repair Cost for Gasoline",
       ylab = "Repair cost", 
       panel = function(x, ...) {
         panel.qqmath(subset(milk1, Type == "1")$R_cost, grid = TRUE, pch = 19)
         panel.qqmathline(subset(milk1, Type == "1")$R_cost, col = "red")
         panel.qqmathci(x, y = x, ci = 0.95)
       })
qqmath(subset(milk1, Type == "1")$C_cost, distribution = qnorm, 
       main ="QQ Plot for Capital Cost for Gasoline",
       ylab = "Capital cost", 
       panel = function(x, ...) {
         panel.qqmath(subset(milk1, Type == "1")$C_cost, grid = TRUE, pch = 19)
         panel.qqmathline(subset(milk1, Type == "1")$C_cost, col = "red")
         panel.qqmathci(x, y = x, ci = 0.95)
       })

shapiro.test(subset(milk1, Type == "1")$F_cost) # Although not normal due to outliers
shapiro.test(subset(milk1, Type == "1")$R_cost)
shapiro.test(subset(milk1, Type == "1")$C_cost)
mvShapiro.Test(as.matrix(subset(milk1, Type == "1")[,-1]))
mvn(subset(milk1, Type == "1")[, -1])

# Normality for Diesel

qqmath(subset(milk1, Type == "2")$F_cost, distribution = qnorm, 
       main ="QQ Plot for Fuel Cost for Diesel",
       ylab = "Fuel cost",
       panel = function(x, ...) {
         panel.qqmath(subset(milk1, Type == "2")$F_cost, grid = TRUE, pch = 19)
         panel.qqmathline(subset(milk1, Type == "2")$F_cost, col = "red")
         panel.qqmathci(x, y = x, ci = 0.95)
       })
qqmath(subset(milk1, Type == "2")$R_cost, distribution = qnorm, 
       main ="QQ Plot for Repair Cost for Diesel",
       ylab = "Repair cost", 
       panel = function(x, ...) {
         panel.qqmath(subset(milk1, Type == "2")$R_cost, grid = TRUE, pch = 19)
         panel.qqmathline(subset(milk1, Type == "2")$R_cost, col = "red")
         panel.qqmathci(x, y = x, ci = 0.95)
       })
qqmath(subset(milk1, Type == "2")$C_cost, distribution = qnorm, 
       main ="QQ Plot for Capital Cost for Diesel",
       ylab = "Capital cost", 
       panel = function(x, ...) {
         panel.qqmath(subset(milk1, Type == "2")$C_cost, grid = TRUE, pch = 19)
         panel.qqmathline(subset(milk1, Type == "2")$C_cost, col = "red")
         panel.qqmathci(x, y = x, ci = 0.95)
       })

shapiro.test(subset(milk1, Type == "2")$F_cost)
shapiro.test(subset(milk1, Type == "2")$R_cost)
shapiro.test(subset(milk1, Type == "2")$C_cost)
mvShapiro.Test(as.matrix(subset(milk1, Type == "2")[,-1]))
mvn(subset(milk1, Type == "2")[, -1])

ellipse_1 <- ellipse3d(cov(subset(milk1, Type == "1")[2:4]), 
                       centre = colMeans(subset(milk1, Type == "1")[2:4]), 
                       level = 0.95, t = sqrt(qchisq(0.95, 3)))
open3d()
plot3d(subset(milk1, Type == "1")[2:4],col = "red", type = "s", size = 1)
shade3d(ellipse_1, col = "#D95F02", alpha = 0.3, lit = FALSE)
wire3d(ellipse_1, col = "#D95F02",  lit = FALSE)

ellipse_2 <- ellipse3d(cov(subset(milk1, Type == "2")[2:4]), 
                       centre = colMeans(subset(milk1, Type == "2")[2:4]), 
                       level = 0.95, t = sqrt(qchisq(0.95, 3)))
open3d()
plot3d(subset(milk1, Type == "2")[2:4], col = "blue", type = "s", size = 1)
shade3d(ellipse_2, col = "#D95F02", alpha = 0.3, lit = FALSE)
wire3d(ellipse_2, col = "#D95F02",  lit = FALSE)

# outlier detection

hat_val_1 <- diag(as.matrix(subset(milk1, Type == "1")[,-1]) %*% 
                    (solve(t(as.matrix(subset(milk1, Type == "1")[,-1])) %*% 
                             as.matrix(subset(milk1, Type == "1")[,-1])))
                  %*% t(as.matrix(subset(milk1, Type == "1")[,-1])))
ind_1 <- hat_val_1 > 2*3/36
xyplot(hat_val_1 ~ seq_along(hat_val_1), grid = TRUE, 
       abline = list( h = 2*3/36, col = "red"), 
       xlab = "Observations", ylab = "Hatvalues", 
       main = "Plot of Hatvalues for Gasoline", pch = 16, cex = 0.9) + 
  layer(panel.text(x[ind_1], y[ind_1], labels = rownames(milk1)[ind_1],
                   pos = 4, col = "grey50"))
hat_val_2 <- diag(as.matrix(subset(milk1, Type == "2")[,-1]) %*% 
                    (solve(t(as.matrix(subset(milk1, Type == "2")[,-1])) %*% 
                             as.matrix(subset(milk1, Type == "2")[,-1])))
                  %*% t(as.matrix(subset(milk1, Type == "2")[,-1])))
ind_2 <- hat_val_2 > 2*3/23
xyplot(hat_val_2 ~ seq_along(hat_val_2), grid = TRUE, 
       abline = list( h = 2*3/23, col = "red"), 
       xlab = "Observations", ylab = "Hatvalues", 
       main = "Plot of Hatvalues for Diesel", pch = 16, cex = 0.9) + 
  layer(panel.text(x[ind_2], y[ind_2], labels = rownames(milk1)[ind_2],
                   pos = 4, col = "grey50"))

milk2 <- milk1
milk2[ 9, -1] <- colMeans(subset(milk1, Type == "1")[-9,-1])
for (i in c(40, 41, 47)){
  milk2[ i, -1] <- colMeans(subset(milk1, Type == "2")[-c(40,41,47),-1])
}


mvn(subset(milk2, Type == "1")[, -1])
mvShapiro.Test(as.matrix(subset(milk1, Type == "1")[,-1]))
mvn(subset(milk2, Type == "2")[, -1])
mvShapiro.Test(as.matrix(subset(milk1, Type == "2")[,-1]))

# PCA for Gasoline

cov(subset(milk2, Type == "1")[2:4])
cor(subset(milk2, Type == "1")[2:4])

pca_1 <- PCA( subset(milk2, Type == "1")[2:4], graph = FALSE, scale.unit = TRUE)
pca_1$eig
sweep(pca_1$var$coord,2,
      sqrt(pca_1$eig[1:ncol(pca_1$var$coord),1]),FUN="/")
# eigen(cor(subset(milk2, Type == "1")[2:4]))$vectors
fviz_eig(pca_1, addlabels = TRUE, ylim = c(0, 100))
fviz_pca_var(pca_1, col.var = "coord",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
pca_1$var$coord

# PCA for Diesel

cov(subset(milk2, Type == "2")[2:4])
cor(subset(milk2, Type == "2")[2:4])

pca_2 <- PCA( subset(milk2, Type == "2")[,-1], graph = FALSE, scale.unit = TRUE)
pca_2$eig
sweep(pca_2$var$coord,2,
      sqrt(pca_2$eig[1:ncol(pca_2$var$coord),1]),FUN="/") # eigenvector
# eigen(cor(subset(milk2, Type == "2")[2:4]))$vectors
fviz_eig(pca_2, addlabels = TRUE, ylim = c(0, 100))
fviz_pca_var(pca_2, col.var = "coord",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
pca_2$var$coord

# Bartlett test

boxM(milk2[,-1], group = milk2$Type)

# Confidence Region

S1 <- cov(subset(milk2, Type == "1")[2:4])
t_a_21 <- qt(0.975, df = n1-1)/sqrt(n1)
ICIU1 <- colMeans(subset(milk2, Type == "1")[2:4]) +
  t_a_21*c( sqrt(S1[1,1]), sqrt(S1[2,2]), sqrt(S1[3,3]))
ICIL1 <- colMeans(subset(milk2, Type == "1")[2:4]) - 
  t_a_21*c( sqrt(S1[1,1]), sqrt(S1[2,2]), sqrt(S1[3,3]))

F_a1 <- sqrt((p*(n1-1)*qf(0.95, p, n1-p))/(n1*(n1-p)))
SCIU1 <- colMeans(subset(milk2, Type == "1")[2:4]) +
  c( F_a1*sqrt(S1[1,1]), F_a1*sqrt(S1[2,2]), F_a1*sqrt(S1[3,3]))
SCIL1 <- colMeans(subset(milk2, Type == "1")[2:4]) -
  c( F_a1*sqrt(S1[1,1]), F_a1*sqrt(S1[2,2]), F_a1*sqrt(S1[3,3]))

t_a_2p1 <- qt((1-0.05/(2*p)), df = n1-1)/sqrt(n1)
BCIU1 <- colMeans(subset(milk2, Type == "1")[2:4]) + 
  t_a_2p1*c( sqrt(S1[1,1]), sqrt(S1[2,2]), sqrt(S1[3,3]))
BCIL1 <- colMeans(subset(milk2, Type == "1")[2:4]) - 
  t_a_2p1*c( sqrt(S1[1,1]), sqrt(S1[2,2]), sqrt(S1[3,3]))

vertices1 <- cbind(
  c(SCIL1[1], SCIL1[2], SCIL1[3]), 
  c(SCIL1[1], SCIL1[2], SCIU1[3]), 
  c(SCIL1[1], SCIU1[2], SCIL1[3]), 
  c(SCIL1[1], SCIU1[2], SCIU1[3]), 
  c(SCIU1[1], SCIL1[2], SCIL1[3]), 
  c(SCIU1[1], SCIL1[2], SCIU1[3]), 
  c(SCIU1[1], SCIU1[2], SCIL1[3]), 
  c(SCIU1[1], SCIU1[2], SCIU1[3])
)
vertices2 <- cbind(
  c(BCIL1[1], BCIL1[2], BCIL1[3]), 
  c(BCIL1[1], BCIL1[2], BCIU1[3]), 
  c(BCIL1[1], BCIU1[2], BCIL1[3]), 
  c(BCIL1[1], BCIU1[2], BCIU1[3]), 
  c(BCIU1[1], BCIL1[2], BCIL1[3]), 
  c(BCIU1[1], BCIL1[2], BCIU1[3]), 
  c(BCIU1[1], BCIU1[2], BCIL1[3]), 
  c(BCIU1[1], BCIU1[2], BCIU1[3])
)
vertices3 <- cbind(
  c(ICIL1[1], ICIL1[2], ICIL1[3]), 
  c(ICIL1[1], ICIL1[2], ICIU1[3]), 
  c(ICIL1[1], ICIU1[2], ICIL1[3]), 
  c(ICIL1[1], ICIU1[2], ICIU1[3]), 
  c(ICIU1[1], ICIL1[2], ICIL1[3]), 
  c(ICIU1[1], ICIL1[2], ICIU1[3]), 
  c(ICIU1[1], ICIU1[2], ICIL1[3]), 
  c(ICIU1[1], ICIU1[2], ICIU1[3])
)
indices <- cbind(
  c(1, 5, 7, 3),
  c(2, 6, 8, 4),
  c(1, 2, 4, 3),
  c(5, 6, 8, 7)
)

cuboid1 <- qmesh3d(
  vertices = vertices1,
  indices = indices,
  homogeneous = FALSE
)
cuboid2 <- qmesh3d(
  vertices = vertices2,
  indices = indices,
  homogeneous = FALSE
)
cuboid3 <- qmesh3d(
  vertices = vertices3,
  indices = indices,
  homogeneous = FALSE
)
ellipse_3 <- ellipse3d(cov(subset(milk2, Type == "1")[2:4])/n1, 
                       centre = colMeans(subset(milk2, Type == "1")[2:4]), 
                       t = sqrt((p*(n1-1)*qf(0.95, p, n1-p))/(n1-p)),
                       level = 0.95)
S2 <- cov(subset(milk2, Type == "2")[2:4])
t_a_22 <- qt(0.975, df = n2-1)/sqrt(n2)
ICIU2 <- colMeans(subset(milk2, Type == "2")[2:4]) +
  t_a_22*c( sqrt(S2[1,1]), sqrt(S2[2,2]), sqrt(S2[3,3]))
ICIL2 <- colMeans(subset(milk2, Type == "2")[2:4]) - 
  t_a_22*c( sqrt(S2[1,1]), sqrt(S2[2,2]), sqrt(S2[3,3]))

F_a2 <- sqrt((p*(n2-1)*qf(0.95, p, n2-p))/(n2*(n2-p)))
SCIU2 <- colMeans(subset(milk2, Type == "2")[2:4]) +
  c( F_a2*sqrt(S2[1,1]), F_a2*sqrt(S2[2,2]), F_a2*sqrt(S2[3,3]))
SCIL2 <- colMeans(subset(milk2, Type == "2")[2:4]) -
  c( F_a2*sqrt(S2[1,1]), F_a2*sqrt(S2[2,2]), F_a2*sqrt(S2[3,3]))

t_a_2p2 <- qt((1-0.05/(2*p)), df = n2-1)/sqrt(n2)
BCIU2 <- colMeans(subset(milk2, Type == "2")[2:4]) + 
  t_a_2p2*c( sqrt(S2[1,1]), sqrt(S2[2,2]), sqrt(S2[3,3]))
BCIL2 <- colMeans(subset(milk2, Type == "2")[2:4]) - 
  t_a_2p2*c( sqrt(S2[1,1]), sqrt(S2[2,2]), sqrt(S2[3,3]))

verticeS4 <- cbind(
  c(SCIL2[1], SCIL2[2], SCIL2[3]), 
  c(SCIL2[1], SCIL2[2], SCIU2[3]), 
  c(SCIL2[1], SCIU2[2], SCIL2[3]), 
  c(SCIL2[1], SCIU2[2], SCIU2[3]), 
  c(SCIU2[1], SCIL2[2], SCIL2[3]), 
  c(SCIU2[1], SCIL2[2], SCIU2[3]), 
  c(SCIU2[1], SCIU2[2], SCIL2[3]), 
  c(SCIU2[1], SCIU2[2], SCIU2[3])
)
vertices5 <- cbind(
  c(BCIL2[1], BCIL2[2], BCIL2[3]), 
  c(BCIL2[1], BCIL2[2], BCIU2[3]), 
  c(BCIL2[1], BCIU2[2], BCIL2[3]), 
  c(BCIL2[1], BCIU2[2], BCIU2[3]), 
  c(BCIU2[1], BCIL2[2], BCIL2[3]), 
  c(BCIU2[1], BCIL2[2], BCIU2[3]), 
  c(BCIU2[1], BCIU2[2], BCIL2[3]), 
  c(BCIU2[1], BCIU2[2], BCIU2[3])
)
vertices6 <- cbind(
  c(ICIL2[1], ICIL2[2], ICIL2[3]), 
  c(ICIL2[1], ICIL2[2], ICIU2[3]), 
  c(ICIL2[1], ICIU2[2], ICIL2[3]), 
  c(ICIL2[1], ICIU2[2], ICIU2[3]), 
  c(ICIU2[1], ICIL2[2], ICIL2[3]), 
  c(ICIU2[1], ICIL2[2], ICIU2[3]), 
  c(ICIU2[1], ICIU2[2], ICIL2[3]), 
  c(ICIU2[1], ICIU2[2], ICIU2[3])
)

cuboid4 <- qmesh3d(
  vertices = verticeS4,
  indices = indices,
  homogeneous = FALSE
)
cuboid5 <- qmesh3d(
  vertices = vertices5,
  indices = indices,
  homogeneous = FALSE
)
cuboid6 <- qmesh3d(
  vertices = vertices6,
  indices = indices,
  homogeneous = FALSE
)
ellipse_4 <- ellipse3d(cov(subset(milk2, Type == "2")[2:4])/n2, 
                       centre = colMeans(subset(milk2, Type == "2")[2:4]), 
                       t = sqrt((p*(n2-1)*qf(0.95, p, n2-p))/(n2-p)),
                       level = 0.95)


open3d()
shade3d(ellipse_3, col = "red", alpha = 0.4, lit = FALSE)
wire3d(ellipse_3, col = "Red",  lit = TRUE)
shade3d(cuboid1, color = "blue", alpha = 0.1)
wire3d(cuboid1, color = "blue")
shade3d(cuboid2, color = "green", alpha = 0.1)
wire3d(cuboid2, color = "green")
shade3d(cuboid3, color = "red", alpha = 0.1)
wire3d(cuboid3, color = "red")
axes3d()
title3d(main = 'Confidence Region', xlab = 'Fuel Cost', ylab =  'Repair Cost'
        , zlab =  'Capital Cost')

open3d()
shade3d(ellipse_4, col = "Blue", alpha = 0.4, lit = FALSE)
wire3d(ellipse_4, col = "Blue",  lit = TRUE)
shade3d(cuboid4, color = "blue", alpha = 0.1)
wire3d(cuboid4, color = "blue")
shade3d(cuboid5, color = "green", alpha = 0.1)
wire3d(cuboid5, color = "green")
shade3d(cuboid6, color = "red", alpha = 0.1)
wire3d(cuboid6, color = "red")
axes3d()
title3d(main = 'Confidence Region', xlab = 'Fuel Cost', ylab =  'Repair Cost'
        , zlab =  'Capital Cost')
open3d()
shade3d(ellipse_3, col = "red", alpha = 0.4, lit = FALSE)
wire3d(ellipse_3, col = "Red",  lit = TRUE)
shade3d(cuboid1, color = "blue", alpha = 0.1)
wire3d(cuboid1, color = "blue")
shade3d(cuboid2, color = "green", alpha = 0.1)
wire3d(cuboid2, color = "green")
shade3d(cuboid3, color = "red", alpha = 0.1)
wire3d(cuboid3, color = "red")
shade3d(ellipse_4, col = "Blue", alpha = 0.4, lit = FALSE)
wire3d(ellipse_4, col = "Blue",  lit = TRUE)
shade3d(cuboid4, color = "blue", alpha = 0.1)
wire3d(cuboid4, color = "blue")
shade3d(cuboid5, color = "green", alpha = 0.1)
wire3d(cuboid5, color = "green")
shade3d(cuboid6, color = "red", alpha = 0.1)
wire3d(cuboid6, color = "red")
axes3d()
title3d(main = 'Confidence Region', xlab = 'Fuel Cost', ylab =  'Repair Cost'
        , zlab =  'Capital Cost')
legend3d("topright", legend = paste('Type', c('Gasoline', 'Diesel'))
         , pch = 16, col = c("red","blue4"), cex = 1, inset = c(0.02))

# Profile Analysis

C <- matrix(c( -1, 1, 0, 0, -1, 1), ncol = 3, nrow = 2, byrow = TRUE)
Sp <- (1/n1 + 1/n2)*((n1-1)*var(subset(milk2, Type == "1")[2:4]) +
                       (n2-1)*var(subset(milk2, Type == "2")[2:4]))/(n1+n2-2)
dif <- (colMeans(subset(milk2, Type == "1")[2:4]) - colMeans(subset(milk2, Type == "2")[2:4]))
t(dif) %*% t(C) %*%
  inv(C %*% Sp %*% t(C)) %*%
  C %*% dif
qf(0.95, df1 = p-1, df2 = n1+n2-p)*(n1+n2-2)*(p-1)/(n1+n2-p)

x_1 <- as.numeric(colMeans(subset(milk2,Type=='1')[,2:4]))
x_2 <- as.numeric(colMeans(subset(milk2,Type=='2')[,2:4]))
data <- rbind(t(x_1), t(x_2))
colnames(data) <- c("Fuel Cost","Repair Cost","Capital Cost")
Fuel <- c('Gasoline','Diesel')
plotdata <- data.frame(Fuel, data)
data1 <- melt(plotdata, id.vars = "Fuel")
data2 <- group_by(data1, Fuel)
ggplot(data2) +
  geom_point(aes(variable, value, group = Fuel,color = Fuel)) +
  geom_line(aes(variable, value, group = Fuel,color = Fuel)) +
  scale_color_manual(values = c("blue", "red")) +
  labs( title = "Profile Plot", color = "Fuel Type", y = "Mean")

#Discriminant Analysis

model <- lda(Type~., data = milk2)
lda_pred_whole<-model %>% predict(milk2)
mean(lda_pred_whole$class==milk2$Type)
APER_LDA<-1-mean(lda_pred_whole$class==milk2$Type)
table(milk2$Type,lda_pred_whole$class, dnn = c('Actual Group','Predicted Group'))
diag<-as.data.frame(cbind(lda_pred_whole$posterior,lda_pred_whole$class))
diag$Fuel<-ifelse(milk2$Type==1,'Gasoline','Diesel')
diag$Pred_Fuel<-ifelse(diag$V3==1,'Gasoline','Diesel')
diag$Prediction<-ifelse(diag$V3==1 & diag$Fuel=='Gasoline'|diag$V3==2 
                        & diag$Fuel=='Diesel','Correct','Incorrect')
ggplot(data = diag)+geom_point(aes(y=`1`,x=`2`,color=Fuel))+
  ylab(' Posterior Probability of Using Gasoline')+
  xlab(' Posterior Probability of Using Diesel') + 
  scale_color_manual(values = c("dodgerblue4","firebrick1"))+theme_bw()
ggplot(data = diag)+geom_point(aes(y=`1`,x=`2`,color=Pred_Fuel))+
  ylab(' Posterior Probability of Using Gasoline')+
  xlab(' Posterior Probability of Using Diesel') + 
  scale_color_manual(values = c("dodgerblue4","firebrick1"))+theme_bw()
ggplot(data = diag)+geom_point(aes(y=`1`,x=`2`,color=Prediction))+
  ylab(' Posterior Probability of Using Gasoline')+
  xlab(' Posterior Probability of Using Diesel') +
  scale_color_manual(values = c("green3","red"))+theme_bw()

model <- qda(Type~., data = milk2)
qda_pred_whole<-model %>% predict(milk2)
mean(qda_pred_whole$class==milk2$Type)
APER_QDA<-1-mean(qda_pred_whole$class==milk2$Type)
table(milk2$Type,qda_pred_whole$class, dnn = c('Actual Group','Predicted Group'))
diag2<-as.data.frame(cbind(qda_pred_whole$posterior,qda_pred_whole$class))
diag2$Fuel<-ifelse(milk2$Type==1,'Gasoline','Diesel')
diag2$Pred_Fuel<-ifelse(diag2$V3==1,'Gasoline','Diesel')
diag2$Prediction<-ifelse(diag2$V3==1 & diag2$Fuel=='Gasoline'|diag2$V3==2 & 
                           diag2$Fuel=='Diesel','Correct','Incorrect')
ggplot(data = diag2)+geom_point(aes(y=`1`,x=`2`,color=Fuel))+
  ylab(' Posterior Probability of Using Gasoline')+
  xlab(' Posterior Probability of Using Diesel') + 
  scale_color_manual(values = c("dodgerblue4","firebrick1")) + 
  theme_bw()
ggplot(data = diag2)+geom_point(aes(y=`1`,x=`2`,color=Pred_Fuel))+
  ylab(' Posterior Probability of Using Gasoline')+
  xlab(' Posterior Probability of Using Diesel') + 
  scale_color_manual(values = c("dodgerblue4","firebrick1")) + 
  theme_bw()
ggplot(data = diag2)+geom_point(aes(y=`1`,x=`2`,color=Prediction))+
  ylab(' Posterior Probability of Using Gasoline') +
  xlab(' Posterior Probability of Using Diesel') + 
  scale_color_manual(values = c("green3","red")) + 
  theme_bw()



#train-test split up of the dataset

set.seed(70)
training = sample(nrow(milk2), floor(nrow(milk2)*0.75)) 
training_data = milk2[training,] 
#randomly choosing almost 75% of the data to be the training set
test_data = milk2[-training,]  
#the remaining almost 25% data is taken to be the test set

#LDA

lda_milk_transportation_2 = lda(Type~.,milk2, subset = training)
lda_milk_transportation_2
prediction_lda_2 = predict(lda_milk_transportation_2, test_data)
table(prediction_lda_2$class, test_data$Type) #confusion matrix obtained for test set from LDA
mean(prediction_lda_2$class!=test_data$Type) #test error obtained from LDA

#QDA

qda_milk_transportation_2 = qda(Type~.,milk2, subset = training)
qda_milk_transportation_2
prediction_qda_2 = predict(qda_milk_transportation_2, test_data)
table(prediction_qda_2$class, test_data$Type) #confusion matrix obtained for test set from QDA
mean(prediction_qda_2$class!=test_data$Type) #test error obtained from QDA

#Logistic Regression

milk2 <- as.data.frame(milk2)
milk2$Type <- as.numeric(milk2$Type)

# Here milk2 data set has been changed, type "factor" column has been converted to "numeric".
# After compiling line 590-591, if you want to compile any code previous to line 591
# please run codes 243-247, to restore milk2 tibble again in "factor" form 

glm_milk_transportation_2 = glm(Type-1~., data = milk2, subset = training, family = binomial)
summary(glm_milk_transportation_2)
Fuel_type_test = test_data$Type
len.1 = length(Fuel_type_test[Fuel_type_test==1])
prob_glm = predict(glm_milk_transportation_2, newdata = test_data, type = "response")
prediction_glm = ifelse(prob_glm<=0.5,1,2) 
table(prediction_glm,Fuel_type_test) 
#confusion matrix obtained for test set from Logistic Regression
mean(prediction_glm!=Fuel_type_test) 
#test error obtained from Logistic Regression

#kNN

error_vec = rep(0,10)
x = 1:10
set.seed(50)
for (i in x) {
  knn.pred = knn(training_data,test_data,training_data$Type,k=i)
  #table(knn.pred1,test_data$Type) #confusion matrix obtained for test set from knn with k=1
  error_vec[i] = mean(knn.pred!=test_data$Type)
}
xyplot(error_vec~x, grid = TRUE, type = c('l','p'), pch = 19,
       xlab = 'Value of k', ylab = "Misclassification Error")


knn.pred3 = knn(training_data,test_data,training_data$Type,k=3)
table(knn.pred3,test_data$Type) #confusion matrix obtained for test set from knn with k=3
mean(knn.pred3!=test_data$Type) #test error obtained from knn with k=3

#Holdout Procedure

model <- lda(Type~., data = milk2,CV=TRUE)
lda_pred_cv<-model$class
table(milk2$Type,lda_pred_cv, dnn = c('Actual Group','Predicted Group'))
mean(lda_pred_cv==milk2$Type)
est_AER_lda<-1-mean(lda_pred_cv==milk2$Type)

model.qda <- qda(Type~., data = milk2,CV=TRUE)
qda_pred_cv<-model.qda$class
table(milk2$Type,qda_pred_cv, dnn = c('Actual Group','Predicted Group'))
mean(qda_pred_cv==milk2$Type)
est_AER_qda<-1-mean(qda_pred_cv==milk2$Type)
