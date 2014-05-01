rm(list=ls())
library(ggplot2)
library(gridExtra)
setwd('/Users/zurich/Google Drive/SITES/FactMachine-Final/PrincipleComponents')
load("samsungData.rda")

#create matrices without Y and Activity type
ssDataMat <- data.matrix(samsungData[,-c(562,563)])
ssDataMat <- scale(ssDataMat)
#covariance matrix
ssCov <- (t(ssDataMat) %*% ssDataMat) / nrow(ssDataMat)
#singular value decomposition
svd <- svd(ssCov)


#construct lattice & scale
ssReduct12 <- scale(ssDataMat %*% svd$u[,c(1,2)])
ssReduct13 <- scale(ssDataMat %*% svd$u[,c(1,3)])
ssReduct23 <- scale(ssDataMat %*% svd$u[,c(2,3)])

aF  <- factor(samsungData$activity, order = TRUE, 
              levels = c("laying", "sitting", "standing", 
                         "walk", "walkdown", "walkup" ))

df12 <- data.frame(X = ssReduct12[,1], Y = ssReduct12[,2] , activity = aF)
df13 <- data.frame(X = ssReduct13[,1], Y = ssReduct13[,2] , activity = aF)
df23 <- data.frame(X = ssReduct23[,1], Y = ssReduct23[,2] , activity = aF)

library(gridExtra)

pointSize = 0.8





colV <- c("#17b12a","#ffff66","#FF0000","#ef43dc","#3333ff","#66cc99")

theme_new <- theme_set(theme_bw())
theme_new <- theme_update(
    legend.position="none",
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    plot.background = element_rect(fill="black",color = NA), 
    panel.background=element_rect(fill="black",color = NA),   
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.border=element_blank()
)


p12 <- ggplot(df12, aes(X, Y, color = aF))
p12 <- p12 + scale_y_continuous(limits=c(-2.5, 2.7))
p12 <- p12 + scale_x_continuous(limits=c(-2, 1.2))
p12 <- p12 + scale_color_manual(values = colV)
p12 <- p12 + geom_point(alpha = I(3/5), size=pointSize)
p12 <- p12




p13 <- ggplot(df13, aes(X, Y, color = aF))
p13 <- p13 + scale_y_continuous(limits=c(-2.5, 2.7))
p13 <- p13 + scale_x_continuous(limits=c(-2, 1.2))
p13 <- p13 + scale_color_manual(values = colV)
p13 <- p13 + geom_point(alpha = I(3/5), size=pointSize)
p13 <- p13 


p23 <- ggplot(df23, aes(X, Y, color = aF))
p23 <- p23 + scale_y_continuous(limits=c(-2.5, 2.7))
p23 <- p23 + scale_x_continuous(limits=c(-2, 1.2))
p23 <- p23 + scale_color_manual(values = colV)
p23 <- p23 + geom_point(alpha = I(3/5), size=pointSize)
p23 <- p23 


#=====reverses
p21 <- ggplot(df12, aes(Y, X, color = aF))
p21 <- p21 + scale_y_continuous(limits=c(-2.5, 1.5))
p21 <- p21 + scale_x_continuous(limits=c(-2, 1.2))
p21 <- p21 + scale_color_manual(values = colV)
p21 <- p21 + geom_point(alpha = I(3/5), size=pointSize)
p21 <- p21 


p31 <- ggplot(df13, aes(Y, X, color = aF))
p31 <- p31 + scale_y_continuous(limits=c(-2.5, 1.5))
p31 <- p31 + scale_x_continuous(limits=c(-2, 1.2))
p31 <- p31 + scale_color_manual(values = colV)
p31 <- p31 + geom_point(alpha = I(3/5), size=pointSize)
p31 <- p31 



p32 <- ggplot(df23, aes(Y, X, color = aF))
p32 <- p32 + scale_y_continuous(limits=c(-2.5, 2.7))
p32 <- p32 + scale_x_continuous(limits=c(-2, 1.2))
p32 <- p32 + scale_color_manual(values = colV)
p32 <- p32 + geom_point(alpha = I(3/5), size=pointSize)
p32 <- p32 



blackPanel<-grid.rect(gp=gpar(fill="black",col="black"))

#left to right; top to bottom
grid.arrange(p13, p23, blackPanel,  
             p12, blackPanel, p32,
             blackPanel, p21, p31,
             ncol=3)