
rm(list=ls())
library(ggplot2)

setwd('/Users/zurich/Google Drive/SITES/FactMachine-Final/PrincipleComponents')
load("samsungData.rda")
#create matrices without Y and Activity type
ssDataMat <- data.matrix(samsungData[,-c(562,563)])
# scale to mean = 0, sd =1. ssdataMat = 7352 x 561 
ssDataMat <- scale(ssDataMat)
# calculate covariance matrix. ssCov = 561 x 561
ssCov <- (t(ssDataMat) %*% ssDataMat) / nrow(ssDataMat)

#singular value decomposition
svd <- svd(ssCov)
#construct PC reduction using first K components
k <-2
#in this case reduce dimensions from 561 to 2
ssReduction <- ssDataMat %*% svd$u[,1:k]

#scale for plotting
ssReduction <- scale(ssReduction)

#construct data frame
aF  <- factor(samsungData$activity, order = TRUE, 
                      levels = c("laying", "sitting", "standing", 
                                "walk", "walkdown", "walkup" ))

dfReduction <- data.frame(X = ssReduction[,1], 
                          Y = ssReduction[,2], activity = aF)
#laying, sitting, standing, walk, walkdown, walkup
#green, yellow, red, magenta, blue, cyan

#laying, sitting, standing, 
#walk, walkdown, walkup
colV <- c("#17b12a","#ffff66","#FF0000",
          "#ef43dc","#3333ff","#66cc99")
theme_new <- theme_set(theme_bw())
theme_new <- theme_update(
    legend.position="none",
    panel.grid.minor=element_blank(),
    panel.grid.major = element_line(colour = 'grey', size = 0.3),
    plot.background = element_rect(fill="white",color = NA), 
    panel.background=element_rect(fill="white",color = NA),   
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.border=element_blank()
)



pointSize = 1.5

p <- ggplot(dfReduction, aes(X, Y, color = aF))
p <- p + scale_y_continuous(limits=c(-5,4))
p <- p + scale_x_continuous(limits=c(-3,1))
p <- p + scale_color_manual(values = colV)
p <- p + geom_point(alpha = I(5/5),  size=pointSize)
p

ggsave(file = "PCA.pdf",  useDingbats=FALSE)

#calculate total variance explained
#1. get the eigenvalues
eigen <- svd$d
#proportion explained by first two evs
firstTwo <- sum(eigen[1:2]) / sum(eigen)
sprintf("Variance explained by first two components: %s", firstTwo)
#2facet


#weights for the 1st component
names <- names(samsungData[,-c(562,563)])
X <- svd$u[,1]
dfX <- data.frame(names = names, X = X)
dfXSorted <- dfX[order(-abs(dfX$X)),]
#first 10 weights
head(dfXSorted, 10)




