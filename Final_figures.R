acu.sad <- cbind(calAccu(sad)[,1],calAccu(hap)[,2:7])
acu.hap <- cbind(calAccu(hap)[,1],calAccu(sad)[,2:7])

heatmap.2(acu.sad,Colv=FALSE,Rowv = FALSE,main= "SAD Accuracy",
          density.info="histogram",
          dendrogram='none',
          trace="none",
          tracecol="white",
          key.title = "Discriminability",     
          col    = col.map$color.palette,   # Colormap
          breaks = col.map$palette.breaks,
          srtCol=1,  
          RowSideColors = c("mediumpurple4","darkolivegreen","darkolivegreen3","mediumpurple")[cases$class])

heatmap.2(acu.hap,Colv=FALSE,Rowv = FALSE,main= "HAPPY Accuracy",
          density.info="histogram",
          dendrogram='none',
          trace="none",
          tracecol="white",
          key.title = "Discriminability",     
          col    = col.map$color.palette,   # Colormap
          breaks = col.map$palette.breaks,
          srtCol=1,  
          RowSideColors = c("mediumpurple4","darkolivegreen","darkolivegreen3","mediumpurple")[cases$class])
par(mfrow=c(1,2))
hist(acu.sad,ylim=c(0,160),breaks = 10,main="Sad Discriminability")
hist(acu.hap,ylim=c(0,160),breaks = 10,main="Happy Discriminability")


# ----------------------------------------------------------------------------------- #
#### HAPPY LINEAR DISCRIMINANT ANALYSIS ####
# https://rstudio-pubs-static.s3.amazonaws.com/35817_2552e05f1d4e4db8ba87b334101a43da.html
# Data Reshape
d <- data.frame(cbind(cases$class,cases$age,cases$music.yrs,cases$gender,acu.hap)) 
colnames(d) <- c("type","age","music.yrs","gender","nb0","nb2","nb4","nb8","nb16","nb32","nb64")
d$type <- cases$class
d$id <- rownames(d)
library(MASS)
library(pander)
library(magrittr)
library(dplyr)
library(ggplot2)

# Linear Discriminant model MASS library
m <- lda(type~nb0+nb2+nb4+nb8+nb16+nb32+nb64, d)
# Obtains the predictor from the LDA to an object
p <- predict(m)
# Accuracy of prediction
freqtable <- table(p$class, d$type)
# Percent correct of each category of G
diag(prop.table(freqtable))
rownames(freqtable) <- paste0("Predicted ", d$type %>% levels)
freqtable %>% addmargins %>% pander("Observed vs. Predicted Frequencies")
# Total percent correct
sum(diag(prop.table(freqtable)))
# Proportions
prop.table(freqtable) %>% addmargins %>% pander("Proportions")
# LDA, 3  used: 
#plot(m, abbrev=1,dimen = 3)
# LDA to variables
d$LDA1 <- p$x[,1]
d$LDA2 <- p$x[,2]
d$LDA3 <- p$x[,3]

ggplot(d, aes(LDA1, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top")
ggplot(d, aes(LDA2, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top")
ggplot(d, aes(LDA3, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top")


gMeans <- d %>% group_by(type) %>% select(LDA1,LDA2,LDA3) %>%  summarise_each(funs(mean)) 
gMeans %>% pander
# PLot the LDA1 vs LDA2 with color per group
ggplot(d, aes(LDA1, LDA2, color = type)) + 
  geom_point(alpha = 0.5) + 
  geom_text(data = gMeans, 
            aes(label = type),
            color = "black", 
            vjust = 1.75) + 
  geom_point(data = gMeans, 
             aes(fill = type), 
             size = 4, 
             color = "black", 
             pch = 21) + 
  theme(legend.position = "none") +
  coord_equal()


# PLots the First Dimension
library(scatterplot3d)
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
scatterplot3d(d$LDA1,d$LDA2,d$LDA3,color = c("red","green","blue","orange")[d$type],pch = 19,box = FALSE,
              main = "Linear Discriminant Functions", xlab = "LDA1", ylab = "LDA2", zlab = "LDA3")
addgrids3d(d$LDA1,d$LDA2,d$LDA3, grid = c("xy", "xz", "yz"))

library(klaR)
drawparti(grouping = d$type, x = d$LDA1, y = d$LDA2, xlab = "LDA1", ylab = "LDA2")
#drawparti(grouping = d$type, x = d$LDA1, y = d$LDA3, xlab = "LDA1", ylab = "LDA3")
# Panels of histograms and overlayed density plots
# for 1st discriminant function
plot(m, dimen=1, type="both") # m is the fit from lda 

# Scatterplot for 3 Group Problem
pairs(d[c("nb0","nb2","nb4","nb8","nb16","nb32","nb64")], main="LDA Accuracy", pch=22,
      bg=c("red","green","blue","orange")[unclass(d$type)])

# MANOVA follow up
library(tables)
m <- lm(cbind(nb0, nb2, nb4, nb8, nb16, nb32, nb64) ~ type, d)
m %>% manovaTable
etasq(m %>% manova)

heplot(m,variables =)

# Canonical Function analysis
library(candisc)
cca <- m %>% candisc 
cca %>% summary(coef = c("raw", "std", "structure"))

# RAW Coefficients
cca$coeffs.raw %>% pander("Raw Coefficients")
# STANDARIZED coefficients
cca$coeffs.std %>% pander("Standardized Coefficients")
# STRUCTURE Coefficients
cca$structure %>% pander("Structure Coefficients")

# CANNONICAL DISCRIMINANT ANALYSIS
par(xpd = T, bty = "n",pty = "s")


par(mfrow=c(1,2))
heplot(cca, bty = "n")
myColors <- hsv((c(0,80,160,240) + 80)/360,s = 0.8,v = 0.8,0.7)
plot(cca, col = myColors, pch = rep(16,4), bty = "n", xpd = T,var.col = "red4",ylim=c(-5,5))


# ----------------------------------------------------------------------------------- #
#### SAD LINEAR DISCRIMINANT ANALYSIS ####
# https://rstudio-pubs-static.s3.amazonaws.com/35817_2552e05f1d4e4db8ba87b334101a43da.html
# Data Reshape
d <- data.frame(cbind(cases$class,cases$age,cases$music.yrs,cases$gender,acu.sad)) 
colnames(d) <- c("type","age","music.yrs","gender","nb0","nb2","nb4","nb8","nb16","nb32","nb64")
d$type <- cases$class
d$id <- rownames(d)

# Linear Discriminant model MASS library
m <- lda(type~nb0+nb2+nb4+nb8+nb16+nb32+nb64, d)
# Obtains the predictor from the LDA to an object
p <- predict(m)
# Accuracy of prediction
freqtable <- table(p$class, d$type)
# Percent correct of each category of G
diag(prop.table(freqtable))
rownames(freqtable) <- paste0("Predicted ", d$type %>% levels)
freqtable %>% addmargins %>% pander("Observed vs. Predicted Frequencies")
# Total percent correct
sum(diag(prop.table(freqtable)))
# Proportions
prop.table(freqtable) %>% addmargins %>% pander("Proportions")
# LDA, 3  used: 
#plot(m, abbrev=1,dimen = 3)
# LDA to variables
d$LDA1 <- p$x[,1]
d$LDA2 <- p$x[,2]
d$LDA3 <- p$x[,3]

ggplot(d, aes(LDA1, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top")
ggplot(d, aes(LDA2, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top")
ggplot(d, aes(LDA3, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top")


gMeans <- d %>% group_by(type) %>% select(LDA1,LDA2,LDA3) %>%  summarise_each(funs(mean)) 
gMeans %>% pander
# PLot the LDA1 vs LDA2 with color per group
ggplot(d, aes(LDA1, LDA2, color = type)) + 
  geom_point(alpha = 0.5) + 
  geom_text(data = gMeans, 
            aes(label = type),
            color = "black", 
            vjust = 1.75) + 
  geom_point(data = gMeans, 
             aes(fill = type), 
             size = 4, 
             color = "black", 
             pch = 21) + 
  theme(legend.position = "none") +
  coord_equal()


# MANOVA follow up
library(tables)
m <- lm(cbind(nb0, nb2, nb4, nb8, nb16, nb32, nb64) ~ type, d)
m %>% manovaTable
etasq(m %>% manova)

heplot(m,variables =)

# Canonical Function analysis
library(candisc)
cca.h <- m %>% candisc 
cca.h %>% summary(coef = c("raw", "std", "structure"))

# RAW Coefficients
cca.h$coeffs.raw %>% pander("Raw Coefficients")
# STANDARIZED coefficients
cca.h$coeffs.std %>% pander("Standardized Coefficients")
# STRUCTURE Coefficients
cca.h$structure %>% pander("Structure Coefficients")

# CANNONICAL DISCRIMINANT ANALYSIS
par(xpd = T, bty = "n",pty = "s")

par(mfrow=c(1,2))
heplot(cca.h, bty = "n",ylim=c(-5,5),main="CDA Happy", scale=5)
heplot(cca, bty = "n",ylim=c(-5,5),main="CDA Sad",xlim=c(-6,4), scale=5)

plot(cca.h, col = myColors, pch = rep(16,4), bty = "n", xpd = T,var.col = "red4",ylim=c(-5,5),xlim=c(-6,4), scale=5,main="CDA Happy")
plot(cca, col = myColors, pch = rep(16,4), bty = "n", xpd = T,var.col = "red4",ylim=c(-5,5),xlim=c(-6,4), scale=5,main="CDA Sad")

cca.m <- candiscList(m)
names(cca.m$type)
plot(cca.m, type="n", ask=FALSE)
heplot(cca.m$type, scale=6)
heplot(cca.m$type, scale=3)

#### TEST LDA ####
n=64
nt=60
neval=n-nt
rep=1000

### LDA
set.seed(123456789)
errlin=dim(rep)
for (k in 1:rep) {
  train=sample(1:n,nt)
  ## linear discriminant analysis
  m1=lda(type~nb0+nb2+nb4+nb8+nb16+nb32+nb64, d[train,])
  predict(m1,d[-train,])$class
  tablin=table(d$type[-train],predict(m1,d[-train,])$class)
  errlin[k]=(neval-sum(diag(tablin)))/neval
}
merrlin=mean(errlin)
merrlin

### QDA
set.seed(123456789)
errqda=dim(rep)
for (k in 1:rep) {
  train=sample(1:n,nt)
  ## quadratic discriminant analysis
  m1=qda(type~nb0+nb2+nb4+nb8+nb16+nb32+nb64, d[train,])
  predict(m1,d[-train,])$class
  tablin=table(d$type[-train],predict(m1,d[-train,])$class)
  errqda[k]=(neval-sum(diag(tablin)))/neval
}
merrqda=mean(errlin)
merrqda