
# NOTE: Run this script after uploading all data with psychophysics_visualization.R

#### R Libraries ####
library(MASS)
library(pander)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tables)
library(candisc)
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')

# ----------------------------------------------------------------------------------- #
#### HAPPY and SAD: Accuracy & Discriminability  ####
acu.sad <- cbind(calAccu(sad)[,1],calAccu(hap)[,2:7])
acu.hap <- cbind(calAccu(hap)[,1],calAccu(sad)[,2:7])

heatmap.2(acu.sad,Colv=FALSE,Rowv = FALSE,main= "Sad Fine Structure",
          density.info="histogram",
          dendrogram='none',
          trace="none",
          tracecol="white",
          key.title = "Discriminability",     
          col    = col.map$color.palette,   # Colormap
          breaks = col.map$palette.breaks,
          srtCol=1,  
          RowSideColors = c("mediumpurple4","darkolivegreen","darkolivegreen3","mediumpurple")[cases$class])

heatmap.2(acu.hap,Colv=FALSE,Rowv = FALSE,main= "Happy Fine structure",
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
hist(acu.hap,ylim=c(0,160),breaks = 10,main="Happy Discriminability", col=col.map$color.palette[c(1,1,1,2,3,4,5,6,7,7)],border = "white")
hist(acu.sad,ylim=c(0,160),breaks = 10,main="Sad Discriminability", col = col.map$color.palette[c(1,1,1,2,3,4,5,6,7,7)],border = "white")


# ----------------------------------------------------------------------------------- #
#### HAPPY: Linear Discriminant Analysis ####
# https://rstudio-pubs-static.s3.amazonaws.com/35817_2552e05f1d4e4db8ba87b334101a43da.html
# Data Reshape
d.happy <- data.frame(cbind(cases$class,cases$age,cases$music.yrs,cases$gender,acu.hap)) 
colnames(d.happy) <- c("type","age","music.yrs","gender","nb0","nb2","nb4","nb8","nb16","nb32","nb64")
d.happy$type <- cases$class
d.happy$id <- rownames(d.happy)

# Linear Discriminant model MASS library
m.happy <- lda(type~nb0+nb2+nb4+nb8+nb16+nb32+nb64, d.happy)
# Obtains the predictor from the LDA to an object
p.happy <- predict(m.happy)
# Accuracy of prediction
freqtable <- table(p$class, d.happy$type)
# Percent correct of each category of G
diag(prop.table(freqtable))
rownames(freqtable) <- paste0("Predicted ", d$type %>% levels)
freqtable %>% addmargins %>% pander("LDA happy: Observed vs. Predicted Frequencies")
# Total percent correct
print(paste("Happy total % correct:",sum(diag(prop.table(freqtable)))))
# Proportions
prop.table(freqtable) %>% addmargins %>% pander("Happy Proportions")
# LDA, 3  used: 
#plot(m, abbrev=1,dimen = 3)
# LDA to variables
d.happy$LDA1 <- p.happy$x[,1]
d.happy$LDA2 <- p.happy$x[,2]
d.happy$LDA3 <- p.happy$x[,3]

# Table
gMeans <- d.happy %>% group_by(type) %>% select(LDA1,LDA2,LDA3) %>%  summarise_each(funs(mean)) 
gMeans %>% pander

# Plot the LDA
grid.arrange(
ggplot(d.happy, aes(LDA1, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top"),
ggplot(d.happy, aes(LDA2, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top"),
ggplot(d.happy, aes(LDA3, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top"),
# Plot the LDA1 vs LDA2 with color per group
ggplot(d.happy, aes(LDA1, LDA2, color = type)) + 
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
  coord_equal(),
ncol=2, nrow = 2)

# Plots the discriminat functions
par(mfrow=c(1,3))
# scater plot of each subject by LDA value
scatterplot3d(d.happy$LDA1,d.happy$LDA2,d.happy$LDA3,color = c("red","green","blue","orange")[d.happy$type],pch = 19,box = FALSE,main = "Happy: Linear Discriminant Functions", xlab = "LDA1", ylab = "LDA2", zlab = "LDA3")
addgrids3d(d.happy$LDA1,d.happy$LDA2,d.happy$LDA3, grid = c("xy", "xz", "yz"))
# Linear discrimant functions
drawparti(grouping = d.happy$type, x = d.happy$LDA1, y = d.happy$LDA2, xlab = "LDA1", ylab = "LDA2")
drawparti(grouping = d.happy$type, x = d.happy$LDA2, y = d.happy$LDA3, xlab = "LDA2", ylab = "LDA3")

# ----------------------------------------------------------------------------------- #
#### Happy: MANOVA follow up #### 
lm.happy <- lm(cbind(nb0, nb2, nb4, nb8, nb16, nb32, nb64) ~ type, d.happy)
lm.happy %>% manovaTable
etasq(lm.happy %>% manova)
# plot the error onf the LDA
par(mfrow=c(1,1))
heplot(lm.happy,variables =)


# ----------------------------------------------------------------------------------- #
#### HAPPY: Canonical Discriminant Analysis ####
# Canonical discriminant analysis
cda.happy <- lm.happy %>% candisc 
cda.happy %>% summary(coef = c("raw", "std", "structure"))

# RAW Coefficients
cda.happy$coeffs.raw %>% pander("Raw Coefficients")
# STANDARIZED coefficients
cda.happy$coeffs.std %>% pander("Standardized Coefficients")
# STRUCTURE Coefficients
cda.happy$structure %>% pander("Structure Coefficients")

# CANNONICAL DISCRIMINANT ANALYSIS
par(xpd = T, bty = "n",pty = "s")

par(mfrow=c(1,2))
heplot(cda.happy, bty = "n", main="Happy CDA")
myColors <- hsv((c(0,80,160,240) + 80)/360,s = 0.8,v = 0.8,0.7)
plot(cda.happy, col = myColors, pch = rep(16,4), bty = "n", xpd = T,var.col = "red4",ylim=c(-5,5), main="Happy CDA")


# ----------------------------------------------------------------------------------- #
#### SAD: Linear Discriminant Analysis ####
# https://rstudio-pubs-static.s3.amazonaws.com/35817_2552e05f1d4e4db8ba87b334101a43da.html
# Data Reshape
d.sad <- data.frame(cbind(cases$class,cases$age,cases$music.yrs,cases$gender,acu.sad)) 
colnames(d.sad) <- c("type","age","music.yrs","gender","nb0","nb2","nb4","nb8","nb16","nb32","nb64")
d.sad$type <- cases$class
d.sad$id <- rownames(d.sad)

# Linear Discriminant model MASS library
m.sad <- lda(type~nb0+nb2+nb4+nb8+nb16+nb32+nb64, d.sad)
# Obtains the predictor from the LDA to an object
p.sad <- predict(m.sad)
# Accuracy of prediction
freqtable <- table(p.sad$class, d.sad$type)
# Percent correct of each category of G
diag(prop.table(freqtable))
rownames(freqtable) <- paste0("Predicted ", d.sad$type %>% levels)
freqtable %>% addmargins %>% pander("LDA sad: Observed vs. Predicted Frequencies")
# Total percent correct
print(paste("Sad total % correct:",sum(diag(prop.table(freqtable)))))
# Proportions
prop.table(freqtable) %>% addmargins %>% pander("Sad Proportions")
# LDA, 3  used: 
#plot(m, abbrev=1,dimen = 3)
# LDA to variables
d.sad$LDA1 <- p.sad$x[,1]
d.sad$LDA2 <- p.sad$x[,2]
d.sad$LDA3 <- p.sad$x[,3]

# LDA table 
gMeans <- d.sad %>% group_by(type) %>% select(LDA1,LDA2,LDA3) %>%  summarise_each(funs(mean)) 
gMeans %>% pander

# PLot the LDA
grid.arrange(
ggplot(d.sad, aes(LDA1, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top"),
ggplot(d.sad, aes(LDA2, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top"),
ggplot(d.sad, aes(LDA3, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top"),
# Plot the LDA1 vs LDA2 with color per group
ggplot(d.sad, aes(LDA1, LDA2, color = type)) + 
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
  coord_equal(),
ncol=2, nrow = 2)

# ----------------------------------------------------------------------------------- #
#### SAD: MANOVA follow up #### 
lm.sad <- lm(cbind(nb0, nb2, nb4, nb8, nb16, nb32, nb64) ~ type, d.sad)
lm.sad %>% manovaTable
etasq(lm.sad %>% manova)
par(mfrow=c(1,1))
heplot(lm.sad,variables =)

# ----------------------------------------------------------------------------------- #
#### SAD: Canonical Discriminant Analysis #### 
cda.sad <- lm.sad %>% candisc 
cda.sad %>% summary(coef = c("raw", "std", "structure"))

# RAW Coefficients
cda.sad$coeffs.raw %>% pander("Raw Coefficients")
# STANDARIZED coefficients
cda.sad$coeffs.std %>% pander("Standardized Coefficients")
# STRUCTURE Coefficients
cda.sad$structure %>% pander("Structure Coefficients")

# CANNONICAL DISCRIMINANT ANALYSIS
par(xpd = T, bty = "n",pty = "s")

par(mfrow=c(1,2))
heplot(cda.happy, bty = "n",ylim=c(-5,5),main="CDA Happy", scale=5)
heplot(cda.sad, bty = "n",ylim=c(-5,5),main="CDA Sad",xlim=c(-6,4), scale=5)


# ----------------------------------------------------------------------------------- #
#### FIGURE 4 - Canonical Discriminant Analysis ####
plot(cda.happy, col = myColors, pch = rep(16,4), bty = "n", xpd = T,var.col = "red4",ylim=c(-5,5),xlim=c(-6,4), scale=5,main="CDA Happy")
plot(cda.sad, col = myColors, pch = rep(16,4), bty = "n", xpd = T,var.col = "red4",ylim=c(-5,5),xlim=c(-6,4), scale=5,main="CDA Sad")

# ----------------------------------------------------------------------------------- #
#### EXTRAS: candisc plot #### 
cca.m <- candiscList(lm.sad)
names(cca.m$type)
plot(cca.m, type="n", ask=FALSE)
heplot(cca.m$type, scale=6)
heplot(cca.m$type, scale=3)

# ----------------------------------------------------------------------------------- #
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
