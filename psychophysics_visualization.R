# Libraries
library(gplots)

# Set path
home="/misc/ernst/rcruces/"
source(paste0(home,"git_here/R-plots/R-colors/colormaps_functions.R"))
setwd(paste0(home,"git_here/2018_music_psychophysics"))

# Read the Subjects Information
cases <- read.csv("database/cases.csv")

# Reads the subjects Responses
result <- read.csv("database/cases_results.csv")

# Slices Happy and Sad
hap <- result[result$emo=="h",]
sad <- result[result$emo=="s",] 

# Creates a matrix with the Percentage of Correct Responses per FINE STRUCTURE stimuli
calAccu <- function(df){
  accuracy <- c()
  for (i in unique(df$fts)) {
    sub.cases <- df[df$fts==i,]
    X <- apply(sub.cases[,7:70],2,function(x) sum(ifelse(sub.cases$emo.fts == x, 1,0))/length(x) )
    accuracy <- cbind(accuracy,X)
  }
  colnames(accuracy) <- unique(df$fts)
  return(as.matrix(accuracy))  
}

# Heatmap
accuracy <- calAccu(result)
colG<-c("lightskyblue","deepskyblue","dodgerblue2","dodgerblue4","navy","red","firebrick2","firebrick3","firebrick","firebrick4")
col.map <- optim.color(accuracy,colG)
heatmap.2(accuracy,Colv=FALSE,main= "Fine Structure Accuracy",
          density.info="histogram",
          dendrogram='row',
          trace="none",
          tracecol="white",
          key.title = "Discriminability",     
          col    = col.map$color.palette,   # Colormap
          breaks = col.map$palette.breaks,
          srtCol=1,  
          RowSideColors = c("mediumpurple4","darkolivegreen","darkolivegreen3","mediumpurple")[cases$class])
j=1
Col=c("mediumpurple4","darkolivegreen","darkolivegreen3","mediumpurple")
for (i in levels(cases$class)) {
  indx <- which(cases$class==i)
  heatmap.2(calAccu(hap)[indx,],Colv=FALSE,main= paste(i,"fst-HAP"),
            density.info="histogram",Rowv = TRUE,
            dendrogram='row',
            trace="row",
            tracecol=NA,
            key.title = "Discriminability",     
            col    = col.map$color.palette,   # Colormap
            breaks = col.map$palette.breaks,
            srtCol=1,  
            RowSideColors = rep(Col[j],length(indx)))
  j=j+1
}


# -------------------------------------------------------------------------------------------------------------------------- #
# Happy vs Sad \  Female vs Men
# -------------------------------------------------------------------------------------------------------------------------- #
library(corrplot)
indx <- which(cases$gender=="M")
M.sad <- calAccu(sad)[indx,2:7]
M.hap <- calAccu(hap)[indx,2:7]
indx <- which(cases$gender=="F")
F.sad <- calAccu(sad)[indx,2:7]
F.hap <- calAccu(hap)[indx,2:7]
par(mfrow=c(2,2))
corrplot(cor(M.hap,M.hap)-cor(F.sad,F.sad), title = "Males Happy",method = "square",diag = FALSE)
corrplot(cor(F.sad,F.sad), title = "Female Happy",method = "square",diag = FALSE)
corrplot(cor(M.hap,M.hap), title = "Males Happy",method = "square",diag = FALSE)
corrplot(cor(F.hap,F.hap), title = "Female Happy",method = "square",diag = FALSE)

#### Blank Plot ####
blank.plot <- function (X,lab,Main) {
  Ax.Col="black"
  Main <- as.character(Main)
  plot(X,ylim=c(0,1),xlim = c(0,length(X)+1),pch=19,ylab="score",main=Main,col=NA,xaxt='n',bty='n',xlab="",axes = FALSE,
       las=1,col.axis=Ax.Col,col.lab=Ax.Col,col.main=Ax.Col,cex.axis=1.2,cex=seq(0.5,1.5,length.out = 35),cex.main=3)
  polygon(c(0,0,8,8,0),c(0,1,1,0,0),col="gray85",border = NA)
  abline(h=seq(0,1,0.2),lty=1,col="white")
  axis(1, at=1:7, col.axis=Ax.Col,labels=lab, lty=1, col=Ax.Col, las=1,lwd=3,cex.axis=1,col.lab=Ax.Col)
  axis(2, at=seq(0,1,0.2), labels=seq(0,1,0.2), lty=1, col.axis=Ax.Col,las=1,lwd=3,cex.axis=1.2,col.lab=Ax.Col,col=Ax.Col)
}
par(mfrow=c(1,4))
x <- apply(accuracy[cases$class == "poor",],2,mean)
# HIGH PERFORMANCE
blank.plot(x,lab=colnames(accuracy),Main = "high")
apply(accuracy[cases$class == "high",],1, function(x) lines(x,col="darkolivegreen",lwd=0.5))
lines(apply(accuracy[cases$class == "high",],2,mean),col="darkolivegreen",lwd=2.5)
points(apply(accuracy[cases$class == "high",],2,mean),bg="darkolivegreen",lwd=2,cex=2,pch=21,col="white")
# LOW PERFORMANCE
blank.plot(x,lab=colnames(accuracy),Main = "low")
apply(accuracy[cases$class == "low",],1, function(x) lines(x,col="darkolivegreen3",lwd=0.5))
lines(apply(accuracy[cases$class == "low",],2,mean),col="darkolivegreen3",lwd=2.5)
points(apply(accuracy[cases$class == "low",],2,mean),bg="darkolivegreen3",lwd=2,cex=2,pch=21,col="white")
# BEST PERFORMANCE
blank.plot(x,lab=colnames(accuracy),Main = "best")
apply(accuracy[cases$class == "best",],1, function(x) lines(x,col="mediumpurple4",lwd=0.5))
lines(apply(accuracy[cases$class == "best",],2,mean),col="mediumpurple4",lwd=2.5)
points(apply(accuracy[cases$class == "best",],2,mean),bg="mediumpurple4",lwd=2,cex=1.5,pch=21,col="white")
# POOR PERFORMANCE
blank.plot(x,lab=colnames(accuracy),Main = "poor")
apply(accuracy[cases$class == "poor",],1, function(x) lines(x,col="mediumpurple",lwd=0.5))
lines(x,col="mediumpurple",lwd=2.5)
points(x,bg="mediumpurple",lwd=2,cex=1.5,pch=21,col="white")

# ----------------------------------------------------------------------------------- #
# LINEAR MIXED EFFECTS MODEL
library(nlme) #Linear mixed effects models
library(lattice) # plots
d <- data.frame(accuracy)
colnames(d) <- c(0,2,4,8,16,32,64)
d$type <- cases$class
d$id <- rownames(d)
# Selects from 2 to 64
d <- d[,c("type","id","2","4","8","16","32","64")]
d <- reshape(d, direction = "long", varying = list(names(d)[3:8]), v.names = "response", 
        idvar = c("type","id"), timevar = "fts", times = c(2,4,8,16,32,64))
rownames(d) <- NULL

# LME with random intercep
fst.lme <- lme(response~fts+type,
                random=~1|id,
                data=d, method="ML")
# LME with random slope
fst.lme1 <- lme(response~fts+type,
                random=~fts|id,
                data=d, method="ML")
# Plot of the response based on fine structure given the group
xyplot(response~fts | id, data=d, layout=c(2,2))

# ANOVA ob both models
anova(fst.lme, fst.lme1)
summary(fst.lme1)
d$pred1 <- predict(fst.lme1)

# New model with a quadratic expression
fst.lme2 <- lme(response~fts + I(fts^2),
                random=~fts|type,
                data=d, method="ML")
anova(fst.lme1, fst.lme2)
d$pred2 <- predict(fst.lme2)
# FUNCTION to plot the regression and predictors
pfun <- function(x, y){
  panel.xyplot(x,y[1:length(x)])
  panel.lines(x, y[1:length(x) + length(x)], lty=1)}
# Line regression with predicted values from model 1
plot(xyplot(cbind(response, pred1)~fts | type, data = d, panel=pfun, layout=c(2,2)))
# Line regression with predicted values from model 2
dev.new()
plot(xyplot(cbind(response, pred2)~fts | type, data = d, panel=pfun, layout=c(2,2),ylab="Response",xlab="Fine Structure"))

# ----------------------------------------------------------------------------------- #
#### ANOVA Comparisons #### 
mod1 <- aov(accuracy[,1]~factor(cases$class)+factor(cases$gender))
TukeyHSD(mod1)

# ----------------------------------------------------------------------------------- #
#### LINEAR DISCRIMINANT ANALYSIS ####
# Data Reshape
d <- data.frame(cbind(cases$class,cases$age,cases$music.yrs,cases$gender,accuracy)) 
colnames(d) <- c("type","age","music.yrs","gender","nb0","nb2","nb4","nb8","nb16","nb32","nb64")
d$type <- cases$class
d$id <- rownames(d)
library(MASS)
library(pander)
library(magrittr)
library(dplyr)
library(ggplot2)

# Linear Discriminant model MASS library
m <- lda(type~+nb2+nb4+nb8+nb16+nb32+nb64+age, d)
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

# LDA plot, Only first 2 used
plot(m, abbrev=1,dimen = 3)

<<<<<<< HEAD
# LDA to variables
=======
# Gets the predicted values for each discriminant function
>>>>>>> 153206cb29502e024310e72f53e7f386e72f3302
d$LDA1 <- p$x[,1]
d$LDA2 <- p$x[,2]
d$LDA3 <- p$x[,3]
par(mfrow=c(1,3))
ggplot(d, aes(LDA1, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top")
ggplot(d, aes(LDA2, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top")
ggplot(d, aes(LDA3, fill = type)) + geom_density(alpha = 0.5, color = NA) + theme(legend.position = "top")


gMeans <- d %>% group_by(type) %>% select(LDA1,LDA2,LDA3) %>%  summarise_each(funs(mean)) 
gMeans %>% pander

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

<<<<<<< HEAD
=======

>>>>>>> 153206cb29502e024310e72f53e7f386e72f3302
# PLots the First Dimension
par(mfrow=c(1,2))
plot(d$LDA1,d$LDA2,col="white",bg=c("red","green","blue","orange")[d$type],pch=21,cex=2.5)
#plot(d$LDA2,d$LDA3,col=c("red","green","blue","orange")[df$type])

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
m <- lm(cbind(nb2, nb4, nb8, nb16, nb32, nb64, age) ~ type, d)
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
heplot(cca)

myColors <- hsv((c(0,80,160,240) + 80)/360,s = 0.8,v = 0.8,0.7)
cca %>% plot(col = myColors, pch = rep(16,4), bty = "n", xpd = T)
plot(cca, col = myColors, pch = rep(16,4), bty = "n", xpd = T,var.col = "red4")

