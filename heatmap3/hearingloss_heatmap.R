#Load necessary packages
library("gplots")
library("devtools")
library("RColorBrewer")
#Load latest version of heatmap.3 function
source_url("https://raw.githubusercontent.com/obigriffith/biostar-tutorials/master/Heatmaps/heatmap.3.R")

#Set a working directory for output files
setwd("/home/rr/git_here/2018_music_psychophysics/heatmap3/")

# Upload the data
data <- read.csv(file = "HearingLoss.csv")
prob_matrix <- as.matrix(scale(data[,40:175])) 
vol_names <- colnames(prob_matrix)
patient_ids <- paste0("Subject_",1:24)
colnames(prob_matrix) <- vol_names
rownames(prob_matrix) <- patient_ids

#Create fake color side bars
side <- c("palegreen4","palegreen")[data[,2]]
gender <- c("navy","skyblue")[data[,3]]

# Call the color function (colorNumeric) to create a new palette function
library("leaflet")
pal <- colorNumeric(brewer.pal(n = 9,"Blues"), 1:20)
# Pass the palette function a data vector to get the corresponding colors
age.scan <- pal(data[,4])
age.audio <- pal(data[,5])

rlab=t(cbind(side,gender,age.scan,age.audio))
rownames(rlab)=c("Side","Gender","Age.Scan","Age.Audio")

#Define custom dist and hclust functions for use with heatmaps
mydist=function(c) {dist(c,method="euclidian")}
myclust=function(c) {hclust(c,method="average")}

# Function that optimizes the color distribution
optim.color <- function(Data,Colors) {
  #       Data: Is the matrix of data to plot
  #       Colors: Vector of colors we want to use
  # 
  mtx <- as.matrix(Data)
  # Following code limits the lowest and highest color to 5%, and 95% of your range, respectively
  quantile.range <- quantile(mtx, probs = seq(0, 1, 0.01))
  palette.breaks <- seq(quantile.range["5%"], quantile.range["95%"], 0.1)
  # Find optimal divergent color palette (or set own)
  color.function <- colorRampPalette(Colors)
  color.palette  <- color.function(length(palette.breaks) - 1)
  # Returns a list with the color map and the optimal number of color breaks
  return(list(color.palette=as.vector(color.palette),palette.breaks=as.vector(palette.breaks)))
}

# We apply our color optimization function to the scaled matrix
col.map <- optim.color(prob_matrix,brewer.pal(n=9,"YlOrRd"))

#Create heatmap using custom heatmap.3 source code loaded above
pdf(file="heatmap3_example.pdf")
main_title="Hearing loss"
par(cex.main=1)
heatmap.3(prob_matrix, 
          hclustfun=myclust, 
          distfun=mydist, 
          na.rm = TRUE, 
          scale="none", 
          dendrogram="both", 
          margins=c(6,12),
          Rowv=TRUE, 
          Colv=TRUE, 
          #ColSideColors=rlab, 
          RowSideColors=rlab, 
          symbreaks=FALSE, 
          key=TRUE, 
          symkey=FALSE,
          density.info="none", 
          trace="none", 
          main=main_title, 
          labCol=FALSE, 
          labRow=patient_ids, 
          cexRow=1, 
          col=col.map$color.palette,
          breaks = col.map$palette.breaks,
          ColSideColorsSize=7, 
          RowSideColorsSize=2, 
          KeyValueName="Scaled Volume")
legend("topright",legend=c("Left","Right","Female","Male"),
       fill=c("palegreen4","palegreen","navy","skyblue"), border=FALSE, bty="n", y.intersp = 0.7, cex=0.7)
dev.off()
