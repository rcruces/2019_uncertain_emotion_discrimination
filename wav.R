#http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html
# Library for wav reading
library(tuneR)

# list all files from a directory ls
home="/home/rr/"
setwd(paste0(home,"git_here/2018_music_psychophysics/files"))
files <- list.files(".")

# Read a wav mono file
wav1 <- readWave(files[7])
plot(wav1@left,type='l')

# Counts the number of points of the file
N <- length(wav1@left)

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

X.k <-fft(wav1@left)
plot.frequency.spectrum(X.k)
