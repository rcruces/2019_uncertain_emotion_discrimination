# Library for wav reading
library(tuneR)

# list all files from a directory ls
files <- list.files("/misc/ernst/rcruces/git_here/2018_music_psychophysics/files/")

# Read a wav mono file
wav1 <- readWave("/misc/ernst/rcruces/git_here/2018_music_psychophysics/files/01.3sec.wav")

# Counts the number of points of the file
N <- length(wav1@left)