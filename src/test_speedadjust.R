rm(list=ls())

library(tuneR)

# Read the stereo audio file
audio <- readWave("inputs/8_Channel_ID.wav")

# Extract left and right channels
newaudio <- audio

chaoticOscillator <- function(Tlen, r, x0) {
  output <- numeric(Tlen)
  output[1] <- x0
  
  for (i in 2:Tlen) {
    output[i] <- r * output[i-1] * (1 - output[i-1])
  }
  
  return(output)
}

# Example usage
Tlen <- dim(newaudio@.Data)[1]  # Length of the array
r <- 3.9      # Parameter controlling the chaos (typically between 3.57 and 4)

newaudio@.Data <- apply(newaudio@.Data,2,function(x)x*chaoticOscillator(Tlen, r, runif(1,0,1)))

#Slow down
slowfactor = 2
newaudio@.Data[1:dim(newaudio@.Data)[1],1]=rep(newaudio@.Data[,1],each=slowfactor)[1:dim(newaudio@.Data)[1]]

#Speed Up
fastfactor = 2
newin = newaudio@.Data[seq(1,dim(newaudio@.Data)[1],fastfactor),2]
newaudio@.Data[1:length(newin),2]= newin

#Thoughts, hard to do anything other than double or half. 

# Create unique channel names
channel_names <- c("FL", "FR", "FC", "LF", "BL", "BR", "SL", "SR")

# Assign the channel names to the WaveMC object
colnames(newaudio@.Data) <- channel_names

# Write the manipulated audio to a new .wav file
writeWave(newaudio, "inputs/newfile.wav")
