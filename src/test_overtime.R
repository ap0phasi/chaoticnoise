rm(list=ls())

library(tuneR)

adjust_speed <- function(signal, original_interval, new_interval) {
  # Calculate the original and new lengths of the signal
  original_length <- length(signal)
  new_length <- round(original_length * new_interval / original_interval)
  
  # Generate the indices for resampling
  original_indices <- seq_along(signal)
  new_indices <- seq(1, original_length, length.out = new_length)
  
  # Perform linear interpolation to adjust the speed
  adjusted_signal <- approx(original_indices, signal, new_indices)$y
  
  return(adjusted_signal)
}


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
dT=round(Tlen/100)
tracklen = dim(newaudio@.Data)[2]
newtracks = list()
for (itrack in 1:tracklen){
  dout = 1
  temptrack = c()
  while ((dout+dT)<Tlen){
    temptrack = c(temptrack,adjust_speed(newaudio@.Data[dout:(dout+dT),itrack],1,runif(1,0.5,2)))
    dout = dout+dT
  }
  newtracks[[itrack]]=temptrack
  top = min(length(temptrack),dim(newaudio@.Data)[1])
  newaudio@.Data[1:top,itrack]=temptrack[1:top]
}

# Create unique channel names
channel_names <- c("FL", "FR", "FC", "LF", "BL", "BR", "SL", "SR")

# Assign the channel names to the WaveMC object
colnames(newaudio@.Data) <- channel_names

# Write the manipulated audio to a new .wav file
writeWave(newaudio, "inputs/newfile.wav")
