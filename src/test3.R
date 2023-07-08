rm(list=ls())

library(tuneR)

# Read the stereo audio file
audio <- readWave("inputs/8_Channel_ID.wav")

# Extract left and right channels
newaudio <- audio

darray <- 1:dim(newaudio@.Data)[1]
oscillator_mask <- cos(0.1*darray)

newaudio@.Data <- apply(newaudio@.Data,2,function(x)x*oscillator_mask)

# Create unique channel names
channel_names <- c("FL", "FR", "FC", "LF", "BL", "BR", "SL", "SR")

# Assign the channel names to the WaveMC object
colnames(newaudio@.Data) <- channel_names

# Write the manipulated audio to a new .wav file
writeWave(newaudio, "inputs/newfile.wav")
