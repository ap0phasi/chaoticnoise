library(tuneR)

# Read the stereo audio file
audio <- readWave("inputs/CantinaBand60.wav")

# Extract left and right channels
left_channel <- audio@left
right_channel <- audio@right

volume_factor_left = 0.1
volume_factor_right = 1.2

# Manipulate left channel (e.g., change volume)
new_left_channel <- left_channel * volume_factor_left

# Manipulate right channel (e.g., change volume)
new_right_channel <- right_channel * volume_factor_right

# Create a new stereo audio object with manipulated channels
new_audio <- Wave(cbind(new_left_channel, new_right_channel), samp.rate = audio@samp.rate)

# Write the manipulated audio to a new .wav file
writeWave(new_audio, "inputs/newfile.wav")
