#### SET UP ####

library(devtools)
install_github("marta-vidalgarcia/ShapeRotator", force = T)
library(ShapeRotator)
library(R.matlab)


#### ADULTS ####
dir(pattern = "*.mat")
data_matlab_adults <- readMat('ToadStereoHeadTailAdults.mat')

setwd("./Adults")
str(data_matlab_adults$ToadData[,,1])

## Now the structure is:
# List of 25
# $ name                  : chr [1, 1] "empty"
# $ ID                    : num [1, 1] 3
# $ Sex                   :List of 1
# ..$ :List of 1
# .. ..$ : chr [1, 1] "M"
# $ JumpID                : num [1, 1] 3
# $ Offset                : num [1, 1] 0
# $ Path                  : num [1:87, 1:3] -99 -99 -99 -99.1 -99.3 ...
# $ cloacaPath            : num [1:87, 1:3] -83.4 -83.7 -83.8 -84 -84.2 ...
# $ RedCam                : num [1:87, 1:5] 645 646 647 648 649 650 651 652 653 654 ...
# $ WhiteCam              : num [1:87, 1:5] 645 646 647 648 649 650 651 652 653 654 ...
# $ RedCoords             : num [1, 1:5] -80 80 2 76 0
# $ RedbodyLength         : num [1, 1] 34.1
# $ RedJumpDist           : num [1, 1] 216
# $ WhiteCoords           : num [1, 1:5] -80 80 2 76 0
# $ WhitebodyLength       : num [1, 1] 34.1
# $ WhiteJumpDist         : num [1, 1] 216
# $ ThreeDCoords          : num [1, 1:3] 49.64 -3.69 28.61
# $ ThreeDbodyLength      : num [1, 1] 17.9
# $ ThreeDbodyLengthEnd   : num [1, 1] 20.4
# $ ThreeDbodyLengthAll   : num [1, 1:87] 18.4 18.2 18 17.9 17.9 ...
# $ ThreeDJumpDist        : num [1, 1] 57.4
# $ ThreeDJumpDistCloaca  : num [1, 1] 54.8
# $ ThreeDbodyLengthMedian: num [1, 1] 20.6
# $ RedRatio              : num [1, 1] 6.33
# $ WhiteRatio            : num [1, 1] 6.33
# $ ThreeDRatio           : num [1, 1] 2.78

#### SVL- ADULTS ####
data_matlab_adults$ToadData[,,1]$ThreeDbodyLengthAll

for (i in 1:dim(data_matlab_adults$ToadData)[3]){
  SVL_table <- cbind(seq(1:length(data_matlab_adults$ToadData[,,i]$ThreeDbodyLengthAll)), t(data_matlab_adults$ToadData[,,i]$ThreeDbodyLengthAll))
  colnames(SVL_table) <- c("frame", "SVL_cm")
  cane_toad_simple_code <- paste(x=c(as.numeric(data_matlab_adults$ToadData[, , i]$ID), as.character(data_matlab_adults$ToadData[, , i]$Sex[[1]])), collapse="")
  write.csv(SVL_table, paste(x=c(cane_toad_simple_code, "_SVL.csv"), collapse=""), row.names = FALSE)
  rm(SVL_table)
}


data_matlab_adults$ToadData # All specimens listed

data_matlab_adults$ToadData[, , 1] # Check first specimen

data_matlab_adults$ToadData[, , dim(data_matlab_adults$ToadData)[3]] # check last specimen

# Check everything in specimen n
n = 2 # e.g. second one
data_matlab_adults$ToadData[, , n]$ID # Part 2 of the toad code
data_matlab_adults$ToadData[, , n]$Sex[[1]] # Part 3 of the toad code
data_matlab_adults$ToadData[, , n]$JumpID # Part 4 of the toad code
data_matlab_adults$ToadData[, , n]$Path # Jumping path for each toad based on snout LMs, the columns are x y z in that order
data_matlab_adults$ToadData[, , n]$cloacaPath # Jumping path for each toad based on cloaca LMs, the columns are x y z in that order

cane_toad_simple_code <- paste(x=c(as.character(data_matlab_adults$ToadData[, , n]$ID), data_matlab_adults$ToadData[, , n]$Sex[[1]]), collapse="")
cane_toad_simple_code

cane_toad_complete_code <- paste(x=c(as.character(data_matlab_adults$ToadData[, , n]$ID), data_matlab_adults$ToadData[, , n]$Sex[[1]], "_", as.character(data_matlab_adults$ToadData[, , n]$JumpID)), collapse="")
cane_toad_complete_code # looks good

jumping_path_snout <- data_matlab_adults$ToadData[, , n]$Path
jumping_path_cloaca  <- data_matlab_adults$ToadData[, , n]$cloacaPath

#### ADULTS - SNOUT ####
# Create the output variables
var_1 <- character(dim(data_matlab_adults$ToadData)[3])
var_2 <- character(dim(data_matlab_adults$ToadData)[3])
var_3 <- numeric(dim(data_matlab_adults$ToadData)[3])
var_4 <- numeric(dim(data_matlab_adults$ToadData)[3])
var_5 <- numeric(dim(data_matlab_adults$ToadData)[3])
var_6 <- numeric(dim(data_matlab_adults$ToadData)[3])
var_7 <- numeric(dim(data_matlab_adults$ToadData)[3])
var_8 <- numeric(dim(data_matlab_adults$ToadData)[3])

# Loop to get information for all specimens
for (i in 1:dim(data_matlab_adults$ToadData)[3]) {
  cane_toad_simple_code <- paste(x=c(as.numeric(data_matlab_adults$ToadData[, , i]$ID), as.character(data_matlab_adults$ToadData[, , i]$Sex[[1]])), collapse="")
  cane_toad_complete_code <- paste(x=c(as.character(data_matlab_adults$ToadData[, , i]$ID), data_matlab_adults$ToadData[, , i]$Sex[[1]], "_", as.character(data_matlab_adults$ToadData[, , i]$JumpID)), collapse="")
  
  jumping_path_snout <- data_matlab_adults$ToadData[, , i]$Path
  
  # Translate the data to point of origin (0,0,0)
  
  vector_start <- as.numeric(jumping_path_snout[1, ])
  
  translated_jumping_path <- sweep(jumping_path_snout, 2, vector_start)
  translated_jumping_path
  
  
  # ROTATION: 
  
  #Choose vector of rotation (here I am constraining it to be z=0 across the whole trajectory, and y=0 for start and end)
  
  vector_start_end <- as.numeric(translated_jumping_path[nrow(translated_jumping_path), ])
  
  vector_u_end <- c(1, 0, 0) 
  
  
  #Angle of rot: 
  
  angle_to_end <- ShapeRotator:::angle_3D (vector_start_end, vector_u_end)
  
  # axis of rotation
  axis_to_end <- ShapeRotator:::cross_3D(vector_start_end, vector_u_end)
  
  # rotation matrix: 
  rotmatrix_to_end <- ShapeRotator:::rotmat_3D(axis_to_end, angle_to_end)
  
  # Obtain the rotation: 
  Rotated_trajectory_3D <- ShapeRotator:::rotveclist_3D(rotmatrix_to_end, translated_jumping_path) # Getting the error here
  
  # Correct the orientation on the y-axis in the middle of the trajectory just in case: 
  mid_point <- round(nrow(Rotated_trajectory_3D)/2) # getting the mid-point but still an integer
  
  rotmatrix_H_Y <- ShapeRotator:::rotmat_3D( c(1,0,0), ShapeRotator:::Y_to_Z_rot_angle_3D(Rotated_trajectory_3D[mid_point,]) ) #picked 20 as it should be positive but it does not matter
  Rotated_trajectory_3D <- ShapeRotator:::rotveclist_3D(rotmatrix_H_Y, Rotated_trajectory_3D)
  
  # Check orientation of the y-axis
  
  if (Rotated_trajectory_3D[mid_point,2] < 0) {
    Rotated_trajectory_3D[,2] <- Rotated_trajectory_3D[,2] * (-1)
  }
  
  # Plot the trajectory of the jump (please note that they 'smash' their face to the ground when they land, so y at landing will be <0)
  
  # plot(Rotated_trajectory_3D[,1], Rotated_trajectory_3D[,2])
  # 
  # title("Jumping trajectory of a cane toad", sub = cane_toad_complete_code,
  #       cex.main = 1.5,   font.main= 2, col.main= "black",
  #       cex.sub = 1.25, font.sub = 4, col.sub = "blue")
  
  file_pdf <- paste(x=c(cane_toad_simple_code, "_SNOUT.pdf"), collapse="")
  
  pdf(file_pdf, width=13.29, height=4.26)
  plot(Rotated_trajectory_3D[,1], Rotated_trajectory_3D[,2], col="darkgreen")
  
  title("Jumping trajectory - SNOUT", sub = cane_toad_simple_code,
        cex.main = 1.5,   font.main= 2, col.main= "black",
        cex.sub = 1.25, font.sub = 4, col.sub = "blue")
  
  dev.off()
  
  # plot(Rotated_trajectory_3D[,1], Rotated_trajectory_3D[,3]) # slight movement of the landmark across z-axis, but it does not affect the trajectory
  
  
  # Output data from jumping trajectory
  
  distance_jump <- max(Rotated_trajectory_3D[,1]) # in mm
  height_jump <- max(Rotated_trajectory_3D[,2]) # in mm 
  
  
  # Calculate time (in seconds) for each frame in the jumping trajectory
  time0 = 0
  frames_per_second = 240 # change it to 120 or 240 depending on the filming speed
  
  time_vector <- seq(time0, (nrow(Rotated_trajectory_3D)-1)/frames_per_second, 1/frames_per_second)
  length(time_vector)
  
  
  
  # Calculate Euclidian distance from each point in the trajectory (arc) to the next
  arc_jump <- vector(mode="numeric", length=nrow(Rotated_trajectory_3D)-1)
  
  for (j in 1:(nrow(Rotated_trajectory_3D)-1)) {
    arc_jump[j] <- sqrt((Rotated_trajectory_3D[j+1,1]-Rotated_trajectory_3D[j,1])^2 + (Rotated_trajectory_3D[j+1,2]-Rotated_trajectory_3D[j,2])^2 + 
                          (Rotated_trajectory_3D[j+1,3]-Rotated_trajectory_3D[j,3])^2)
  }  
  print(arc_jump)
  
  
  # Calculate velocity from Euclidian distances and time for each frame in the jump
  
  velocity_per_frame <- arc_jump/(1/frames_per_second) #mm per second
  
  average_velocity <- mean(velocity_per_frame) # AVERAGE VELOCITY
  
  maximum_velocity <- max(velocity_per_frame) # MAXIMUM VELOCITY
  
  
  # Calculate angles in jumping trajectory
  
  # Take-off angle
  first_take_off_y <- which(Rotated_trajectory_3D[,2] > max(Rotated_trajectory_3D[,2])*0.05)[1]
  
  last_take_off_y <- first_take_off_y + 5 # I put 5 to make sure it is the take-off only but it could be 10, 15 or something else
  
  x_diff_take_off <- Rotated_trajectory_3D[last_take_off_y,1] - Rotated_trajectory_3D[first_take_off_y,1]
  y_diff_take_off <- Rotated_trajectory_3D[last_take_off_y,2] - Rotated_trajectory_3D[first_take_off_y,2]
  
  theta_take_off <-  atan2(y_diff_take_off, x_diff_take_off) # in radians
  
  angle_take_off <- 360/(2* pi)*theta_take_off # in degrees
  
  
  # Landing angle
  last_landing_y <- which(Rotated_trajectory_3D[ 2:nrow(Rotated_trajectory_3D),2] == min(Rotated_trajectory_3D[round(nrow(Rotated_trajectory_3D)/2):nrow(Rotated_trajectory_3D),2]))
  first_landing_y <- last_landing_y - 10 # I've put 10 as the landing seems to take longer than take-off and often messier, We can change it though
  
  x_diff_landing <- Rotated_trajectory_3D[last_landing_y,1] - Rotated_trajectory_3D[first_landing_y,1]
  y_diff_landing <- Rotated_trajectory_3D[last_landing_y,2] - Rotated_trajectory_3D[first_landing_y,2]
  
  
  theta_landing <-  atan2(y_diff_landing, x_diff_landing) # in radians
  
  angle_landing <- 360/(2* pi)*theta_landing # in degrees
  
 
   # Save the output data
write.csv(Rotated_trajectory_3D[,1:2], paste(x=c(cane_toad_simple_code, "_final_2D_trajectory_SNOUT.csv"), collapse=""))

  # IDs
  var_1[i] <- as.character(cane_toad_simple_code)
  var_2[i] <- as.character(cane_toad_complete_code)
  
  # Snout
  var_3[i] <- as.numeric(distance_jump)
  var_4[i] <- as.numeric(height_jump)
  var_5[i] <- as.numeric(average_velocity)
  var_6[i] <- as.numeric(maximum_velocity)
  var_7[i] <- as.numeric(angle_take_off)
  var_8[i] <- as.numeric(angle_landing)

}
  
# Make the final database with all the output variables from all specimens
output_data_jumping <- data.frame(var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, stringsAsFactors=FALSE)
colnames(output_data_jumping) <- c("cane_toad_simple_code", "cane_toad_complete_code", "distance_jump_snout", "height_jump_snout", "average_velocity_snout", 
                                   "maximum_velocity_snout", "angle_take_off_snout", "angle_landing_snout")


# Save the final dataset with all the output data from all the toads (ONLY RUN THIS AT THE END)

write.csv(output_data_jumping, "output_data_jumping_ADULTS_SNOUT.csv", row.names = FALSE)


#### ADULTS - CLOACA ####
# Remove previous output variables
rm(var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, vector_start, translated_jumping_path, vector_start_end, vector_u_end, angle_to_end, axis_to_end, 
   rotmatrix_to_end, Rotated_trajectory_3D, mid_point, rotmatrix_H_Y, distance_jump, height_jump, frames_per_second, time_vector, 
   arc_jump, velocity_per_frame, average_velocity, maximum_velocity)

# Create the output variables
var_1 <- character(dim(data_matlab_adults$ToadData)[3])
var_2 <- character(dim(data_matlab_adults$ToadData)[3])
var_3 <- numeric(dim(data_matlab_adults$ToadData)[3])
var_4 <- numeric(dim(data_matlab_adults$ToadData)[3])
var_5 <- numeric(dim(data_matlab_adults$ToadData)[3])
var_6 <- numeric(dim(data_matlab_adults$ToadData)[3])
var_7 <- numeric(dim(data_matlab_adults$ToadData)[3])
var_8 <- numeric(dim(data_matlab_adults$ToadData)[3])

# Loop to get information for all specimens
for (i in 1:dim(data_matlab_adults$ToadData)[3]) {
  cane_toad_simple_code <- paste(x=c(as.numeric(data_matlab_adults$ToadData[, , i]$ID), as.character(data_matlab_adults$ToadData[, , i]$Sex[[1]])), collapse="")
  cane_toad_complete_code <- paste(x=c(as.character(data_matlab_adults$ToadData[, , i]$ID), data_matlab_adults$ToadData[, , i]$Sex[[1]], "_", as.character(data_matlab_adults$ToadData[, , i]$JumpID)), collapse="")
  jumping_path <- data_matlab_adults$ToadData[, , i]$cloacaPath
  
  # Translate the data to point of origin (0,0,0)
  
  vector_start <- as.numeric(jumping_path[1, ])
  
  translated_jumping_path <- sweep(jumping_path, 2, vector_start)
  translated_jumping_path
  
  
  # ROTATION: 
  
  #Choose vector of rotation (here I am constraining it to be z=0 across the whole trajectory, and y=0 for start and end)
  
  vector_start_end <- as.numeric(translated_jumping_path[nrow(translated_jumping_path), ])
  
  vector_u_end <- c(1, 0, 0) 
  
  
  #Angle of rot: 
  
  angle_to_end <- ShapeRotator:::angle_3D (vector_start_end, vector_u_end)
  
  # axis of rotation
  axis_to_end <- ShapeRotator:::cross_3D(vector_start_end, vector_u_end)
  
  # rotation matrix: 
  rotmatrix_to_end <- ShapeRotator:::rotmat_3D(axis_to_end, angle_to_end)
  
  # Obtain the rotation: 
  Rotated_trajectory_3D <- ShapeRotator:::rotveclist_3D(rotmatrix_to_end, translated_jumping_path) # Getting the error here
  
  # Correct the orientation on the y-axis in the middle of the trajectory just in case: 
  mid_point <- round(nrow(Rotated_trajectory_3D)/2) # getting the mid-point but still an integer
  
  rotmatrix_H_Y <- ShapeRotator:::rotmat_3D( c(1,0,0), ShapeRotator:::Y_to_Z_rot_angle_3D(Rotated_trajectory_3D[mid_point,]) ) #picked 20 as it should be positive but it does not matter
  Rotated_trajectory_3D <- ShapeRotator:::rotveclist_3D(rotmatrix_H_Y, Rotated_trajectory_3D)
  
  # Check orientation of the y-axis
  
  if (Rotated_trajectory_3D[mid_point,2] < 0) {
    Rotated_trajectory_3D[,2] <- Rotated_trajectory_3D[,2] * (-1)
  }
  
  # Plot the trajectory of the jump (please note that they 'smash' their face to the ground when they land, so y at landing will be <0)
  
  # plot(Rotated_trajectory_3D[,1], Rotated_trajectory_3D[,2])
  # 
  # title("Jumping trajectory of a cane toad", sub = cane_toad_complete_code,
  #       cex.main = 1.5,   font.main= 2, col.main= "black",
  #       cex.sub = 1.25, font.sub = 4, col.sub = "blue")
  
  file_pdf <- paste(x=c(cane_toad_simple_code, "_CLOACA.pdf"), collapse="")
  
  pdf(file_pdf, width=13.29, height=4.26)
  plot(Rotated_trajectory_3D[,1], Rotated_trajectory_3D[,2], col="darkgreen")
  
  title("Jumping trajectory - CLOACA", sub = cane_toad_simple_code,
        cex.main = 1.5,   font.main= 2, col.main= "black",
        cex.sub = 1.25, font.sub = 4, col.sub = "blue")
  
  dev.off()
  
  # plot(Rotated_trajectory_3D[,1], Rotated_trajectory_3D[,3]) # slight movement of the landmark across z-axis, but it does not affect the trajectory
  
  
  # Output data from jumping trajectory
  
  distance_jump <- max(Rotated_trajectory_3D[,1]) # in mm
  height_jump <- max(Rotated_trajectory_3D[,2]) # in mm 
  
  
  # Calculate time (in seconds) for each frame in the jumping trajectory
  time0 = 0
  frames_per_second = 240 # change it to 120 or 240 depending on the filming speed
  
  time_vector <- seq(time0, (nrow(Rotated_trajectory_3D)-1)/frames_per_second, 1/frames_per_second)
  length(time_vector)
  
  
  
  # Calculate Euclidian distance from each point in the trajectory (arc) to the next
  arc_jump <- vector(mode="numeric", length=nrow(Rotated_trajectory_3D)-1)
  
  for (j in 1:(nrow(Rotated_trajectory_3D)-1)) {
    arc_jump[j] <- sqrt((Rotated_trajectory_3D[j+1,1]-Rotated_trajectory_3D[j,1])^2 + (Rotated_trajectory_3D[j+1,2]-Rotated_trajectory_3D[j,2])^2 + 
                          (Rotated_trajectory_3D[j+1,3]-Rotated_trajectory_3D[j,3])^2)
  }  
  print(arc_jump)
  
  
  # Calculate velocity from Euclidian distances and time for each frame in the jump
  
  velocity_per_frame <- arc_jump/(1/frames_per_second) #mm per second
  
  average_velocity <- mean(velocity_per_frame) # AVERAGE VELOCITY
  
  maximum_velocity <- max(velocity_per_frame) # MAXIMUM VELOCITY
  
  
  # Calculate angles in jumping trajectory
  
  # Take-off angle
  first_take_off_y <- which(Rotated_trajectory_3D[,2] > max(Rotated_trajectory_3D[,2])*0.05)[1]
  
  last_take_off_y <- first_take_off_y + 5 # I put 5 to make sure it is the take-off only but it could be 10, 15 or something else
  
  x_diff_take_off <- Rotated_trajectory_3D[last_take_off_y,1] - Rotated_trajectory_3D[first_take_off_y,1]
  y_diff_take_off <- Rotated_trajectory_3D[last_take_off_y,2] - Rotated_trajectory_3D[first_take_off_y,2]
  
  theta_take_off <-  atan2(y_diff_take_off, x_diff_take_off) # in radians
  
  angle_take_off <- 360/(2* pi)*theta_take_off # in degrees
  
  
  # Landing angle
  last_landing_y <- which(Rotated_trajectory_3D[ 2:nrow(Rotated_trajectory_3D),2] == min(Rotated_trajectory_3D[round(nrow(Rotated_trajectory_3D)/2):nrow(Rotated_trajectory_3D),2]))
  first_landing_y <- last_landing_y - 10 # I've put 10 as the landing seems to take longer than take-off and often messier, We can change it though
  
  x_diff_landing <- Rotated_trajectory_3D[last_landing_y,1] - Rotated_trajectory_3D[first_landing_y,1]
  y_diff_landing <- Rotated_trajectory_3D[last_landing_y,2] - Rotated_trajectory_3D[first_landing_y,2]
  
  
  theta_landing <-  atan2(y_diff_landing, x_diff_landing) # in radians
  
  angle_landing <- 360/(2* pi)*theta_landing # in degrees
  
  
  # Save the output data
  write.csv(Rotated_trajectory_3D[,1:2], paste(x=c(cane_toad_simple_code, "_final_2D_trajectory_CLOACA.csv"), collapse=""))
  
  # IDs
  var_1[i] <- as.character(cane_toad_simple_code)
  var_2[i] <- as.character(cane_toad_complete_code)
  
  # Snout
  var_3[i] <- as.numeric(distance_jump)
  var_4[i] <- as.numeric(height_jump)
  var_5[i] <- as.numeric(average_velocity)
  var_6[i] <- as.numeric(maximum_velocity)
  var_7[i] <- as.numeric(angle_take_off)
  var_8[i] <- as.numeric(angle_landing)
  
}

# Make the final database with all the output variables from all specimens
output_data_jumping <- data.frame(var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, stringsAsFactors=FALSE)
colnames(output_data_jumping) <- c("cane_toad_simple_code", "cane_toad_complete_code", "distance_jump_cloaca", "height_jump_cloaca", "average_velocity_cloaca", 
                                   "maximum_velocity_cloaca", "angle_take_off_cloaca", "angle_landing_cloaca")


# Save the final dataset with all the output data from all the toads (ONLY RUN THIS AT THE END)

write.csv(output_data_jumping, "output_data_jumping_ADULTS_CLOACA.csv", row.names = FALSE)


#### JUVENILES ####
# Remove previous output variables
rm(var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, vector_start, translated_jumping_path, vector_start_end, vector_u_end, angle_to_end, axis_to_end, 
   rotmatrix_to_end, Rotated_trajectory_3D, mid_point, rotmatrix_H_Y, distance_jump, height_jump, frames_per_second, time_vector, 
   arc_jump, velocity_per_frame, average_velocity, maximum_velocity)

setwd("../")
data_matlab_juveniles <- readMat('ToadStereoHeadTailJuveniles.mat')

setwd("./Juveniles")
str(data_matlab_juveniles$ToadData[,,1])

## Now the structure is:
# List of 25
# $ name                  : chr [1, 1] "empty"
# $ ID                    : num [1, 1] 177
# $ Sex                   :List of 1
# ..$ :List of 1
# .. ..$ : chr [1, 1] "J"
# $ JumpID                : num [1, 1] 1
# $ Offset                : int [1, 1] 209
# $ Path                  : num [1:68, 1:3] -23 -22.8 -22.6 -22.4 -22.2 ...
# $ cloacaPath            : num [1:68, 1:3] -30.4 -30.4 -30.4 -30.4 -30.4 ...
# $ RedCam                : num [1:68, 1:5] 3945 3946 3947 3948 3949 ...
# $ WhiteCam              : num [1:68, 1:5] 4154 4155 4156 4157 4158 ...
# $ RedCoords             : num [1, 1:5] -61 -47 1 -39 1
# $ RedbodyLength         : num [1, 1] 15.2
# $ RedJumpDist           : num [1, 1] 23.3
# $ WhiteCoords           : num [1, 1:5] -61 -46 -2 -38 0
# $ WhitebodyLength       : num [1, 1] 16.6
# $ WhiteJumpDist         : num [1, 1] 105
# $ ThreeDCoords          : num [1, 1:3] -55.556 -0.476 10.878
# $ ThreeDbodyLength      : num [1, 1] 10.1
# $ ThreeDbodyLengthEnd   : num [1, 1] 18.1
# $ ThreeDbodyLengthAll   : num [1, 1:68] 9.38 9.5 9.77 10.13 10.44 ...
# $ ThreeDJumpDist        : num [1, 1] 56.6
# $ ThreeDJumpDistCloaca  : num [1, 1] 48
# $ ThreeDbodyLengthMedian: num [1, 1] 16.1
# $ RedRatio              : num [1, 1] 1.53
# $ WhiteRatio            : num [1, 1] 6.32
# $ ThreeDRatio           : num [1, 1] 3.52


#### SVL - JUVELINES ####
for (i in 1:dim(data_matlab_juveniles$ToadData)[3]){
  SVL_table <- cbind(seq(1:length(data_matlab_juveniles$ToadData[,,i]$ThreeDbodyLengthAll)), t(data_matlab_juveniles$ToadData[,,i]$ThreeDbodyLengthAll))
  colnames(SVL_table) <- c("frame", "SVL_cm")
  cane_toad_simple_code <- paste(x=c(as.numeric(data_matlab_juveniles$ToadData[, , i]$ID), as.character(data_matlab_juveniles$ToadData[, , i]$Sex[[1]])), collapse="")
  write.csv(SVL_table, paste(x=c(cane_toad_simple_code, "_SVL.csv"), collapse=""), row.names = FALSE)
  rm(SVL_table)
}


data_matlab_juveniles$ToadData # All specimens listed

data_matlab_juveniles$ToadData[, , 1] # Check first specimen

data_matlab_juveniles$ToadData[, , dim(data_matlab_juveniles$ToadData)[3]] # check last specimen

# Check everything in specimen n
n = 2 # e.g. second one
data_matlab_juveniles$ToadData[, , n]$ID # Part 2 of the toad code
data_matlab_juveniles$ToadData[, , n]$Sex[[1]] # Part 3 of the toad code
data_matlab_juveniles$ToadData[, , n]$JumpID # Part 4 of the toad code
data_matlab_juveniles$ToadData[, , n]$Path # Jumping path for each toad based on snout LMs, the columns are x y z in that order
data_matlab_juveniles$ToadData[, , n]$cloacaPath # Jumping path for each toad based on cloaca LMs, the columns are x y z in that order

cane_toad_simple_code <- paste(x=c(as.character(data_matlab_juveniles$ToadData[, , n]$ID), data_matlab_juveniles$ToadData[, , n]$Sex[[1]]), collapse="")
cane_toad_simple_code

cane_toad_complete_code <- paste(x=c(as.character(data_matlab_juveniles$ToadData[, , n]$ID), data_matlab_juveniles$ToadData[, , n]$Sex[[1]], "_", as.character(data_matlab_juveniles$ToadData[, , n]$JumpID)), collapse="")
cane_toad_complete_code # looks good

jumping_path_snout <- data_matlab_juveniles$ToadData[, , n]$Path
jumping_path_cloaca  <- data_matlab_juveniles$ToadData[, , n]$cloacaPath

#### JUVENILES - SNOUT ####
# Create the output variables
var_1 <- character(dim(data_matlab_juveniles$ToadData)[3])
var_2 <- character(dim(data_matlab_juveniles$ToadData)[3])
var_3 <- numeric(dim(data_matlab_juveniles$ToadData)[3])
var_4 <- numeric(dim(data_matlab_juveniles$ToadData)[3])
var_5 <- numeric(dim(data_matlab_juveniles$ToadData)[3])
var_6 <- numeric(dim(data_matlab_juveniles$ToadData)[3])
var_7 <- numeric(dim(data_matlab_juveniles$ToadData)[3])
var_8 <- numeric(dim(data_matlab_juveniles$ToadData)[3])

# Loop to get information for all specimens
for (i in 1:dim(data_matlab_juveniles$ToadData)[3]) {
  cane_toad_simple_code <- paste(x=c(as.numeric(data_matlab_juveniles$ToadData[, , i]$ID), as.character(data_matlab_juveniles$ToadData[, , i]$Sex[[1]])), collapse="")
  cane_toad_complete_code <- paste(x=c(as.character(data_matlab_juveniles$ToadData[, , i]$ID), data_matlab_juveniles$ToadData[, , i]$Sex[[1]], "_", as.character(data_matlab_juveniles$ToadData[, , i]$JumpID)), collapse="")
  
  jumping_path_snout <- data_matlab_juveniles$ToadData[, , i]$Path
  
  # Translate the data to point of origin (0,0,0)
  
  vector_start <- as.numeric(jumping_path_snout[1, ])
  
  translated_jumping_path <- sweep(jumping_path_snout, 2, vector_start)
  translated_jumping_path
  
  
  # ROTATION: 
  
  #Choose vector of rotation (here I am constraining it to be z=0 across the whole trajectory, and y=0 for start and end)
  
  vector_start_end <- as.numeric(translated_jumping_path[nrow(translated_jumping_path), ])
  
  vector_u_end <- c(1, 0, 0) 
  
  
  #Angle of rot: 
  
  angle_to_end <- ShapeRotator:::angle_3D (vector_start_end, vector_u_end)
  
  # axis of rotation
  axis_to_end <- ShapeRotator:::cross_3D(vector_start_end, vector_u_end)
  
  # rotation matrix: 
  rotmatrix_to_end <- ShapeRotator:::rotmat_3D(axis_to_end, angle_to_end)
  
  # Obtain the rotation: 
  Rotated_trajectory_3D <- ShapeRotator:::rotveclist_3D(rotmatrix_to_end, translated_jumping_path) # Getting the error here
  
  # Correct the orientation on the y-axis in the middle of the trajectory just in case: 
  mid_point <- round(nrow(Rotated_trajectory_3D)/2) # getting the mid-point but still an integer
  
  rotmatrix_H_Y <- ShapeRotator:::rotmat_3D( c(1,0,0), ShapeRotator:::Y_to_Z_rot_angle_3D(Rotated_trajectory_3D[mid_point,]) ) #picked 20 as it should be positive but it does not matter
  Rotated_trajectory_3D <- ShapeRotator:::rotveclist_3D(rotmatrix_H_Y, Rotated_trajectory_3D)
  
  # Check orientation of the y-axis
  
  if (Rotated_trajectory_3D[mid_point,2] < 0) {
    Rotated_trajectory_3D[,2] <- Rotated_trajectory_3D[,2] * (-1)
  }
  
  # Plot the trajectory of the jump (please note that they 'smash' their face to the ground when they land, so y at landing will be <0)
  
  # plot(Rotated_trajectory_3D[,1], Rotated_trajectory_3D[,2])
  # 
  # title("Jumping trajectory of a cane toad", sub = cane_toad_complete_code,
  #       cex.main = 1.5,   font.main= 2, col.main= "black",
  #       cex.sub = 1.25, font.sub = 4, col.sub = "blue")
  
  file_pdf <- paste(x=c(cane_toad_simple_code, "_SNOUT.pdf"), collapse="")
  
  pdf(file_pdf, width=13.29, height=4.26)
  plot(Rotated_trajectory_3D[,1], Rotated_trajectory_3D[,2], col="darkgreen")
  
  title("Jumping trajectory - SNOUT", sub = cane_toad_simple_code,
        cex.main = 1.5,   font.main= 2, col.main= "black",
        cex.sub = 1.25, font.sub = 4, col.sub = "blue")
  
  dev.off()
  
  # plot(Rotated_trajectory_3D[,1], Rotated_trajectory_3D[,3]) # slight movement of the landmark across z-axis, but it does not affect the trajectory
  
  
  # Output data from jumping trajectory
  
  distance_jump <- max(Rotated_trajectory_3D[,1]) # in mm
  height_jump <- max(Rotated_trajectory_3D[,2]) # in mm 
  
  
  # Calculate time (in seconds) for each frame in the jumping trajectory
  time0 = 0
  frames_per_second = 240 # change it to 120 or 240 depending on the filming speed
  
  time_vector <- seq(time0, (nrow(Rotated_trajectory_3D)-1)/frames_per_second, 1/frames_per_second)
  length(time_vector)
  
  
  
  # Calculate Euclidian distance from each point in the trajectory (arc) to the next
  arc_jump <- vector(mode="numeric", length=nrow(Rotated_trajectory_3D)-1)
  
  for (j in 1:(nrow(Rotated_trajectory_3D)-1)) {
    arc_jump[j] <- sqrt((Rotated_trajectory_3D[j+1,1]-Rotated_trajectory_3D[j,1])^2 + (Rotated_trajectory_3D[j+1,2]-Rotated_trajectory_3D[j,2])^2 + 
                          (Rotated_trajectory_3D[j+1,3]-Rotated_trajectory_3D[j,3])^2)
  }  
  print(arc_jump)
  
  
  # Calculate velocity from Euclidian distances and time for each frame in the jump
  
  velocity_per_frame <- arc_jump/(1/frames_per_second) #mm per second
  
  average_velocity <- mean(velocity_per_frame) # AVERAGE VELOCITY
  
  maximum_velocity <- max(velocity_per_frame) # MAXIMUM VELOCITY
  
  
  # Calculate angles in jumping trajectory
  
  # Take-off angle
  first_take_off_y <- which(Rotated_trajectory_3D[,2] > max(Rotated_trajectory_3D[,2])*0.05)[1]
  
  last_take_off_y <- first_take_off_y + 5 # I put 5 to make sure it is the take-off only but it could be 10, 15 or something else
  
  x_diff_take_off <- Rotated_trajectory_3D[last_take_off_y,1] - Rotated_trajectory_3D[first_take_off_y,1]
  y_diff_take_off <- Rotated_trajectory_3D[last_take_off_y,2] - Rotated_trajectory_3D[first_take_off_y,2]
  
  theta_take_off <-  atan2(y_diff_take_off, x_diff_take_off) # in radians
  
  angle_take_off <- 360/(2* pi)*theta_take_off # in degrees
  
  
  # Landing angle
  last_landing_y <- which(Rotated_trajectory_3D[ 2:nrow(Rotated_trajectory_3D),2] == min(Rotated_trajectory_3D[round(nrow(Rotated_trajectory_3D)/2):nrow(Rotated_trajectory_3D),2]))
  first_landing_y <- last_landing_y - 10 # I've put 10 as the landing seems to take longer than take-off and often messier, We can change it though
  
  x_diff_landing <- Rotated_trajectory_3D[last_landing_y,1] - Rotated_trajectory_3D[first_landing_y,1]
  y_diff_landing <- Rotated_trajectory_3D[last_landing_y,2] - Rotated_trajectory_3D[first_landing_y,2]
  
  
  theta_landing <-  atan2(y_diff_landing, x_diff_landing) # in radians
  
  angle_landing <- 360/(2* pi)*theta_landing # in degrees
  
  
  # Save the output data
  write.csv(Rotated_trajectory_3D[,1:2], paste(x=c(cane_toad_simple_code, "_final_2D_trajectory_SNOUT.csv"), collapse=""))
  
  # IDs
  var_1[i] <- as.character(cane_toad_simple_code)
  var_2[i] <- as.character(cane_toad_complete_code)
  
  # Snout
  var_3[i] <- as.numeric(distance_jump)
  var_4[i] <- as.numeric(height_jump)
  var_5[i] <- as.numeric(average_velocity)
  var_6[i] <- as.numeric(maximum_velocity)
  var_7[i] <- as.numeric(angle_take_off)
  var_8[i] <- as.numeric(angle_landing)
  
}

# Make the final database with all the output variables from all specimens
output_data_jumping <- data.frame(var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, stringsAsFactors=FALSE)
colnames(output_data_jumping) <- c("cane_toad_simple_code", "cane_toad_complete_code", "distance_jump_snout", "height_jump_snout", "average_velocity_snout", 
                                   "maximum_velocity_snout", "angle_take_off_snout", "angle_landing_snout")


# Save the final dataset with all the output data from all the toads (ONLY RUN THIS AT THE END)

write.csv(output_data_jumping, "output_data_jumping_JUVENILES_SNOUT.csv", row.names = FALSE)


#### JUVENILES - CLOACA ####
# Remove previous output variables
rm(var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, vector_start, translated_jumping_path, vector_start_end, vector_u_end, angle_to_end, axis_to_end, 
   rotmatrix_to_end, Rotated_trajectory_3D, mid_point, rotmatrix_H_Y, distance_jump, height_jump, frames_per_second, time_vector, 
   arc_jump, velocity_per_frame, average_velocity, maximum_velocity)

# Create the output variables
var_1 <- character(dim(data_matlab_juveniles$ToadData)[3])
var_2 <- character(dim(data_matlab_juveniles$ToadData)[3])
var_3 <- numeric(dim(data_matlab_juveniles$ToadData)[3])
var_4 <- numeric(dim(data_matlab_juveniles$ToadData)[3])
var_5 <- numeric(dim(data_matlab_juveniles$ToadData)[3])
var_6 <- numeric(dim(data_matlab_juveniles$ToadData)[3])
var_7 <- numeric(dim(data_matlab_juveniles$ToadData)[3])
var_8 <- numeric(dim(data_matlab_juveniles$ToadData)[3])

# Loop to get information for all specimens
for (i in 1:dim(data_matlab_juveniles$ToadData)[3]) {
  cane_toad_simple_code <- paste(x=c(as.numeric(data_matlab_juveniles$ToadData[, , i]$ID), as.character(data_matlab_juveniles$ToadData[, , i]$Sex[[1]])), collapse="")
  cane_toad_complete_code <- paste(x=c(as.character(data_matlab_juveniles$ToadData[, , i]$ID), data_matlab_juveniles$ToadData[, , i]$Sex[[1]], "_", as.character(data_matlab_juveniles$ToadData[, , i]$JumpID)), collapse="")
  jumping_path <- data_matlab_juveniles$ToadData[, , i]$cloacaPath
  
  # Translate the data to point of origin (0,0,0)
  
  vector_start <- as.numeric(jumping_path[1, ])
  
  translated_jumping_path <- sweep(jumping_path, 2, vector_start)
  translated_jumping_path
  
  
  # ROTATION: 
  
  #Choose vector of rotation (here I am constraining it to be z=0 across the whole trajectory, and y=0 for start and end)
  
  vector_start_end <- as.numeric(translated_jumping_path[nrow(translated_jumping_path), ])
  
  vector_u_end <- c(1, 0, 0) 
  
  
  #Angle of rot: 
  
  angle_to_end <- ShapeRotator:::angle_3D (vector_start_end, vector_u_end)
  
  # axis of rotation
  axis_to_end <- ShapeRotator:::cross_3D(vector_start_end, vector_u_end)
  
  # rotation matrix: 
  rotmatrix_to_end <- ShapeRotator:::rotmat_3D(axis_to_end, angle_to_end)
  
  # Obtain the rotation: 
  Rotated_trajectory_3D <- ShapeRotator:::rotveclist_3D(rotmatrix_to_end, translated_jumping_path) # Getting the error here
  
  # Correct the orientation on the y-axis in the middle of the trajectory just in case: 
  mid_point <- round(nrow(Rotated_trajectory_3D)/2) # getting the mid-point but still an integer
  
  rotmatrix_H_Y <- ShapeRotator:::rotmat_3D( c(1,0,0), ShapeRotator:::Y_to_Z_rot_angle_3D(Rotated_trajectory_3D[mid_point,]) ) #picked 20 as it should be positive but it does not matter
  Rotated_trajectory_3D <- ShapeRotator:::rotveclist_3D(rotmatrix_H_Y, Rotated_trajectory_3D)
  
  # Check orientation of the y-axis
  
  if (Rotated_trajectory_3D[mid_point,2] < 0) {
    Rotated_trajectory_3D[,2] <- Rotated_trajectory_3D[,2] * (-1)
  }
  
  # Plot the trajectory of the jump (please note that they 'smash' their face to the ground when they land, so y at landing will be <0)
  
  # plot(Rotated_trajectory_3D[,1], Rotated_trajectory_3D[,2])
  # 
  # title("Jumping trajectory of a cane toad", sub = cane_toad_complete_code,
  #       cex.main = 1.5,   font.main= 2, col.main= "black",
  #       cex.sub = 1.25, font.sub = 4, col.sub = "blue")
  
  file_pdf <- paste(x=c(cane_toad_simple_code, "_CLOACA.pdf"), collapse="")
  
  pdf(file_pdf, width=13.29, height=4.26)
  plot(Rotated_trajectory_3D[,1], Rotated_trajectory_3D[,2], col="darkgreen")
  
  title("Jumping trajectory - CLOACA", sub = cane_toad_simple_code,
        cex.main = 1.5,   font.main= 2, col.main= "black",
        cex.sub = 1.25, font.sub = 4, col.sub = "blue")
  
  dev.off()
  
  # plot(Rotated_trajectory_3D[,1], Rotated_trajectory_3D[,3]) # slight movement of the landmark across z-axis, but it does not affect the trajectory
  
  
  # Output data from jumping trajectory
  
  distance_jump <- max(Rotated_trajectory_3D[,1]) # in mm
  height_jump <- max(Rotated_trajectory_3D[,2]) # in mm 
  
  
  # Calculate time (in seconds) for each frame in the jumping trajectory
  time0 = 0
  frames_per_second = 240 # change it to 120 or 240 depending on the filming speed
  
  time_vector <- seq(time0, (nrow(Rotated_trajectory_3D)-1)/frames_per_second, 1/frames_per_second)
  length(time_vector)
  
  
  
  # Calculate Euclidian distance from each point in the trajectory (arc) to the next
  arc_jump <- vector(mode="numeric", length=nrow(Rotated_trajectory_3D)-1)
  
  for (j in 1:(nrow(Rotated_trajectory_3D)-1)) {
    arc_jump[j] <- sqrt((Rotated_trajectory_3D[j+1,1]-Rotated_trajectory_3D[j,1])^2 + (Rotated_trajectory_3D[j+1,2]-Rotated_trajectory_3D[j,2])^2 + 
                          (Rotated_trajectory_3D[j+1,3]-Rotated_trajectory_3D[j,3])^2)
  }  
  print(arc_jump)
  
  
  # Calculate velocity from Euclidian distances and time for each frame in the jump
  
  velocity_per_frame <- arc_jump/(1/frames_per_second) #mm per second
  
  average_velocity <- mean(velocity_per_frame) # AVERAGE VELOCITY
  
  maximum_velocity <- max(velocity_per_frame) # MAXIMUM VELOCITY
  
  
  # Calculate angles in jumping trajectory
  
  # Take-off angle
  first_take_off_y <- which(Rotated_trajectory_3D[,2] > max(Rotated_trajectory_3D[,2])*0.05)[1]
  
  last_take_off_y <- first_take_off_y + 5 # I put 5 to make sure it is the take-off only but it could be 10, 15 or something else
  
  x_diff_take_off <- Rotated_trajectory_3D[last_take_off_y,1] - Rotated_trajectory_3D[first_take_off_y,1]
  y_diff_take_off <- Rotated_trajectory_3D[last_take_off_y,2] - Rotated_trajectory_3D[first_take_off_y,2]
  
  theta_take_off <-  atan2(y_diff_take_off, x_diff_take_off) # in radians
  
  angle_take_off <- 360/(2* pi)*theta_take_off # in degrees
  
  
  # Landing angle
  last_landing_y <- which(Rotated_trajectory_3D[ 2:nrow(Rotated_trajectory_3D),2] == min(Rotated_trajectory_3D[round(nrow(Rotated_trajectory_3D)/2):nrow(Rotated_trajectory_3D),2]))
  first_landing_y <- last_landing_y - 10 # I've put 10 as the landing seems to take longer than take-off and often messier, We can change it though
  
  x_diff_landing <- Rotated_trajectory_3D[last_landing_y,1] - Rotated_trajectory_3D[first_landing_y,1]
  y_diff_landing <- Rotated_trajectory_3D[last_landing_y,2] - Rotated_trajectory_3D[first_landing_y,2]
  
  
  theta_landing <-  atan2(y_diff_landing, x_diff_landing) # in radians
  
  angle_landing <- 360/(2* pi)*theta_landing # in degrees
  
  
  # Save the output data
  write.csv(Rotated_trajectory_3D[,1:2], paste(x=c(cane_toad_simple_code, "_final_2D_trajectory_CLOACA.csv"), collapse=""))
  
  # IDs
  var_1[i] <- as.character(cane_toad_simple_code)
  var_2[i] <- as.character(cane_toad_complete_code)
  
  # Snout
  var_3[i] <- as.numeric(distance_jump)
  var_4[i] <- as.numeric(height_jump)
  var_5[i] <- as.numeric(average_velocity)
  var_6[i] <- as.numeric(maximum_velocity)
  var_7[i] <- as.numeric(angle_take_off)
  var_8[i] <- as.numeric(angle_landing)
  
}

# Make the final database with all the output variables from all specimens
output_data_jumping <- data.frame(var_1, var_2, var_3, var_4, var_5, var_6, var_7, var_8, stringsAsFactors=FALSE)
colnames(output_data_jumping) <- c("cane_toad_simple_code", "cane_toad_complete_code", "distance_jump_cloaca", "height_jump_cloaca", "average_velocity_cloaca", 
                                   "maximum_velocity_cloaca", "angle_take_off_cloaca", "angle_landing_cloaca")


# Save the final dataset with all the output data from all the toads (ONLY RUN THIS AT THE END)

write.csv(output_data_jumping, "output_data_jumping_JUVENILES_CLOACA.csv", row.names = FALSE)

