#### PACKAGES ####

library(geomorph)
library(geometry)
library(ggplot2)

#### VENTRAL ####
# importing and prepping data
data_ventral <- readland.tps("Masoko_VH_scaled_converted.TPS")
data_ventral <- two.d.array(data_ventral)
data_ventral_transpose <- t(data_ventral)
data_ventral_transpose[1:18]
dim(data_ventral_transpose)

# loop for calculating pore areas

pore1_left_areas <- numeric()
pore2_left_areas <- numeric()
pore3_left_areas <- numeric()
pore4_left_areas <- numeric()
pore5_left_areas <- numeric()
pore6_left_areas <- numeric()
pore7_left_areas <- numeric()
pore8_left_areas <- numeric()
pore9_left_areas <- numeric()
pore10_left_areas <- numeric()

pore1_right_areas <- numeric()
pore2_right_areas <- numeric()
pore3_right_areas <- numeric()
pore4_right_areas <- numeric()
pore5_right_areas <- numeric()
pore6_right_areas <- numeric()
pore7_right_areas <- numeric()
pore8_right_areas <- numeric()
pore9_right_areas <- numeric()
pore10_right_areas <- numeric()

for(i in 1:205){
  
  pore1_left <- as.data.frame(data_ventral_transpose[1:18, i])
  pore1_left_x_coords <- c(pore1_left[1,], pore1_left[3,], pore1_left[5,], pore1_left[7,], pore1_left[9,], pore1_left[11,], pore1_left[13,], pore1_left[15,], pore1_left[17,])
  pore1_left_y_coords <- c(pore1_left[2,], pore1_left[4,], pore1_left[6,], pore1_left[8,], pore1_left[10,], pore1_left[12,], pore1_left[14,], pore1_left[16,], pore1_left[18,])
  pore1_left <- cbind(pore1_left_x_coords, pore1_left_y_coords)
  pore1_left_areas[i] <- polyarea(pore1_left_x_coords, pore1_left_y_coords)
  
  pore2_left <- as.data.frame(data_ventral_transpose[21:38, i])
  pore2_left_x_coords <- c(pore2_left[1,], pore2_left[3,], pore2_left[5,], pore2_left[7,], pore2_left[9,], pore2_left[11,], pore2_left[13,], pore2_left[15,], pore2_left[17,])
  pore2_left_y_coords <- c(pore2_left[2,], pore2_left[4,], pore2_left[6,], pore2_left[8,], pore2_left[10,], pore2_left[12,], pore2_left[14,], pore2_left[16,], pore2_left[18,])
  pore2_left <- cbind(pore2_left_x_coords, pore2_left_y_coords)
  pore2_left_areas[i] <- polyarea(pore2_left_x_coords, pore2_left_y_coords)
  
  pore3_left <- as.data.frame(data_ventral_transpose[41:58, i])
  pore3_left_x_coords <- c(pore3_left[1,], pore3_left[3,], pore3_left[5,], pore3_left[7,], pore3_left[9,], pore3_left[11,], pore3_left[13,], pore3_left[15,], pore3_left[17,])
  pore3_left_y_coords <- c(pore3_left[2,], pore3_left[4,], pore3_left[6,], pore3_left[8,], pore3_left[10,], pore3_left[12,], pore3_left[14,], pore3_left[16,], pore3_left[18,])
  pore3_left <- cbind(pore3_left_x_coords, pore3_left_y_coords)
  pore3_left_areas[i] <- polyarea(pore3_left_x_coords, pore3_left_y_coords)
  
  pore4_left <- as.data.frame(data_ventral_transpose[61:78, i])
  pore4_left_x_coords <- c(pore4_left[1,], pore4_left[3,], pore4_left[5,], pore4_left[7,], pore4_left[9,], pore4_left[11,], pore4_left[13,], pore4_left[15,], pore4_left[17,])
  pore4_left_y_coords <- c(pore4_left[2,], pore4_left[4,], pore4_left[6,], pore4_left[8,], pore4_left[10,], pore4_left[12,], pore4_left[14,], pore4_left[16,], pore4_left[18,])
  pore4_left <- cbind(pore4_left_x_coords, pore4_left_y_coords)
  pore4_left_areas[i] <- polyarea(pore4_left_x_coords, pore4_left_y_coords)
  
  pore5_left <- as.data.frame(data_ventral_transpose[81:98, i])
  pore5_left_x_coords <- c(pore5_left[1,], pore5_left[3,], pore5_left[5,], pore5_left[7,], pore5_left[9,], pore5_left[11,], pore5_left[13,], pore5_left[15,], pore5_left[17,])
  pore5_left_y_coords <- c(pore5_left[2,], pore5_left[4,], pore5_left[6,], pore5_left[8,], pore5_left[10,], pore5_left[12,], pore5_left[14,], pore5_left[16,], pore5_left[18,])
  pore5_left <- cbind(pore5_left_x_coords, pore5_left_y_coords)
  pore5_left_areas[i] <- polyarea(pore5_left_x_coords, pore5_left_y_coords)
  
  pore6_left <- as.data.frame(data_ventral_transpose[101:118, i])
  pore6_left_x_coords <- c(pore6_left[1,], pore6_left[3,], pore6_left[5,], pore6_left[7,], pore6_left[9,], pore6_left[11,], pore6_left[13,], pore6_left[15,], pore6_left[17,])
  pore6_left_y_coords <- c(pore6_left[2,], pore6_left[4,], pore6_left[6,], pore6_left[8,], pore6_left[10,], pore6_left[12,], pore6_left[14,], pore6_left[16,], pore6_left[18,])
  pore6_left <- cbind(pore6_left_x_coords, pore6_left_y_coords)
  pore6_left_areas[i] <- polyarea(pore6_left_x_coords, pore6_left_y_coords)
  
  pore7_left <- as.data.frame(data_ventral_transpose[121:138, i])
  pore7_left_x_coords <- c(pore7_left[1,], pore7_left[3,], pore7_left[5,], pore7_left[7,], pore7_left[9,], pore7_left[11,], pore7_left[13,], pore7_left[15,], pore7_left[17,])
  pore7_left_y_coords <- c(pore7_left[2,], pore7_left[4,], pore7_left[6,], pore7_left[8,], pore7_left[10,], pore7_left[12,], pore7_left[14,], pore7_left[16,], pore7_left[18,])
  pore7_left <- cbind(pore7_left_x_coords, pore7_left_y_coords)
  pore7_left_areas[i] <- polyarea(pore7_left_x_coords, pore7_left_y_coords)
  
  pore8_left <- as.data.frame(data_ventral_transpose[141:158, i])
  pore8_left_x_coords <- c(pore8_left[1,], pore8_left[3,], pore8_left[5,], pore8_left[7,], pore8_left[9,], pore8_left[11,], pore8_left[13,], pore8_left[15,], pore8_left[17,])
  pore8_left_y_coords <- c(pore8_left[2,], pore8_left[4,], pore8_left[6,], pore8_left[8,], pore8_left[10,], pore8_left[12,], pore8_left[14,], pore8_left[16,], pore8_left[18,])
  pore8_left <- cbind(pore8_left_x_coords, pore8_left_y_coords)
  pore8_left_areas[i] <- polyarea(pore8_left_x_coords, pore8_left_y_coords)
  
  pore9_left <- as.data.frame(data_ventral_transpose[161:178, i])
  pore9_left_x_coords <- c(pore9_left[1,], pore9_left[3,], pore9_left[5,], pore9_left[7,], pore9_left[9,], pore9_left[11,], pore9_left[13,], pore9_left[15,], pore9_left[17,])
  pore9_left_y_coords <- c(pore9_left[2,], pore9_left[4,], pore9_left[6,], pore9_left[8,], pore9_left[10,], pore9_left[12,], pore9_left[14,], pore9_left[16,], pore9_left[18,])
  pore9_left <- cbind(pore9_left_x_coords, pore9_left_y_coords)
  pore9_left_areas[i] <- polyarea(pore9_left_x_coords, pore9_left_y_coords)
  
  pore10_left <- as.data.frame(data_ventral_transpose[181:198, i])
  pore10_left_x_coords <- c(pore10_left[1,], pore10_left[3,], pore10_left[5,], pore10_left[7,], pore10_left[9,], pore10_left[11,], pore10_left[13,], pore10_left[15,], pore10_left[17,])
  pore10_left_y_coords <- c(pore10_left[2,], pore10_left[4,], pore10_left[6,], pore10_left[8,], pore10_left[10,], pore10_left[12,], pore10_left[14,], pore10_left[16,], pore10_left[18,])
  pore10_left <- cbind(pore10_left_x_coords, pore10_left_y_coords)
  pore10_left_areas[i] <- polyarea(pore10_left_x_coords, pore10_left_y_coords)
  
  
  
  pore1_right <- as.data.frame(data_ventral_transpose[201:218, i])
  pore1_right_x_coords <- c(pore1_right[1,], pore1_right[3,], pore1_right[5,], pore1_right[7,], pore1_right[9,], pore1_right[11,], pore1_right[13,], pore1_right[15,], pore1_right[17,])
  pore1_right_y_coords <- c(pore1_right[2,], pore1_right[4,], pore1_right[6,], pore1_right[8,], pore1_right[10,], pore1_right[12,], pore1_right[14,], pore1_right[16,], pore1_right[18,])
  pore1_right <- cbind(pore1_right_x_coords, pore1_right_y_coords)
  pore1_right_areas[i] <- polyarea(pore1_right_x_coords, pore1_right_y_coords)
  
  pore2_right <- as.data.frame(data_ventral_transpose[221:238, i])
  pore2_right_x_coords <- c(pore2_right[1,], pore2_right[3,], pore2_right[5,], pore2_right[7,], pore2_right[9,], pore2_right[11,], pore2_right[13,], pore2_right[15,], pore2_right[17,])
  pore2_right_y_coords <- c(pore2_right[2,], pore2_right[4,], pore2_right[6,], pore2_right[8,], pore2_right[10,], pore2_right[12,], pore2_right[14,], pore2_right[16,], pore2_right[18,])
  pore2_right <- cbind(pore2_right_x_coords, pore2_right_y_coords)
  pore2_right_areas[i] <- polyarea(pore2_right_x_coords, pore2_right_y_coords)
  
  pore3_right <- as.data.frame(data_ventral_transpose[241:258, i])
  pore3_right_x_coords <- c(pore3_right[1,], pore3_right[3,], pore3_right[5,], pore3_right[7,], pore3_right[9,], pore3_right[11,], pore3_right[13,], pore3_right[15,], pore3_right[17,])
  pore3_right_y_coords <- c(pore3_right[2,], pore3_right[4,], pore3_right[6,], pore3_right[8,], pore3_right[10,], pore3_right[12,], pore3_right[14,], pore3_right[16,], pore3_right[18,])
  pore3_right <- cbind(pore3_right_x_coords, pore3_right_y_coords)
  pore3_right_areas[i] <- polyarea(pore3_right_x_coords, pore3_right_y_coords)
  
  pore4_right <- as.data.frame(data_ventral_transpose[261:278, i])
  pore4_right_x_coords <- c(pore4_right[1,], pore4_right[3,], pore4_right[5,], pore4_right[7,], pore4_right[9,], pore4_right[11,], pore4_right[13,], pore4_right[15,], pore4_right[17,])
  pore4_right_y_coords <- c(pore4_right[2,], pore4_right[4,], pore4_right[6,], pore4_right[8,], pore4_right[10,], pore4_right[12,], pore4_right[14,], pore4_right[16,], pore4_right[18,])
  pore4_right <- cbind(pore4_right_x_coords, pore4_right_y_coords)
  pore4_right_areas[i] <- polyarea(pore4_right_x_coords, pore4_right_y_coords)
  
  pore5_right <- as.data.frame(data_ventral_transpose[281:298, i])
  pore5_right_x_coords <- c(pore5_right[1,], pore5_right[3,], pore5_right[5,], pore5_right[7,], pore5_right[9,], pore5_right[11,], pore5_right[13,], pore5_right[15,], pore5_right[17,])
  pore5_right_y_coords <- c(pore5_right[2,], pore5_right[4,], pore5_right[6,], pore5_right[8,], pore5_right[10,], pore5_right[12,], pore5_right[14,], pore5_right[16,], pore5_right[18,])
  pore5_right <- cbind(pore5_right_x_coords, pore5_right_y_coords)
  pore5_right_areas[i] <- polyarea(pore5_right_x_coords, pore5_right_y_coords)
  
  pore6_right <- as.data.frame(data_ventral_transpose[301:318, i])
  pore6_right_x_coords <- c(pore6_right[1,], pore6_right[3,], pore6_right[5,], pore6_right[7,], pore6_right[9,], pore6_right[11,], pore6_right[13,], pore6_right[15,], pore6_right[17,])
  pore6_right_y_coords <- c(pore6_right[2,], pore6_right[4,], pore6_right[6,], pore6_right[8,], pore6_right[10,], pore6_right[12,], pore6_right[14,], pore6_right[16,], pore6_right[18,])
  pore6_right <- cbind(pore6_right_x_coords, pore6_right_y_coords)
  pore6_right_areas[i] <- polyarea(pore6_right_x_coords, pore6_right_y_coords)
  
  pore7_right <- as.data.frame(data_ventral_transpose[321:338, i])
  pore7_right_x_coords <- c(pore7_right[1,], pore7_right[3,], pore7_right[5,], pore7_right[7,], pore7_right[9,], pore7_right[11,], pore7_right[13,], pore7_right[15,], pore7_right[17,])
  pore7_right_y_coords <- c(pore7_right[2,], pore7_right[4,], pore7_right[6,], pore7_right[8,], pore7_right[10,], pore7_right[12,], pore7_right[14,], pore7_right[16,], pore7_right[18,])
  pore7_right <- cbind(pore7_right_x_coords, pore7_right_y_coords)
  pore7_right_areas[i] <- polyarea(pore7_right_x_coords, pore7_right_y_coords)
  
  pore8_right <- as.data.frame(data_ventral_transpose[341:358, i])
  pore8_right_x_coords <- c(pore8_right[1,], pore8_right[3,], pore8_right[5,], pore8_right[7,], pore8_right[9,], pore8_right[11,], pore8_right[13,], pore8_right[15,], pore8_right[17,])
  pore8_right_y_coords <- c(pore8_right[2,], pore8_right[4,], pore8_right[6,], pore8_right[8,], pore8_right[10,], pore8_right[12,], pore8_right[14,], pore8_right[16,], pore8_right[18,])
  pore8_right <- cbind(pore8_right_x_coords, pore8_right_y_coords)
  pore8_right_areas[i] <- polyarea(pore8_right_x_coords, pore8_right_y_coords)
  
  pore9_right <- as.data.frame(data_ventral_transpose[361:378, i])
  pore9_right_x_coords <- c(pore9_right[1,], pore9_right[3,], pore9_right[5,], pore9_right[7,], pore9_right[9,], pore9_right[11,], pore9_right[13,], pore9_right[15,], pore9_right[17,])
  pore9_right_y_coords <- c(pore9_right[2,], pore9_right[4,], pore9_right[6,], pore9_right[8,], pore9_right[10,], pore9_right[12,], pore9_right[14,], pore9_right[16,], pore9_right[18,])
  pore9_right <- cbind(pore9_right_x_coords, pore9_right_y_coords)
  pore9_right_areas[i] <- polyarea(pore9_right_x_coords, pore9_right_y_coords)
  
  pore10_right <- as.data.frame(data_ventral_transpose[381:398, i])
  pore10_right_x_coords <- c(pore10_right[1,], pore10_right[3,], pore10_right[5,], pore10_right[7,], pore10_right[9,], pore10_right[11,], pore10_right[13,], pore10_right[15,], pore10_right[17,])
  pore10_right_y_coords <- c(pore10_right[2,], pore10_right[4,], pore10_right[6,], pore10_right[8,], pore10_right[10,], pore10_right[12,], pore10_right[14,], pore10_right[16,], pore10_right[18,])
  pore10_right <- cbind(pore10_right_x_coords, pore10_right_y_coords)
  pore10_right_areas[i] <- polyarea(pore10_right_x_coords, pore10_right_y_coords)
}

#### INFRAORBITAL LEFT ####

data_ioc_l <- readland.tps("Masoko_IOC_L_scaled_converted.TPS")


data_ioc_l <- two.d.array(data_ioc_l)
data_ioc_l_transpose <- t(data_ioc_l)

data_ioc_l_transpose[1:18]

dim(data_ioc_l_transpose)

# loop for calculating pore areas

pore1_ioc_l_areas <- numeric()
pore2_ioc_l_areas <- numeric()
pore3_ioc_l_areas <- numeric()

for(i in 1:205){
  
  pore1_left <- as.data.frame(data_ioc_l_transpose[1:18, i])
  pore1_left_x_coords <- c(pore1_left[1,], pore1_left[3,], pore1_left[5,], pore1_left[7,], pore1_left[9,], pore1_left[11,], pore1_left[13,], pore1_left[15,], pore1_left[17,])
  pore1_left_y_coords <- c(pore1_left[2,], pore1_left[4,], pore1_left[6,], pore1_left[8,], pore1_left[10,], pore1_left[12,], pore1_left[14,], pore1_left[16,], pore1_left[18,])
  pore1_left <- cbind(pore1_left_x_coords, pore1_left_y_coords)
  pore1_ioc_l_areas[i] <- polyarea(pore1_left_x_coords, pore1_left_y_coords)
  
  pore2_left <- as.data.frame(data_ioc_l_transpose[21:38, i])
  pore2_left_x_coords <- c(pore2_left[1,], pore2_left[3,], pore2_left[5,], pore2_left[7,], pore2_left[9,], pore2_left[11,], pore2_left[13,], pore2_left[15,], pore2_left[17,])
  pore2_left_y_coords <- c(pore2_left[2,], pore2_left[4,], pore2_left[6,], pore2_left[8,], pore2_left[10,], pore2_left[12,], pore2_left[14,], pore2_left[16,], pore2_left[18,])
  pore2_left <- cbind(pore2_left_x_coords, pore2_left_y_coords)
  pore2_ioc_l_areas[i] <- polyarea(pore2_left_x_coords, pore2_left_y_coords)
  
  pore3_left <- as.data.frame(data_ioc_l_transpose[41:58, i])
  pore3_left_x_coords <- c(pore3_left[1,], pore3_left[3,], pore3_left[5,], pore3_left[7,], pore3_left[9,], pore3_left[11,], pore3_left[13,], pore3_left[15,], pore3_left[17,])
  pore3_left_y_coords <- c(pore3_left[2,], pore3_left[4,], pore3_left[6,], pore3_left[8,], pore3_left[10,], pore3_left[12,], pore3_left[14,], pore3_left[16,], pore3_left[18,])
  pore3_left <- cbind(pore3_left_x_coords, pore3_left_y_coords)
  pore3_ioc_l_areas[i] <- polyarea(pore3_left_x_coords, pore3_left_y_coords)
  
}

#### INFRAORBITAL RIGHT ####

data_ioc_r <- readland.tps("Masoko_IOC_R_scaled_converted.TPS")


data_ioc_r <- two.d.array(data_ioc_r)
data_ioc_r_transpose <- t(data_ioc_r)

data_ioc_r_transpose[1:18]

dim(data_ioc_r_transpose)

# loop for calculating pore areas

pore1_ioc_r_areas <- numeric()
pore2_ioc_r_areas <- numeric()
pore3_ioc_r_areas <- numeric()

for(i in 1:205){
  
  pore1_left <- as.data.frame(data_ioc_r_transpose[1:18, i])
  pore1_left_x_coords <- c(pore1_left[1,], pore1_left[3,], pore1_left[5,], pore1_left[7,], pore1_left[9,], pore1_left[11,], pore1_left[13,], pore1_left[15,], pore1_left[17,])
  pore1_left_y_coords <- c(pore1_left[2,], pore1_left[4,], pore1_left[6,], pore1_left[8,], pore1_left[10,], pore1_left[12,], pore1_left[14,], pore1_left[16,], pore1_left[18,])
  pore1_left <- cbind(pore1_left_x_coords, pore1_left_y_coords)
  pore1_ioc_r_areas[i] <- polyarea(pore1_left_x_coords, pore1_left_y_coords)
  
  pore2_left <- as.data.frame(data_ioc_r_transpose[21:38, i])
  pore2_left_x_coords <- c(pore2_left[1,], pore2_left[3,], pore2_left[5,], pore2_left[7,], pore2_left[9,], pore2_left[11,], pore2_left[13,], pore2_left[15,], pore2_left[17,])
  pore2_left_y_coords <- c(pore2_left[2,], pore2_left[4,], pore2_left[6,], pore2_left[8,], pore2_left[10,], pore2_left[12,], pore2_left[14,], pore2_left[16,], pore2_left[18,])
  pore2_left <- cbind(pore2_left_x_coords, pore2_left_y_coords)
  pore2_ioc_r_areas[i] <- polyarea(pore2_left_x_coords, pore2_left_y_coords)
  
  pore3_left <- as.data.frame(data_ioc_r_transpose[41:58, i])
  pore3_left_x_coords <- c(pore3_left[1,], pore3_left[3,], pore3_left[5,], pore3_left[7,], pore3_left[9,], pore3_left[11,], pore3_left[13,], pore3_left[15,], pore3_left[17,])
  pore3_left_y_coords <- c(pore3_left[2,], pore3_left[4,], pore3_left[6,], pore3_left[8,], pore3_left[10,], pore3_left[12,], pore3_left[14,], pore3_left[16,], pore3_left[18,])
  pore3_left <- cbind(pore3_left_x_coords, pore3_left_y_coords)
  pore3_ioc_r_areas[i] <- polyarea(pore3_left_x_coords, pore3_left_y_coords)
  
}

#### LATERAL ####

data_lateral <- readland.tps("Masoko_LH_scaled_converted.TPS")

data_lateral <- two.d.array(data_lateral)
data_lateral_transpose <- t(data_lateral)

data_lateral_transpose[1:18]

dim(data_lateral_transpose)

# loop

pore1_lateral_areas <- numeric()
pore2_lateral_areas <- numeric()
pore3_lateral_areas <- numeric()
pore4_lateral_areas <- numeric()
pore5_lateral_areas <- numeric()

for(i in 1:205){
  
  pore1 <- as.data.frame(data_lateral_transpose[1:18, i])
  pore1_x_coords <- c(pore1[1,], pore1[3,], pore1[5,], pore1[7,], pore1[9,], pore1[11,], pore1[13,], pore1[15,], pore1[17,])
  pore1_y_coords <- c(pore1[2,], pore1[4,], pore1[6,], pore1[8,], pore1[10,], pore1[12,], pore1[14,], pore1[16,], pore1[18,])
  pore1 <- cbind(pore1_x_coords, pore1_y_coords)
  pore1_lateral_areas[i] <- polyarea(pore1_x_coords, pore1_y_coords)
  
  pore2 <- as.data.frame(data_lateral_transpose[21:38, i])
  pore2_x_coords <- c(pore2[1,], pore2[3,], pore2[5,], pore2[7,], pore2[9,], pore2[11,], pore2[13,], pore2[15,], pore2[17,])
  pore2_y_coords <- c(pore2[2,], pore2[4,], pore2[6,], pore2[8,], pore2[10,], pore2[12,], pore2[14,], pore2[16,], pore2[18,])
  pore2 <- cbind(pore2_x_coords, pore2_y_coords)
  pore2_lateral_areas[i] <- polyarea(pore2_x_coords, pore2_y_coords)
  
  pore3 <- as.data.frame(data_lateral_transpose[41:58, i])
  pore3_x_coords <- c(pore3[1,], pore3[3,], pore3[5,], pore3[7,], pore3[9,], pore3[11,], pore3[13,], pore3[15,], pore3[17,])
  pore3_y_coords <- c(pore3[2,], pore3[4,], pore3[6,], pore3[8,], pore3[10,], pore3[12,], pore3[14,], pore3[16,], pore3[18,])
  pore3 <- cbind(pore3_x_coords, pore3_y_coords)
  pore3_lateral_areas[i] <- polyarea(pore3_x_coords, pore3_y_coords)
  
  pore4 <- as.data.frame(data_lateral_transpose[61:78, i])
  pore4_x_coords <- c(pore4[1,], pore4[3,], pore4[5,], pore4[7,], pore4[9,], pore4[11,], pore4[13,], pore4[15,], pore4[17,])
  pore4_y_coords <- c(pore4[2,], pore4[4,], pore4[6,], pore4[8,], pore4[10,], pore4[12,], pore4[14,], pore4[16,], pore4[18,])
  pore4 <- cbind(pore4_x_coords, pore4_y_coords)
  pore4_lateral_areas[i] <- polyarea(pore4_x_coords, pore4_y_coords)
  
  pore5 <- as.data.frame(data_lateral_transpose[81:98, i])
  pore5_x_coords <- c(pore5[1,], pore5[3,], pore5[5,], pore5[7,], pore5[9,], pore5[11,], pore5[13,], pore5[15,], pore5[17,])
  pore5_y_coords <- c(pore5[2,], pore5[4,], pore5[6,], pore5[8,], pore5[10,], pore5[12,], pore5[14,], pore5[16,], pore5[18,])
  pore5 <- cbind(pore5_x_coords, pore5_y_coords)
  pore5_lateral_areas[i] <- polyarea(pore5_x_coords, pore5_y_coords)
  
}

#### BINDING INTO DATAFRAME ####

#binding into dataframe

classifiers <- read.csv("classifiers.csv")
pore_areas <- as.numeric()
pore_areas <- cbind(classifiers, pore1_left_areas, pore2_left_areas,
                    pore3_left_areas, pore4_left_areas, pore5_left_areas,
                    pore6_left_areas, pore7_left_areas, pore8_left_areas,
                    pore9_left_areas, pore10_left_areas,pore1_right_areas, 
                    pore2_right_areas, pore3_right_areas, pore4_right_areas, 
                    pore5_right_areas, pore6_right_areas, pore7_right_areas, 
                    pore8_right_areas, pore9_right_areas, pore10_right_areas,
                    pore1_ioc_l_areas, pore2_ioc_l_areas, pore3_ioc_l_areas,
                    pore1_ioc_r_areas, pore2_ioc_r_areas, pore3_ioc_r_areas,
                    pore1_lateral_areas, pore2_lateral_areas, pore3_lateral_areas,
                    pore4_lateral_areas, pore5_lateral_areas)


#### MEAN PORE AREAS FOR EACH FOCAL CANAL ####

mean_mandibular_left_pore_area <- as.numeric()
mean_mandibular_right_pore_area <- as.numeric()
mean_mandibular_pore_area <- as.numeric()
mean_preopercular_left_pore_area <-as.numeric()
mean_preopercular_right_pore_area <- as.numeric()
mean_preopercular_pore_area <- as.numeric()
mean_orbital_left_area <- as.numeric()
mean_orbital_right_area <- as.numeric()
mean_orbital_pore_area <- as.numeric()
mean_otic_pore_area <- as.numeric()

for(i in 1:205){
  mean_mandibular_left_pore_area[i] <- (sum(pore_areas$pore1_left_areas[i], pore_areas$pore2_left_areas[i], pore_areas$pore3_left_areas[i], pore_areas$pore4_left_areas[i], pore_areas$pore5_left_areas[i]))/5
  mean_mandibular_right_pore_area[i] <- (sum(pore_areas$pore1_right_areas[i], pore_areas$pore2_right_areas[i], pore_areas$pore3_right_areas[i], pore_areas$pore4_right_areas[i], pore_areas$pore5_right_areas[i]))/5
  mean_mandibular_pore_area[i] <- (sum(mean_mandibular_left_pore_area[i], mean_mandibular_right_pore_area[i]))/2
  
  mean_preopercular_left_pore_area[i] <- (sum(pore_areas$pore7_left_areas[i], pore_areas$pore8_left_areas[i], pore_areas$pore9_left_areas[i], pore_areas$pore10_left_areas[i]))/4
  mean_preopercular_right_pore_area[i] <- (sum(pore_areas$pore7_right_areas[i], pore_areas$pore8_right_areas[i], pore_areas$pore9_right_areas[i], pore_areas$pore10_right_areas[i]))/4
  mean_preopercular_pore_area[i] <- (sum(mean_preopercular_left_pore_area[i], mean_preopercular_right_pore_area[i]))/2
  
  mean_orbital_left_area[i] <- (sum(pore_areas$pore1_ioc_l_areas[i], pore_areas$pore2_ioc_l_areas[i], pore_areas$pore3_ioc_l_areas[i]))/3
  mean_orbital_right_area[i] <- (sum(pore_areas$pore1_ioc_r_areas[i], pore_areas$pore2_ioc_r_areas[i], pore_areas$pore3_ioc_r_areas[i]))/3
  mean_orbital_pore_area[i] <- (sum(mean_orbital_left_area[i], mean_orbital_right_area[i]))/2
  
  mean_otic_pore_area[i] <- (sum(pore_areas$pore3_lateral_areas[i], pore_areas$pore4_lateral_areas[i]))/2
}

pore_areas <- cbind(pore_areas, 
                    mean_mandibular_left_pore_area, 
                    mean_mandibular_right_pore_area,
                    mean_mandibular_pore_area,
                    mean_preopercular_left_pore_area,
                    mean_preopercular_right_pore_area,
                    mean_preopercular_pore_area,
                    mean_orbital_left_area,
                    mean_orbital_right_area,
                    mean_orbital_pore_area,
                    mean_otic_pore_area)

# output

write.csv(pore_areas, file="pore_areas_masoko.csv")