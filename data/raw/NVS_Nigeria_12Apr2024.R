setwd("/Users/tybeal/Library/CloudStorage/GoogleDrive-tbeal@ucdavis.edu/My Drive/GAIN/NVS")
d <- read.csv("Data/FCD_Nigeria_12Apr2024.csv")

# d <- d[d$Country != "Nigeria", ]

# # Get duplicate rows based on Food column
# duplicate_rows <- duplicated(d$Food)
# 
# # Remove duplicate rows 
# d <- d[!duplicate_rows,]

# Drop oils and fats
d <- d[d$Food_group != "Oils and fats", ]

RNI <- read.csv("Data/RNI.csv")
# Reshape RNI to column vector
d2 <- as.vector(t(RNI))

# Scale milk powder to same energy density as liquid milk
d[d$Food_long == "Milk (cow), dry, whole, without added vitamin D" | d$Food_long == "Milk (cow), dry, whole, without added vitamin A and vitamin D", 5:44] <- sweep(d[, 5:44], 1, 496/61, FUN = "/")[d$Food_long == "Milk (cow), dry, whole, without added vitamin D", ]

d[d$Food_long == "Milk, dry, nonfat, regular, without added vitamin A and vitamin D", 5:44] <- sweep(d[, 5:44], 1, 362/35, FUN = "/")[d$Food_long == "Milk, dry, nonfat, regular, without added vitamin A and vitamin D", ]


#######################################################################
# Calculate V_E

# Extract vitamins and energy 
v_E <- d[,c(5, 12:22)]

# Adjust to per 300 kcal
v_E[,2:12] <- sweep(v_E[,2:12], 1, 300/v_E$Energy_kcal, FUN="*")

# Drop energy
v_E <- v_E[, -1]

# Calculate proportion of RNI
prop_RNI <- sweep(v_E, 2, d2[1:11], FUN="/")

# Cap proportions at 1  
prop_RNI[prop_RNI > 1] <- 1

# Calculate average capped proportion
d$V_E <- 100*rowMeans(prop_RNI, na.rm=TRUE)

# Normalize to 1-100 scale 
d$V_E <- (d$V_E - min(d$V_E)) * (100 - 1) / (max(d$V_E) - min(d$V_E)) + 1

# Clip normalized values to 1-100
d$V_E <- pmin(100, pmax(1, d$V_E))


# Calculate V_M

# Extract vitamins 
v_M <- d[,c(12:22)]

# Adjust to per 231 g
v_M <- sweep(v_M, 1, 2.31, FUN="*")

# Calculate proportion of RNI
prop_RNI <- sweep(v_M, 2, d2[1:11], FUN="/")

# Cap proportions at 1  
prop_RNI[prop_RNI > 1] <- 1

# Calculate average capped proportion
d$V_M <- 100*rowMeans(prop_RNI, na.rm=TRUE)

# Normalize to 1-100 scale 
d$V_M <- (d$V_M - min(d$V_M)) * (100 - 1) / (max(d$V_M) - min(d$V_M)) + 1

# Clip normalized values to 1-100
d$V_M <- pmin(100, pmax(1, d$V_M))

d$V <- d$V_E + d$V_M

# Normalize to 1-100 scale 
d$V <- (d$V - min(d$V)) * (100 - 1) / (max(d$V) - min(d$V)) + 1

# Clip normalized values to 1-100
d$V <- pmin(100, pmax(1, d$V))

# For liquids and semi-liquid dairy, using energy only as the reference unit.
d$V <- ifelse(d$Liquids == 1, d$V_E, d$V)

#######################################################################
# Calculate M_E

# Extract minerals and energy 
m_E <- d[,c(5, 23:27, 47:48)]

# Adjust to per 300 kcal
m_E[,2:6] <- sweep(m_E[,2:6], 1, 300/m_E$Energy_kcal, FUN="*")

# Drop energy
m_E <- m_E[, -1]

# Calculate proportion of RNI for Ca, K, and Mg
prop_RNI <- sweep(m_E[, 3:5], 2, d2[19:21], FUN="/")

# Calculate proportion fo RNI for Iron, adjusting for bioavailability
prop_RNI$Iron_mg <- m_E$Iron_mg/ifelse(m_E$Iron_Abs == 0.2, d2[12], 
                       ifelse(m_E$Iron_Abs == 0.15, d2[13], 
                              d2[14]))

# Calculate proportion fo RNI for Zinc, adjusting for bioavailability
prop_RNI$Zinc_mg <- m_E$Zinc_mg/ifelse(m_E$Zinc_Abs == 0.44, d2[15], 
                                     ifelse(m_E$Zinc_Abs == 0.35, d2[16],
                                            ifelse(m_E$Zinc_Abs == 0.3, d2[17], 
                                            d2[18])))

# Cap proportions at 1  
prop_RNI[prop_RNI > 1] <- 1

# Calculate average capped proportion
d$M_E <- 100*rowMeans(prop_RNI, na.rm=TRUE)

# Normalize to 1-100 scale 
d$M_E <- (d$M_E - min(d$M_E)) * (100 - 1) / (max(d$M_E) - min(d$M_E)) + 1

# Clip normalized values to 1-100
d$M_E <- pmin(100, pmax(1, d$M_E))


# Calculate M_M

# Extract minerals 
m_M <- d[,c(23:27, 47:48)]

# Adjust to per 231 g
m_M <- sweep(m_M, 1, 2.31, FUN="*")

# Calculate proportion of RNI for Ca, K, and Mg
prop_RNI <- sweep(m_M[, 3:5], 2, d2[19:21], FUN="/")

# Calculate proportion fo RNI for Iron, adjusting for bioavailability
prop_RNI$Iron_mg <- m_M$Iron_mg/ifelse(m_M$Iron_Abs == 0.2, d2[12], 
                                       ifelse(m_M$Iron_Abs == 0.15, d2[13], 
                                              d2[14]))

# Calculate proportion fo RNI for Zinc, adjusting for bioavailability
prop_RNI$Zinc_mg <- m_M$Zinc_mg/ifelse(m_M$Zinc_Abs == 0.44, d2[15], 
                                       ifelse(m_M$Zinc_Abs == 0.35, d2[16],
                                              ifelse(m_M$Zinc_Abs == 0.3, d2[17], 
                                                     d2[18])))

# Cap proportions at 1  
prop_RNI[prop_RNI > 1] <- 1

# Calculate average capped proportion
d$M_M <- 100*rowMeans(prop_RNI, na.rm=TRUE)

# Normalize to 1-100 scale 
d$M_M <- (d$M_M - min(d$M_M)) * (100 - 1) / (max(d$M_M) - min(d$M_M)) + 1

# Clip normalized values to 1-100
d$M_M <- pmin(100, pmax(1, d$M_M))

d$M <- d$M_E + d$M_M

# Normalize to 1-100 scale 
d$M <- (d$M - min(d$M)) * (100 - 1) / (max(d$M) - min(d$M)) + 1

# Clip normalized values to 1-100
d$M <- pmin(100, pmax(1, d$M))

# For liquids and semi-liquid dairy, using energy only as the reference unit.
d$M <- ifelse(d$Liquids == 1, d$M_E, d$M)


#######################################################################
# Calculate N3_E

# Extract N3 and energy 
n3_E <- d[,c(5, 29:32)]

# Adjust to per 300 kcal
n3_E[,2:5] <- sweep(n3_E[,2:5], 1, 300/n3_E$Energy_kcal, FUN="*")

n3_E$LCN3 <- n3_E$DHA_g + n3_E$EPA_g + n3_E$DPA_g

d$N3_E <- pmax(n3_E$LCN3/0.25, n3_E$ALA_g/1.24)

# Normalize to 1-100 scale 
d$N3_E <- (d$N3_E - min(d$N3_E)) * (100 - 1) / (max(d$N3_E) - min(d$N3_E)) + 1

# Clip normalized values to 1-100
d$N3_E <- pmin(100, pmax(1, d$N3_E))


# Calculate N3_M

# Extract N3
n3_M <- d[,c(29:32)]

# Adjust to per 231 g
n3_M <- sweep(n3_M, 1, 2.31, FUN="*")

n3_M$LCN3 <- n3_M$DHA_g + n3_M$EPA_g + n3_M$DPA_g

d$N3_M <- pmax(n3_M$LCN3/0.25, n3_M$ALA_g/1.24)

# Normalize to 1-100 scale 
d$N3_M <- (d$N3_M - min(d$N3_M)) * (100 - 1) / (max(d$N3_M) - min(d$N3_M)) + 1

# Clip normalized values to 1-100
d$N3_M <- pmin(100, pmax(1, d$N3_M))


d$N3 <- d$N3_E + d$N3_M

# Normalize to 1-100 scale 
d$N3 <- (d$N3 - min(d$N3)) * (100 - 1) / (max(d$N3) - min(d$N3)) + 1

# Clip normalized values to 1-100
d$N3 <- pmin(100, pmax(1, d$N3))

# For liquids and semi-liquid dairy, using energy only as the reference unit.
d$N3 <- ifelse(d$Liquids == 1, d$N3_E, d$N3)

#######################################################################
# Calculate EAA_E

# Extract EAAs, DIASS, and energy 
eaa_E <- d[,c(5, 44:45)]

# Adjust to per 300 kcal
eaa_E$EAA_quantity <- eaa_E$EAA_quantity*300/eaa_E$Energy_kcal

d$eaa_E <- eaa_E$EAA_quantity

# Normalize to 1-100 scale 
d$eaa_E <- (d$eaa_E - min(d$eaa_E)) * (100 - 1) / (max(d$eaa_E) - min(d$eaa_E)) + 1

# Clip normalized values to 1-100
d$eaa_E <- pmin(100, pmax(1, d$eaa_E))


# Calculate EAA_M

# Extract EAAs, DIASS, and energy 
eaa_M <- d[,c(44:45)]

# Adjust to per 231 g
eaa_M$EAA_quantity <- eaa_M$EAA_quantity*2.31

d$eaa_M <- eaa_M$EAA_quantity

# Normalize to 1-100 scale 
d$eaa_M <- (d$eaa_M - min(d$eaa_M)) * (100 - 1) / (max(d$eaa_M) - min(d$eaa_M)) + 1

# Clip normalized values to 1-100
d$eaa_M <- pmin(100, pmax(1, d$eaa_M))


d$eaa <- d$eaa_E + d$eaa_M

# Normalize to 1-100 scale 
d$eaa <- (d$eaa - min(d$eaa)) * (100 - 1) / (max(d$eaa) - min(d$eaa)) + 1

# Clip normalized values to 1-100
d$eaa <- pmin(100, pmax(1, d$eaa))

# For liquids and semi-liquid dairy, using energy only as the reference unit.
d$eaa <- ifelse(d$Liquids == 1, d$eaa_E, d$eaa)

# Normalize to 1-100 scale 
d$DIAAS <- (d$DIAAS - min(d$DIAAS)) * (100 - 1) / (max(d$DIAAS) - min(d$DIAAS)) + 1

# Clip normalized values to 1-100
d$DIAAS <- pmin(100, pmax(1, d$DIAAS))

d$EAA <- d$eaa + d$DIAAS

# Normalize to 1-100 scale 
d$EAA <- (d$EAA - min(d$EAA)) * (100 - 1) / (max(d$EAA) - min(d$EAA)) + 1

# Clip normalized values to 1-100
d$EAA <- pmin(100, pmax(1, d$EAA))


#######################################################################
# Calculate F_E

# Extract fiber and energy 
f_E <- d[,c(5, 7)]

# Adjust to per 300 kcal
f_E$Fiber <- f_E$Fiber*300/f_E$Energy_kcal

d$F_E <- f_E$Fiber

# Normalize to 1-100 scale 
d$F_E <- (d$F_E - min(d$F_E)) * (100 - 1) / (max(d$F_E) - min(d$F_E)) + 1

# Clip normalized values to 1-100
d$F_E <- pmin(100, pmax(1, d$F_E))


# Calculate F_M

# Extract fiber
f_M <- d[,c(7)]

# Adjust to per 231 g
f_M <- f_M * 2.31

d$F_M <- f_M

# Normalize to 1-100 scale 
d$F_M <- (d$F_M - min(d$F_M)) * (100 - 1) / (max(d$F_M) - min(d$F_M)) + 1

# Clip normalized values to 1-100
d$F_M <- pmin(100, pmax(1, d$F_M))


d$F <- d$F_E + d$F_M

# Normalize to 1-100 scale 
d$F <- (d$F - min(d$F)) * (100 - 1) / (max(d$F) - min(d$F)) + 1

# Clip normalized values to 1-100
d$F <- pmin(100, pmax(1, d$F))


# For liquids and semi-liquid dairy, using energy only as the reference unit.
d$F <- ifelse(d$Liquids == 1, d$F_E, d$F)


#######################################################################
# Calculate NR

# # Extract select nutrients and energy 
# nr <- d[,c("Unsaturated_FAs_g", "Saturated_FAs_g", "Fiber", "Carbohydrates", "Potassium_mg", "Sodium", "Energy_kcal")]

# SUR
d$SUR <- ifelse(9*(d$Saturated_FAs_g + d$Unsaturated_FAs_g) < d$Energy_kcal/10, NA, d$Saturated_FAs_g/d$Unsaturated_FAs_g)

# Get min and max F values
SURmin <- min(d$SUR, na.rm = TRUE)
SURmax <- max(d$SUR, na.rm = TRUE)

# Normalize to -100 to 0 scale
d$SUR <- -(d$SUR - SURmin) / (SURmax - SURmin) * 100

d$SUR <- ifelse(is.na(d$SUR), 0, d$SUR)

# CFR
d$CFR <- ifelse(d$Food_group == "Animal-source foods" | d$Food_long == "Soymilk, original and vanilla, unfortified", NA, d$Carbohydrates/d$Fiber)

# Get min and max F values
CFRmin <- min(d$CFR, na.rm = TRUE)
CFRmax <- max(d$CFR, na.rm = TRUE)

# Normalize to -100 to 0 scale
d$CFR <- -(d$CFR - CFRmin) / (CFRmax - CFRmin) * 100

d$CFR <- ifelse(is.na(d$CFR), 0, d$CFR)

# Calculate negative sodium score

d$NaKR <- ifelse(d$Sodium/d$Energy_kcal < 0.9, NA, d$Sodium/d$Potassium_mg)

# Get min and max F values
NaKRmin <- min(d$NaKR, na.rm = TRUE)
NaKRmax <- max(d$NaKR, na.rm = TRUE)

# Normalize to -100 to 0 scale
d$NaKR <- -(d$NaKR - NaKRmin) / (NaKRmax - NaKRmin) * 100
d$NaKR <- ifelse(is.na(d$NaKR), 0, d$NaKR)

# Calculate composite nutrient ratio score
d$NR <- d$SUR + d$CFR + d$NaKR

# Normalize to 1-100 scale 
d$NR <- (d$NR - min(d$NR)) * (100 - 1) / (max(d$NR) - min(d$NR)) + 1

# Clip normalized values to 1-100
d$NR <- pmin(100, pmax(1, d$NR))

#######################################################################
# Calculate Energy Density (EMR)
# EMR
d$EMR <- ifelse(d$Energy_kcal < 130, NA, d$Energy_kcal/100)

# Get min and max MER values
EMRmin <- min(d$EMR, na.rm = TRUE)
EMRmax <- max(d$EMR, na.rm = TRUE)

# Normalize to -100 to 0 scale
d$EMR <- -(d$EMR - EMRmin) / (EMRmax - EMRmin) * 100
d$EMR <- ifelse(is.na(d$EMR), 0, d$EMR)

############################################################
# Calculate nutrient density score
d$NDS <- (3.5*d$V + 3.5*d$M + 2*d$EAA + 1*d$N3)/10

# Normalize to 1-100 scale 
d$NDS <- (d$NDS - min(d$NDS)) * (100 - 1) / (max(d$NDS) - min(d$NDS)) + 1

# Clip normalized values to 1-100
d$NDS <- pmin(100, pmax(1, d$NDS))

############################################################
# Calculate nutritional value score
d$NVS <- (2*d$V + 2*d$M + 1.25*d$EAA + 1*d$N3 + 0.75*d$F + 0.75*d$EMR + 2.25*d$NR)/10

# NCD-focused sensitivity analysis
#d$NVS <- (1*d$V + 1*d$M + 1*d$EAA + 2*d$N3 + 2*d$F + 3*d$NR)/10

# Nutrient density-focused sensitivity analysis
#d$NVS <- (3*d$V + 3*d$M + 2*d$EAA + 1*d$N3 + 0.5*d$F + 0.5*d$NR)/10

NVS <- d[, c("Food", "NVS")]

# Winsorizing sensitivity analysis
# # Calculate 5th and 95th percentiles
# NVSq5 <- quantile(d$NVS, probs = 0.05, na.rm = TRUE)
# NVSq95 <- quantile(d$NVS, probs = 0.95, na.rm = TRUE)
# 
# # Truncate at 5th and 95 percentiles
# d$NVS <- pmin(NVSq95, pmax(NVSq5, d$NVS))

# Normalize to 1-100 scale 
d$NVS <- (d$NVS - min(d$NVS)) * (100 - 1) / (max(d$NVS) - min(d$NVS)) + 1

# Clip normalized values to 1-100
d$NVS <- pmin(100, pmax(1, d$NVS))

# Calculate mass quantity per NVS of 100
d$NVS_100 <- 231*100/d$NVS

# Calculate Calorie quantity per NVS of 100
d$NVS_100_Cal <- 300*100/d$NVS

d$Cal_1000 <- 100*1000/d$Energy_kcal

write.csv(d, "Data/NVS_Nigeria_12Apr2024.csv", row.names = F)
#write.csv(d, "Data/data_NCD-F_15Sept2023_Indonesia.csv", row.names = F)
#write.csv(d, "Data/data_ND-F_15Sept2023_Indonesia.csv", row.names = F)
#write.csv(d, "Data/data_15_Win_Sept2023_Indonesia.csv", row.names = F)

############################################################
d <- d[, c("Food", "Food_group", "DQQ_question", "Energy_kcal", "NVS")]

# Calculate mass quantity per NVS of 100
d$nFU <- 231*100/d$NVS

# Calculate Calorie quantity per NVS of 100
d$NVS_100_Cal <- 300*100/d$NVS

d$Cal_1000 <- 100*1000/d$Energy_kcal

write.csv(d, "Data/NVS_nFU_Nigeria_12Apr2024.csv", row.names = F)
