library(psych)
library(scales)

### Data preparation
# Load data
data <- read.csv("./data/music_preferences_data.csv", sep = ";")
# Only STOMP and MUSIC #
data <- data[,1:64] # data frame contains various questionnaires like TIPI, Music use

# Rescaling so that both preference questionnaires are from 1 to 5
data$stomp_1_i <- rescale(data$stomp_1_i, to = c(1,5))
data$stomp_2_s <- rescale(data$stomp_2_s, to = c(1,5))
data$stomp_3_s <- rescale(data$stomp_3_s, to = c(1,5))
data$stomp_4_s <- rescale(data$stomp_4_s, to = c(1,5))
data$stomp_5_u <- rescale(data$stomp_5_u, to = c(1,5))
data$stomp_6_m <- rescale(data$stomp_6_m, to = c(1,5))
data$stomp_7_s <- rescale(data$stomp_7_s, to = c(1,5))
data$stomp_8_c <- rescale(data$stomp_8_c, to = c(1,5))
data$stomp_9_s <- rescale(data$stomp_9_s, to = c(1,5))
data$stomp_10_i <- rescale(data$stomp_10_i, to = c(1,5))
data$stomp_11_m <- rescale(data$stomp_11_m, to = c(1,5))
data$stomp_12_s <- rescale(data$stomp_12_s, to = c(1,5))
data$stomp_13_m <- rescale(data$stomp_13_m, to = c(1,5))
data$stomp_14_oldies <- rescale(data$stomp_14_oldies, to = c(1,5))
data$stomp_15_s <- rescale(data$stomp_15_s, to = c(1,5))
data$stomp_16_u <- rescale(data$stomp_16_u, to = c(1,5))
data$stomp_17_i <- rescale(data$stomp_17_i, to = c(1,5))
data$stomp_18_c <- rescale(data$stomp_18_c, to = c(1,5))
data$stomp_19_c <- rescale(data$stomp_19_c, to = c(1,5))
data$stomp_20_u <- rescale(data$stomp_20_u, to = c(1,5))
data$stomp_21_i <- rescale(data$stomp_21_i, to = c(1,5))
data$stomp_22_c <- rescale(data$stomp_22_c, to = c(1,5))
data$stomp_23_soundtrack <- rescale(data$stomp_23_soundtrack, to = c(1,5))

# Average scores
# MUSIC listening task
data$mean_M_music <- rowMeans(data[,7:11], 
                              na.rm = TRUE)

data$mean_U_music <- rowMeans(data[,12:15], 
                              na.rm = TRUE)

data$mean_S_music <- rowMeans(data[,16:21], 
                              na.rm = TRUE)

data$mean_I_music <- rowMeans(data[,22:26], 
                              na.rm = TRUE)

data$mean_C_music <- rowMeans(data[,27:31], 
                              na.rm = TRUE)

#STOMP Musical preferences short test
data$mean_M_stomp <- rowMeans(data[,c(47,52,54)], 
                              na.rm = TRUE)

data$mean_U_stomp <- rowMeans(data[,c(46,57,61)], 
                              na.rm = TRUE)

data$mean_S_stomp <- rowMeans(data[,c(43,44,45,48,50,53,56)], 
                              na.rm = TRUE)

data$mean_I_stomp <- rowMeans(data[,c(42,51,58,62)], 
                              na.rm = TRUE)

data$mean_C_stomp <- rowMeans(data[,c(49,59,60,63)], 
                              na.rm = TRUE)

## scaled data if needed
sdata <- scale(data[,7:64])
sdata <- as.data.frame(sdata)
sdata <- cbind(sdata, data$id)

sdata$M_music_mean <- rowMeans(sdata[,1:5], 
                               na.rm = TRUE)

sdata$U_music_mean <- rowMeans(sdata[,6:9], 
                               na.rm = TRUE)

sdata$S_music_mean <- rowMeans(sdata[,10:15], 
                               na.rm = TRUE)

sdata$I_music_mean <- rowMeans(sdata[,16:20], 
                               na.rm = TRUE)

sdata$C_music_mean <- rowMeans(sdata[,21:25], 
                               na.rm = TRUE)

sdata$M_stomp_mean <- rowMeans(sdata[,c(41,46,48)], 
                              na.rm = TRUE)

sdata$U_stomp_mean <- rowMeans(sdata[,c(40,51,55)], 
                              na.rm = TRUE)

sdata$S_stomp_mean <- rowMeans(sdata[,c(37,38,39,42,44,47,50)], 
                              na.rm = TRUE)

sdata$I_stomp_mean <- rowMeans(sdata[,c(36,45,52,56)], 
                              na.rm = TRUE)

sdata$C_stomp_mean <- rowMeans(sdata[,c(43,53,54,57)], 
                              na.rm = TRUE)

## Renaming columns

names(sdata)[names(sdata) == "M_music_mean"] <- "acoustic_pref_mellow"
names(sdata)[names(sdata) == "U_music_mean"] <- "acoustic_pref_unpretentious"
names(sdata)[names(sdata) == "S_music_mean"] <- "acoustic_pref_sophisticated"
names(sdata)[names(sdata) == "I_music_mean"] <- "acoustic_pref_intense"
names(sdata)[names(sdata) == "C_music_mean"] <- "acoustic_pref_contemporary"

names(sdata)[names(sdata) == "M_stomp_mean"] <- "verbal_pref_mellow"
names(sdata)[names(sdata) == "U_stomp_mean"] <- "verbal_pref_unpretentious"
names(sdata)[names(sdata) == "S_stomp_mean"] <- "verbal_pref_sophisticated"
names(sdata)[names(sdata) == "I_stomp_mean"] <- "verbal_pref_intense"
names(sdata)[names(sdata) == "C_stomp_mean"] <- "verbal_pref_contemporary"
