library(ggplot2)

### Pairwise t.tests
# Individual tests between 5 meta genres MUSIC/STOMP
M <- t.test(data$mean_M_music, data$mean_M_stomp, paired = TRUE)
print(M)
U <- t.test(data$mean_U_music, data$mean_U_stomp, paired = TRUE)
print(U)
S <- t.test(data$mean_S_music, data$mean_S_stomp, paired = TRUE)
print(S)
I <- t.test(data$mean_I_music, data$mean_I_stomp, paired = TRUE)
print(I)
C <- t.test(data$mean_C_music, data$mean_C_stomp, paired = TRUE)
print(C)

# Data set preparation for mean differences bar plot with 95% CIs
test <- rbind("M", "U", "S", "I", "C")
difference <- rbind(M$estimate, U$estimate, S$estimate, I$estimate, C$estimate)
CI95 <- rbind(M$conf.int, U$conf.int, S$conf.int, I$conf.int, C$conf.int)
tt_data <- cbind(test, difference, CI95)
rownames(tt_data) <- c("M", "U", "S", "I", "C")
colnames(tt_data) <- c("test", "difference", "lowerCI", "upperCI")
tt_data <- as.data.frame(tt_data)
tt_data$test <- factor(tt_data$test, levels = c("M", "U", "S", "I", "C"))

# Plotting mean differences per meta genre
ggplot(tt_data, aes(x = as.factor(test), 
                    y = as.numeric(difference), 
                    fill = test)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = as.numeric(lowerCI), ymax = as.numeric(upperCI)),
                width = .2,                  
                position = position_dodge(.9)) +
  xlab("5 meta genres") +
  ylab("Mean differences MUSIC/STOMP") +
  scale_y_continuous(limits = c(-1,1)) +
  scale_fill_brewer(name = "Meta genres",
                    palette = "Set1") +
  theme_bw()
