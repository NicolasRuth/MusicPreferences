# Musical Preferences Tree Models analysis Script

library(tidyverse)
dat <- read.csv("data/new_labeled_scores.csv")
dat <- dat[,c(4,66:75)]

MUSIC_dat <- dat[,c(1:6)] %>% pivot_longer(names_to = "MUSIC_genre", values_to = "MUSIC_rating", c(2:6))
STOMP_dat <- dat[,c(1,7:11)] %>% pivot_longer(names_to = "STOMP_genre", values_to = "STOMP_rating", c(2:6))

genre_dat <- cbind(MUSIC_dat,STOMP_dat)[,-4]

Musical_collaboration <- read.csv2("~/Documents/Conferences/2021_DGM/Preferences/Musical_collaboration.csv")
full_dat <- merge(genre_dat, Musical_collaboration[,c(28:31,163:167,174:178)], all=T, by.x = "id", by.y= "id", all.x = T)
full_dat$STOMP_genre <- as.factor(full_dat$STOMP_genre)
full_dat$MUSIC_genre <- as.factor(full_dat$MUSIC_genre)

library(glmertree)
m_1 <- lmertree(MUSIC_rating ~ STOMP_rating | id | age + gender + education +STOMP_genre+TIPI_Extroversion + TIPI_Agreeableness+TIPI_Emotional.Stability+TIPI_Conscientiousness+TIPI_Openness+Emotional.Mean+Cognitive.Mean+Background+EQ.score.mean+SQ.score.mean, data = full_dat)

library(R.devices)
eps("glmertree_scatter.eps",height=9,width=16,horizontal=F)
plot(m_1,which = "tree", fitted = "marginal")
dev.off()

eps("glmertree_reg_coefs.eps",height=9,width=16,horizontal=F)
plot(m_1,which = "tree", type = "simple")
dev.off()

full_dat$rating_diff <- full_dat$MUSIC_rating - full_dat$STOMP_rating
full_dat$rating_diff_abs <- abs(full_dat$rating_diff)
dat_agg <- with(full_dat, aggregate(rating_diff_abs, by=list(id),mean))
colnames(dat_agg) <- c("id","rating_diff_abs")
dat_agg <- merge(dat_agg, Musical_collaboration[,c(28:31,163:167,174:178)], by="id",all=F)

library(partykit)
m_2 <- lmtree(rating_diff_abs ~ age + gender + education +TIPI_Extroversion + TIPI_Agreeableness+TIPI_Emotional.Stability+TIPI_Conscientiousness+TIPI_Openness+Emotional.Mean+Cognitive.Mean+Background+EQ.score.mean+SQ.score.mean, data=dat_agg)
eps("lmtree_boxplot.eps",height=7,width=7,horizontal=F)
plot(m_2)
dev.off()