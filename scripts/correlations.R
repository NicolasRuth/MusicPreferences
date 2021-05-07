library(psych)
library(ggplot2)
library(GGally)
library(reshape2)

### Correlation matrix including p-values
corr.test(sdata[,60:64], sdata[,65:69])

### Heatmap visualisation
cor_matrix <- round(cor(sdata[,60:64], sdata[,65:69]), 2)

melted_cormat <- melt(cor_matrix)

# Preparing triangle matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cor_matrix)
melted_tri <- melt(upper_tri, na.rm = TRUE)

# Heatmap plotting
heatmap <- ggplot(data = melted_tri, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 9, hjust = 1)) +
  coord_fixed()

heatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


