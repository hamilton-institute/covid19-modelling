# Create a calendar image using a background

# Clear workspace and load packages
rm(list = ls())
library(ggpubr)
library(png)
library(ggplot2)

# Add an image
img = png::readPNG('blank_cal3.png')

df = data.frame(x = seq(0, 1, by = 0.1),
                y = seq(0, 1, by = 0.1))

ggplot(df, aes(x = x, y = y)) +
  background_image(img) +
  geom_point(alpha = 0) +
  annotate("text", x = 0.5, y = 0.9, label = "August", 
           colour = "white",
           size = 40) +
  annotate("text", x = 0.5, y = 0.5, label = "29", 
           colour = "black",
           size = 70) +
  annotate("text", x = 0.5, y = 0.15, label = "2021", 
           colour = "black",
           size = 30) +
  ggtitle("10% chance it will be extinct by...") + 
  theme(plot.title = element_text(size = rel(3.5))) +
  theme_void(base_size = 30)
  

