setwd("~/Research/DrawRhythm/TickTack/Data/")
library(tidyverse)
scale = 0.5625
ghost <- read.csv("usa_2013.csv")
traj <- read.csv("path_follow_13181.csv")
#ggplot(data=ghost, mapping=aes(x=x,y=y)) + geom_point(color="gray") + geom_point(data=traj/scale, color="red")
ggplot(data=traj) + geom_point(mapping=aes(x=x,y=y), color="gray") + geom_point(mapping=aes(x=xGhost,y=yGhost), color="red")




