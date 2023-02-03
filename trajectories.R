setwd("~/Research/DrawRhythm/TickTack/Experiment_1/Participant_10")
library(tidyverse)
scale = 0.5625
ghost <- read.csv("../Data/usa_2013.csv")
traj <- read.csv("path_follow_31057.csv")
#ggplot(data=ghost, mapping=aes(x=x,y=y)) + geom_point(color="gray") + geom_point(data=traj/scale, color="red")
ggplot(data=traj) + geom_point(mapping=aes(x=x,y=y), color="gray") + geom_point(mapping=aes(x=xGhost,y=yGhost), color="red", shape=".")




