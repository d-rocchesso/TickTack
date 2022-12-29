library(tidyverse)
scale = 0.5625
# Compute distance to target and statistics for 2 and 3 taps
ghost <- read.csv("../Data/usa_2013.csv")
files <- list.files(".", pattern="path_follow_[0-9]+.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
distances <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("taps", "mean", "sd"))
for (cf in files) {
  print(cf)
  traj <- read.csv(cf)
  print(ggplot(data=traj) + geom_point(mapping=aes(x=x,y=y), color="gray") + geom_point(mapping=aes(x=xGhost,y=yGhost), color="red", shape="."))
  readline()
  distTraj <- sqrt((traj$x - traj$xGhost)^2 + (traj$y - traj$yGhost)^2)
  summary(distTraj)
  if ("taps...2" %in% colnames(traj)) {
    distances <- add_row(distances, taps = 2, mean = mean(distTraj,na.rm=T), sd = sd(distTraj,na.rm=T))
  }
  if ("taps...3" %in% colnames(traj)) {
    distances <- add_row(distances, taps = 3, mean = mean(distTraj,na.rm=T), sd = sd(distTraj,na.rm=T))
  }
}
aggregate(distances, by = list(distances$taps), FUN = mean)


# Compute histograms of magnitude velocity and direction during exploration
files <- list.files(".", pattern="path_[0-9]+.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)

traj2taps <- data.frame()
for (cf in files) {
  rc <- read.csv(cf)
  if ("taps...2" %in% colnames(rc)) {
    print(cf)
    traj2taps <- rbind(traj2taps, rc)
    print(ggplot(data=rc) + geom_point(mapping=aes(x=x,y=y), color="gray") )
    readline()
  }
}
velo <- append(0, sqrt((diff(traj2taps$x))^2 + (diff(traj2taps$y))^2) * traj2taps$frameRate[2:nrow(traj2taps)])
dire <- append(0, atan2(diff(traj2taps$y),diff(traj2taps$x)))
traj2taps_v <- traj2taps %>% mutate(velo) %>% mutate(dire)
ggplot(data=traj2taps_v, aes(velo)) + geom_histogram(binwidth = 2) + # speed in pixels/sec
  coord_cartesian(xlim = c(0.0,300))
ggplot(data=traj2taps_v, aes(dire)) + geom_histogram(binwidth = pi/90) + # binwidth = 2 degrees
  coord_polar(start=pi/2,direction=-1) + xlim(-pi,pi)

traj3taps <- data.frame()
for (cf in files) {
  rc <- read.csv(cf)
  if ("taps...3" %in% colnames(rc)) {
    print(cf)
    traj3taps <- rbind(traj3taps, rc)
    print(ggplot(data=rc) + geom_point(mapping=aes(x=x,y=y), color="gray") )
    readline()
  }
}
velo <- append(0, sqrt((diff(traj3taps$x))^2 + (diff(traj3taps$y))^2) * traj3taps$frameRate[2:nrow(traj3taps)])
dire <- append(0, atan2(diff(traj3taps$y),diff(traj3taps$x)))
traj3taps_v <- traj3taps %>% mutate(velo) %>% mutate(dire)
ggplot(data=traj3taps_v, aes(velo)) + geom_histogram(binwidth = 2) +
  coord_cartesian(xlim = c(0.0,300))
ggplot(data=traj3taps_v, aes(dire)) + geom_histogram(binwidth = pi/90) + # binwidth = 2 degrees
  coord_polar(start=pi/2,direction=-1) + xlim(-pi,pi)

