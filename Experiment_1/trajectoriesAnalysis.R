library(tidyverse)
library(rstatix) 
library(ggpubr)
scale = 0.5625
# Compute distance to target and statistics for 2 and 3 taps
setwd("/Users/roc/Research/DrawRhythm/TickTack/Experiment_1/")
ghost <- read.csv("../Data/usa_2013.csv")
files <- list.files(".", pattern="path_follow_[0-9]+.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
distances <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("taps", "mean", "sd"))
for (cf in files) {
  print(cf)
  traj <- read.csv(cf)
  print(ggplot(data=traj) + geom_point(mapping=aes(x=x,y=y), color="gray") + geom_point(mapping=aes(x=xGhost,y=yGhost), color="red", shape="."))
  # readline()
  distTraj <- sqrt((traj$x - traj$xGhost)^2 + (traj$y - traj$yGhost)^2)
  summary(distTraj)
  if ("taps...2" %in% colnames(traj)) {
    print("Duplets")
    distances <- add_row(distances, taps = 2, mean = mean(distTraj,na.rm=T), sd = sd(distTraj,na.rm=T))
  }
  if ("taps...3" %in% colnames(traj)) {
    print("Triplets")
    distances <- add_row(distances, taps = 3, mean = mean(distTraj,na.rm=T), sd = sd(distTraj,na.rm=T))
  }
}

aggregate(distances, by = list(distances$taps), FUN = mean)

# --------- testing of mean differences
# --------- non parametric testing 
distances2taps <- distances$mean[distances$taps == 2]
distances3taps <- distances$mean[distances$taps == 3]
# datagroups
taps <- factor(rep(c("2taps", "3taps"), c(length(distances2taps),length(distances3taps))))
id <- factor(c(1:length(distances2taps), 1:length(distances3taps)))
# data frame
meanData <- tibble(id, taps, c(distances2taps,distances3taps))
names(meanData) <- c("id", "taps", "meanDistance")
# summary statistics
meanData %>%
  group_by(taps) %>%
  get_summary_stats(meanDistance, type = "median_iqr")
# visualization
bxp <- ggpaired(meanData, x = "taps", y = "meanDistance", order = c("2taps", "3taps"),
                ylab = "meanDistance", xlab = "taps")
bxp
# assumption
# The test assumes that differences between paired samples should be distributed 
# symmetrically around the median.
differences = data.frame(differences = distances2taps - distances3taps)
gghistogram(differences, x = "differences", y = "..density..",
            fill = "steelblue",bins = 6, add_density = TRUE)
# computation of test
stat.test <- meanData %>%
  wilcox_test(meanDistance ~ taps, paired = TRUE) %>% add_significance()
stat.test
# effect size
meanData %>%
  wilcox_effsize(meanDistance ~ taps, paired = TRUE)
# report
stat.test <- stat.test %>% add_xy_position(x = "taps") 
bxp + stat_pvalue_manual(stat.test, tip.length = 0) + labs(subtitle = get_test_label(stat.test, detailed= TRUE))
# ------- parametric testing 
# procedure as in https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/
# device comparison within subjects: one factor, two levels
# check assumptions
# any outliers?
meanData %>% group_by(taps) %>% identify_outliers(meanDistance)
# normality via Shapiro-Wilk test (normal if p>0.05)
meanData %>% group_by(taps) %>% shapiro_test(meanDistance)
# normality via qqplot
ggqqplot(meanData, "meanDistance", facet.by="taps")
# assumption of sphericity will be automatically checked during 
# the computation of the ANOVA test using the R function anova_test() 
# [rstatix package]. The Mauchly’s test is internally used to assess 
# the sphericity assumption.
res.ttest <- meanData %>%
  t_test(meanDistance ~ taps, paired = TRUE) %>% add_significance()
res.ttest
res.aov <- anova_test(data = meanData, dv = meanDistance, wid = id, within = taps )
get_anova_table(res.aov)
# ges is the generalized effect size (amount of variability due to the within-subjects factor)
print("The mean distance was not statistically significantly different between 2 and 3 taps, F(1, 8) = 2.821, n.s., eta2[g] = 0.119.")

# --------- testing the progress between first and second half
# --------- non parametric testing 
distancesFirst <- distances$mean[c(TRUE,FALSE)] 
distancesSecond <- distances$mean[c(FALSE,TRUE)]
# datagroups
half <- factor(rep(c("first half", "second half"), c(length(distancesFirst),length(distancesSecond))))
id <- factor(c(1:length(distancesFirst), 1:length(distancesSecond)))
# data frame
meanData <- tibble(id, half, c(distancesFirst,distancesSecond))
names(meanData) <- c("id", "half", "meanDistance")
# summary statistics
meanData %>%
  group_by(half) %>%
  get_summary_stats(meanDistance, type = "median_iqr")
# visualization
bxp <- ggpaired(meanData, x = "half", y = "meanDistance", order = c("first half", "second half"),
                ylab = "meanDistance", xlab = "half")
bxp
# assumption
# The test assumes that differences between paired samples should be distributed 
# symmetrically around the median.
differences = data.frame(differences = distancesFirst - distancesSecond)
gghistogram(differences, x = "differences", y = "..density..",
            fill = "steelblue",bins = 6, add_density = TRUE)
# computation of test
stat.test <- meanData %>%
  wilcox_test(meanDistance ~ half, paired = TRUE) %>% add_significance()
stat.test
# effect size
meanData %>%
  wilcox_effsize(meanDistance ~ half, paired = TRUE)
# report
stat.test <- stat.test %>% add_xy_position(x = "half") 
bxp + stat_pvalue_manual(stat.test, tip.length = 0) + labs(subtitle = get_test_label(stat.test, detailed= TRUE))
# ------- parametric testing 
# procedure as in https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/
# device comparison within subjects: one factor, two levels
# check assumptions
# any outliers?
meanData %>% group_by(half) %>% identify_outliers(meanDistance)
# normality via Shapiro-Wilk test (normal if p>0.05)
meanData %>% group_by(half) %>% shapiro_test(meanDistance)
# normality via qqplot
ggqqplot(meanData, "meanDistance", facet.by="half")
# assumption of sphericity will be automatically checked during 
# the computation of the ANOVA test using the R function anova_test() 
# [rstatix package]. The Mauchly’s test is internally used to assess 
# the sphericity assumption.
res.ttest <- meanData %>%
  t_test(meanDistance ~ half, paired = TRUE) %>% add_significance()
res.ttest
res.aov <- anova_test(data = meanData, dv = meanDistance, wid = id, within = half )
get_anova_table(res.aov)
# ges is the generalized effect size (amount of variability due to the within-subjects factor)
print("The mean distance was statistically significantly different between first and second half, F(1, 8) = 6.927, p = 0.03, eta2[g] = 0.212.")


# Compute histograms of magnitude velocity and direction during exploration
files <- list.files(".", pattern="path_[0-9]+.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)

traj2taps <- data.frame()
for (cf in files) {
  rc <- read.csv(cf)
  if ("taps...2" %in% colnames(rc)) {
    print(cf)
    traj2taps <- rbind(traj2taps, rc)
    print(ggplot(data=rc) + geom_point(mapping=aes(x=x,y=y), color="gray") )
    # readline()
  }
}
velo <- append(0, sqrt((diff(traj2taps$x))^2 + (diff(traj2taps$y))^2) * traj2taps$frameRate[2:nrow(traj2taps)])
dire <- append(0, atan2(diff(traj2taps$y),diff(traj2taps$x)))
traj2taps_v <- traj2taps %>% mutate(velo) %>% mutate(dire)
ggplot(data=traj2taps_v, aes(velo)) + geom_histogram(binwidth = 2) + # speed in pixels/sec
  coord_cartesian(xlim = c(0.0,200))
ggplot(data=traj2taps_v, aes(dire)) + geom_histogram(binwidth = pi/90) + # binwidth = 2 degrees
  coord_polar(start=pi/2,direction=-1) + xlim(-pi,pi) + xlab("") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

traj3taps <- data.frame()
for (cf in files) {
  rc <- read.csv(cf)
  if ("taps...3" %in% colnames(rc)) {
    print(cf)
    traj3taps <- rbind(traj3taps, rc)
    print(ggplot(data=rc) + geom_point(mapping=aes(x=x,y=y), color="gray") )
    # readline()
  }
}
velo <- append(0, sqrt((diff(traj3taps$x))^2 + (diff(traj3taps$y))^2) * traj3taps$frameRate[2:nrow(traj3taps)])
dire <- append(0, atan2(diff(traj3taps$y),diff(traj3taps$x)))
traj3taps_v <- traj3taps %>% mutate(velo) %>% mutate(dire)
ggplot(data=traj3taps_v, aes(velo)) + geom_histogram(binwidth = 2) +
  coord_cartesian(xlim = c(0.0,200))
ggplot(data=traj3taps_v, aes(dire)) + geom_histogram(binwidth = pi/90) + # binwidth = 2 degrees
  coord_polar(start=pi/2,direction=-1) + xlim(-pi,pi) + xlab("") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
  

