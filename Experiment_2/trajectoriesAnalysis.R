library(tidyverse)
library(rstatix) 
library(ggpubr)
library(patchwork)
scale = 0.5625
# Compute distance to target and statistics for 2 and 3 taps
setwd("/Users/roc/Research/DrawRhythm/TickTack/Experiment_2/")
ghost <- read.csv("../Data/usa_2013.csv")
files <- list.files(".", pattern="path_follow_[0-9]+.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
distances <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("hiding", "mean", "sd"))
for (cf in files) {
  print(cf)
  traj <- read.csv(cf)
  print(ggplot(data=traj) + geom_point(mapping=aes(x=x,y=y), color="gray") + geom_point(mapping=aes(x=xGhost,y=yGhost), color="red", shape="."))
  readline()
  distTraj <- sqrt((traj$x - traj$xGhost)^2 + (traj$y - traj$yGhost)^2)
  summary(distTraj)
  if ("hiding...false" %in% colnames(traj)) {
    print("No hiding")
    distances <- add_row(distances, hiding = FALSE, mean = mean(distTraj,na.rm=T), sd = sd(distTraj,na.rm=T))
  }
  if ("hiding...true" %in% colnames(traj)) {
    print("Hiding")
    distances <- add_row(distances, hiding = TRUE, mean = mean(distTraj,na.rm=T), sd = sd(distTraj,na.rm=T))
  }
}

aggregate(distances, by = list(distances$hiding), FUN = mean)

# --------- testing of mean differences
# --------- non parametric testing 
distancesNohide <- distances$mean[distances$hiding == FALSE]
distancesHide <- distances$mean[distances$hiding == TRUE]
# datagroups
hides <- factor(rep(c("Nohide", "Hide"), c(length(distancesNohide),length(distancesHide))))
id <- factor(c(1:length(distancesNohide), 1:length(distancesHide)))
# data frame
meanData <- tibble(id, hides, c(distancesNohide,distancesHide))
names(meanData) <- c("id", "hiding", "meanDistance")
# summary statistics
meanData %>%
  group_by(hiding) %>%
  get_summary_stats(meanDistance, type = "median_iqr")
# visualization
bxp <- ggpaired(meanData, x = "hiding", y = "meanDistance", order = c("Nohide", "Hide"),
                ylab = "meanDistance", xlab = "hiding")
bxp
# assumption
# The test assumes that differences between paired samples should be distributed 
# symmetrically around the median.
differences = data.frame(differences = distancesNohide - distancesHide)
gghistogram(differences, x = "differences", y = "..density..",
            fill = "steelblue",bins = 6, add_density = TRUE)
# computation of test
stat.test <- meanData %>%
  wilcox_test(meanDistance ~ hiding, paired = TRUE) %>% add_significance()
stat.test
# effect size
meanData %>%
  wilcox_effsize(meanDistance ~ hiding, paired = TRUE)
# report
stat.test <- stat.test %>% add_xy_position(x = "hiding") 
bxp + stat_pvalue_manual(stat.test, tip.length = 0) + labs(subtitle = get_test_label(stat.test, detailed= TRUE))
# ------- parametric testing 
# procedure as in https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/
# device comparison within subjects: one factor, two levels
# check assumptions
# any outliers?
meanData %>% group_by(hiding) %>% identify_outliers(meanDistance)
# normality via Shapiro-Wilk test (normal if p>0.05)
meanData %>% group_by(hiding) %>% shapiro_test(meanDistance)
# normality via qqplot
ggqqplot(meanData, "meanDistance", facet.by="hiding")
# assumption of sphericity will be automatically checked during 
# the computation of the ANOVA test using the R function anova_test() 
# [rstatix package]. The Mauchly’s test is internally used to assess 
# the sphericity assumption.
res.ttest <- meanData %>%
  t_test(meanDistance ~ hiding, paired = TRUE) %>% add_significance()
res.ttest
res.aov <- anova_test(data = meanData, dv = meanDistance, wid = id, within = hiding )
get_anova_table(res.aov)
# ges is the generalized effect size (amount of variability due to the within-subjects factor)
# print("The mean distance was not statistically significantly different between Hiding and Nohiding, F(1, 2) = 1.092, n.s., eta2[g] = 0.161.")

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
# print("The mean distance was not statistically significantly different between first and second half, F(1, 2) = 0.621, p = 0.013, eta2[g] = 0.264.")


# Compute histograms of magnitude velocity and direction during exploration
files <- list.files(".", pattern="path_[0-9]+.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)

trajNohide <- data.frame()
for (cf in files) {
  rc <- read.csv(cf)
  if ("hiding...false" %in% colnames(rc)) {
    print(cf)
    trajNohide <- rbind(trajNohide, rc)
    print(ggplot(data=rc) + geom_point(mapping=aes(x=x,y=y), color="gray") )
    # readline()
  }
}
velo <- append(0, sqrt((diff(trajNohide$x))^2 + (diff(trajNohide$y))^2) * trajNohide$frameRate[2:nrow(trajNohide)])
dire <- append(0, atan2(diff(trajNohide$y),diff(trajNohide$x)))
trajNohide_v <- trajNohide %>% mutate(velo) %>% mutate(dire)
hist1 <- ggplot(data=trajNohide_v, aes(velo)) + geom_histogram(binwidth = 2) + # speed in pixels/sec
  coord_cartesian(xlim = c(0.0,200)) + xlab("velocity") + ylab("") +
  theme(aspect.ratio=1/1)
hist1
polar1 <- ggplot(data=trajNohide_v, aes(dire)) + geom_histogram(binwidth = pi/90) + # binwidth = 2 degrees
  coord_polar(start=pi/2,direction=-1) + xlim(-pi,pi) + xlab("") + ylab("") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
polar1

