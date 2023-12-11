library(tidyverse)
library(rstatix) 
library(ggpubr)
library(patchwork)
scale = 0.5625
# Compute distance to target and statistics for 2 and 3 taps
setwd("/Users/roc/Research/DrawRhythm/TickTack/Experiment_1/")
ghost <- read.csv("../Data/usa_2013.csv")
files <- list.files(".", pattern="path_follow_[0-9]+.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
distances <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("taps", "half", "mean", "sd"))
half <- 0
for (cf in files) {
  print(cf)
  traj <- read.csv(cf)
  print(ggplot(data=traj) + geom_point(mapping=aes(x=x,y=y), color="gray") + geom_point(mapping=aes(x=xGhost,y=yGhost), color="red", shape="."))
  # readline()
  distTraj <- sqrt((traj$x - traj$xGhost)^2 + (traj$y - traj$yGhost)^2)
  summary(distTraj)
  if ("taps...2" %in% colnames(traj)) {
    print("Duplets")
    distances <- add_row(distances, taps = 2, half = half+1, mean = mean(distTraj,na.rm=T), sd = sd(distTraj,na.rm=T))
  }
  if ("taps...3" %in% colnames(traj)) {
    print("Triplets")
    distances <- add_row(distances, taps = 3, half = half+1, mean = mean(distTraj,na.rm=T), sd = sd(distTraj,na.rm=T))
  }
  half <- !half
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
print("The mean distance was not statistically significantly different between 2 and 3 taps, F(1, 9) = 4.469, n.s., eta2[g] = 0.169.")

# ******* mixed anova ******
# Hiding (true or false) is within subjects
# order (hiding first or hiding second) is between subjects
# https://www.datanovia.com/en/lessons/mixed-anova-in-r/
subject <- factor(rep(1:10, each=2))
first <- factor(distances$taps[rep(seq(1,nrow(distances),2),each=2)])
distancesInt <- distances %>% mutate(subject) %>% mutate(first)
distancesInt$hh <- interaction(distancesInt$taps,distancesInt$first)
distancesInt <- as_tibble(distancesInt)
interaction.plot(distancesInt$first,distancesInt$taps,distancesInt$mean)
# Summary statistics 
distancesInt %>%
  group_by(taps, first) %>%
  get_summary_stats(mean, type="mean_sd")
# visualization
bxp <- ggboxplot(
  distancesInt, x="taps", y="mean", color="first", palette="jco", ylab="mean distance"
)
bxp
# outliers
distancesInt %>%
  group_by(taps, first) %>%
  identify_outliers(mean)
# normality assumption
distancesInt %>%
  group_by(taps, first) %>%
  shapiro_test(mean)
ggqqplot(distancesInt, "mean", ggtheme = theme_bw()) +
  facet_grid(taps ~ first)
# homogeneity of variance
distancesInt %>%
  group_by(taps) %>%
  levene_test(mean ~ first)
# homogeneity of covariance (not violated if p>0.001)
box_m(distancesInt[, "mean", drop = FALSE], distancesInt$first)
# two-way mixed ANOVA test
res.aov <- anova_test(
  data = distancesInt, dv = mean, wid = subject,
  between = first, within = taps
)
get_anova_table(res.aov)
# procedure for a significant two-way interaction
# simple main effect of first (kind of first exposure) variable
one.way <- distancesInt %>%
  group_by(taps) %>%
  anova_test(dv=mean, wid=subject, between=first) %>%
  get_anova_table() %>%
  adjust_pvalue(method="bonferroni")
one.way # simple main effect of first (exposure) was significant for the 3-taps condition
# simple main effect of taps variable
one.way2 <- distancesInt %>%
  group_by(first) %>%
  anova_test(dv=mean, wid=subject, within=taps) %>%
  get_anova_table() %>%
  adjust_pvalue(method="bonferroni")
one.way2 # simple main effect of number of taps was significant for the group who was first exposed to 3 taps
# Report:
# There was a statistically significant interaction between group of first exposure and number of taps
# in explaining the mean distance from target, F(1, 8) = 27.39, p<0.001.
# Considering the Bonferroni adjusted p-value, the simple main effect of group of first exposure
# was significant for the 3-taps condition (p<0.0001) but not for 2 taps. 
# Considering the Bonferroni adjusted p-value, the simple main effect of number of taps 
# was significant for first exposure to 3 taps (p=0.012) but not for first exposure to 2 taps. 
# Visualization: boxplots with p-values
pwc <- one.way %>% add_xy_position(x="taps") # gives error
bxp + stat_pvalue_manual(pwc, tip.length=0, hide.ns=TRUE)
# asymmetric skill transfer # sbagliato
pwc2 <- one.way2 %>% add_xy_position(x="half")
bxp2 <- ggboxplot(distancesInt, x="half", "y=mean", color="first", add="mean_se", ylab="mean distance") 
bxp2 + stat_pvalue_manual(pwc2, tip.length=0, hide.ns=TRUE)


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
print("The mean distance was statistically significantly different between first and second half, F(1, 9) = 9.639, p = 0.013, eta2[g] = 0.264.")


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
hist1 <- ggplot(data=traj2taps_v, aes(velo)) + geom_histogram(binwidth = 2) + # speed in pixels/sec
  coord_cartesian(xlim = c(0.0,200)) + xlab("velocity") + ylab("") +
  theme(aspect.ratio=1/1)
hist1
polar1 <- ggplot(data=traj2taps_v, aes(dire)) + geom_histogram(binwidth = pi/90) + # binwidth = 2 degrees
  coord_polar(start=pi/2,direction=-1) + xlim(-pi,pi) + xlab("") + ylab("") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
polar1

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
hist2 <- ggplot(data=traj3taps_v, aes(velo)) + geom_histogram(binwidth = 2) +
  coord_cartesian(xlim = c(0.0,200)) + xlab("velocity") + ylab("") +
  theme(aspect.ratio=1/1)
hist2
polar2 <- ggplot(data=traj3taps_v, aes(dire)) + geom_histogram(binwidth = pi/90) + # binwidth = 2 degrees
  coord_polar(start=pi/2,direction=-1) + xlim(-pi,pi) + xlab("") + ylab("") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
polar2
hist1 + polar1 + hist2 + polar2
