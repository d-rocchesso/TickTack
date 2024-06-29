library(tidyverse)
library(rstatix) 
library(ggpubr)
library(patchwork)
library(R.utils)
library(lubridate)
library(pwr)
scale = 0.5625
# Compute distance to target and statistics for hide and noHide 
setwd("/Users/roc/Research/DrawRhythm/TickTack/Experiment_2/")
ghost <- read.csv("../Data/usa_2013.csv")
files <- list.files(".", pattern="path_follow_[0-9]+.csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
exclude <- c(c(5)*2 - 1, c(5)*2) ;
files <- files[-exclude]
distances <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("hiding", "half", "mean", "sd"))
half <- 0
for (cf in files) {
  print(cf)
  # secs <- 
  # millisecs <- system(paste("gdate +%s%N -r ", cf), intern=TRUE) 
  # ms <- as.numeric(sub("^.*(...)$", "\\1", sub('.{6}$','',millisecs))) # ms of modif. time
  traj <- read.csv(cf)
  fine <- lastModified(cf)
  durata <- (traj$time[nrow(traj)] - traj$time[1]) # in ms
  inizio <- fine - seconds(durata/1000)
  timestamp <- seq(inizio,fine,length.out=nrow(traj))
  traj <- traj %>% mutate(timestamp) # add column of timestamp
  hide <- (second(timestamp)<5) | (second(timestamp)>=10) & (second(timestamp)<15) |
    (second(timestamp)>=20) & (second(timestamp)<25) |
    (second(timestamp)>=30) & (second(timestamp)<35) |
    (second(timestamp)>=40) & (second(timestamp)<45) |
    (second(timestamp)>=50) & (second(timestamp)<55) 
  traj <- traj %>% mutate(hide) # add column of hiding segments
  
  if ("hiding...true" %in% colnames(traj))
    print(ggplot(data=traj) + geom_point(mapping=aes(x=x,y=y,color=hide),shape=20,alpha=0.03) + 
            scale_color_manual(values = c("red", "#FFCC00")) +
            geom_point(mapping=aes(x=xGhost,y=yGhost), color="blue", shape=20, size=0.6, alpha=0.03) +
            # guides(color = guide_legend(override.aes = list(size = 5, alpha=1)) +
            theme(legend.position="none"))
  else
    print(ggplot(data=traj) + geom_point(mapping=aes(x=x,y=y,color="red"),shape=20,alpha=0.03) + 
            geom_point(mapping=aes(x=xGhost,y=yGhost), color="blue", shape=20, size=0.6, alpha=0.03) +
            theme(legend.position="none"))
  # readline()
  distTraj <- sqrt((traj$x - traj$xGhost)^2 + (traj$y - traj$yGhost)^2)
  summary(distTraj)
  
  
  if ("hiding...false" %in% colnames(traj)) {
    print("No hiding")
    distances <- add_row(distances, hiding = FALSE, half = half+1, mean = mean(distTraj,na.rm=T), sd = sd(distTraj,na.rm=T))
  }
  if ("hiding...true" %in% colnames(traj)) {
    print("Hiding")
    distances <- add_row(distances, hiding = TRUE, half = half+1, mean = mean(distTraj,na.rm=T), sd = sd(distTraj,na.rm=T))
  }
  half <- !half
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
  # get_summary_stats(meanDistance, type = "median_iqr") # %>%
  get_summary_stats(meanDistance, type = "mean_sd")
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

# Power of the test, indicating ideal number of participants
power.t.test(delta=8, sd=40, power=.85, type="paired", alternative="two.sided",sig.level=0.05)
pwr.t.test(n=16,d=0.8,type='paired',sig.level=0.05)

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
  # get_summary_stats(meanDistance, type = "median_iqr") # %>%
  get_summary_stats(meanDistance, type = "mean_sd")
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

# Power of the test, indicating ideal number of participants
power.t.test(delta=19, sd=41.76, power=.85, type="paired", alternative="two.sided",sig.level=0.05)


# ******* mixed anova ******
# Hiding (true or false) is within subjects
# order (hiding first or hiding second) is between subjects
# https://www.datanovia.com/en/lessons/mixed-anova-in-r/
subject <- rep(1:(nrow(distances)/2), each=2)
hideFirst <- distances$hiding[rep(seq(1,nrow(distances),2),each=2)]
distancesInt <- distances %>% mutate(subject) %>% mutate(hideFirst)
distancesInt$hh <- interaction(distancesInt$hiding,distancesInt$hideFirst)
interaction.plot(distancesInt$hideFirst,distancesInt$hiding,distancesInt$mean)
# Summary statistics 
distancesInt %>%
  group_by(hiding, hideFirst) %>%
  get_summary_stats(mean, type="mean_sd")
# visualization
bxp <- ggboxplot(
  # distancesInt, x="hiding", y="mean", color="hideFirst", palette="jco", ylab="mean distance"#, add="jitter"
  distancesInt, x="half", y="mean", color="hiding", palette="jco", ylab="mean distance"
)
bxp
# outliers
distancesInt %>%
  group_by(hiding, hideFirst) %>%
  identify_outliers(mean)
# normality assumption
distancesInt %>%
  group_by(hiding, hideFirst) %>%
  shapiro_test(mean)
ggqqplot(distancesInt, "mean", ggtheme = theme_bw()) +
  facet_grid(hiding ~ hideFirst)
# homogeneity of variance
distancesInt %>%
  group_by(hiding) %>%
  levene_test(mean ~ hideFirst)
# homogeneity of covariance (not violated if p>0.001)
box_m(distancesInt[, "mean", drop = FALSE], distancesInt$hideFirst)
# two-way mixed ANOVA test
res.aov <- anova_test(
  data = distancesInt, dv = mean, wid = subject,
  between = hideFirst, within = hiding
)
get_anova_table(res.aov)
# procedure for a significant two-way interaction
# simple main effect of hideFirst (kind of first exposure) variable
one.way <- distancesInt %>%
  group_by(hiding) %>%
  anova_test(dv=mean, wid=subject, between=hideFirst) %>%
  get_anova_table() # %>%
  #  adjust_pvalue(method="bonferroni")
one.way # simple main effect of hideFirst (exposure) was almost significant for hiding
# simple main effect of hiding variable
one.way2 <- distancesInt %>%
  group_by(hideFirst) %>%
  anova_test(dv=mean, wid=subject, within=hiding) %>%
  get_anova_table() # %>%
  # adjust_pvalue(method="bonferroni")
one.way2 # simple main effect of number of taps was significant for the group who was first exposed to 3 taps
# Report:
# There was a statistically significant yet small interaction between group of first exposure and hiding condition
# in explaining the mean distance from target, F(1, 14) = 5.454, p<0.05, ges=0.060.
# The simple main effect of group of first exposure
# was significant for the hiding condition (p<0.05) but not for no hiding. 
# The simple main effect of hiding 
# was not significant for neither kind of first exposure.
# Visualization: boxplots with p-values
pwc <- one.way %>% add_xy_position(x="hide") # gives error
bxp + stat_pvalue_manual(pwc, tip.length=0, hide.ns=TRUE)

# asymmetric skill transfer
# ggboxplot(distancesInt, x="half", "y=mean", color="hiding", add="mean_se", ylab="mean distance") 
ggboxplot(distancesInt, x="half", "y=mean", color="hideFirst", add="mean_se", ylab="mean distance") +
  labs(color="hiding at first exposure")
int.aov <- anova_test(distancesInt, dv=mean, wid=subject, between=hideFirst, within=hiding)
get_anova_table(int.aov)
summary(int.aov)

# First and second half separately
ow1 <- anova_test(data = distancesInt[distancesInt$half == '1',], mean ~ hiding)
get_anova_table(ow1)
ow2 <- anova_test(data = distancesInt[distancesInt$half == '2',], mean ~ hiding)
get_anova_table(ow2)


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
tNh_v <- trajNohide_v %>% filter(abs(velo)>.001) %>% filter(abs(dire)>.001 & abs(dire)<3.141) %>% filter(abs(dire)<1.57 | abs(dire)>1.58) 
hist1 <- ggplot(data=tNh_v, aes(velo)) + geom_histogram(binwidth = 2) + # speed in pixels/sec
  coord_cartesian(xlim = c(0.0,200)) + xlab("velocity") + ylab("") +
  theme(aspect.ratio=1/1)
hist1
polar1 <- ggplot(data=tNh_v, aes(dire)) + geom_histogram(binwidth = 3*pi/90) + # binwidth = 3 degrees
  coord_polar(start=pi/2,direction=-1) + xlim(-pi,pi) + xlab("") + ylab("") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
polar1
hist1 + polar1 
