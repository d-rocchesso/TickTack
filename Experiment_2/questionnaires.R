library(tidyverse)
library(rstatix) 
library(ggpubr)
setwd("/Users/roc/Research/DrawRhythm/TickTack/Experiment_2/")

age = c(22, 23, 39, 38, 34, 35, 28, 25, 29, 30, 23, 23, 22, 29, 29, 31, 28)
exclude = c(5) #remove participants 5, ...
median(age[-exclude]) 
IQR(age[-exclude])

nasatlx <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("part","order","hide", "question", "answer"))
nasatlx <- nasatlx %>%   # participant 1
  add_row(part=1, order=1, hide=0, question=1, answer=-6) %>% 
  add_row(part=1, order=1, hide=0, question=2, answer=-8) %>%
  add_row(part=1, order=1, hide=0, question=3, answer=-10) %>%
  add_row(part=1, order=1, hide=0, question=4, answer=-2) %>%
  add_row(part=1, order=1, hide=0, question=5, answer=3) %>%
  add_row(part=1, order=1, hide=0, question=6, answer=-7) %>%
  add_row(part=1, order=2, hide=1, question=1, answer=-6) %>%
  add_row(part=1, order=2, hide=1, question=2, answer=-7) %>%
  add_row(part=1, order=2, hide=1, question=3, answer=-10) %>%
  add_row(part=1, order=2, hide=1, question=4, answer=4) %>%
  add_row(part=1, order=2, hide=1, question=5, answer=5) %>%
  add_row(part=1, order=2, hide=1, question=6, answer=-8) 
nasatlx <- nasatlx %>%   # participant 2
  add_row(part=2, order=1, hide=1, question=1, answer=5) %>% 
  add_row(part=2, order=1, hide=1, question=2, answer=2) %>%
  add_row(part=2, order=1, hide=1, question=3, answer=3) %>%
  add_row(part=2, order=1, hide=1, question=4, answer=1) %>%
  add_row(part=2, order=1, hide=1, question=5, answer=4) %>%
  add_row(part=2, order=1, hide=1, question=6, answer=4) %>%
  add_row(part=2, order=2, hide=0, question=1, answer=-4) %>%
  add_row(part=2, order=2, hide=0, question=2, answer=-5) %>%
  add_row(part=2, order=2, hide=0, question=3, answer=-3) %>%
  add_row(part=2, order=2, hide=0, question=4, answer=-8) %>%
  add_row(part=2, order=2, hide=0, question=5, answer=-3) %>%
  add_row(part=2, order=2, hide=0, question=6, answer=-7) 
nasatlx <- nasatlx %>%   # participant 3
  add_row(part=3, order=1, hide=0, question=1, answer=-4) %>% 
  add_row(part=3, order=1, hide=0, question=2, answer=-8) %>%
  add_row(part=3, order=1, hide=0, question=3, answer=3) %>%
  add_row(part=3, order=1, hide=0, question=4, answer=-3) %>%
  add_row(part=3, order=1, hide=0, question=5, answer=4) %>%
  add_row(part=3, order=1, hide=0, question=6, answer=-8) %>%
  add_row(part=3, order=2, hide=1, question=1, answer=3) %>%
  add_row(part=3, order=2, hide=1, question=2, answer=-8) %>%
  add_row(part=3, order=2, hide=1, question=3, answer=-1) %>%
  add_row(part=3, order=2, hide=1, question=4, answer=-1) %>%
  add_row(part=3, order=2, hide=1, question=5, answer=6) %>%
  add_row(part=3, order=2, hide=1, question=6, answer=-5) 
nasatlx <- nasatlx %>%   # participant 4
  add_row(part=4, order=1, hide=1, question=1, answer=-4) %>% 
  add_row(part=4, order=1, hide=1, question=2, answer=-9) %>%
  add_row(part=4, order=1, hide=1, question=3, answer=-9) %>%
  add_row(part=4, order=1, hide=1, question=4, answer=4) %>%
  add_row(part=4, order=1, hide=1, question=5, answer=6) %>%
  add_row(part=4, order=1, hide=1, question=6, answer=-9) %>%
  add_row(part=4, order=2, hide=0, question=1, answer=-4) %>%
  add_row(part=4, order=2, hide=0, question=2, answer=-9) %>%
  add_row(part=4, order=2, hide=0, question=3, answer=-10) %>%
  add_row(part=4, order=2, hide=0, question=4, answer=-1) %>%
  add_row(part=4, order=2, hide=0, question=5, answer=2) %>%
  add_row(part=4, order=2, hide=0, question=6, answer=-9)   
nasatlx <- nasatlx %>%   # participant 5
  add_row(part=5, order=1, hide=0, question=1, answer=4) %>% 
  add_row(part=5, order=1, hide=0, question=2, answer=-10) %>%
  add_row(part=5, order=1, hide=0, question=3, answer=-10) %>%
  add_row(part=5, order=1, hide=0, question=4, answer=-1) %>%
  add_row(part=5, order=1, hide=0, question=5, answer=5) %>%
  add_row(part=5, order=1, hide=0, question=6, answer=-1) %>%
  add_row(part=5, order=2, hide=1, question=1, answer=5) %>%
  add_row(part=5, order=2, hide=1, question=2, answer=-10) %>%
  add_row(part=5, order=2, hide=1, question=3, answer=-10) %>%
  add_row(part=5, order=2, hide=1, question=4, answer=3) %>%
  add_row(part=5, order=2, hide=1, question=5, answer=6) %>%
  add_row(part=5, order=2, hide=1, question=6, answer=4) 
nasatlx <- nasatlx %>%   # participant 6
  add_row(part=6, order=1, hide=1, question=1, answer=4) %>% 
  add_row(part=6, order=1, hide=1, question=2, answer=-7) %>%
  add_row(part=6, order=1, hide=1, question=3, answer=-9) %>%
  add_row(part=6, order=1, hide=1, question=4, answer=-1) %>%
  add_row(part=6, order=1, hide=1, question=5, answer=5) %>%
  add_row(part=6, order=1, hide=1, question=6, answer=-1) %>%
  add_row(part=6, order=2, hide=0, question=1, answer=2) %>%
  add_row(part=6, order=2, hide=0, question=2, answer=-8) %>%
  add_row(part=6, order=2, hide=0, question=3, answer=-9) %>%
  add_row(part=6, order=2, hide=0, question=4, answer=-4) %>%
  add_row(part=6, order=2, hide=0, question=5, answer=3) %>%
  add_row(part=6, order=2, hide=0, question=6, answer=-3) 
nasatlx <- nasatlx %>%   # participant 7 
  add_row(part=7, order=1, hide=1, question=1, answer=1) %>% 
  add_row(part=7, order=1, hide=1, question=2, answer=-8) %>%
  add_row(part=7, order=1, hide=1, question=3, answer=3) %>%
  add_row(part=7, order=1, hide=1, question=4, answer=-3) %>%
  add_row(part=7, order=1, hide=1, question=5, answer=6) %>%
  add_row(part=7, order=1, hide=1, question=6, answer=-2) %>%
  add_row(part=7, order=2, hide=0, question=1, answer=7) %>%
  add_row(part=7, order=2, hide=0, question=2, answer=-8) %>%
  add_row(part=7, order=2, hide=0, question=3, answer=4) %>%
  add_row(part=7, order=2, hide=0, question=4, answer=1) %>%
  add_row(part=7, order=2, hide=0, question=5, answer=5) %>%
  add_row(part=7, order=2, hide=0, question=6, answer=6) 
nasatlx <- nasatlx %>%   # participant 8
  add_row(part=8, order=1, hide=1, question=1, answer=-3) %>% 
  add_row(part=8, order=1, hide=1, question=2, answer=-7) %>%
  add_row(part=8, order=1, hide=1, question=3, answer=-3) %>%
  add_row(part=8, order=1, hide=1, question=4, answer=-7) %>%
  add_row(part=8, order=1, hide=1, question=5, answer=-3) %>%
  add_row(part=8, order=1, hide=1, question=6, answer=-10) %>%
  add_row(part=8, order=2, hide=0, question=1, answer=-6) %>%
  add_row(part=8, order=2, hide=0, question=2, answer=-7) %>%
  add_row(part=8, order=2, hide=0, question=3, answer=-5) %>%
  add_row(part=8, order=2, hide=0, question=4, answer=-9) %>%
  add_row(part=8, order=2, hide=0, question=5, answer=-6) %>%
  add_row(part=8, order=2, hide=0, question=6, answer=-10) 
nasatlx <- nasatlx %>%   # participant 9
  add_row(part=9, order=1, hide=0, question=1, answer=2) %>% 
  add_row(part=9, order=1, hide=0, question=2, answer=-8) %>%
  add_row(part=9, order=1, hide=0, question=3, answer=1) %>%
  add_row(part=9, order=1, hide=0, question=4, answer=-3) %>%
  add_row(part=9, order=1, hide=0, question=5, answer=1) %>%
  add_row(part=9, order=1, hide=0, question=6, answer=-9) %>%
  add_row(part=9, order=2, hide=1, question=1, answer=2) %>%
  add_row(part=9, order=2, hide=1, question=2, answer=-8) %>%
  add_row(part=9, order=2, hide=1, question=3, answer=2) %>%
  add_row(part=9, order=2, hide=1, question=4, answer=-1) %>%
  add_row(part=9, order=2, hide=1, question=5, answer=4) %>%
  add_row(part=9, order=2, hide=1, question=6, answer=-1) 
nasatlx <- nasatlx %>%   # participant 10
  add_row(part=10, order=1, hide=1, question=1, answer=-1) %>% 
  add_row(part=10, order=1, hide=1, question=2, answer=-8) %>%
  add_row(part=10, order=1, hide=1, question=3, answer=-10) %>%
  add_row(part=10, order=1, hide=1, question=4, answer=-1) %>%
  add_row(part=10, order=1, hide=1, question=5, answer=-1) %>%
  add_row(part=10, order=1, hide=1, question=6, answer=-8) %>%
  add_row(part=10, order=2, hide=0, question=1, answer=-3) %>%
  add_row(part=10, order=2, hide=0, question=2, answer=-8) %>%
  add_row(part=10, order=2, hide=0, question=3, answer=-10) %>%
  add_row(part=10, order=2, hide=0, question=4, answer=-4) %>%
  add_row(part=10, order=2, hide=0, question=5, answer=-1) %>%
  add_row(part=10, order=2, hide=0, question=6, answer=-6) 
nasatlx <- nasatlx %>%   # participant 11
  add_row(part=11, order=1, hide=0, question=1, answer=-1) %>% 
  add_row(part=11, order=1, hide=0, question=2, answer=-7) %>%
  add_row(part=11, order=1, hide=0, question=3, answer=2) %>%
  add_row(part=11, order=1, hide=0, question=4, answer=-3) %>%
  add_row(part=11, order=1, hide=0, question=5, answer=2) %>%
  add_row(part=11, order=1, hide=0, question=6, answer=5) %>%
  add_row(part=11, order=2, hide=1, question=1, answer=7) %>%
  add_row(part=11, order=2, hide=1, question=2, answer=-1) %>%
  add_row(part=11, order=2, hide=1, question=3, answer=7) %>%
  add_row(part=11, order=2, hide=1, question=4, answer=-2) %>%
  add_row(part=11, order=2, hide=1, question=5, answer=6) %>%
  add_row(part=11, order=2, hide=1, question=6, answer=6) 
nasatlx <- nasatlx %>%   # participant 12
  add_row(part=12, order=1, hide=1, question=1, answer=-4) %>% 
  add_row(part=12, order=1, hide=1, question=2, answer=-5) %>%
  add_row(part=12, order=1, hide=1, question=3, answer=-6) %>%
  add_row(part=12, order=1, hide=1, question=4, answer=-5) %>%
  add_row(part=12, order=1, hide=1, question=5, answer=3) %>%
  add_row(part=12, order=1, hide=1, question=6, answer=1) %>%
  add_row(part=12, order=2, hide=0, question=1, answer=-7) %>%
  add_row(part=12, order=2, hide=0, question=2, answer=-3) %>%
  add_row(part=12, order=2, hide=0, question=3, answer=-8) %>%
  add_row(part=12, order=2, hide=0, question=4, answer=-1) %>%
  add_row(part=12, order=2, hide=0, question=5, answer=-1) %>%
  add_row(part=12, order=2, hide=0, question=6, answer=-4) 
nasatlx <- nasatlx %>%   # participant 13
  add_row(part=13, order=1, hide=0, question=1, answer=2) %>% 
  add_row(part=13, order=1, hide=0, question=2, answer=1) %>%
  add_row(part=13, order=1, hide=0, question=3, answer=7) %>%
  add_row(part=13, order=1, hide=0, question=4, answer=-8) %>%
  add_row(part=13, order=1, hide=0, question=5, answer=-1) %>%
  add_row(part=13, order=1, hide=0, question=6, answer=-8) %>%
  add_row(part=13, order=2, hide=1, question=1, answer=6) %>%
  add_row(part=13, order=2, hide=1, question=2, answer=4) %>%
  add_row(part=13, order=2, hide=1, question=3, answer=7) %>%
  add_row(part=13, order=2, hide=1, question=4, answer=-7) %>%
  add_row(part=13, order=2, hide=1, question=5, answer=3) %>%
  add_row(part=13, order=2, hide=1, question=6, answer=-4) 
nasatlx <- nasatlx %>%   # participant 14
  add_row(part=14, order=1, hide=0, question=1, answer=-7) %>% 
  add_row(part=14, order=1, hide=0, question=2, answer=-9) %>%
  add_row(part=14, order=1, hide=0, question=3, answer=-9) %>%
  add_row(part=14, order=1, hide=0, question=4, answer=-5) %>%
  add_row(part=14, order=1, hide=0, question=5, answer=5) %>%
  add_row(part=14, order=1, hide=0, question=6, answer=-2) %>%
  add_row(part=14, order=2, hide=1, question=1, answer=-4) %>%
  add_row(part=14, order=2, hide=1, question=2, answer=-9) %>%
  add_row(part=14, order=2, hide=1, question=3, answer=-5) %>%
  add_row(part=14, order=2, hide=1, question=4, answer=-4) %>%
  add_row(part=14, order=2, hide=1, question=5, answer=7) %>%
  add_row(part=14, order=2, hide=1, question=6, answer=3) 
nasatlx <- nasatlx %>%   # participant 15
  add_row(part=15, order=1, hide=0, question=1, answer=3) %>% 
  add_row(part=15, order=1, hide=0, question=2, answer=2) %>%
  add_row(part=15, order=1, hide=0, question=3, answer=4) %>%
  add_row(part=15, order=1, hide=0, question=4, answer=5) %>%
  add_row(part=15, order=1, hide=0, question=5, answer=6) %>%
  add_row(part=15, order=1, hide=0, question=6, answer=1) %>%
  add_row(part=15, order=2, hide=1, question=1, answer=4) %>%
  add_row(part=15, order=2, hide=1, question=2, answer=2) %>%
  add_row(part=15, order=2, hide=1, question=3, answer=4) %>%
  add_row(part=15, order=2, hide=1, question=4, answer=4) %>%
  add_row(part=15, order=2, hide=1, question=5, answer=7) %>%
  add_row(part=15, order=2, hide=1, question=6, answer=2) 
nasatlx <- nasatlx %>%   # participant 16
  add_row(part=16, order=1, hide=1, question=1, answer=7) %>% 
  add_row(part=16, order=1, hide=1, question=2, answer=-4) %>%
  add_row(part=16, order=1, hide=1, question=3, answer=1) %>%
  add_row(part=16, order=1, hide=1, question=4, answer=-3) %>%
  add_row(part=16, order=1, hide=1, question=5, answer=9) %>%
  add_row(part=16, order=1, hide=1, question=6, answer=-5) %>%
  add_row(part=16, order=2, hide=0, question=1, answer=5) %>%
  add_row(part=16, order=2, hide=0, question=2, answer=-4) %>%
  add_row(part=16, order=2, hide=0, question=3, answer=-1) %>%
  add_row(part=16, order=2, hide=0, question=4, answer=-7) %>%
  add_row(part=16, order=2, hide=0, question=5, answer=8) %>%
  add_row(part=16, order=2, hide=0, question=6, answer=-4) 
nasatlx <- nasatlx %>%   # participant 17
  add_row(part=17, order=1, hide=1, question=1, answer=-5) %>% 
  add_row(part=17, order=1, hide=1, question=2, answer=-8) %>%
  add_row(part=17, order=1, hide=1, question=3, answer=-2) %>%
  add_row(part=17, order=1, hide=1, question=4, answer=-6) %>%
  add_row(part=17, order=1, hide=1, question=5, answer=-2) %>%
  add_row(part=17, order=1, hide=1, question=6, answer=-5) %>%
  add_row(part=17, order=2, hide=0, question=1, answer=-6) %>%
  add_row(part=17, order=2, hide=0, question=2, answer=-8) %>%
  add_row(part=17, order=2, hide=0, question=3, answer=-3) %>%
  add_row(part=17, order=2, hide=0, question=4, answer=-7) %>%
  add_row(part=17, order=2, hide=0, question=5, answer=-5) %>%
  add_row(part=17, order=2, hide=0, question=6, answer=-7) 
aggregate(nasatlx[nasatlx$part!=exclude,], by = list(nasatlx$question[nasatlx$part!=exclude]), FUN = mean)

# --------- testing differences between Hide and noHide
# --------- non parametric testing 
questionN <- 6 # set the question number here (1 to 6)
nasaNoHide <- nasatlx$answer[nasatlx$hide == 0 & nasatlx$question == questionN]
nasaHide <- nasatlx$answer[nasatlx$hide == 1 & nasatlx$question == questionN]
# datagroups
hides <- factor(rep(c("Hide", "NoHide"), c(length(nasaHide),length(nasaNoHide))))
id <- factor(c(1:length(nasaHide), 1:length(nasaNoHide)))
# data frame
answers <- tibble(id, hides, c(nasaHide,nasaNoHide))
names(answers) <- c("id", "hide", "answer")
# summary statistics
answers[answers$id!=exclude,] %>%
  group_by(hide) %>%
  get_summary_stats(answer, type = "median_iqr")
  # get_summary_stats(answer, type = "mean_sd")
nasaquestion <- nasatlx[nasatlx$part!=exclude & nasatlx$question==questionN,]
aggregate(nasaquestion, by = list(nasaquestion$hide), FUN = mean)
stat.test <- answers %>%
  wilcox_test(answer ~ hide, paired = TRUE) %>% add_significance()
stat.test
# effect size
answers %>%
  wilcox_effsize(answer ~ hide, paired = TRUE)
# report with visualization
stat.test <- stat.test %>% add_xy_position(x = "hide") 
# visualization
bxp <- ggpaired(answers, x = "hide", y = "answer", order = c("Hide", "NoHide"),
                ylab = "answer", xlab = "hide")
bxp + stat_pvalue_manual(stat.test, tip.length = 0) + labs(subtitle = get_test_label(stat.test, detailed= TRUE))


# --------- testing the progress between first and second half
# --------- non parametric testing 
questionN <- 6 # set the question number here (1 to 6)
nasa1 <- nasatlx$answer[nasatlx$part!=exclude & nasatlx$order == 1 & nasatlx$question == questionN]
nasa2 <- nasatlx$answer[nasatlx$part!=exclude & nasatlx$order == 2 & nasatlx$question == questionN]
# datagroups
order <- factor(rep(c("first", "second"), c(length(nasa1),length(nasa2))))
id <- factor(c(1:length(nasa1), 1:length(nasa2)))
# data frame
answersord <- tibble(id, order, c(nasa1,nasa2))
names(answersord) <- c("id", "order", "answer")
# summary statistics
answersord %>%
  group_by(order) %>%
  get_summary_stats(answer, type = "median_iqr")
# get_summary_stats(answer, type = "mean_sd")
nasaquestion <- nasatlx[nasatlx$part!=exclude & nasatlx$question==questionN,]
aggregate(nasaquestion, by = list(nasaquestion$order), FUN = mean)
stat.test <- answersord %>%
  wilcox_test(answer ~ order, paired = TRUE) %>% add_significance() # sign_test would not rely on symmetry, but gives similar results
stat.test
# effect size
answersord %>%
  wilcox_effsize(answer ~ order, paired = TRUE)
# report with visualization
stat.test <- stat.test %>% add_xy_position(x = "order") 
# visualization
bxp <- ggpaired(answersord, x = "order", y = "answer", order = c("first", "second"),
                ylab = "answer", xlab = "order")
bxp + stat_pvalue_manual(stat.test, tip.length = 0) + labs(subtitle = get_test_label(stat.test, detailed= TRUE))

