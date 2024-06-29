library(tidyverse)
library(rstatix) 
library(ggpubr)
setwd("/Users/roc/Research/DrawRhythm/TickTack/Experiment_1/")
nasatlx <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("part","order","taps", "question", "answer"))
nasatlx <- nasatlx %>%   # participant 1
  add_row(part=1, order=1, taps=2, question=1, answer=1) %>% 
  add_row(part=1, order=1, taps=2, question=2, answer=-7) %>%
  add_row(part=1, order=1, taps=2, question=3, answer=-7) %>%
  add_row(part=1, order=1, taps=2, question=4, answer=-3) %>%
  add_row(part=1, order=1, taps=2, question=5, answer=2) %>%
  add_row(part=1, order=1, taps=2, question=6, answer=-5) %>%
  add_row(part=1, order=2, taps=3, question=1, answer=3) %>%
  add_row(part=1, order=2, taps=3, question=2, answer=-7) %>%
  add_row(part=1, order=2, taps=3, question=3, answer=-7) %>%
  add_row(part=1, order=2, taps=3, question=4, answer=-6) %>%
  add_row(part=1, order=2, taps=3, question=5, answer=-1) %>%
  add_row(part=1, order=2, taps=3, question=6, answer=-5) 
nasatlx <- nasatlx %>%   # participant 2
  add_row(part=2, order=1, taps=3, question=1, answer=3) %>% 
  add_row(part=2, order=1, taps=3, question=2, answer=-5) %>%
  add_row(part=2, order=1, taps=3, question=3, answer=5) %>%
  add_row(part=2, order=1, taps=3, question=4, answer=-5) %>%
  add_row(part=2, order=1, taps=3, question=5, answer=5) %>%
  add_row(part=2, order=1, taps=3, question=6, answer=6) %>%
  add_row(part=2, order=2, taps=2, question=1, answer=3) %>%
  add_row(part=2, order=2, taps=2, question=2, answer=-6) %>%
  add_row(part=2, order=2, taps=2, question=3, answer=5) %>%
  add_row(part=2, order=2, taps=2, question=4, answer=-5) %>%
  add_row(part=2, order=2, taps=2, question=5, answer=5) %>%
  add_row(part=2, order=2, taps=2, question=6, answer=5) 
nasatlx <- nasatlx %>%   # participant 3
  add_row(part=3, order=1, taps=2, question=1, answer=3) %>% 
  add_row(part=3, order=1, taps=2, question=2, answer=7) %>%
  add_row(part=3, order=1, taps=2, question=3, answer=-7) %>%
  add_row(part=3, order=1, taps=2, question=4, answer=-5) %>%
  add_row(part=3, order=1, taps=2, question=5, answer=2) %>%
  add_row(part=3, order=1, taps=2, question=6, answer=-7) %>%
  add_row(part=3, order=2, taps=3, question=1, answer=4) %>%
  add_row(part=3, order=2, taps=3, question=2, answer=9) %>%
  add_row(part=3, order=2, taps=3, question=3, answer=-9) %>%
  add_row(part=3, order=2, taps=3, question=4, answer=-8) %>%
  add_row(part=3, order=2, taps=3, question=5, answer=6) %>%
  add_row(part=3, order=2, taps=3, question=6, answer=-7) 
nasatlx <- nasatlx %>%   # participant 4
  add_row(part=4, order=1, taps=3, question=1, answer=4) %>% 
  add_row(part=4, order=1, taps=3, question=2, answer=3) %>%
  add_row(part=4, order=1, taps=3, question=3, answer=-3) %>%
  add_row(part=4, order=1, taps=3, question=4, answer=3) %>%
  add_row(part=4, order=1, taps=3, question=5, answer=3) %>%
  add_row(part=4, order=1, taps=3, question=6, answer=-5) %>%
  add_row(part=4, order=2, taps=2, question=1, answer=2) %>%
  add_row(part=4, order=2, taps=2, question=2, answer=4) %>%
  add_row(part=4, order=2, taps=2, question=3, answer=-3) %>%
  add_row(part=4, order=2, taps=2, question=4, answer=3) %>%
  add_row(part=4, order=2, taps=2, question=5, answer=2) %>%
  add_row(part=4, order=2, taps=2, question=6, answer=-7)   
nasatlx <- nasatlx %>%   # participant 5
  add_row(part=5, order=1, taps=2, question=1, answer=6) %>% 
  add_row(part=5, order=1, taps=2, question=2, answer=-10) %>%
  add_row(part=5, order=1, taps=2, question=3, answer=-9) %>%
  add_row(part=5, order=1, taps=2, question=4, answer=-8) %>%
  add_row(part=5, order=1, taps=2, question=5, answer=-1) %>%
  add_row(part=5, order=1, taps=2, question=6, answer=-10) %>%
  add_row(part=5, order=2, taps=3, question=1, answer=4) %>%
  add_row(part=5, order=2, taps=3, question=2, answer=-10) %>%
  add_row(part=5, order=2, taps=3, question=3, answer=-10) %>%
  add_row(part=5, order=2, taps=3, question=4, answer=-9) %>%
  add_row(part=5, order=2, taps=3, question=5, answer=-2) %>%
  add_row(part=5, order=2, taps=3, question=6, answer=-10) 
nasatlx <- nasatlx %>%   # participant 6
  add_row(part=6, order=1, taps=3, question=1, answer=-1) %>% 
  add_row(part=6, order=1, taps=3, question=2, answer=-7) %>%
  add_row(part=6, order=1, taps=3, question=3, answer=5) %>%
  add_row(part=6, order=1, taps=3, question=4, answer=-3) %>%
  add_row(part=6, order=1, taps=3, question=5, answer=2) %>%
  add_row(part=6, order=1, taps=3, question=6, answer=-4) %>%
  add_row(part=6, order=2, taps=2, question=1, answer=-5) %>%
  add_row(part=6, order=2, taps=2, question=2, answer=-3) %>%
  add_row(part=6, order=2, taps=2, question=3, answer=5) %>%
  add_row(part=6, order=2, taps=2, question=4, answer=3) %>%
  add_row(part=6, order=2, taps=2, question=5, answer=2) %>%
  add_row(part=6, order=2, taps=2, question=6, answer=-4) 
nasatlx <- nasatlx %>%   # participant 7
  add_row(part=7, order=1, taps=2, question=1, answer=3) %>% 
  add_row(part=7, order=1, taps=2, question=2, answer=-6) %>%
  add_row(part=7, order=1, taps=2, question=3, answer=-7) %>%
  add_row(part=7, order=1, taps=2, question=4, answer=-1) %>%
  add_row(part=7, order=1, taps=2, question=5, answer=-4) %>%
  add_row(part=7, order=1, taps=2, question=6, answer=6) %>%
  add_row(part=7, order=2, taps=3, question=1, answer=-6) %>%
  add_row(part=7, order=2, taps=3, question=2, answer=-8) %>%
  add_row(part=7, order=2, taps=3, question=3, answer=-8) %>%
  add_row(part=7, order=2, taps=3, question=4, answer=-5) %>%
  add_row(part=7, order=2, taps=3, question=5, answer=0) %>%
  add_row(part=7, order=2, taps=3, question=6, answer=1) 
nasatlx <- nasatlx %>%   # participant 8
  add_row(part=8, order=1, taps=3, question=1, answer=6) %>% 
  add_row(part=8, order=1, taps=3, question=2, answer=-10) %>%
  add_row(part=8, order=1, taps=3, question=3, answer=-7) %>%
  add_row(part=8, order=1, taps=3, question=4, answer=-1) %>%
  add_row(part=8, order=1, taps=3, question=5, answer=5) %>%
  add_row(part=8, order=1, taps=3, question=6, answer=-8) %>%
  add_row(part=8, order=2, taps=2, question=1, answer=4) %>%
  add_row(part=8, order=2, taps=2, question=2, answer=-10) %>%
  add_row(part=8, order=2, taps=2, question=3, answer=-9) %>%
  add_row(part=8, order=2, taps=2, question=4, answer=5) %>%
  add_row(part=8, order=2, taps=2, question=5, answer=0) %>%
  add_row(part=8, order=2, taps=2, question=6, answer=-9) 
nasatlx <- nasatlx %>%   # participant 9
  add_row(part=9, order=1, taps=2, question=1, answer=-5) %>% 
  add_row(part=9, order=1, taps=2, question=2, answer=-8) %>%
  add_row(part=9, order=1, taps=2, question=3, answer=-7) %>%
  add_row(part=9, order=1, taps=2, question=4, answer=4) %>%
  add_row(part=9, order=1, taps=2, question=5, answer=2) %>%
  add_row(part=9, order=1, taps=2, question=6, answer=-9) %>%
  add_row(part=9, order=2, taps=3, question=1, answer=-2) %>%
  add_row(part=9, order=2, taps=3, question=2, answer=-4) %>%
  add_row(part=9, order=2, taps=3, question=3, answer=-9) %>%
  add_row(part=9, order=2, taps=3, question=4, answer=-7) %>%
  add_row(part=9, order=2, taps=3, question=5, answer=-2) %>%
  add_row(part=9, order=2, taps=3, question=6, answer=-10) 
nasatlx <- nasatlx %>%   # participant 10
  add_row(part=10, order=1, taps=3, question=1, answer=6) %>% 
  add_row(part=10, order=1, taps=3, question=2, answer=-1) %>%
  add_row(part=10, order=1, taps=3, question=3, answer=4) %>%
  add_row(part=10, order=1, taps=3, question=4, answer=-7) %>%
  add_row(part=10, order=1, taps=3, question=5, answer=4) %>%
  add_row(part=10, order=1, taps=3, question=6, answer=6) %>%
  add_row(part=10, order=2, taps=2, question=1, answer=-1) %>%
  add_row(part=10, order=2, taps=2, question=2, answer=-4) %>%
  add_row(part=10, order=2, taps=2, question=3, answer=-1) %>%
  add_row(part=10, order=2, taps=2, question=4, answer=7) %>%
  add_row(part=10, order=2, taps=2, question=5, answer=-3) %>%
  add_row(part=10, order=2, taps=2, question=6, answer=-4) 

aggregate(nasatlx, by = list(nasatlx$question), FUN = mean)

# --------- testing differences between 2 and 3 taps
# --------- non parametric testing 
questionN <- 4 # set the question number here (1 to 6)
nasa2taps <- nasatlx$answer[nasatlx$taps == 2 & nasatlx$question == questionN]
nasa3taps <- nasatlx$answer[nasatlx$taps == 3 & nasatlx$question == questionN]
# datagroups
taps <- factor(rep(c("2taps", "3taps"), c(length(nasa2taps),length(nasa3taps))))
id <- factor(c(1:length(nasa2taps), 1:length(nasa3taps)))
# data frame
answers <- tibble(id, taps, c(nasa2taps,nasa3taps))
names(answers) <- c("id", "taps", "answer")
# summary statistics
answers %>%
  group_by(taps) %>%
  get_summary_stats(answer, type = "median_iqr")
  # get_summary_stats(answer, type = "mean_sd")
nasaquestion <- nasatlx[nasatlx$question==questionN,]
aggregate(nasaquestion, by = list(nasaquestion$taps), FUN = mean)
stat.test <- answers %>%
  wilcox_test(answer ~ taps, paired = TRUE) %>% add_significance()
stat.test
# effect size
answers %>%
  wilcox_effsize(answer ~ taps, paired = TRUE)
# report with visualization
stat.test <- stat.test %>% add_xy_position(x = "taps") 
# visualization
bxp <- ggpaired(answers, x = "taps", y = "answer", order = c("2taps", "3taps"),
                ylab = "answer", xlab = "taps")
bxp + stat_pvalue_manual(stat.test, tip.length = 0) + labs(subtitle = get_test_label(stat.test, detailed= TRUE))

# --------- testing the progress between first and second half
# --------- non parametric testing 
questionN <- 3 # set the question number here (1 to 6)
nasa1 <- nasatlx$answer[nasatlx$order == 1 & nasatlx$question == questionN]
nasa2 <- nasatlx$answer[nasatlx$order == 2 & nasatlx$question == questionN]
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
nasaquestion <- nasatlx[nasatlx$question==questionN,]
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

