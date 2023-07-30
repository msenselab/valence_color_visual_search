# color association analysis
library(knitr)
library(MASS)
library(tidyverse)
library(ggpubr)
library(rstatix) # using tidyverse friendly statistics
library(BayesFactor) # in case need Bayes factor analysis
library(bayestestR)
library(cowplot)
library(WRS2)
library(stats)
library(coda)
library(lsr)
library(ggpattern)
library(nlme)

# kill the twin process
#kill_the_twin <- function(df) {
#  trl_err = df %>% filter(Correct == 0) %>% arrange(RT)
#  trl_cor = df %>% filter(Correct == 1) %>% arrange(RT)
#  
#  for (i in 1:nrow(trl_err)){
#    err_rt = trl_err$RT[i] #select one RT from the error trials
#    idx = max(1, which.min(trl_cor$RT<=err_rt)-1) # fine the match one
#    trl_cor = trl_cor[-idx, ] # kill the twin
#  }
  # return the correct trials
#  return(trl_cor) 
#}

load('data/raw.RData')

# -------------------------------- Experiment 1 --------------------------------

# use the kill-the-twin method to correct RTs, training
#raw_exp1$training %>% group_by(name, Association, target, Group) %>% nest() %>%
#  mutate(ktw = map(data, kill_the_twin)) %>% dplyr::select(-data) %>% unnest(ktw) -> exp1_training_cor
# remove RT with 3MAD
raw_exp1$training %>%  group_by(name) %>%
  mutate(RT = ifelse(RT > mean(RT) + 3*sd(RT) | RT < mean(RT) - 3*sd(RT), NA, RT)) %>%
  ungroup() %>% filter(!is.na(RT))-> exp1_training_cor

# average participants
exp1_training_cor %>%
  group_by(name, Association, target, Group) %>%
  summarise(accuracy = mean(Correct)) -> exp1_training_accuracy

# visualize correct RTs - training
exp1_tr_mrt = exp1_training_cor %>% filter(Correct ==1) %>% 
  group_by(name, Association, target, Group) %>% 
  summarise(RT = mean(RT)) 
# factorize subject
exp1_tr_mrt$name = factor(exp1_tr_mrt$name)

# use the kill-the-twin method to correct RTs, test
#exp1_test = raw_exp1$testing %>% group_by(name, Association, Group, Duration, distractor) %>% nest() %>%
#  mutate(ktw = map(data, kill_the_twin)) %>% dplyr::select(-data) %>% unnest(ktw)
# remove RT with 3MAD
raw_exp1$testing %>% group_by(name) %>%
  mutate(RT = ifelse(RT > mean(RT) + 3*sd(RT) | RT < mean(RT) - 3*sd(RT), NA, RT)) %>%
  ungroup() %>% filter(!is.na(RT))-> exp1_test_cor

# average participants 
exp1_test_cor %>%
  group_by(name, Association, Group, Duration, distractor) %>% 
  summarise(Accuracy = mean(Correct)) -> exp1_test_accuracy #create a new table with accuracies


exp1_test_mrt = exp1_test_cor %>% filter(Correct == 1)  %>% 
  group_by(name, Association, Group, Duration, distractor) %>%
  summarise(RT = mean(RT)) 

#combine
exp1_train_m = left_join(exp1_tr_mrt, exp1_training_accuracy, 
                         by = c('name', 'Association', 'target', 'Group')) %>%
    ungroup() %>% mutate(Group = as.factor(Group))
exp1_test_m = left_join(exp1_test_mrt, exp1_test_accuracy, 
                        by = c('name', 'Association', 'Group', 'distractor', 'Duration')) %>% 
  ungroup() %>% mutate(Group = as.factor(Group))

# Experiment 1
# add average RT from each participants
#exp1_train_m = left_join(exp1_train_m, 
#                         raw_exp1$training %>% filter(Correct == 1) %>% 
#                           group_by(name) %>% summarise(avgRT = mean(RT)), 
#                         by = c('name')) %>%
#  mutate(dRT = (RT - avgRT)*1000, rRT = dRT/avgRT/1000) # relative delta RT and ratio RT

#exp1_test_m = left_join(exp1_test_m, 
#                        raw_exp1$testing %>% filter(Correct == 1) %>%
#                          group_by(name, Duration) %>% summarise(avgRT = mean(RT)),
#                        by = c('name', 'Duration')) %>%
#  mutate(dRT = (RT - avgRT)*1000, rRT = dRT/avgRT/1000 )

# remove temp. vars
rm(exp1_test_accuracy, exp1_test_mrt, exp1_tr_mrt, exp1_training_accuracy)

# ------ Experiment 2 -----

# create Group, and remapping arousal 
#association = raw_exp2$training %>% select(name, Arousal, Valence, target) %>% filter(Valence == 'Pleasant', Arousal == 'high') %>%
#  unite("Group", Arousal, Valence, target, remove = FALSE) %>% select(-target, -Valence, -Arousal) %>%
#  distinct() %>% right_join(., raw_exp2$training %>% select(name, session, Arousal) %>% distinct(), by = c('name'))

#exp2_training = left_join(raw_exp2$training %>% select(-Arousal), association, by = c("name", "session"))
#exp2_test = left_join(raw_exp2$testing %>% select(-Arousal), association, by = c("name", "session"))

# error correction with ktw method
#exp2_tr_ktw = raw_exp2$training %>% group_by(name, Arousal, Valence,target, Group) %>% nest() %>%
#  mutate(ktw = map(data, kill_the_twin)) %>% select(-data) %>% unnest(ktw) 
# remove RT with 3MAD
raw_exp2$training %>% group_by(name) %>%
  mutate(RT = ifelse(RT > mean(RT) + 3*sd(RT) | RT < mean(RT) - 3*sd(RT), NA, RT)) %>%
  ungroup() %>% filter(!is.na(RT))-> exp2_tr_cor

# average participants 
exp2_tr_cor %>%
  group_by(name, Arousal, Valence, target, Group) %>% 
  summarise(Accuracy = mean(Correct)) -> exp2_tr_accuracy #create a new table with accuracies

# mean RTs
exp2_tr_mrt = exp2_tr_cor %>% filter(Correct == 1) %>%
  group_by(name, Arousal, Valence,target, Group) %>%
  summarise(RT = mean(RT))
#exp2_tr_mrt$name = as.factor(exp2_tr_mrt$name)

# recode NA to 'Absent' in association column
#exp2_test$Valence = as.character(fct_explicit_na(exp2_test$Valence, na_level = "Absent"))

# error correction with ktw method
#exp2_test_ktw = exp2_test %>% group_by(name, Arousal, Valence, distractor, Group, Duration) %>% nest() %>%
#  mutate(ktw = map(data, kill_the_twin)) %>% select(-data) %>% unnest(ktw) 
# remove RT with 3MAD
raw_exp2$testing %>% group_by(name) %>%
  mutate(RT = ifelse(RT > mean(RT) + 3*sd(RT) | RT < mean(RT) - 3*sd(RT), NA, RT)) %>%
  ungroup() %>% filter(!is.na(RT))-> exp2_test_cor

# mean RTs
exp2_test_mrt = exp2_test_cor %>% filter(Correct == 1) %>%
  group_by(name, Arousal, Valence,distractor, Duration, Group) %>%
  summarise(RT = mean(RT))
#exp2_test_mrt$name = as.factor(exp2_test_mrt$name)
#exp2_test_mrt$Arousal = as.factor(exp2_test_mrt$Arousal)
#exp2_test_mrt$Valence = as.factor(exp2_test_mrt$Valence)
#exp2_test_mrt$Duration = as.factor(exp2_test_mrt$Duration)

# 
#average test accuracy
exp2_test_accuracy = exp2_test_cor %>% 
  group_by(name, Arousal, Valence, distractor, Duration, Group) %>%
  summarise( accuracy = mean(Correct)) #use 

exp2_train_m = left_join(exp2_tr_mrt, exp2_tr_accuracy, 
                         by = c('name','Arousal','Valence','target','Group')) %>%
  ungroup()

exp2_test_m = left_join(exp2_test_mrt, exp2_test_accuracy, 
                        by = c('name','Arousal','Valence','distractor','Duration','Group')) %>% 
  ungroup()
# add relative and ratio RT
#exp2_train_m = left_join(exp2_train_m, 
#                         raw_exp2$training %>% filter(Correct == 1) %>% 
#                           group_by(name, Arousal) %>% summarise(avgRT = mean(RT)) %>% mutate(name = as.factor(name)), 
#                         by = c('name','Arousal')) %>%
#  mutate(dRT = (RT - avgRT)*1000, rRT = dRT/avgRT/1000) # relative delta RT and ratio RT

#exp2_test_m = left_join(exp2_test_m, 
#                        raw_exp2$testing %>% filter(Correct == 1) %>%
#                          group_by(name, Duration, Arousal) %>% summarise(avgRT = mean(RT)) %>% mutate(name = as.factor(name)),
#                        by = c('name', 'Duration', 'Arousal')) %>%
#  mutate(dRT = (RT - avgRT)*1000, rRT = dRT/avgRT/1000 )
rm(exp2_tr_accuracy, exp2_test_accuracy, exp2_test_mrt, exp2_tr_mrt)

# ----- Experiment 3 -----
# remoe RT with 3MAD
raw_exp3$training %>% group_by(participants) %>%
  mutate(rt = ifelse(rt > mean(rt) + 3*sd(rt) | rt < mean(rt) - 3*sd(rt), NA, rt)) %>%
  ungroup() %>% filter(!is.na(rt))-> exp3_tr_cor
raw_exp3$testing %>% group_by(participants) %>%
  mutate(rt = ifelse(rt > mean(rt) + 3*sd(rt) | rt < mean(rt) - 3*sd(rt), NA, rt)) %>%
  ungroup() %>% filter(!is.na(rt))-> exp3_test_cor

exp3_tr_cor %>% filter(accuracy == 1) %>%
  group_by(participants, valence, target) %>%
  summarise(RT = mean(rt)) -> exp3_learning_rt
#average rt of the test by participants, exposure and target_status
exp3_test_cor %>% filter(accuracy == 1) %>%
  group_by(participants, exposure, target_status, distractor) %>%
  summarise(RT = mean(rt)) -> exp3_testing_rt

#average accuracy of the association phase by participants and valence 
exp3_tr_cor %>%
  group_by(participants, valence) %>%
  summarise(Accuracy = mean(accuracy)) -> exp3_learning_accuracy

#average accuracy of the test by participants, exposure and target_status
exp3_test_cor %>%
  group_by(participants, exposure, target_status) %>%
  summarise(Accuracy = mean(accuracy)) -> exp3_testing_accuracy

# add relative measure
# Experiment 3
exp3_train_m = left_join(exp3_learning_rt, exp3_learning_accuracy, 
                              by = c("participants","valence"))
exp3_test_m = left_join(exp3_testing_rt, exp3_testing_accuracy, 
                             by = c("participants","exposure","target_status")) %>%
  rename(target = distractor) # here not a distractor color, but a target color

#exp3_train_m = left_join(exp3_learning_rt1, 
#                         exp3_learning_ktw_rt %>% filter(accuracy == 1) %>% 
#                           group_by(participants) %>% summarise(avgRT = mean(rt)) ,  by = c('participants')) %>%
#  mutate(dRT = (RT - avgRT)*1000, rRT = dRT/avgRT/1000, participants = as.factor(participants)) # relative delta RT and ratio RT

#exp3_test_m = left_join(exp3_testing_rt1, 
#                        exp3_testing_ktw_rt %>% filter(accuracy == 1) %>%
#                          group_by(participants, exposure) %>% summarise(avgRT = mean(rt)) %>% 
#                          mutate(participants = as.factor(participants)) ,
#                        by = c('participants','exposure')) %>%
#  mutate(dRT = (RT - avgRT)*1000, rRT = dRT/avgRT/1000 ) %>% 
#  rename(target = distractor) # here not a distractor color, but a target color
rm(exp3_learning_accuracy, exp3_testing_accuracy, exp3_testing_rt, exp3_learning_rt)


# ----- Combination for Correlation -----
# resummarize association phase
exp1_train_m %>%
  group_by(name, Association) %>%
  summarise(RT = mean(RT)) %>% 
  pivot_wider(names_from = Association, values_from = RT) %>%
  mutate(learning = (Neutral - Pleasant)*1000) -> exp1_tr_wide

# resummarize test phase
exp1_test_m %>%
  group_by(name, Association, Duration) %>%
  summarise(RT = mean(RT)) %>% 
  pivot_wider(names_from = Association, values_from = RT) %>% 
  mutate(DS_Pleasant = 1000*(Pleasant - Absent), 
         DS_Neutral = 1000*(Neutral - Absent), 
         DS = 1000*(Pleasant - Neutral)) -> exp1_test_wide

#form table from association and test table
exp1_test_wide %>% select(name, Duration, DS_Pleasant, DS_Neutral, DS) %>% 
  left_join(., exp1_tr_wide %>% select(name, learning), by = "name") -> exp1_corr

#Exp. 2
exp2_train_m %>% ungroup() %>% select(name, Arousal, Valence, RT) %>% 
  pivot_wider(names_from = Valence, values_from = RT ) %>%
  mutate(learning = (Neutral - Pleasant)*1000) %>%
  select(-Neutral, -Pleasant) -> exp2_tr_wide

exp2_test_m %>% ungroup() %>% group_by(name, Arousal, Valence) %>%
  summarise(RT = mean(RT)) %>%
  pivot_wider(names_from = Valence, values_from = RT) %>% 
  mutate(DS_Pleasant = 1000*(Pleasant - Absent), 
         DS_Neutral = 1000*(Neutral - Absent), 
         DS = 1000*(Pleasant - Neutral)) %>%
  select(-Pleasant, - Absent, -Neutral)  -> exp2_test_wide

left_join(exp2_test_wide, exp2_tr_wide, by = c("name", "Arousal")) -> exp2_corr


exp3_train_m %>% ungroup() %>% select(-target, -Accuracy) %>% 
  pivot_wider(names_from = valence, values_from = RT ) %>%
  mutate(learning = (neutral - pleasant)*1000)  -> exp3_tr_wide

exp3_test_m %>% ungroup() %>% select(-target, -Accuracy) %>%
  pivot_wider(names_from = target_status, values_from = RT) %>% 
  mutate(DS_Pleasant = 1000*(pleasant - none), 
         DS_Neutral = 1000*(neutral - none), 
         DS = 1000*(pleasant - neutral)) -> exp3_test_wide

exp3_test_wide %>% 
  dplyr::select(participants, exposure, DS_Pleasant, DS_Neutral, DS) %>% 
  left_join(., exp3_tr_wide %>% 
              dplyr::select(participants, learning), by = "participants") -> exp3_corr

# remove temp. vars
rm(exp1_test_mrt, exp1_test_wide, exp1_tr_mrt, exp1_tr_wide, 
   exp2_test_accuracy, exp2_training_accuracy, exp2_test_mrt, exp2_test_wide, exp2_tr_mrt, exp2_tr_wide,
   exp3_learning_accuracy, exp3_learning_rt, exp3_test_wide, exp3_testing_accuracy, exp3_testing_rt, exp3_tr_wide)
