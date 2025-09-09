# Simulate data based on STRESS data

# based on Koliopanos et al., 2023; https://doi.org/10.1055/a-2048-7692

# load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # data formatting, ggplot2
               magrittr, # %<>%
               modgo, # simulate dataset based on existing dataset
               here) # start paths at project root

################################################################################
# AUDIO ########################################################################
################################################################################

# import audio features data set 
# Note. I previously first merged, created a condition variable, then used this as basis for a simulated dataset, but then the condition is simulated non-equally, 
# better to simulate three datasets and then merge

audio_neu <- read.csv("../../data/code/openwillis/v3.0_extraction/output/audio_features/audio_summary_neutral.csv") %>% # remembering neutral event
  select(-c(mean_hrf:acohnr, file_path, participant_id)) %>%
  na.omit() # modgo cannot handle missings

audio_pos <- read.csv("../../data/code/openwillis/v3.0_extraction/output/audio_features/audio_summary_pos.csv") %>% # remembering happy event
  select(-c(mean_hrf:acohnr, file_path, participant_id)) %>%
  na.omit() # modgo cannot handle missings

audio_neg <- read.csv("../../data/code/openwillis/v3.0_extraction/output/audio_features/audio_summary_neg.csv") %>% # remembering stressful event
  select(-c(mean_hrf:acohnr, file_path, participant_id)) %>%
  na.omit() # modgo cannot handle missings

# audio_all <- rbind(audio_neu, audio_pos, audio_neg) %>% # merge audio data 
#   mutate(condition = case_when(
#     str_detect(file_path, "Neu") ~ 0,
#     str_detect(file_path, "Pos") ~ 1,
#     str_detect(file_path, "Neg") ~ 2),
#     condition = as.numeric(condition)) %>%
#   select(-c(mean_hrf:acohnr, file_path, participant_id)) %>%
#   na.omit() # modgo cannot handle missings
# 
# # simulate audio features data set  
# simulation <- modgo(data = audio_all, categ_variables = "condition", seed = 42, nrep = 500) # 500 simulations

# simulate audio features data set  
audio_neu_sim <- modgo(data = audio_neu, seed = 42, nrep = 1) # 500 simulations - nope, let's make it easy and just do 1
audio_pos_sim <- modgo(data = audio_pos, seed = 42, nrep = 1) # 500 simulations
audio_neg_sim <- modgo(data = audio_neg, seed = 42, nrep = 1) # 500 simulations

# # stack 500 simulations to create one df, add row_id for matching rows
# audio_neu_sim_all <- map_dfr(
#   audio_neu_sim$simulated_data,
#   ~ mutate(.x, row_id = row_number()),
#   .id = "sim_id"
# )
# 
# # average across simulations for each row_id
# audio_neu_sim_avg <- audio_neu_sim_all %>%
#   group_by(row_id) %>%
#   summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
#   select(-row_id)  # remove row index
# 

# check correlations for original and simulated data
corr_plots(
  audio_neu_sim,
  sim_dataset = 1,
  variables = colnames(audio_neu_sim[["simulated_data"]][[1]])
)

# TODO: ideally, check all


# extract simulated data
audio_neu_sim_df <- audio_neu_sim$simulated_data[[1]] 
audio_pos_sim_df <- audio_pos_sim$simulated_data[[1]] 
audio_neg_sim_df <- audio_neg_sim$simulated_data[[1]] 

# create condition variable
audio_neu_sim_df$condition <- "neutral"
audio_pos_sim_df$condition <- "happy"
audio_neg_sim_df$condition <- "stress"

# combine all simulated datasets into one
audio_sim_all <- bind_rows(audio_neu_sim_df, audio_pos_sim_df, audio_neg_sim_df)

audio_sim_all %<>%
  group_by(condition) %>%
  mutate(id = row_number()) %>%
  ungroup() %>%
  select(id, condition, everything())

################################################################################
# VIDEO ########################################################################
################################################################################

# import video emotional expressivity features data set 
video_neu <- read.csv("../../data/code/openwillis/v3.0_extraction/output/video_features/no_baseline/emo_exp_summary_no_baseline_neutral.csv") %>% # remembering neutral event
  select(-c(file_path, participant_id))

video_pos <- read.csv("../../data/code/openwillis/v3.0_extraction/output/video_features/no_baseline/emo_exp_summary_no_baseline_pos.csv") %>% # remembering happy event
  select(-c(file_path, participant_id))

video_neg <- read.csv("../../data/code/openwillis/v3.0_extraction/output/video_features/no_baseline/emo_exp_summary_no_baseline_neg.csv") %>% # remembering stressful event
  select(-c(file_path, participant_id))

# simulate video features data set  
video_neu_sim <- modgo(data = video_neu, seed = 42, nrep = 1) # 500 simulations - nope, let's make it easy and just do 1
video_pos_sim <- modgo(data = video_pos, seed = 42, nrep = 1) # 500 simulations
video_neg_sim <- modgo(data = video_neg, seed = 42, nrep = 1) # 500 simulations

# extract simulated data
video_neu_sim_df <- video_neu_sim$simulated_data[[1]] 
video_pos_sim_df <- video_pos_sim$simulated_data[[1]] 
video_neg_sim_df <- video_neg_sim$simulated_data[[1]] 

# create condition variable
video_neu_sim_df$condition <- "neutral"
video_pos_sim_df$condition <- "happy"
video_neg_sim_df$condition <- "stress"

# combine all simulated datasets into one
video_sim_all <- bind_rows(video_neu_sim_df, video_pos_sim_df, video_neg_sim_df)

video_sim_all %<>%
  group_by(condition) %>%
  mutate(id = row_number()) %>%
  ungroup() %>%
  select(id, condition, everything())

################################################################################
# MENTAL HEALTH ################################################################
################################################################################

# import mental health and stress questionnaire data
mh <- read.csv("data/postscore_data_long2025-03-26.csv") %>% # only T0
  select(id, sex,
         starts_with("GHQ_"), starts_with("STADI_"), PSS_total, # mental health
         LEC_exposure, MIMIS_burden, MIMIS_exposure, CTQ_total) %>% # stress, trauma
  select(-"id") %>%
  mutate(sex = case_when(
    sex == "male" ~ 0,
    sex == "female" ~ 1
  )) %>%
  na.omit()
  
# simulate mh data set  
mh_sim <- modgo(data = mh, bin_variables = "sex", seed = 42, nrep = 1) # 500 simulations - nope, let's make it easy and just do 1

# extract simulated data
mh_sim_df <- mh_sim$simulated_data[[1]] 

# add id
mh_sim_df %<>%
  mutate(id = row_number()) %>%
  select(id, everything()) %>%
  mutate(sex = case_when(
    sex == 0 ~ "male",
    sex == 1 ~ "female"))

# check correlations for original and simulated data
corr_plots(
  mh_sim,
  sim_dataset = 1,
  variables = colnames(mh_sim[["simulated_data"]][[1]])
)

################################################################################
# MERGE ########################################################################
################################################################################

data_combined <- list(audio_sim_all, video_sim_all) %>% reduce(left_join, by = c("id", "condition"))
data_combined <- list(data_combined, mh_sim_df) %>% reduce(left_join, by = c("id"))
data_combined %<>%
  select(id, sex:CTQ_total, condition, everything()) %>%
  arrange(id)

################################################################################
# SAVE SIMULATED DATA ##########################################################
################################################################################

write.table(audio_sim_all, here("../../data/code/openwillis/workshop_10092025/hands-on-session2_analysis/simulated_processed_data/simulated_audio_feature_set.csv"), sep = ",", col.names = TRUE, row.names = FALSE)
write.table(video_sim_all, here("../../data/code/openwillis/workshop_10092025/hands-on-session2_analysis/simulated_processed_data/simulated_video_emo_feature_set.csv"), sep = ",", col.names = TRUE, row.names = FALSE)
write.table(mh_sim_df, here("../../data/code/openwillis/workshop_10092025/hands-on-session2_analysis/simulated_processed_data/simulated_mental_health_survey_scores.csv"), sep = ",", col.names = TRUE, row.names = FALSE)
write.table(data_combined, here("../../data/code/openwillis/workshop_10092025/hands-on-session2_analysis/simulated_processed_data/combined.csv"), sep = ",", col.names = TRUE, row.names = FALSE)
