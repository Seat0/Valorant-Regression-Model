vct_championships_2024 <- read.csv("2024/Valorant_Championships2024_gs.csv")

## only uses group stage stats for champs 2024
Champs_2024 <- vct_championships_2024 %>%
  separate(Player, into = c("Player_Name", "Team"), sep = "\n", remove = FALSE) %>%
  mutate(Team = str_trim(Team)) %>% 
  mutate(Open_Duel_Won = FK / (FK + FD)) %>%
  group_by(Team) %>%
  summarise(across(-c(Player_Name,Player, Agents, CL), list(mean), na.rm = TRUE)) %>% 
  mutate(GS_win = ifelse(Team %in% c("DRX","TH","TE","G2"),1,0)) %>% 
  mutate(experience = ifelse(Team %in% Madrid2024_Ranks$Team | Team %in% Shanghai2024_Ranks$Team, 1, 0)) %>%
  left_join(Madrid2024_Ranks %>% select(Team, Per_mad = Percentile), by = "Team") %>%
  left_join(Shanghai2024_Ranks %>% select(Team, Per_shang = Percentile), by = "Team") %>%
  mutate(performance = ifelse(experience == 1 & (Per_mad <= 70 | Per_shang <= 70), 1, 0),
         performance = ifelse(is.na(performance), 0, performance)) %>% 
  select(-Per_mad, -Per_shang)


Pred <- Champs_2024 %>% 
  filter(!Team %in% c("FUT", "TLN", "FPX", "BLG", "PRX", "VIT", "GEN", "KRÜ")) %>% ## Removes teams that didn't qualify for playoffs, their placements are already determined
  mutate(prediction = 236.123 + R_1*-165.90 + GS_win*-14.284 + performance*-13.81) %>%  ## calculating expected placement value from regression
  select(Team,prediction) %>% 
  arrange(prediction)

ggplot(data = Pred, aes(x = reorder(Team, prediction), y = prediction)) + 
  geom_col() 

pred <- ggplot(data = Pred, aes(x = reorder(Team, prediction), y = prediction)) + 
  geom_col() 



setwd("../output")
write_csv(Total_Champs, "Champs.csv")
ggsave("pred_graph.png", pred, width = 8, height = 6, dpi = 300)


ggplot(data = Total_Champs, aes(x = Percentile, y = R_1 )) + 
  geom_point() 

reg.regression <- lm(Percentile ~ R_1 + GS_win + performance, data = Total_Champs) 

stargazer(reg.regression, type = "text", out = "regression_results.txt")


