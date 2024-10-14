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

pred <- ggplot(data = Pred, aes(x = reorder(Team, prediction), y = prediction, fill = Team)) + 
  geom_col() + labs(title = "Valorant Teams Predicited Placings (Playoffs)",x = "Teams", y = "Predicted Placement") +
  #scale_fill_manual(values = c("G2" = "gold", "SEN" = "grey70", "TH" = "gold4", "FNC" = "sienna4")) +
  theme_bw() +
  annotate("text", x = 1, y = -1, label = "1st", size = 4.5) + 
  annotate("text", x = 2, y = -1, label = "2nd", size = 4.5) +
  annotate("text", x = 3, y = -1, label = "3rd", size = 4.5) +
  annotate("text", x = 4, y = -1, label = "4th", size = 4.5) +
  annotate("text", x = 5, y = -1, label = "5th", size = 4.5) +
  annotate("text", x = 6, y = -1, label = "6th", size = 4.5) +
  annotate("text", x = 7, y = -1, label = "7th", size = 4.5) +
  annotate("text", x = 8, y = -1, label = "8th", size = 4.5) 

actual <- ggplot(data = Pred, aes(x = factor(Team, levels = c("EDG","TH","LEV","SEN","DRX","FNC","TE","G2")), y = prediction, fill = Team)) + 
  geom_col() + labs(title = "Valorant Teams Predicited Placings (Playoffs)",x = "Teams", y = "Predicted Placement") +
  #scale_fill_manual(values = c("G2" = "gold", "SEN" = "grey70", "TH" = "gold4", "FNC" = "sienna4")) +
  theme_bw() +
  annotate("text", x = 1, y = -1, label = "1st", size = 4.5) + 
  annotate("text", x = 2, y = -1, label = "2nd", size = 4.5) +
  annotate("text", x = 3, y = -1, label = "3rd", size = 4.5) +
  annotate("text", x = 4, y = -1, label = "4th", size = 4.5) +
  annotate("text", x = 5, y = -1, label = "5th", size = 4.5) +
  annotate("text", x = 6, y = -1, label = "6th", size = 4.5) +
  annotate("text", x = 7, y = -1, label = "7th", size = 4.5) +
  annotate("text", x = 8, y = -1, label = "8th", size = 4.5) 
print(actual)

print(pred)

setwd("../output")
write_csv(Total_Champs, "Champs.csv")
ggsave("pred_graph.png", pred, width = 8, height = 6, dpi = 300)


ggplot(data = Total_Champs, aes(x = Percentile, y = R_1 )) + 
  geom_point() 

reg.regression <- lm(Percentile ~ R_1 + GS_win + performance, data = Total_Champs) 

pred_reg <- plm(Percentile ~ R_1 + GS_win + performance, data = Total_Champs, model = "pooling")

summary(pred_reg)
summary(reg.regression)

stargazer(pred_reg, type = "latex", out = "regression_results.txt")


