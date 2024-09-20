library(tidyverse)
library(plm)
library(stargazer)
setwd("../data")

vct_championships_2021 <- read.csv("2021/Valorant_Championships2021_gs.csv")
vct_championships_2022 <- read.csv("2022/Valorant_Championships2022_gs.csv")
vct_championships_2023 <- read.csv("2023/Valorant_Championships2023_gs.csv")

## cleaning data, adding Open DUel won variable in case if needed
Championships_2021 <- vct_championships_2021 %>%
  separate(Player, into = c("Player_Name", "Team"), sep = "\n", remove = FALSE) %>%
  mutate(Open_Duel_Won = FK / (FK + FD)) %>%
  group_by(Team) %>%
  summarise(across(-c(Player_Name,Player, Agents, CL), list(mean), na.rm = TRUE))

Championships_2022 <- vct_championships_2022 %>%
  separate(Player, into = c("Player_Name", "Team"), sep = "\n", remove = FALSE) %>%
  mutate(Open_Duel_Won = FK / (FK + FD)) %>%
  group_by(Team) %>%
  summarise(across(-c(Player_Name,Player, Agents, CL), list(mean), na.rm = TRUE))

Championships_2023 <- vct_championships_2023 %>%
  separate(Player, into = c("Player_Name", "Team"), sep = "\n", remove = FALSE) %>%
  mutate(Open_Duel_Won = FK / (FK + FD)) %>%
  group_by(Team) %>%
  summarise(across(-c(Player_Name,Player, Agents, CL), list(mean), na.rm = TRUE))

######
Champ2021_Ranks <- Championships_2021 %>%
  mutate(rank = case_when(
    Team %in% c("FUR", "FS", "CR", "KS") ~ 14.5,
    Team %in% c("VS", "VKS", "SEN", "NV") ~ 10.5,
    Team %in% c("TS", "C9", "X10", "FNC") ~ 6.5,
    Team %in% c("KRÜ", "TL") ~ 3.5,
    Team == "GMB" ~ 2,
    Team == "ACE" ~ 1)) %>%
  mutate(GS_win = ifelse(Team %in% c("ACE", "TL", "GMB", "FNC"), 1, 0)) %>%
  mutate(ecdf_rank = ecdf(rank)(rank),    ## calc percentile team placed, lower = better placement 
         Percentile = ecdf_rank * 100) %>%
  rename(CL = CL._1) %>%
  mutate(Tournament = "Championships 2021") %>%
  mutate(experience = ifelse(Team %in% Reykjavik2021_Ranks$Team | Team %in% Berlin2021_Ranks$Team, 1, 0)) %>% # Experience var determines if a team has been to any internation event(didn't end up using)
  left_join(Reykjavik2021_Ranks %>% select(Team, Per_rey = Percentile, rankRey21), by = "Team") %>%
  left_join(Berlin2021_Ranks %>% select(Team, Per_ber = Percentile, rankBer21), by = "Team") %>%
  mutate(performance = ifelse(experience == 1 & (Per_rey <= 70 | Per_ber <= 70), 1, 0),
         performance = ifelse(is.na(performance), 0, performance),
         win = ifelse(rankRey21 == 1 | rankBer21 ==1 , 1,0)) %>% 
  replace_na(list(win = 0)) %>%
  select(-Per_rey, -Per_ber)



Champ2022_Ranks <- Championships_2022 %>% ## manually entering team placements from tourny. If 8-12th average placenment 10th
  mutate(rank = case_when(
    Team %in% c("FUR", "XIA", "BME", "EDG") ~ 14.5,
    Team %in% c("100T", "KRÜ", "ZETA", "PRX") ~ 10.5,
    Team %in% c("TL", "LEV") ~ 7.5,
    Team %in% c("XSET", "FNC") ~ 5.5,
    Team == "FPX" ~ 4,
    Team == "DRX" ~ 3,
    Team == "OPTC" ~ 2,
    Team == "LOUD" ~ 1)) %>% 
  mutate(GS_win = ifelse(Team %in% c("LEV","OPTC","XSET","DRX"),1,0)) %>% 
  mutate(ecdf_rank = ecdf(rank)(rank),    ## calc percentile team placed, lower = better placement 
         Percentile = ecdf_rank * 100) %>% 
  rename(CL = CL._1) %>% 
  mutate(Tournament = "Championships 2022") %>% 
  mutate(experience = ifelse(Team %in% Reykjavik2022_Ranks$Team | Team %in% Copenhagen2022_Ranks$Team, 1, 0)) %>%   # Experience var determines if a team has been to any internation event(didn't end up using)
  left_join(Reykjavik2022_Ranks %>% select(Team, Per_rey = Percentile,rankRey22), by = "Team") %>%
  left_join(Copenhagen2022_Ranks %>% select(Team, Per_cop = Percentile,rankCop22), by = "Team") %>%
  mutate(performance = ifelse(experience == 1 & (Per_rey <= 70 | Per_cop <= 70), 1, 0),
         performance = ifelse(is.na(performance), 0, performance),
         win = ifelse(rankRey22 == 1 | rankCop22 ==1 , 1,0)) %>% 
  replace_na(list(win = 0)) %>%
  select(-Per_rey, -Per_cop)



Champ2023_Ranks <- Championships_2023 %>% ## manually entering team placements from tourny. If 8-12th average placenment 10th
  mutate(rank = case_when(
    Team %in% c("ZETA", "TL", "FPX", "KRÜ") ~ 14.5,
    Team %in% c("NAVI", "T1", "GIA", "NRG") ~ 10.5,
    Team %in% c("FUT", "BLG") ~ 7.5,
    Team %in% c("EDG", "DRX") ~ 5.5,
    Team == "FNC" ~ 4,
    Team == "LOUD" ~ 3,
    Team == "PRX" ~ 2,
    Team == "EG" ~ 1)) %>% 
  mutate(GS_win = ifelse(Team %in% c("PRX","EG","FNC","DRX"),1,0)) %>% 
  mutate(ecdf_rank = ecdf(rank)(rank),    ## calc percentile team placed, lower = better placement 
         Percentile = ecdf_rank * 100) %>% 
  rename(CL = CL._1) %>% 
  mutate(Tournament = "Championships 2023") %>% 
  #mutate(experience = ifelse(Team %in% Paulo2023_Ranks$Team | Team %in% Tokyo2023_Ranks$Team, 1, 0)) %>%
  mutate(experience = ifelse(Team %in% Tokyo2023_Ranks$Team, 1, 0)) %>%
  #left_join(Paulo2023_Ranks %>% select(Team, Per_sao = Percentile), by = "Team") %>%
  left_join(Tokyo2023_Ranks %>% select(Team, Per_tok = Percentile,rankTok23), by = "Team") %>%
  #mutate(performance = ifelse(experience == 1 & (Per_sao <= 70 | Per_tok <= 70), 1, 0), ## add if want to add sao paolo     # Decided to not use this tournament since all teams auto qualified for it 
         #performance = ifelse(is.na(performance), 0, performance)) %>% 
  #select(-Per_sao, -Per_tok)
  mutate(performance = ifelse(experience == 1 & (Per_tok <= 70), 1, 0),
         performance = ifelse(is.na(performance), 0, performance),
         win = ifelse(rankTok23 ==1 , 1,0)) %>% 
  replace_na(list(win = 0)) %>%
  select(-Per_tok)

##################################### c

##########
Total_Champs <- bind_rows(Champ2021_Ranks,Champ2022_Ranks,Champ2023_Ranks)

#GS_win = team won group stage - puts team in advantage in playoffs
# R_1 = player rating calculated by VLR.GG to determine how impactful a player was on their team 


                                                            