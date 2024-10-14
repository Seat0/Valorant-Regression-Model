library(tidyverse)
library(plm)
setwd("../data")
## Notes
# Run order 1, Masters, 2 champs, 3 pred
###########################2021 Season
Masters_Reykjavik_2021 <- read.csv("2021/Masters_Reykjavik2021.csv")
Masters_Berlin_2021 <- read.csv("2021/Masters_Berlin2021.csv")

#Stage 1 Masters Rek
Masters_Reykjavik2021_t <- Masters_Reykjavik_2021 %>%
  separate(Player, into = c("Player_Name", "Team"), sep = "\n", remove = FALSE) %>%
  mutate(Open_Duel_Won = FK / (FK + FD)) %>%
  group_by(Team) %>%
  summarise(across(-c(Player_Name,Player, Agents, CL), list(mean), na.rm = TRUE))

Reykjavik2021_Ranks <- Masters_Reykjavik2021_t %>% ## manually entering team placements from tourny.  ex, If 8-12th, average placement would be 10th
  mutate(rankRey21 = case_when(
    Team %in% c("CR", "SHK") ~ 9.5,
    Team %in% c("KRÜ", "X10") ~ 7.5,
    Team %in% c("V1", "VKS") ~ 5.5,
    Team == "TL" ~ 4,
    Team == "NU" ~ 3,
    Team == "FNC" ~ 2,
    Team == "SEN" ~ 1)) %>% 
  #mutate(GS_win = ifelse(Team %in% c("ACE","TL","GMB","FNC"),1,0)) %>% 
  mutate(ecdf_rank = ecdf(rankRey21)(rankRey21),    ## calc percentile team placed, lower = better placement 
         Percentile = ecdf_rank * 100) %>% 
  rename(CL = CL._1) %>% 
  mutate(Tournament = "Reykjavik 2021")

#Stage 2 Masters Berlin 
Masters_Berlin_2021_t <- Masters_Berlin_2021 %>%
  separate(Player, into = c("Player_Name", "Team"), sep = "\n", remove = FALSE) %>%
  mutate(Open_Duel_Won = FK / (FK + FD)) %>%
  group_by(Team) %>%
  summarise(across(-c(Player_Name,Player, Agents, CL), list(mean), na.rm = TRUE))

Berlin2021_Ranks <- Masters_Berlin_2021_t %>% ## manually entering team placements from tourny. If 8-12th average placenment 10th
  mutate(rankBer21 = case_when(
    Team %in% c("LBR", "ZETA","PRX") ~ 14,
    Team %in% c("F4Q", "CR","KS","SUP") ~ 10.5,
    Team %in% c("ACE", "SEN", "VS","KRÜ") ~ 6.5,
    Team %in% c("100T", "G2") ~ 3.5,
    Team == "NV" ~ 2,
    Team == "GMB" ~ 1)) %>% 
  #mutate(GS_win = ifelse(Team %in% c("ACE","TL","GMB","FNC"),1,0)) %>% 
  mutate(ecdf_rank = ecdf(rankBer21)(rankBer21),    ## calc percentile team placed, lower = better placement 
         Percentile = ecdf_rank * 100) %>% 
  rename(CL = CL._1) %>% 
  mutate(Tournament = "Berlin 2021")
######################################################

########################################## #2022 Season
Masters_Reykjavik_2022 <- read.csv("2022/Masters_Reykjavik2022.csv")
Masters_Copenhagen_2022 <- read.csv("2022/Masters_Copenhagen2022.csv")

#Stage 1 Masters Rek
Masters_Reykjavik2022_t <- Masters_Reykjavik_2022 %>%
  separate(Player, into = c("Player_Name", "Team"), sep = "\n", remove = FALSE) %>%
  mutate(Open_Duel_Won = FK / (FK + FD)) %>%
  group_by(Team) %>%
  summarise(across(-c(Player_Name,Player, Agents, CL), list(mean), na.rm = TRUE))

Reykjavik2022_Ranks <- Masters_Reykjavik2022_t %>% ## manually entering team placements from tourny. If 8-12th average placenment 10th
  mutate(rankRey22 = case_when(
    Team %in% c("KRÜ", "FNC") ~ 11.5,
    Team %in% c("XIA", "NIP") ~ 9.5,
    Team %in% c("TL", "TGRD") ~ 7.5,
    Team %in% c("DRX", "G2") ~ 5.5,
    Team == "PRX" ~ 4,
    Team == "ZETA" ~ 3,
    Team == "LOUD" ~ 2,
    Team == "OPTC" ~ 1)) %>% 
  #mutate(GS_win = ifelse(Team %in% c("ACE","TL","GMB","FNC"),1,0)) %>% 
  mutate(ecdf_rank = ecdf(rankRey22)(rankRey22),    ## calc percentile team placed, lower = better placement 
         Percentile = ecdf_rank * 100) %>% 
  rename(CL = CL._1) %>% 
  mutate(Tournament = "Reykjavik 2022")

#Stage 2 Masters Copenhagen 
Masters_Copenhagen_2022_t <- Masters_Copenhagen_2022 %>%
  separate(Player, into = c("Player_Name", "Team"), sep = "\n", remove = FALSE) %>%
  mutate(Open_Duel_Won = FK / (FK + FD)) %>%
  group_by(Team) %>%
  summarise(across(-c(Player_Name,Player, Agents, CL), list(mean), na.rm = TRUE))

Copenhagen2022_Ranks <- Masters_Copenhagen_2022_t %>% ## manually entering team placements from tourny. If 8-12th average placenment 10th
  mutate(rankCop22 = case_when(
    Team %in% c("XIA", "LOUD") ~ 11.5,
    Team %in% c("NTH", "KRÜ") ~ 9.5,
    Team %in% c("XSET", "GLD") ~ 7.5,
    Team %in% c("DRX", "LEV") ~ 5.5,
    Team == "FNC" ~ 4,
    Team == "OPTC" ~ 3,
    Team == "PRX" ~ 2,
    Team == "FPX" ~ 1)) %>% 
  #mutate(GS_win = ifelse(Team %in% c("ACE","TL","GMB","FNC"),1,0)) %>% 
  mutate(ecdf_rank = ecdf(rankCop22)(rankCop22),    ## calc percentile team placed, lower = better placement 
         Percentile = ecdf_rank * 100) %>% 
  rename(CL = CL._1) %>% 
  mutate(Tournament = "Copenhagen 2022")
##########################################

######################################### 2023 Season
LOCKIN_Sao_Paulo_2023 <- read.csv("2023/LOCKIN_Sao_Paulo2023.csv")
Masters_Tokyo_2023 <- read.csv("2023/Masters_Tokyo2023.csv")

# stage 1 lock in San Paulo
LOCKIN_Sao_Paulo_2023_t <- LOCKIN_Sao_Paulo_2023 %>%
  separate(Player, into = c("Player_Name", "Team"), sep = "\n", remove = FALSE) %>%
  mutate(Open_Duel_Won = FK / (FK + FD)) %>%
  group_by(Team) %>%
  summarise(across(-c(Player_Name,Player, Agents, CL), list(mean), na.rm = TRUE))

Paulo2023_Ranks <- LOCKIN_Sao_Paulo_2023_t %>% ## manually entering team placements from tourny. If 8-12th average placenment 10th
  mutate(rankPau23 = case_when(
    Team %in% c("T1", "SEN","EDG","RRQ","GE","ZETA", "KRÜ","TL","MIBR","TH","PRX", "BBL","FPX","GEN","DFM","KOI") ~ 24.5,
    Team %in% c("FUR", "FUT","VIT","TS","EG","C9", "KC","GIA") ~ 12.5,
    Team %in% c("NRG", "TLN","LEV","100T") ~ 6.5,
    Team %in% c("NAVI", "DRX") ~ 3.5,
    Team == "LOUD" ~ 2,
    Team == "FNC" ~ 1)) %>% 
  #mutate(GS_win = ifelse(Team %in% c("ACE","TL","GMB","FNC"),1,0)) %>% 
  mutate(ecdf_rank = ecdf(rankPau23)(rankPau23),    ## calc percentile team placed, lower = better placement 
         Percentile = ecdf_rank * 100) %>% 
  rename(CL = CL._1) %>% 
  mutate(Tournament = "Sao Paulo 2023")

# stage 2 Tokyo 
Masters_Tokyo_2023_t <- Masters_Tokyo_2023 %>%
  separate(Player, into = c("Player_Name", "Team"), sep = "\n", remove = FALSE) %>%
  mutate(Open_Duel_Won = FK / (FK + FD)) %>%
  group_by(Team) %>%
  summarise(across(-c(Player_Name,Player, Agents, CL), list(mean), na.rm = TRUE))

Tokyo2023_Ranks <- Masters_Tokyo_2023_t %>% ## manually entering team placements from tourny. If 8-12th average placenment 10th
  mutate(rankTok23 = case_when(
    Team %in% c("ASE", "NAVI") ~ 11.5,
    Team %in% c("FUT", "T1") ~ 9.5,
    Team %in% c("DRX", "LOUD") ~ 7.5,
    Team %in% c("EDG", "TL") ~ 5.5,
    Team == "NRG" ~ 4,
    Team == "PRX" ~ 3,
    Team == "EG" ~ 2,
    Team == "FNC" ~ 1)) %>% 
  #mutate(GS_win = ifelse(Team %in% c("ACE","TL","GMB","FNC"),1,0)) %>% 
  mutate(ecdf_rank = ecdf(rankTok23)(rankTok23),    ## calc percentile team placed, lower = better placement 
         Percentile = ecdf_rank * 100) %>% 
  rename(CL = CL._1) %>% 
  mutate(Tournament = "Tokyo 2023")
############################################################

######################################### 2024 Season
Masters_Madrid2024 <- read.csv("2024/Masters_Madrid2024.csv")
Masters_Shanghai2024 <- read.csv("2024/Masters_Shanghai2024.csv")

#Stage 1 Masters Madrid
Masters_Madrid2024_t <- Masters_Madrid2024 %>%
  separate(Player, into = c("Player_Name", "Team"), sep = "\n", remove = FALSE) %>%
  mutate(Team = str_trim(Team)) %>% 
  mutate(Open_Duel_Won = FK / (FK + FD)) %>%
  group_by(Team) %>%
  summarise(across(-c(Player_Name,Player, Agents, CL), list(mean), na.rm = TRUE))

Madrid2024_Ranks <- Masters_Madrid2024_t %>% ## manually entering team placements from tourny. If 8-12th average placenment 10th
  mutate(RankMad24 = case_when(
    Team %in% c("TH", "FPX") ~ 7.5,
    Team %in% c("EDG", "KC") ~ 5.5,
    Team == "LOUD" ~ 4,
    Team == "PRX" ~ 3,
    Team == "GEN" ~ 2,
    Team == "SEN" ~ 1)) %>% 
  mutate(ecdf_rank = ecdf(RankMad24)(RankMad24),    ## calc percentile team placed, lower = better placement 
         Percentile = ecdf_rank * 100) %>% 
  rename(CL = CL._1) %>% 
  mutate(Tournament = "Madrid 2024")

## stage 2 shanghai 

Masters_Shanghai2024_t <- Masters_Shanghai2024 %>%
  separate(Player, into = c("Player_Name", "Team"), sep = "\n", remove = FALSE) %>%
  mutate(Team = str_trim(Team)) %>% 
  mutate(Open_Duel_Won = FK / (FK + FD)) %>%
  group_by(Team) %>%
  summarise(across(-c(Player_Name,Player, Agents, CL), list(mean), na.rm = TRUE))

Shanghai2024_Ranks <- Masters_Shanghai2024_t %>% ## manually entering team placements from tourny. If 8-12th average placenment 10th
  mutate(RankShang24 = case_when(
    Team %in% c("T1", "DRG") ~ 11.5,
    Team %in% c("LEV", "FPX") ~ 9.5,
    Team %in% c("FNC", "EDG") ~ 7.5,
    Team %in% c("FUT", "PRX") ~ 5.5,
    Team == "100T" ~ 4,
    Team == "G2" ~ 3,
    Team == "TH" ~ 2,
    Team == "GEN" ~ 1)) %>% 
  mutate(ecdf_rank = ecdf(RankShang24)(RankShang24),    ## calc percentile team placed, lower = better placement 
         Percentile = ecdf_rank * 100) %>% 
  rename(CL = CL._1) %>% 
  mutate(Tournament = "Shanghai 2024")
