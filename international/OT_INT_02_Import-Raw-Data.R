
# OT Win - International

# Import raw data from data input scripts to .rds files

source("international/OT_INT_01_Input-Functions.R")

source("international/Raw-Data/Data Input Scripts/game-level_elb.R")



source("international/Raw-Data/Data Input Scripts/game-level_jbl.R")

source("international/Raw-Data/Data Input Scripts/game-level_cba.R")

source("international/Raw-Data/Data Input Scripts/game-level_nbl.R")

source("international/Raw-Data/Data Input Scripts/season-level_jbl.R")

source("international/Raw-Data/Data Input Scripts/season-level_cba.R")

source("international/Raw-Data/Data Input Scripts/season-level_nbl.R")


write_rds(dat_game, "international/Initial-Data_Game.rds")

write_rds(dat_st, "international/Initial-Data_Season.rds")

rm(list = ls())
