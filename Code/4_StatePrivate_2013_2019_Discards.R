# State Private Discards
# Methods:
# 1. Import season length by based on management history SS provided by SERO
# 2. Calculate number of total days per wave 
# 4. Calculate the proportion of open season days per wave
# 5. Use recreational landings/discards SS (e.g., RS_rec_catGEN_8119_20211123) 
# to create pivot tables of discards (b2). For 2013-2016 filter to the "state" jurisdiction 
# and summarize by "NEW_STA", wave,gulf region and year.
# 6. Use proportions from 4 to partition discards by wave to either the open or closed season.
# 7. Sum discards over wave and gulf region, by open/closed season to get total discards.
# 8. Steps 5-7 are repeated for years after 2016 except in these years step 5 does not include
# the "state" jurisdiction filter because at this time the states control all waters. 
# 9. Question your life decisions. 

# NOTE: The above steps are completed twice, once for 2013-2016 and then for 2017 on. 
# For 2017 on There is no need for a jurisdiction identifier because the states control all waters.

library(tidyverse)
library(readxl)
user = Sys.getenv("USERNAME")

# State Jurisdiction 2013-2016
# 1 - Determine Season Length based on mode ####
InputDir = paste0("C:/Users/",user,"/Documents/GitHub/OpenClosedRecDiscards/InputFiles")
ServerDir = "S:/SEDAR 74RT GM RS 2019/Data inputs/Fishery Dependent"
OutputDir = paste0("C:/Users/",user,"/Documents/GitHub/OpenClosedRecDiscards/OutputFiles")
FileName = "RS_rec_catGEN_8119_20220707"

# File Checks: If it exists in the S drive then just load the R data, 
# if not change name of RecCatch and do read_excel
if(file.exists(paste0(ServerDir,"/",FileName,".xlsx")) && file.exists(paste0(InputDir,"/",FileName,".RData"))){
  load(paste0(InputDir,"/",FileName,".RData"))
  RecCatch = RecCatch
  cat("\nfile exists on S drive and Github so loading data from github\n")
}else if(file.exists(paste0(ServerDir,"/",FileName,".xlsx")) && !file.exists(paste0(InputDir,"/",FileName,".RData"))){
  cat("\nfile does exist, but is not saved on github\n")
  RecCatch = read_excel(paste0(ServerDir,"/",FileName,".xlsx"), sheet = "RS_rec_catGEN_8119_20220707") # OLdFile: RS_rec_catGEN_8119_20211123.csv"
  save.image(file = paste0(InputDir,"/",FileName,".Rdata"))
}else if(!file.exists(paste0(ServerDir,"/",FileName,".xlsx"))){
  cat("\nNeed to find the new file name on the S drive and change the FileName object in this script\n")}

Regs = read.csv(paste0(InputDir,"/RSN_StatePrivate_Regs_table.csv"))
Regs.help = read.csv(paste0(InputDir,"/RSN_ForHire_Regs_through2021.csv")) # needed for month length doesnt matter 

head(Regs)
table(Regs.help$year)
# Getting to the proportion of open season days by wave
StateRegs = Regs %>% select(-Annual_Open)%>% pivot_longer(cols = starts_with("Wave_"), names_to = "WAVE",names_prefix ="Wave_", values_to = "open")%>%
  mutate(NEW_STA = ifelse(State=="Texas","TX",
                          ifelse(State=="Alabama","AL",
                                 ifelse(State=="Mississippi","MS",
                                        ifelse(State == "Louisiana","LA",
                                        ifelse(State=="Florida","FLW",NA))))))
 
head(StateRegs)

P.Open.Days = Regs.help%>%mutate(wave = ifelse(month %in% c(1,2),1,
                                           ifelse(month %in% c(3,4),2,
                                                  ifelse(month %in% c(5,6),3,
                                                         ifelse(month %in% c(7,8),4,
                                                                ifelse(month %in% c(9,10),5,
                                                                       ifelse(month %in% c(11,12),6,NA)))))))%>%
  group_by(year,wave)%>% 
  summarize(length = length(reg_season))

head(StateRegs)
colnames(StateRegs) = c("State" ,"year",  "wave" , "open"  ,"NEW_STA")

P.Open.State = merge(StateRegs,P.Open.Days, by = c("year","wave"))
P.Open.State$p_open = P.Open.State$open/P.Open.State$length
head(P.Open.State)
P.Open.State$p_closed = 1-P.Open.State$p_open
colnames(P.Open.State) = c("YEAR","WAVE","State","open","NEW_STA","length","p_open","p_closed")


# 2 - Choose Fishing Mode ###
Fish.Mode ="Priv" # "Cbt"        "Hbt"        "Priv"       "Priv/Shore"

# 3 - Calculate discards for each state season ####
# Pivot discard data by wave and year, using fed season now instead
RecCatch.state = RecCatch %>% filter(NEW_MODEN==Fish.Mode,JURISDICTION=="State")%>% 
  group_by(YEAR,Gulf,NEW_STA,WAVE)%>%summarise(sum(B2))
colnames(RecCatch.state) = c("YEAR","Gulf","NEW_STA","WAVE","B2")
table(RecCatch.state$NEW_STA,RecCatch.state$Gulf)

# merge based on wave and year to get proportion of season open and closed and discards in one the table
RecCatch.state= merge(RecCatch.state,P.Open.State, by= c("YEAR","WAVE","NEW_STA"))

head(RecCatch.state)
# multiply proportions of season by sum(B2)
RecCatch.state$Open = as.numeric(RecCatch.state$B2)*as.numeric(RecCatch.state$p_open)
RecCatch.state$Closed = as.numeric(RecCatch.state$B2)*as.numeric(RecCatch.state$p_closed)

RecCatch.state.sum.2016 = RecCatch.state %>% filter(between(YEAR,2013,2016))%>%
  group_by(YEAR,Gulf)%>%
  summarise(Open=sum(Open),
            Closed = sum(Closed))

# No jurisdiction 2017-2019
# Pivot discard data by wave and year, using fed season now instead
RecCatch.state.late = RecCatch %>% filter(NEW_MODEN==Fish.Mode)%>% 
  group_by(YEAR,Gulf,NEW_STA,WAVE)%>%summarise(sum(B2))
colnames(RecCatch.state.late) = c("YEAR","Gulf","NEW_STA","WAVE","B2")
table(RecCatch.state.late$NEW_STA,RecCatch.state.late$Gulf)

# merge based on wave and year to get proportion of season open and closed and discards in one the table
RecCatch.state.late= merge(RecCatch.state.late,P.Open.State, by= c("YEAR","WAVE","NEW_STA"))

head(RecCatch.state.late)
dim(RecCatch.state.late)

# multiply proportions of season by sum(B2)
RecCatch.state.late$Open = as.numeric(RecCatch.state.late$B2)*as.numeric(RecCatch.state.late$p_open)
RecCatch.state.late$Closed = as.numeric(RecCatch.state.late$B2)*as.numeric(RecCatch.state.late$p_closed)

RecCatch.state.sum.2019 = RecCatch.state.late %>% filter(between(YEAR,2017,2019))%>%
  group_by(YEAR,Gulf)%>%
  summarise(Open=sum(Open),
            Closed = sum(Closed))


StatePriv = rbind(RecCatch.state.sum.2016,RecCatch.state.sum.2019)

# 5 - Organize and export for SS ####
Discards = StatePriv%>%
  pivot_wider(
    names_from = Gulf,
    values_from = c(Open, Closed),values_fill = 0)

colnames(Discards)[2:dim(Discards)[2]] = paste0("State",Fish.Mode,"_",colnames(Discards)[2:dim(Discards)[2]])

write.csv(Discards,paste0(OutputDir,"\\State",Fish.Mode,"_Discards.csv"),row.names = FALSE)

cat("\nDone producing private state discards, look in output folder\n")

              