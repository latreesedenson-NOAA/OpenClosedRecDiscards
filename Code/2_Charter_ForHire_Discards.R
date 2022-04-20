# Charter Discards
# Methods:
# 1. Calculate season length by mode based on management history SS provided by SERO
# 2. Caclulate number of days per wave (simple)
# 3. Calculate the number of open season days by wave based on the season length by mode
# 4. Calculate the proportion of open season days per wave
# 5. Use recreational landings/discards SS (e.g., GENRECcatch_rsnap81_16_09Aug17) to create pivot tables of discards (b2) by category Fed_closed (0=open season, 1=both open and closed season (i.e., season closed mid wave), and 2=closed season) and wave.
# 6. Use proportions from 4 to parition discards by wave to either the open or closed season.
# 7. Sum discards by open/closed season to get total discards (i.e., add discards for waves that are completely open/closed to those that are of category Fed_closed=1 and have been partitioned to the associated open/closed season).
# 8. Question your life decisions. 

library(tidyverse)

# 1 - Determine Season Length based on mode ####
InputDir = "C:\\Users\\latreese.denson\\Desktop\\RecDiscards_forSS\\InputFiles"
OutputDir = "C:\\Users\\latreese.denson\\Desktop\\RecDiscards_forSS\\OutputFiles"
Regs = read.csv(paste0(InputDir,"\\RSN_ForHire_Regs_through2021.csv"))
RecCatch = read.csv(paste0(InputDir,"/RS_rec_catGEN_8119_20211123.csv"))

# Getting to the proportion of open season days by wave
P.Open.Days = Regs %>%mutate(wave = ifelse(month %in% c(1,2),1,
                                              ifelse(month %in% c(3,4),2,
                                              ifelse(month %in% c(5,6),3,
                                              ifelse(month %in% c(7,8),4,
                                              ifelse(month %in% c(9,10),5,
                                              ifelse(month %in% c(11,12),6,NA)))))))%>%
  group_by(year,wave)%>% 
  summarize(p_open = (sum(reg_season =="open")/length(reg_season)))
P.Open.Days$p_closed = 1-P.Open.Days$p_open
colnames(P.Open.Days) = c("YEAR","WAVE","p_open","p_closed")

# 2 - Choose Fishing Mode ###
Fish.Mode ="Cbt" # "Cbt"        "Hbt"        "Priv"       "Priv/Shore"

# 3 - Calculate discards for each fed season ####
# 1 - open and closed season occurs in a given wave
# Pivot discard data by wave and year to get open and closed season discards
RecCatch.cbt.fed1 = RecCatch %>% filter(NEW_MODEN==Fish.Mode,fed_closed==1)%>% 
  group_by(YEAR,Gulf,WAVE)%>%summarise(sum(B2))
colnames(RecCatch.cbt.fed1) = c("YEAR","Gulf","WAVE","B2")

# merge based on wave and year to get proportion of season open and closed and discards in one the table
RecCatch.cbt.fed1= merge(RecCatch.cbt.fed1,P.Open.Days, by= c("YEAR","WAVE"))

# multiply proportions of season by sum(B2)
RecCatch.cbt.fed1$Open = as.numeric(RecCatch.cbt.fed1$B2)*as.numeric(RecCatch.cbt.fed1$p_open)
RecCatch.cbt.fed1$Closed = as.numeric(RecCatch.cbt.fed1$B2)*as.numeric(RecCatch.cbt.fed1$p_closed)

RecCatch.cbt.fed1.sum = RecCatch.cbt.fed1 %>%
  group_by(YEAR,Gulf)%>%
  summarise(Open=sum(Open),
            Closed = sum(Closed))

# 0 - Open season only (fed_closed=0) discards (b2)
#Pivot discard data by wave and year to get open and closed season discards
RecCatch.cbt.fed0 = RecCatch %>% filter(NEW_MODEN==Fish.Mode,fed_closed==0)%>% 
  group_by(YEAR,Gulf,WAVE)%>%summarise(sum(B2))
colnames(RecCatch.cbt.fed0) = c("YEAR","Gulf","WAVE","B2")
head(RecCatch.cbt.fed0)

# merge based on wave and year to get proportion of season open and closed and discards in one the table
RecCatch.cbt.fed0 = merge(RecCatch.cbt.fed0,P.Open.Days, by= c("YEAR","WAVE"))

# multiply proportions of season by sum(B2)
RecCatch.cbt.fed0$Open = as.numeric(RecCatch.cbt.fed0$B2)

RecCatch.cbt.fed0.sum = RecCatch.cbt.fed0 %>%
  group_by(YEAR,Gulf)%>%
  summarise(Open=sum(Open))


# 2 - Fed_season=2 (closed season)
#Pivot discard data by wave and year to get open and closed season discards
RecCatch.cbt.fed2 = RecCatch %>% filter(NEW_MODEN==Fish.Mode,fed_closed==2)%>% 
  group_by(YEAR,Gulf,WAVE)%>%summarise(sum(B2))
colnames(RecCatch.cbt.fed2) = c("YEAR","Gulf","WAVE","B2")


head(RecCatch.cbt.fed2)
# merge based on wave and year to get proportion of season open and closed and discards in one the table
RecCatch.cbt.fed2 = merge(RecCatch.cbt.fed2,P.Open.Days, by= c("YEAR","WAVE"))

# multiply proportions of season by sum(B2)
RecCatch.cbt.fed2$Closed = as.numeric(RecCatch.cbt.fed2$B2)

RecCatch.cbt.fed2.sum = RecCatch.cbt.fed2 %>%
  group_by(YEAR,Gulf)%>%
  summarise(Closed=sum(Closed))

# 4 - Put all fed seasons data together ####
Cbt.OpenClosed = merge(RecCatch.cbt.fed2.sum, RecCatch.cbt.fed1.sum, by=c("YEAR","Gulf"), all = TRUE)
Cbt.OpenClosed = merge(Cbt.OpenClosed, RecCatch.cbt.fed0.sum, by=c("YEAR","Gulf"), all = TRUE)

Cbt.OpenClosed = Cbt.OpenClosed %>% rowwise()%>%
  mutate(Open = sum(c_across(starts_with("Open")), na.rm = T),
                                           Closed =  sum(c_across(starts_with("Closed")), na.rm = T))
# 5 - Organize and export for SS ####
Cbt = Cbt.OpenClosed %>% select(c(-Closed.x,-Closed.y,-Open.x,-Open.y))%>%
  pivot_wider(
    names_from = Gulf,
    values_from = c(Open, Closed),values_fill = 0)
colnames(Cbt)[2:dim(Cbt)[2]] = paste0("Cbt_",colnames(Cbt)[2:dim(Cbt)[2]])

write.csv(Cbt,paste0(OutputDir,"\\Cbt_Discards.csv"),row.names = FALSE)

              