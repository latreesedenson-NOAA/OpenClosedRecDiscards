# Fed waters - Private Discards
# Methods:
# 1. Calculate season length by mode based on management history SS provided by SERO
# 2. Calculate number of days per wave (simple)
# 3. Calculate the number of open season days by wave based on the season length by mode
# 4. Calculate the proportion of open season days per wave
# 5. Use recreational landings/discards SS (e.g., RS_rec_catGEN_8119_20211123) to create pivot tables of discards (b2) 
    # by category Fed_closed (0=open season, 1=both open and closed season (i.e., season closed mid wave), 
    # and 2=closed season) and wave. This is only for all years prior to 2013 since this is when state control kicks in.
# 6. Use proportions from 4 to partition discards by wave to either the open or closed season.
    # Texas state waters are always open, if there are discards and fed_closed == 1 or 2, 
    # they need to be added to the open season (fed_closed = 0) discards.
# 7. Sum discards by open/closed season to get total discards (i.e., add discards for waves that are 
    # completely open/closed to those that are of category Fed_closed=1 and have been partitioned to the associated 
    # open/closed season).
# 8. Repeat steps 5-7 for years 2013-2016 with a federal jurisdiction. These are the
    # last years that the federal season is adhered to. Use original fed_closed here.
# 9. Question your life decisions. 

library(tidyverse)

# 1 - Determine Season Length based on mode ####
InputDir = "C:\\Users\\latreese.denson\\Desktop\\RecDiscards_forSS\\InputFiles"
OutputDir = "C:\\Users\\latreese.denson\\Desktop\\RecDiscards_forSS\\OutputFiles"
Regs = read.csv(paste0(InputDir,"\\RSN_FedPrivate_Regs_through2016.csv"))
RecCatch = read.csv(paste0(InputDir,"/RS_rec_catGEN_8119_20211123.csv"))

# Getting to the proportion of open season days by wave
P.Open.Days = Regs%>%mutate(wave = ifelse(month %in% c(1,2),1,
                                              ifelse(month %in% c(3,4),2,
                                              ifelse(month %in% c(5,6),3,
                                              ifelse(month %in% c(7,8),4,
                                              ifelse(month %in% c(9,10),5,
                                              ifelse(month %in% c(11,12),6,NA)))))))%>%
  group_by(year,wave)%>% 
  summarize(p_open = (sum(reg_season =="open")/length(reg_season)))
P.Open.Days$p_closed = 1-P.Open.Days$p_open
colnames(P.Open.Days) = c("YEAR","WAVE","p_open","p_closed")

# 1a - Manipulate GenRec/fed_closed Data to include Texas State waters as Always Open ####
RecCatch = RecCatch %>% mutate(new_fed_closed = ifelse((NEW_STA == "TX"& JURISDICTION=="State"),0,fed_closed)) # 0 = always open

check = RecCatch%>%filter(NEW_STA=="TX",NEW_MODEN=="Priv")
head(check)
table(check$fed_closed,check$JURISDICTION,check$new_fed_closed) # Good to go! All texas state jur. 1's or 2's are now all 0's (all open) 

# 2 - Choose Fishing Mode (CHB, HBT, PRIV) ####
Fish.Mode ="Priv" # "Cbt"        "Hbt"        "Priv"       "Priv/Shore"

# No Jurisdiction but TX always open up to 2012 ####
# 3 - Calculate discards for each new fed season (0 - open, 1 - open/closed, 2 - closed) ####
# fed_closed = 1 - open and closed season occurs in a given wave
# Pivot discard data by wave and year to get open and closed season discards
# Should this also be reduced to catches only in federal waters?NO, because everything is federal prior to 2013
RecCatch.fed1 = RecCatch %>% filter(NEW_MODEN==Fish.Mode,new_fed_closed==1)%>% 
  group_by(YEAR,Gulf,WAVE)%>%summarise(sum(B2))
colnames(RecCatch.fed1) = c("YEAR","Gulf","WAVE","B2")

# merge based on wave and year to get proportion of season open and closed and discards in one the table
RecCatch.fed1= merge(RecCatch.fed1,P.Open.Days, by= c("YEAR","WAVE"))

# multiply proportions of season by sum(B2)
RecCatch.fed1$Open = as.numeric(RecCatch.fed1$B2)*as.numeric(RecCatch.fed1$p_open)
RecCatch.fed1$Closed = as.numeric(RecCatch.fed1$B2)*as.numeric(RecCatch.fed1$p_closed)

RecCatch.fed1.sum = RecCatch.fed1 %>%
  group_by(YEAR,Gulf)%>%
  summarise(Open=sum(Open),
            Closed = sum(Closed))

# fed_closed = 0 - Open season only discards (b2)
#Pivot discard data by wave and year to get open and closed season discards
RecCatch.fed0 = RecCatch %>% filter(NEW_MODEN==Fish.Mode,new_fed_closed==0)%>% 
  group_by(YEAR,Gulf,WAVE)%>%summarise(sum(B2))
colnames(RecCatch.fed0) = c("YEAR","Gulf","WAVE","B2")

# merge based on wave and year to get proportion of season open and closed and discards in one the table
RecCatch.fed0 = merge(RecCatch.fed0,P.Open.Days, by= c("YEAR","WAVE"))

# multiply proportions of season by sum(B2)
RecCatch.fed0$Open = as.numeric(RecCatch.fed0$B2)

RecCatch.fed0.sum = RecCatch.fed0 %>%
  group_by(YEAR,Gulf)%>%
  summarise(Open=sum(Open))

# fed_closed = closed season
#Pivot discard data by wave and year to get open and closed season discards
RecCatch.fed2 = RecCatch %>% filter(NEW_MODEN==Fish.Mode,new_fed_closed==2)%>% 
  group_by(YEAR,Gulf,WAVE)%>%summarise(sum(B2))
colnames(RecCatch.fed2) = c("YEAR","Gulf","WAVE","B2")

# merge based on wave and year to get proportion of season open and closed and discards in one the table
RecCatch.fed2 = merge(RecCatch.fed2,P.Open.Days, by= c("YEAR","WAVE"))

# multiply proportions of season by sum(B2)
RecCatch.fed2$Closed = as.numeric(RecCatch.fed2$B2)

RecCatch.fed2.sum = RecCatch.fed2 %>%
  group_by(YEAR,Gulf)%>%
  summarise(Closed=sum(Closed))

# 4 - Get total fed seasons ####
OpenClosed = merge(RecCatch.fed2.sum, RecCatch.fed1.sum, by=c("YEAR","Gulf"), all = TRUE)
OpenClosed = merge(OpenClosed, RecCatch.fed0.sum, by=c("YEAR","Gulf"), all = TRUE)

OpenClosed = OpenClosed %>% rowwise()%>%
  mutate(Open = sum(c_across(starts_with("Open")), na.rm = T),
        Closed =  sum(c_across(starts_with("Closed")), na.rm = T))

# 5 - Organize and export for SS ####
Discards = OpenClosed %>% select(c(-Closed.x,-Closed.y,-Open.x,-Open.y))%>%
  pivot_wider(
    names_from = Gulf,
    values_from = c(Open, Closed),values_fill = 0)%>%filter(between(YEAR,1980,2012))

colnames(Discards)[2:dim(Discards)[2]] = paste0("Fed",Fish.Mode,"_",colnames(Discards)[2:dim(Discards)[2]])

# Federal Jurisdiction 2013-2016 ####
# 3 - Calculate discards for each fed season (0 - open, 1 - open/closed, 2 - closed) ####
# fed_closed = 1 - open and closed season occurs in a given wave
# Pivot discard data by wave and year to get open and closed season discards
# Reduced to catches only in federal waters here and use original fed_closed factor.
RecCatch.fed1 = RecCatch %>% filter(NEW_MODEN==Fish.Mode,fed_closed==1, JURISDICTION=="Federal")%>% 
  group_by(YEAR,Gulf,WAVE)%>%summarise(sum(B2))
colnames(RecCatch.fed1) = c("YEAR","Gulf","WAVE","B2")

# merge based on wave and year to get proportion of season open and closed and discards in one the table
RecCatch.fed1= merge(RecCatch.fed1,P.Open.Days, by= c("YEAR","WAVE"))

# multiply proportions of season by sum(B2)
RecCatch.fed1$Open = as.numeric(RecCatch.fed1$B2)*as.numeric(RecCatch.fed1$p_open)
RecCatch.fed1$Closed = as.numeric(RecCatch.fed1$B2)*as.numeric(RecCatch.fed1$p_closed)

RecCatch.fed1.sum = RecCatch.fed1 %>%
  group_by(YEAR,Gulf)%>%
  summarise(Open=sum(Open),
            Closed = sum(Closed))

# fed_closed = 0 - Open season only discards (b2)
#Pivot discard data by wave and year to get open and closed season discards
RecCatch.fed0 = RecCatch %>% filter(NEW_MODEN==Fish.Mode,fed_closed==0, JURISDICTION=="Federal")%>% 
  group_by(YEAR,Gulf,WAVE)%>%summarise(sum(B2))
colnames(RecCatch.fed0) = c("YEAR","Gulf","WAVE","B2")

# merge based on wave and year to get proportion of season open and closed and discards in one the table
RecCatch.fed0 = merge(RecCatch.fed0,P.Open.Days, by= c("YEAR","WAVE"))

# multiply proportions of season by sum(B2)
RecCatch.fed0$Open = as.numeric(RecCatch.fed0$B2)

RecCatch.fed0.sum = RecCatch.fed0 %>%
  group_by(YEAR,Gulf)%>%
  summarise(Open=sum(Open))

# fed_closed = closed season
#Pivot discard data by wave and year to get open and closed season discards
RecCatch.fed2 = RecCatch %>% filter(NEW_MODEN==Fish.Mode,fed_closed==2, JURISDICTION=="Federal")%>% 
  group_by(YEAR,Gulf,WAVE)%>%summarise(sum(B2))
colnames(RecCatch.fed2) = c("YEAR","Gulf","WAVE","B2")

# merge based on wave and year to get proportion of season open and closed and discards in one the table
RecCatch.fed2 = merge(RecCatch.fed2,P.Open.Days, by= c("YEAR","WAVE"))

# multiply proportions of season by sum(B2)
RecCatch.fed2$Closed = as.numeric(RecCatch.fed2$B2)

RecCatch.fed2.sum = RecCatch.fed2 %>%
  group_by(YEAR,Gulf)%>%
  summarise(Closed=sum(Closed))

# 4 - Get total fed seasons ####
OpenClosed = merge(RecCatch.fed2.sum, RecCatch.fed1.sum, by=c("YEAR","Gulf"), all = TRUE)
OpenClosed = merge(OpenClosed, RecCatch.fed0.sum, by=c("YEAR","Gulf"), all = TRUE)

OpenClosed = OpenClosed %>% rowwise()%>%
  mutate(Open = sum(c_across(starts_with("Open")), na.rm = T),
         Closed =  sum(c_across(starts_with("Closed")), na.rm = T))

# 5 - Organize and export for SS ####
Discards.late = OpenClosed %>% select(c(-Closed.x,-Closed.y,-Open.x,-Open.y))%>%
  pivot_wider(
    names_from = Gulf,
    values_from = c(Open, Closed),values_fill = 0)%>%filter(between(YEAR,2013,2016))

colnames(Discards.late)[2:dim(Discards.late)[2]] = paste0("Fed",Fish.Mode,"_",colnames(Discards.late)[2:dim(Discards.late)[2]])

# Combine years ####
FedPriv = rbind(Discards,Discards.late)

write.csv(FedPriv,paste0(OutputDir,"\\Fed",Fish.Mode,"_Discards.csv"),row.names = FALSE)

              