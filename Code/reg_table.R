#Creating the regulations table
library(tidyverse)
#Terminal year of model
Termyear<-2021
Years<-seq(1980,Termyear,1)
Years

#Leap year
LeapYr<-c(seq(1980,Termyear,4))
DaysLeap<-366
DaysPerMonthLeap<-c(seq(1,31,1),seq(1,29,1),seq(1,31,1),seq(1,30,1),seq(1,31,1),seq(1,30,1),
                seq(1,31,1),seq(1,31,1),seq(1,30,1),seq(1,31,1),seq(1,30,1),seq(1,31,1))
MonthsLeap<-c(rep(1,31),rep(2,29),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),
          rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))

#Non-leap year
NonLeapYr<-Years[!Years %in% LeapYr]
Days<-365
DaysPerMonth<-c(seq(1,31,1),seq(1,28,1),seq(1,31,1),seq(1,30,1),seq(1,31,1),seq(1,30,1),
                seq(1,31,1),seq(1,31,1),seq(1,30,1),seq(1,31,1),seq(1,30,1),seq(1,31,1))
Months<-c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),
          rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31))

RegTable<-function(Year){
  if(Year %in% NonLeapYr) {
    dat<-as.data.frame(cbind(rep(Year,Days),Months,DaysPerMonth))
    colnames(dat)<-c("year","month","day")
    dat$date<-paste0(dat$month,"/",dat$day,"/",dat$year,sep="")
    dat$date<-as.Date(dat$date,"%m/%d/%Y")
    dat
    }
  else if (Year %in% LeapYr) {
    dat<-as.data.frame(cbind(rep(Year,DaysLeap),MonthsLeap,DaysPerMonthLeap))
    colnames(dat)<-c("year","month","day")
    dat$date<-paste0(dat$month,"/",dat$day,"/",dat$year,sep="")
    dat$date<-as.Date(dat$date,"%m/%d/%Y")
    dat
  }
}

Regdat<-rbind(RegTable(Years[1]), RegTable(Years[2]), RegTable(Years[3]), RegTable(Years[4]),
             RegTable(Years[5]),RegTable(Years[6]),RegTable(Years[7]),RegTable(Years[8]),
             RegTable(Years[9]),RegTable(Years[10]),RegTable(Years[11]),RegTable(Years[12]),
             RegTable(Years[13]),RegTable(Years[14]),RegTable(Years[15]),RegTable(Years[16]),
             RegTable(Years[17]),RegTable(Years[18]),RegTable(Years[19]),RegTable(Years[20]),
             RegTable(Years[21]),RegTable(Years[22]),RegTable(Years[23]),RegTable(Years[24]),
             RegTable(Years[25]),RegTable(Years[26]),RegTable(Years[27]),RegTable(Years[28]),
             RegTable(Years[29]),RegTable(Years[30]),RegTable(Years[31]),RegTable(Years[32]),
             RegTable(Years[33]),RegTable(Years[34]),RegTable(Years[35]),RegTable(Years[36]),
             RegTable(Years[37]),RegTable(Years[38]),RegTable(Years[39]),RegTable(Years[40]),
             RegTable(Years[41]),RegTable(Years[42]))
dim(Regdat)
summary(Regdat)

DIR<-'C:\\Users\\latreese.denson\\Desktop\\RecDiscards_forSS\\InputFiles\\'

#Include Regulations
Regdat$reg_season="open"

####################################################################################################
#------------------------------- PRIVATE REGULATIONS THROUGH 2016----------------------------------#
####################################################################################################
# 62 FR 61700
#Closure is effective 12:01 a.m., local time, November 27, 1997, through December 31, 1997.
Regdat$reg_season[Regdat$year==1997 & Regdat$month==11 & Regdat$day>=27]="closed"
Regdat$reg_season[Regdat$year==1997 & Regdat$month==12]="closed"
table(Regdat$reg_season[Regdat$year==1997])[2]
# Matches reg table in 062017 file = 330 days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf

#################################################################################################
# 63 FR 45760
# The recreational red snapper fishery was closed on September 30, 1998 (63 FR 45760).
Regdat$reg_season[Regdat$year==1998 & Regdat$month==9 & Regdat$day>=30]="closed"
Regdat$reg_season[Regdat$year==1998 & Regdat$month>9]="closed"
table(Regdat$reg_season[Regdat$year==1998])[2] 
# Matches reg table in 062017 file = 272 days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf

#############################################################################################
# 64 FR 30445
# The closure of the recreational fishery for red snapper in the EEZ of the Gulf of Mexico is effective 12:01 a.m.,
# local time, August 29, 1999, through December 31, 1999.
Regdat$reg_season[Regdat$year==1999 & Regdat$month==8 & Regdat$day>=29]="closed"
Regdat$reg_season[Regdat$year==1999 & Regdat$month>8]="closed"
table(Regdat$reg_season[Regdat$year==1999])[2] #matches reg table = 240
# Matches reg table in 062017 file = 240 days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf

############################################################################################################################
# 65 FR 50158
#close the recreational red snapper fishery from January 1 through April 20 and from November 1 through December 31
#####################################################################################################
#-----http://archive.gulfcouncil.org/docs//amendments/Regional%20Management%20Red%20Snapper.pdf-----#
# A fixed recreational season of April 21 through October 31 (194 days) was established for 2000 through 2007. 
Regdat$reg_season[(Regdat$year>=2000 & Regdat$year<=2007) & Regdat$month<=3]="closed"
Regdat$reg_season[(Regdat$year>=2000 & Regdat$year<=2007) & Regdat$month==4 & Regdat$day<21]="closed"
Regdat$reg_season[(Regdat$year>=2000 & Regdat$year<=2007) & Regdat$month>=11]="closed"
table(Regdat$reg_season[Regdat$year==2000])[2] 
# Matches reg table in 062017 file = 194 days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf
table(Regdat$reg_season[Regdat$year==2001])[2] #matches reg table = 194
# Matches reg table in 062017 file = 194 days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf
table(Regdat$reg_season[Regdat$year==2002])[2] #matches reg table = 194
# Matches reg table in 062017 file = 194 days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf
table(Regdat$reg_season[Regdat$year==2003])[2] #matches reg table = 194
# Matches reg table in 062017 file = 194 days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf
table(Regdat$reg_season[Regdat$year==2004])[2] #matches reg table = 194
# Matches reg table in 062017 file = 194 days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf
table(Regdat$reg_season[Regdat$year==2005])[2] #matches reg table = 194
# Matches reg table in 062017 file = 194 days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf
table(Regdat$reg_season[Regdat$year==2006])[2] #matches reg table = 194
# Matches reg table in 062017 file = 194 days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf
table(Regdat$reg_season[Regdat$year==2007])[2] #matches reg table = 194

#Original coding from 2014 --- cannot find any reference to opening day of April 26:
# Is listed in Stock Assessment of Red Snapper in the Gulf of Mexico, December 3, 2009, but not referenced
#
# Regdat$reg_season[Regdat$year==2007 & Regdat$month<=3]="closed"
# Regdat$reg_season[Regdat$year==2007 & Regdat$month==4 & Regdat$day<26]="closed"
# Regdat$reg_season[Regdat$year==2007 & Regdat$month>=11]="closed"
# table(Regdat$reg_season[Regdat$year==2007])[2] 
# Does not match reg table in 062017 file = 194 days
# Does not match Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf
#matches reg table = 189 (corrected on table from 194)

################################################################################################
# 73 FR 15674
# The recreational fishery for red snapper in the EEZ of the GOM closure is effective 12:01 a.m., 
# Local time, August 5, 2008, through December 31, 2008, the end of the current fishing year. 
Regdat$reg_season[Regdat$year==2008 & Regdat$month<=5]="closed"                    
Regdat$reg_season[Regdat$year==2008 & Regdat$month==8 & Regdat$day>=5]="closed"     
Regdat$reg_season[Regdat$year==2008 & Regdat$month>=9]="closed"
table(Regdat$reg_season[Regdat$year==2008])[2]
# Matches reg table in 062017 file = 65 days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf

############################################################################################
# 74 FR 21558
# NMFS is closing the recreational red snapper fishery in the Gulf EEZ effective 12:01 a.m. local time on August 15, 2009; 
Regdat$reg_season[Regdat$year==2009 & Regdat$month<=5]="closed"
Regdat$reg_season[Regdat$year==2009 & Regdat$month==8 & Regdat$day>=15]="closed"
Regdat$reg_season[Regdat$year==2009 & Regdat$month>=9]="closed"
table(Regdat$reg_season[Regdat$year==2009])[2] 
# Matches reg table in 062017 file = 75 days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf

############################################################################################
# 75 FR 23186
# Closes the recreational red snapper component of the Gulf reef fish fishery at 12:01 a.m., local time, July 24, 2010
Regdat$reg_season[Regdat$year==2010 & Regdat$month<=5]="closed"
Regdat$reg_season[Regdat$year==2010 & Regdat$month==7 & Regdat$day>=24]="closed"
Regdat$reg_season[Regdat$year==2010 & Regdat$month>=8]="closed"
table(Regdat$reg_season[Regdat$year==2010])[2] 
# Matches reg table in 062017 file = 53 days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf

#deep water horizon special weekend openings
Regdat$reg_season[Regdat$year==2010 & Regdat$month==10 & (Regdat$day>=1 & Regdat$day<=3)]="open"
Regdat$reg_season[Regdat$year==2010 & Regdat$month==10 & (Regdat$day>=8 & Regdat$day<=10)]="open"
Regdat$reg_season[Regdat$year==2010 & Regdat$month==10 & (Regdat$day>=15 & Regdat$day<=17)]="open"
Regdat$reg_season[Regdat$year==2010 & Regdat$month==10 & (Regdat$day>=22 & Regdat$day<=24)]="open"
Regdat$reg_season[Regdat$year==2010 & Regdat$month==10 & (Regdat$day>=29 & Regdat$day<=31)]="open"
Regdat$reg_season[Regdat$year==2010 & Regdat$month==11 & (Regdat$day>=5 & Regdat$day<=7)]="open"
Regdat$reg_season[Regdat$year==2010 & Regdat$month==11 & (Regdat$day>=12 & Regdat$day<=14)]="open"
Regdat$reg_season[Regdat$year==2010 & Regdat$month==11 & (Regdat$day>=19 & Regdat$day<=21)]="open"
table(Regdat$reg_season[Regdat$year==2010])[2] 
# Matches reg table in 062017 file = 53 days + 24 day fall season due to DWH days
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf

#################################################################################################
# 76 FR 50143
# Closure date for the recreational red snapper season of 12:01 a.m., local time, July 19, 2011.
Regdat$reg_season[Regdat$year==2011 & Regdat$month<=5]="closed"
Regdat$reg_season[Regdat$year==2011 & Regdat$month==7 & Regdat$day>=19]="closed"
Regdat$reg_season[Regdat$year==2011 & Regdat$month>=8]="closed"
table(Regdat$reg_season[Regdat$year==2011])[2] 
# Does NOT match reg table in 062017 file = 49 days (incorrect)
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf

################################################################################################
# 77 FR 39647
# NMFS is extending the recreational red snapper fishing season for 6 days. The extension is effective from
# 12:01 a.m., local time, July 11, 2012, until 12:01 a.m., local time, July 17, 2012.  The season will close at
# 12:01 a.m. local time on July 17, 2012. 
Regdat$reg_season[Regdat$year==2012 & Regdat$month<=5]="closed"
Regdat$reg_season[Regdat$year==2012 & Regdat$month==7 & Regdat$day>=17]="closed"
Regdat$reg_season[Regdat$year==2012 & Regdat$month>=8]="closed"
table(Regdat$reg_season[Regdat$year==2012])[2] 
# Matches reg table in 062017 file = 46 
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf


###############################################################################################
# 78 FR 34586
# NMFS closes the recreational sector for red snapper in the entire Gulf EEZ at 12:01 a.m., local time, June 29, 2013
Regdat$reg_season[Regdat$year==2013 & Regdat$month<=5]="closed"
Regdat$reg_season[Regdat$year==2013 & Regdat$month==6 & Regdat$day>=29]="closed"
Regdat$reg_season[Regdat$year==2013 & Regdat$month>=7]="closed"
########################################################################################################
# 78 FR 57313
# NMFS will close recreational harvest of red snapper in the Gulf EEZ at 12:01 a.m., local time, October 15, 2013,
# and it will remain closed until the start of the next fishing season.
Regdat$reg_season[Regdat$year==2013 & Regdat$month==10 & (Regdat$day>=1 & Regdat$day<=14)]="open"
table(Regdat$reg_season[Regdat$year==2013])[2] 
# Matches reg table in 062017 file = 42 days 
# Matches Table 1.1.2 in http://sero.nmfs.noaa.gov/sustainable_fisheries/gulf_fisheries/reef_fish/2014/rs_am_framework/documents/pdfs/gulf_framework_rs_am_ea.pdf


###############################################################################################
# 79 FR 27768
# the recreational fishing season will open at 12:01 a.m., local time, on June 1, 2014, and close at 12:01
# a.m., local time, on June 10, 2014.
Regdat$reg_season[Regdat$year==2014 & Regdat$month<=5]="closed"
Regdat$reg_season[Regdat$year==2014 & Regdat$month==6 & Regdat$day>=10]="closed"
Regdat$reg_season[Regdat$year==2014 & Regdat$month>=7]="closed"
table(Regdat$reg_season[Regdat$year==2014])[2] 
# Matches reg table in 062017 file = 9 days


############################################################################################
# 80 FR 24832
# The for-hire component will close at 12:01 a.m., local time, on July 15, 2015.
# The private angling component will close at 12:01 a.m., local time, June 11, 2015.
Regdat$reg_season[Regdat$year==2015 & Regdat$month<=5]="closed"
Regdat$reg_season[Regdat$year==2015 & Regdat$month==6 & Regdat$day>=11]="closed"
Regdat$reg_season[Regdat$year==2015 & Regdat$month>=7]="closed"
table(Regdat$reg_season[Regdat$year==2015])[2]
# Matches reg table in 062017 file = 10 days

###############################################################################################
# 81 FR 38110
# The Federal season for the Federal for-hire component began at 12:01 a.m., local time, June 1, 2016, and will close
# at 12:01 a.m., local time, July 17, 2016
# NMFS projects the recreational red snapper season for the private angling component can be
# extended for an additional 2 days, and will therefore close at 12:01 a.m., local time, on June 12, 2016
Regdat$reg_season[Regdat$year==2016 & Regdat$month<=5]="closed"
Regdat$reg_season[Regdat$year==2016 & Regdat$month==6 & Regdat$day>=12]="closed"
Regdat$reg_season[Regdat$year==2016 & Regdat$month>=7]="closed"
table(Regdat$reg_season[Regdat$year==2016])[2] 
# Matches reg table in 062017 file = 11 days

# Write out the federal private regs to the terminal year
RegdatPrivate = Regdat%>% filter(between(year,1980,2016))
RegdatPrivate$mode<-"Private"
head(RegdatPrivate)
write.csv(RegdatPrivate,paste0(DIR,"RSN_FedPrivate_Regs_through2016.csv"))

######################################################################################################
#-------------------------------- FOR HIRE REGULATIONS THROUGH 2021 ----------------------------------#
######################################################################################################

RegdatForHire<-Regdat
# Reset the overlapping years (a bandaid)
RegdatForHire$reg_season[RegdatForHire$year==2015 |RegdatForHire$year==2016|RegdatForHire$year==2017|RegdatForHire$year==2018|RegdatForHire$year==2019|RegdatForHire$year==2020|RegdatForHire$year==2021] = "open"
RegdatForHire$mode<-NA
RegdatForHire$mode<-"For-hire"
head(RegdatForHire)

############################################################################################
# 80 FR 24832
# The for-hire component will close at 12:01 a.m., local time, on July 15, 2015.
# The private angling component will close at 12:01 a.m., local time, June 11, 2015.
RegdatForHire$reg_season[RegdatForHire$year==2015 & RegdatForHire$month<=5]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2015 & RegdatForHire$month==7 & RegdatForHire$day>=15]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2015 & RegdatForHire$month>=8]="closed"
table(RegdatForHire$reg_season[RegdatForHire$year==2015])[2]
# Matches reg table in 062017 file = 44 days

###############################################################################################
# 81 FR 25583
# The Federal season for the Federal for-hire component began at 12:01 a.m., local time, June 1, 2016, and will close
# at 12:01 a.m., local time, July 17, 2016
# NMFS projects the recreational red snapper season for the private angling component can be
# extended for an additional 2 days, and will therefore close at 12:01 a.m., local time, on June 12, 2016
RegdatForHire$reg_season[RegdatForHire$year==2016 & RegdatForHire$month<=5]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2016 & RegdatForHire$month==7 & RegdatForHire$day>=17]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2016 & RegdatForHire$month>=8]="closed"
table(RegdatForHire$reg_season[RegdatForHire$year==2016])[2] 
# Matches reg table in 062017 file = 46 days

###############################################################################################
# 82 FR 21140
# The Federal recreational season for red snapper in the Gulf EEZ begins at 12:01 a.m., local time, on June 1, 2017.
# For recreational harvest by the private angling component, the season closes at 12:01 a.m., local time, on June 4, 2017.
# For recreational harvest by the Federal for-hire component, the season closes at
# 12:01 a.m., local time, on July 20, 2017. 
RegdatForHire$reg_season[RegdatForHire$year==2017 & RegdatForHire$month<=5]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2017 & RegdatForHire$month==7 & RegdatForHire$day>=20]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2017 & RegdatForHire$month>=8]="closed"
table(RegdatForHire$reg_season[RegdatForHire$year==2017])[2] 
# Matches reg table in 12282021 file = 49 days

###############################################################################################
# 83 FR 17623
# The Federal recreational season for red snapper in the Gulf EEZ begins at 12:01 a.m., local time, on June 1, 2018.
# For recreational harvest by the private angling component, the season closes at 12:01 a.m., local time, on June 1, 2018. 
# For recreational harvest by the Federal for-hire component, the season closes at 12:01 a.m., local time, on July 22, 2018.
RegdatForHire$reg_season[RegdatForHire$year==2018 & RegdatForHire$month<=5]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2018 & RegdatForHire$month==7 & RegdatForHire$day>=22]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2018 & RegdatForHire$month>=8]="closed"
table(RegdatForHire$reg_season[RegdatForHire$year==2018])[2] 
# Matches reg table in 12282021 file = 51 days

###############################################################################################
# 84 FR 8825 
# The season for the recreational sector for red snapper in the Gulf EEZ opens on June 1, each year. 
# For recreational harvest by the private angling component, the season closes at 12:01 a.m., local time, June 1, 2019. 
# NMFS has issued exempted fishing permits (EFPs) that allow each Gulf state (Texas, Louisiana, Mississippi, Alabama, and Florida)
# to set the private recreational season for red snapper that are landed from state and Federal waters in that state during 2018 and 2019. 
# For recreational harvest by the Federal for-hire component, the season closes at 12:01 a.m., local time, on August 2, 2019. 
RegdatForHire$reg_season[RegdatForHire$year==2019 & RegdatForHire$month<=5]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2019 & RegdatForHire$month==8 & RegdatForHire$day>=2]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2019 & RegdatForHire$month>=9]="closed"
table(RegdatForHire$reg_season[RegdatForHire$year==2019])[2] 
# Does not match the reg table in 12282021 file = 48 days

###############################################################################################
# 85 FR 14171
# The red snapper recreational for-hire component in the Gulf EEZ opens on June 1, 2020, 
# and will close at 12:01 a.m., local time, on August 2, 2020. 
RegdatForHire$reg_season[RegdatForHire$year==2020 & RegdatForHire$month<=5]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2020 & RegdatForHire$month==8 & RegdatForHire$day>=2]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2020 & RegdatForHire$month>=9]="closed"
table(RegdatForHire$reg_season[RegdatForHire$year==2020])[2] 
# ? Matches reg table in 12282021 file = 63 days

###############################################################################################
# 86 FR 15430
# The red snapper recreational for-hire component in the Gulf EEZ opens on June 1, 2021, 
# and will close at 12:01 a.m., local time, on August 3, 2021.
RegdatForHire$reg_season[RegdatForHire$year==2021 & RegdatForHire$month<=5]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2021 & RegdatForHire$month==8 & RegdatForHire$day>=3]="closed"
RegdatForHire$reg_season[RegdatForHire$year==2021 & RegdatForHire$month>=9]="closed"
table(RegdatForHire$reg_season[RegdatForHire$year==2021])[2] 
# not included in reg table yet

# Write out For Hire Regs ####
write.csv(RegdatForHire,paste0(DIR,"RSN_ForHire_Regs_through2021.csv")) 


