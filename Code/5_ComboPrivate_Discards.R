# Merge state and fed by code to reduce issues 
library(tidyverse)
user = Sys.getenv("USERNAME")
OutputDir = paste0("C:/Users/",user,"/Documents/GitHub/OpenClosedRecDiscards/OutputFiles")

StatePriv_discard = read.csv(paste0(OutputDir,"/StatePriv_Discards.csv"))
StatePriv_discard_pre = data.frame(YEAR = 1981:2012,matrix(0,32,6),colnames=F)
colnames(StatePriv_discard_pre) = colnames(StatePriv_discard)
StatePriv = bind_rows(StatePriv_discard_pre,StatePriv_discard) # produces and extra col thats not needed

FedPriv_discard = read.csv(paste0(OutputDir,"/FedPriv_Discards.csv"))
FedPriv_discard_post = data.frame(YEAR = 2017:2019,matrix(0,3,6))
colnames(FedPriv_discard_post) = colnames(FedPriv_discard)
FedPriv = bind_rows(as.data.frame(FedPriv_discard),FedPriv_discard_post)

Priv_discard = data.frame(YEAR = 1981:2019,matrix(0,39,6))
colnames(Priv_discard) = c("YEAR","Priv_Open_West","Priv_Open_Central","Priv_Open_East",
                           "Priv_Closed_West","Priv_Closed_Central","Priv_Closed_East")
#Open
Priv_discard$Priv_Open_West = FedPriv$FedPriv_Open_West + StatePriv$StatePriv_Open_West
Priv_discard$Priv_Open_Central = FedPriv$FedPriv_Open_Central + StatePriv$StatePriv_Open_Central
Priv_discard$Priv_Open_East = FedPriv$FedPriv_Open_East + StatePriv$StatePriv_Open_East

#Closed
Priv_discard$Priv_Closed_West = FedPriv$FedPriv_Closed_West + StatePriv$StatePriv_Closed_West
Priv_discard$Priv_Closed_Central = FedPriv$FedPriv_Closed_Central + StatePriv$StatePriv_Closed_Central
Priv_discard$Priv_Closed_East = FedPriv$FedPriv_Closed_East + StatePriv$StatePriv_Closed_East

write.csv(Priv_discard,paste0(OutputDir,"\\TotalPriv_Discards.csv"),row.names = FALSE)

cat("\nDone producing total (open/closed) Private discards, look in output folder\n")

