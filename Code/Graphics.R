# Code to graph the discards for different modes
library(tidyverse)
user = Sys.getenv("USERNAME")
OutputDir = paste0("C:/Users/",user,"/Documents/GitHub/OpenClosedRecDiscards/OutputFiles")

# mode = "Cbt", "FedPriv","StatePriv", "HBT"

timeseries.graph = function(mode = "Cbt",Dir = OutputDir){
  discard.dat = read.csv(paste0(Dir,"\\",dir(Dir)[grep(paste0(mode,"_Discards.csv"),dir(Dir))]))
discard.dat.long = discard.dat %>% pivot_longer(cols = starts_with(mode),
                                                names_to = "Factor",
                                                names_prefix = paste0(mode,"_"),
                                                values_to = "B2",
                                                values_drop_na = TRUE)%>% separate(Factor,c("Season","Gulf"))%>%
  mutate(Relative = B2/mean(B2))
  
ggplot(discard.dat.long,aes(x=YEAR,y=B2, linetype = Season))+ geom_line() +facet_wrap(~Gulf)
ggsave(paste0(mode,"_Discards.png"),device = png,path = Dir)

#ggplot(discard.dat.long,aes(x=YEAR,y=Relative, linetype = Season))+ geom_line() +facet_wrap(~Gulf)

}

timeseries.graph(mode="StatePriv")
timeseries.graph(mode="FedPriv")
timeseries.graph(mode="Cbt")
