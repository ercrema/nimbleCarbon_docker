library(rcarbon)
library(here)
data(euroevol)
DK=subset(euroevol,Country=="Denmark") #subset of Danish dates
write.csv(DK,file=here('data','DK.csv'),row.names=FALSE)
