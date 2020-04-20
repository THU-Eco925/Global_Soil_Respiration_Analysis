library(dplyr) 
library(magrittr)
workingpath = getwd()
res.data <- read.csv(choose.files(),check.names=FALSE) 

### check the negative value
res.data$Ra_annual[which(res.data$Ra_annual<0)] %>% abs(.)
res.data$Rh_annual[which(res.data$Rh_annual<0)] %>% abs(.)

# calculate the missing value by "rs = ra + rh"
rs.lost = res.data %$% which(!is.na(Rh_annual)&!is.na(Ra_annual)&is.na(Rs_annual))
rh.lost = res.data %$% which(!is.na(Rs_annual)&!is.na(Ra_annual)&is.na(Rh_annual))
ra.lost = res.data %$% which(!is.na(Rs_annual)&!is.na(Rh_annual)&is.na(Ra_annual))

res.data$Rs_annual[rs.lost] = res.data$Ra_annual[rs.lost] + res.data$Rh_annual[rs.lost]
res.data$Rh_annual[rh.lost] = res.data$Rs_annual[rh.lost] - res.data$Ra_annual[rh.lost]
res.data$Ra_annual[ra.lost] = res.data$Rs_annual[ra.lost] - res.data$Rh_annual[ra.lost]

### check the data without year or latitude.
year.lost = res.data %$% which(!is.na(Rh_annual)&is.na(Study_midyear))
lat.lost = res.data %$% which(!is.na(Rh_annual)&is.na(Latitude))

print(res.data[c(year.lost,lat.lost),])

# we manaually add the data according to the available references
attach(res.data)
res.data[Record_number==3260,"Latitude"] = 35
res.data[Study_number==6243,"Study_midyear"] = 2008
res.data[Record_number==2975,"Latitude"] = 68.3 
res.data[Study_number == 4119,'Study_midyear'] = 2005.25
res.data[Study_number == 4119,'YearsOfData'] = 1
detach(res.data)

rm(a,NAyear,NAlat)

### --------------------------------------------------------------
res.data$Site = paste(res.data$Lat,res.data$Lon)
res.data$Coord = paste(res.data$Latitude,res.data$Longitude)

### adjust some categorical factors
# forest type
attach(res.data)
leaflabel = Leaf_habit[which(Ecosystem_type == 'Forest')]
forestlabel = Ecosystem_type[which(Ecosystem_type == 'Forest')]
forest_type = paste(leaflabel,forestlabel)
forest_type[which(forest_type == ' Forest')] = 'Others'
res.data$Ecosystem_type = as.character(res.data$Ecosystem_type)
res.data$Ecosystem_type[which(Ecosystem_type == 'Forest')] = forest_type

# method type
Meas_method[which(Meas_method == 'Gas chromatography')] = "Gas Chromatography"
Meas_method[which(Meas_method == 'IRGA, static, gradient')] = "IRGA"

detach(res.data)

# save result
write.csv(res.data,'01_cleandata.csv',row.names=FALSE)
  
