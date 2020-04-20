source('0_plot.R')
library('dplyr') #if funciton masked, please run it again
library("Hmisc")
library('magrittr')

###-------------------------------------------------------------------------------
work.data <- filter(res.data,Rh_ratio >= 0.25 & Study_midyear>=1987&  Meas_method %in% c('IRGA','Gas Chromatography') &Ecosystem_type != 'Agriculture')
dv = c('Rh_annual','Ra_annual','Rh_ratio')
idv = c('Study_midyear','Latitude','Elevation',
        'MAT','MAP','ΔMean.Temp.','Lat','Lon',
        'ΔMean.Precip.','Record_number','Latitude','Longitude','Site',
        'Meas_method','Ecosystem_type','Partition_method','Stage','SOC_015')
work.data = na.omit(work.data[,c(idv,dv)])
work.data = droplevels(work.data)
position = select(work.data,Record_number,Study_midyear,Latitude,Longitude,Site)

Sitedata = unique(data.frame(work.data$Lat,work.data$Lon,work.data$Site))
workingdata = filter(position, Site %in% temp.data$Site )
workingdata$Site = as.character(workingdata$Site)
temp.data$Site = as.character(temp.data$Site)
precip.data$Site = as.character(precip.data$Site)
Mean.Temp. = read.csv("Mean_Temp.csv",check.names = FALSE)
Mean.Precip. = read.csv("Mean_Precip.csv",check.names = FALSE)

Tempdata = Mean.Temp.%>% distinct(Site.x, .keep_all = TRUE)
#Mean.Temp.%<>% distinct(Latitude, .keep_all = TRUE) %>% .[,c(3,6:37)] %>% unlist(.)
Precipdata = Mean.Precip.%>% distinct(Site.x, .keep_all = TRUE)

Tempdata$Lat.Area = as.character(lapply(Tempdata$Latitude,labelLat)) %>% as.factor()
Precipdata$Lat.Area = as.character(lapply(Precipdata$Latitude,labelLat)) %>% as.factor()
temp = unlist(Tempdata[,c(6:37)])
precip = unlist(Precipdata[,c(6:37)])

lat = rep(Sitedata$work.data.Lat,times=32) %>% abs(.)
year = rep(1986:2017,each=123)
tt.temp = data.frame(lat,temp,year)
tt.precip= data.frame(lat,precip,year)
#-----

#-----
tt.temp$Lat.Area = as.character(lapply(tt.temp$lat,labelLat)) %>% as.factor()
tt.precip$Lat.Area = as.character(lapply(tt.precip$lat,labelLat)) %>% as.factor()
tt.temp$year %<>% as.numeric(.)
tt.precip$year %<>% as.numeric(.)
library(ggplot2)

Innerlmplot   <-geom_smooth(method='lm',aes(color = tt.precip[,4]))
#regression coefficients
factorselect = names(summary(tt.precip[,4]))
eqstring1=c()
eqstring2=c()
for (q in 1:length(factorselect)){
  lmsubdata =  filter(tt.precip,tt.precip[,4] == factorselect[q])
  eq= lm_eqn(lmsubdata)
  eqstring1[q]=paste0("S:",eq[1]," R^2:",eq[2])
  eqstring2[q]=paste0("P:",eq[3]," n=",nrow(lmsubdata))
}
eq1 <- data.frame(
  eq1   = eqstring1,
  eq2   = eqstring2,
  Lat.Area   = factorselect
)
eqn1_text <-geom_text(  data = eq1,size = 7,
                        mapping = aes(x=min(tt.precip[,3]), 
                                      y=0.9*max(tt.precip[,2]), label = eq1),
                        hjust=0)
eqn2_text <-geom_text(  data = eq1,size = 7,
                        mapping = aes(x=min(tt.precip[,3]), 
                                      y=max(tt.precip[,2]), label = eq2),
                        hjust=0)
PointThemeset <- theme(text = element_text(size=20),legend.position = "none",
                       axis.text.x = element_text(size=12, hjust = 0.5,vjust = 0.5),
                       panel.background=element_blank(),
                       panel.border = element_rect(colour = "grey", fill=NA, size=1))
Pointset <-geom_point(color = 'black',
                      alpha = 0.5,stroke = 1.5,
                      size=1,shape=21,
                      aes_string(fill=tt.precip[,4]))
pointplot <- ggplot(tt.precip, aes_string(year, precip)) +Pointset+Innerlmplot +PointThemeset
(pointplot <- pointplot + eqn1_text+eqn2_text+facet_grid(.~Lat.Area))#+scale_x_continuous(limits = c(1985,2018))
ggsave(plot = pointplot,filename =" global.precip.trend.png",width=15, dpi = 100,height=4)
  