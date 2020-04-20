source('0_plot.R')
workingpath = getwd()

mycolors<-colorRampPalette(c("#e41a1c",'#4daf4a',"#377eb8" ))

rcdata = select(res.data,Latitude,Rh_annual,Study_midyear)

###generate the coefficients of the window
t=q=30;p=1;i=1
ord = Lat.Area = Slope = R.square = P.value = Num = Mid.Lat = c()
while (t <= max(rcdata$Latitude)+p) {
  i = i+1
  ord = c(ord,i)
  win = filter(rcdata,Latitude >t-q & Latitude <t)
  eq= lm_eqn(df=win)
  Lat.Area=c(Lat.Area,paste0((t-q),'_',t))
  Slope=c(Slope,eq[1])
  R.square=c(R.square,eq[2])
  P.value=c(P.value,eq[3])
  Num = c(Num,nrow(win))
  Mid.Lat = c(Mid.Lat,t-(0.5*q))
  t = t+p
}
winresult = data.frame(ord,Lat.Area,Slope,R.square,P.value,Num,Mid.Lat)

winresult$Lat.Area <- factor(winresult$Lat.Area,levels=c(winresult$Lat.Area))#prevent the reorder of levels 
winresult$sig = ''
winresult[winresult$P.value<0.1,'sig'] = '.'
winresult[winresult$P.value<0.05,'sig'] = '*'
winresult[winresult$P.value<0.001,'sig']= '**'
winresult[winresult$P.value<0.001,'sig']= '***'

star <- geom_text(aes(x=Lat.Area, y=Slope+0.2,label = sig),size=3.5,vjust = 0)
winresult$Num.italic = paste0('italic(',winresult$Num,')')
count <- geom_text(aes(x=Lat.Area, y=25,label = Num.italic),parse = TRUE,size=1.5)
SFM = scale_fill_manual(values = mycolors(nrow(winresult)))
barplot = ggplot(winresult, aes(x=Lat.Area,y=Slope)) + geom_bar(aes(fill=Lat.Area),alpha=0.8,stat="identity")
barplot = barplot+TS+SFM+count+coord_cartesian(ylim=c(1:25))#+SYC+star


ggsave(plot=barplot,path='D:/Global_Tem_SPEI/SRDB_V4_CHN/latitude move window/',dpi = 300,
       filename=paste('lat_move_',q,'_',p,'.png'),width = 6.7,height = 2.2)



###fig at different range xaxis is study midyear; yaxis is latitude
d = 3
p=50;q=30
rcdata$Lat.Area ='High latitude'
rcdata[rcdata$Latitude<p,'Lat.Area'] = 'Middle latitude'
rcdata[rcdata$Latitude<q,'Lat.Area']= 'Low latitude'
rcdata$Lat.Area = factor(rcdata$Lat.Area, levels=c("Low latitude","Middle latitude","High latitude"))

sy1 =scale_y_continuous(breaks = c(700,1400,2100,2800), limits = c(0, 2900))
sy2 =scale_y_continuous(breaks = c(300,600,900,1200), limits = c(0, 1300))
sy3 =scale_y_continuous(breaks = c(250,500,750,1000), limits = c(0, 1100))
sy = c(sy1,sy2,sy3)

rcdata$Lat.Area = paste0(p,'-69')
rcdata[rcdata$Latitude<p,which(colnames(rcdata)=='Lat.Area')] = paste0(q,'-',p)
rcdata[rcdata$Latitude<q,which(colnames(rcdata)=='Lat.Area')]= paste0('0-',q)

PBg <- ggplot(rcdata, aes_string(x = 'Study_midyear',y = tar[d]))
LC <- coord_cartesian(ylim=c(0,1),xlim=c(1987, 2015))
TLM =  PBg + LC
mycolor = mycolors(9)
LM   <-geom_smooth(data=rcdata,method='lm',aes(color =Lat.Area))
#regression coefficients 
factorselect = names(summary(rcdata$Lat.Area))
eqstring1=c()

TS <- theme(text = element_text(size=6),
            legend.position = "none",
            axis.text.x = element_text(angle=45,size=5, hjust = 0.6,vjust = 0.5),
            panel.background=element_blank(),
            panel.border = element_rect(colour = "grey", fill=NA, size=1))

factorselect = levels(rcdata$Lat.Area)
plotcol =c('brown1','forestgreen','royalblue1')
for (i in 1:length(factorselect)) {
  PBg <- ggplot(rcdata[which(rcdata$Lat.Area==factorselect[i]),], 
                aes_string(x = 'Study_midyear',y = tar[d]))
  PS <-geom_point(color = 'black',#,color=Latitude
                  alpha = 0.6,stroke = 0.2,
                  size=2,shape=21,fill = plotcol[i])
  LM   <-geom_smooth(method='lm',color = plotcol[i],alpha=0)
  sxc <- scale_x_continuous(limits =c(1986,2017))
  pointplot <-PBg + PS +TS +LM +sxc+sy[i]
  ggsave(plot=pointplot,filename=paste(factorselect[i],'.png'),width=2.4,height = 1.6,dpi = 300)
}

  
  
  
  
  
  
  