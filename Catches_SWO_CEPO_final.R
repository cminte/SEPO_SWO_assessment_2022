# Catch estimation by quarter for 2022 SWO assessment
rm(list=ls())
#
# Carolina Minte-Vera
# Final 3 /16 / 2022
# revisited 3/11/2022
# revisited 03/09/2022
# revisited 2/28/2022
# revisited  2/15/2022
# updated 9/15/2021
# updated 4/21/2021
# April 1 2021
#install.packages("viridis")
#install.packages("hrbrthemes")
#install.packages("mapdata")
require(tidyverse)
require(viridis)
require(hrbrthemes)
require(mapdata)

#if this not exist make one:
TheDir<-"C:\\Users\\cminte\\Documents\\Carolina2022\\IATTC\\SWO\\catches\\Results_Catch_estimation\\"
MyDir<-TheDir
##dir.create(MyDir)
setwd(TheDir)

########### STEP 1 get FSR data (all species), correct China ###########
# select variables we need
Get_FSR<-function(DoPlot=TRUE, ResDir=MyDir,nyears=10){
  #nyears is the number of years to check for NA and
  # replace the NAs for the previous catches 
  require(tidyverse)
  DataDir<-"C:\\Users\\cminte\\Documents\\Carolina2021\\IATTC\\DATA\\LL\\"
  #update on 9/15/2021
  FSRfile<-"dbo_CatchTotalOtherGears.txt"
  Flagid<-read_csv(paste0(DataDir,"dbo_tblFlag.txt"),col_names=T)
  Speciesid<-read_csv(paste0(DataDir,"dbo_tblSpecies.txt"),col_names=T)
  Speciesid%>%filter(SpeciesAbv=="SWO")
  Gearid<-read_csv(paste0(DataDir,"dbo_tblGear.txt"),col_names=T)
  Gear<-Gearid%>%select(GearID,GearAbv,Description)
  FSR.all<-read_csv(paste0(DataDir,FSRfile),col_names=T) #,col_types = cols
  FSR.all<-full_join(Gear,FSR.all)
  FSR.all<-full_join(Flagid,FSR.all)
  #China 2006 needs correction, only reported observer data
  tmp1<-FSR.all%>%filter(SpeciesID==123, FlagAbv=="CHN")
  p1<-ggplot(tmp1)+geom_col(aes(x=Year,y=Catch))+theme_bw(25)+labs(title="Aggregated data China not corrected")
  if(DoPlot) ggsave(plot=p1, file=paste0(ResDir,"FSR_EPO_SWO_China_not_corrected.png"), width=25, height=15)
  #for SWO assume 2006 is equal to 2005
  tmp1<-tmp1%>%filter(Year==2005)
  tmp1$Year<-2006
  #remove old 2006 year
  CHN.line<-which(FSR.all$FlagAbv=="CHN" &FSR.all$Year==2006 & FSR.all$SpeciesID==123)
  tmp2<-FSR.all[-CHN.line,]
  tmp3<-FSR.all[CHN.line,]
  FSR.all<-rbind(tmp1,tmp2)
  #with China corrected
  FSR<-FSR.all%>%filter(SpeciesID==Speciesid$SpeciesID[Speciesid$SpeciesAbv=="SWO"])%>%
    select(FlagAbv,GearAbv,Year,Catch)
  
  #table(FSR$GearAbv)
  FSR<-FSR%>%mutate(
    GearAbv=ifelse(GearAbv%in%c("LL","LLD","LLX"), "LL",
            ifelse(GearAbv%in%c("NK","OTR"),"GN",
            ifelse(GearAbv=="LP","HAR",GearAbv))))%>%
  group_by(FlagAbv,GearAbv,Year)%>%summarize(Catch=sum(Catch,na.rm = T))
  ##FILL last 10 years of NA with previous year catches
  maxYear<-max(FSR$Year,na.rm=T)
  #FSR2<-FSR%>%filter(Year<(maxYear-11))
  TheYear<-(maxYear-(nyears+1))
  FSR2=FSR[FSR$Year<TheYear,]
  FSR3=FSR[FSR$Year>(TheYear+1),]
  #add NA to the years where there is nodata
  Gear<-unique(FSR3$GearAbv)
  
  for(g in 1:length(Gear))
  { 
    tmp3<-FSR3%>%filter(GearAbv==Gear[g])
    tmp4<-tmp3%>%pivot_wider(names_from = FlagAbv,values_from = Catch)%>%arrange(Year)
    n<-dim(tmp4)[2] #number of columns (flags +1)
    y<-dim(tmp4)[1] #number of years
    if(y>0){
     for(i in 2:n){
      for(j in 2:y){
        if(is.na(tmp4[j,i])) tmp4[j,i]=tmp4[j-1,i]}}
    }
    
    if(Gear[g]=="LL") x1<-tmp4
    if(Gear[g]=="GN") x2<-tmp4
    if(Gear[g]=="HAR") x3<-tmp4
    
  }
  
  FSR3<-rbind(x1,x2,x3)
  FSR3<-pivot_longer(FSR3,cols=2:n,values_to="Catch",names_to="FlagAbv",values_drop_na = FALSE)
  #FSR<-rbind(FSR2,FSR3)
  
  p2<-ggplot(FSR)+geom_col(aes(x=Year,y=Catch, color=GearAbv,fill=GearAbv))+
    facet_wrap(~FlagAbv)+ theme_bw(25)+labs(title="Aggregated data")
  if(DoPlot) ggsave(plot=p2, file=paste0(ResDir,"FSR_EPO_SWO.png"), width=25, height=15)
  
  write.table(FSR, paste0(ResDir,"CatchesFSR_China_Corrected.txt"))
  return(FSR)
}

#
#MyDir<-TheDir
FSR<-Get_FSR()
#pivot_wider(FSR,names_from = FlagAbv, values_from = Catch) 
  
########### STEP 2 get data from FAO PACIFIC ###########
Get_FAO<-function(DoPlot=TRUE,ResDir=MyDir){
  require(tidyverse)
  require(lubridate)
  FAO_Dir<-"C:\\Users\\cminte\\Documents\\Carolina2022\\IATTC\\SWO\\catches\\FAO_2019\\"
  FAO<-read_csv(paste0(FAO_Dir,"SWO_Pacific.csv"))
  #names(FAO)
  #dim(FAO)
  FAO<-FAO%>%select(c(names(FAO[,1:5]),starts_with("[")))
  FAO<-FAO%>%pivot_longer(cols=starts_with("["),names_to = "Year",values_to = "Catch",values_drop_na = TRUE)
  #head(FAO)
  FAO$Year<-parse_number(FAO$Year)
  names(FAO)<-c("Flag","Species","FAO_area","Unit_name","Unit","Year","Catch")
  unique(FAO$Unit_name)
  SWO.FAO.W<-FAO%>%filter(Species=="Swordfish",Unit_name=="Tonnes - live weight")%>%group_by(Flag,Year)%>%summarize(Catch=sum(Catch, na.rm=T))
  #no data in N or NA
  #SWO.FAO.N<-FAO%>%filter(Species=="Swordfish",Unit_name=="Number")%>%group_by(Flag,Year)%>%summarize(Catch=sum(Catch, na.rm=T))
  #SWO.FAO.NA<-FAO%>%filter(Species=="Swordfish",is.na(Unit_name))%>%group_by(Flag,Year)%>%summarize(Catch=sum(Catch, na.rm=T))
  #windows()
  p1<-ggplot(SWO.FAO.W)+geom_col(aes(x=Year,y=Catch,fill=Flag), color="black")+
    ylab("catch in weight") +xlab("Year")+
    labs(title="FAO database")+theme_bw(25)
  if(DoPlot) ggsave(plot=p1, file=paste0(ResDir,"FAO_catchW.png"), width=25, height=15)
  
  tmp6<-pivot_wider(SWO.FAO.W,names_from = "Year",values_from="Catch")
  write_csv(tmp6,file=paste0(ResDir,"FAO_PO_catches.csv"))
  return(SWO.FAO.W)
}

SWO.FAO.W<-Get_FAO()
#pivot_wider(SWO.FAO.W,names_from = Flag, values_from = Catch)
#tail(pivot_wider(SWO.FAO.W,names_from = Flag, values_from = Catch))
########### STEP 3 get Peru from FAO ###########
Do_Catches_Peru<-function(DataFAO,DataFSR,DoPlot=TRUE,ResDir=MyDir){
  FSR<-DataFSR
  SWO.FAO.W<-DataFAO
  #compare Peru FSR and FAO
  PERU<-FSR%>%filter(FlagAbv=="PER")%>%rename(CatchFSR=Catch)
  print(table(PERU$GearAbv) )
  #NK 
  #66 #all gear is unknown
  
  PERU<-PERU%>%select(FlagAbv,Year,CatchFSR)
  FAO.Peru<-SWO.FAO.W%>%filter(Flag=="Peru")
  FAO.Peru$FlagAbv<-"PER"
  Per<-full_join(FAO.Peru,PERU)
  
  
  p1<-ggplot(Per)+geom_point(aes(x=Year,y=CatchFSR), color="black",fill="black",alpha=1,size=3)+
    theme_bw(25)+ylab("catch in 1000 tons")+ geom_col(aes(x=Year,y=Catch),fill="blue",alpha=0.5)
  
  if(DoPlot) ggsave(plot=p1, file=paste0(ResDir,"Catches_Peru_FSR_FAO.png"), width=15, height=10)
  
  ### The largest catches for each year will be used for Peru 
  Per<-Per%>%mutate(FinalCatchW=ifelse(is.na(CatchFSR),Catch,ifelse(CatchFSR>=Catch,CatchFSR,Catch)))
  
  p2<-ggplot(Per)+geom_point(aes(x=Year,y=CatchFSR,fill="IATTC aggregated"), color="black",alpha=1,size=3)+
    theme_bw(25)+ylab("catch in  tons")+ geom_col(aes(x=Year,y=Catch,fill="FAO"),alpha=0.5)+
    geom_point(aes(x=Year,y=FinalCatchW,fill="Data used"), color="red",alpha=0.5,size=5,shape=22)+
    xlab("Year")+
    labs(fill = "Data type",    
         title="Perú")
  
  if(DoPlot) ggsave(plot=p2, file=paste0(ResDir,"Catches_Peru_FSR_FAO_Final.png"), width=15, height=10)
  
  ## separation of the data by gear and quarter:
  # 78W 13S for position: core of the catches
  print(names(Per))
  Per<-Per%>%select(Year,FinalCatchW)%>%mutate(FlagAbv="PER",Lon=-78,Lat=-13,
                                               SumOfNumber=NA,SpeciesAbv="SWO")%>%
    rename(SumOfWeight=FinalCatchW,Year=Year)
  # from Guevara-Carrasco & Bertrand (2017). 
  
  # Quarter	/ % catches
  # 1	15
  # 2	43
  # 3	25
  # 4	18
  # 
  Q1<-Per%>%mutate(Quarter=1,SumOfWeight=0.15*SumOfWeight)
  Q2<-Per%>%mutate(Quarter=2,SumOfWeight=0.43*SumOfWeight)
  Q3<-Per%>%mutate(Quarter=3,SumOfWeight=0.25*SumOfWeight)
  Q4<-Per%>%mutate(Quarter=4,SumOfWeight=0.18*SumOfWeight)
  Per<-rbind(Q1,Q2,Q3,Q4)
  #
  head(Per)
  
  # all catches were assigned to longline before 2003, 2003:  50% longline and 50% gillnet, 2004: all gillnet,2005-2008: 50% longline and 50% gillnet, after 2008: all gillnets;
  # according to information from Guevara-Carrasco & Bertrand (2017). 
  Per<-Per%>%mutate(PropLL=ifelse(Year<2003,1,ifelse(Year%in%c(2003,2005,2006,2007,2008),0.5,0)))
  #Correction here on 05/09/2022
  Per<-Per%>%mutate(LL=PropLL*SumOfWeight,GN=(1-PropLL)*SumOfWeight)
  Per<-Per%>%select(FlagAbv,Lon,Lat,Year, Quarter,LL,GN)
  Per<-Per%>%pivot_longer(cols=c(LL,GN),names_to = "GearAbv",values_to = "Catches")
  Per<-Per%>%mutate(SumOfNumber=NA)%>%rename(SumOfWeight=Catches)
  Per.grided<-Per%>%select(FlagAbv,Lon,Lat,Year,
                           Quarter,SumOfWeight,SumOfNumber,GearAbv)
  
  return(Per.grided[,2:9]) #####THIS IS READY TO GO TO GRIDDED 
  
}

#
Per.grided<-Do_Catches_Peru(DataFAO=SWO.FAO.W,DataFSR=FSR)
  #tail(pivot_wider(Per.grided,values_from = SumOfWeight,names_from = GearAbv))

########### STEP 4 Catches ECUADOR from self reported, FSR and FAO ###########
Do_Catches_Ecuador<-function(DataFSR=FSR,DataFAO=SWO.FAO.W,DoPlot=TRUE,ResDir=MyDir){
  #Ecuatorian data 
  #03/04/2022, 03/07/2022, 03/08/2022, 03/10/2022
  FSR<-DataFSR
  SWO.FAO.W<-DataFAO
  require(tidyverse)
  require(lubridate)
  
  ECU.Dir<-"C:\\Users\\cminte\\Documents\\Carolina2021\\IATTC\\SWO\\DATA_PREP\\Ecuador\\"
  ECU.2016<-read_csv(paste0(ECU.Dir,"ECU_2016.csv"))
  ECU.2017<-read_csv(paste0(ECU.Dir,"ECU_2017.csv"))
  ECU.2018<-read_csv(paste0(ECU.Dir,"ECU_2018.csv"))
  ECU.2019<-read_csv(paste0(ECU.Dir,"ECU_2019.csv"))
  ECU.2020<-read_csv(paste0(ECU.Dir,"ECU_2020.csv"))
  
  names(ECU.2016)
  table(ECU.2016$`Nombre Común`)
  e.2016<-ECU.2016%>%select('Fecha de emisión','Fecha de zarpe','Fecha de arribo','Arte de Pesca','Nro. individuos','Peso (Kg)')
  e.2016$year<-2016
  names(e.2016)<-c("date_permit","date_out","date_in","gear","number","weight","year")
  e.2016$date_out<-mdy(e.2016$date_out)
  e.2016$date_in<-mdy(e.2016$date_in)
  e.2016$date_permit<-mdy(e.2016$date_permit)
  
  names(ECU.2017)
  table(ECU.2017$`Nombre Común`)
  e.2017<-ECU.2017%>%select('Fecha de emisión','Fecha de zarpe','Fecha de arribo','Arte de Pesca','Nro. individuos','Peso (Kg)')
  e.2017$year<-2017
  names(e.2017)<-c("date_permit","date_out","date_in","gear","number","weight","year")
  e.2017$date_out<-mdy(e.2017$date_out)
  e.2017$date_in<-mdy(e.2017$date_in)
  e.2017$date_permit<-mdy(e.2017$date_permit)
  
  names(ECU.2018)
  table(ECU.2018$`Nombre Común`)
  e.2018<-ECU.2018%>%select('Fecha de emisión','Fecha de zarpe','Fecha de arribo','Arte de Pesca','Nro. individuos','Peso (Kg)')
  e.2018$year<-2018
  names(e.2018)<-c("date_permit","date_out","date_in","gear","number","weight","year")
  e.2018$date_out<-mdy(e.2018$date_out)
  e.2018$date_in<-mdy(e.2018$date_in)
  e.2018$date_permit<-mdy(e.2018$date_permit)
  
  # windows()
  # par(mfrow=c(2,3))
  # plot(sqrt(e.2016$`Nro. individuos`),sqrt(e.2016$`Peso (Kg)`))
  # plot(sqrt(e.2017$`Nro. individuos`),sqrt(e.2017$`Peso (Kg)`))
  # plot(sqrt(e.2018$`Nro. individuos`),sqrt(e.2018$`Peso (Kg)`))
  # plot(sqrt(e.2019$`Nro. individuos`),sqrt(e.2019$`Peso (Kg)`))
  # plot(sqrt(e.2020$`Nro. individuos`),sqrt(e.2020$`Peso (Kg)`))
  # 
  # head(e.2016)
  # 
  # table(e.2016$`Arte de Pesca`)
  # #Línea de mano             Palangre Red de 
  # #4               3942               2637                 14 
  # 
  # table(e.2017$`Arte de Pesca`)
  # #Línea de mano             Palangre Red de cerco atunero            Trasmallo   Trasmallo/Palangre 
  # #25                 6588                    1                 3877                   93 
  # 
  # table(e.2018$`Arte de Pesca`)
  # #          Línea de mano                Palangre    Red de cerco atunera Red de Enmalle de fondo               Trasmallo      Trasmallo/Palangre 
  # #5                    5887                       1                      22                    2236                      61 
  # 
  # table(e.2019$`Arte de Pesca`)
  # # Palangre de fondo       Palangre de superficie         Red de cerco atunera      Red de enmalle de fondo Red de enmalle de superficie  Red de trasmallo superficie 
  # #28                         2926                            1                           18                          274                           21 
  # 
  # table(e.2020$`Arte de Pesca`)
  # #PALANGRE DE SUPERFICIE                 PALANGRE/ESPINEL DE FONDO               PALANGRE/ESPINEL SUPERFICIE           RED DE ENMALLE FONDO/SUPERFICIE 
  # #1                                         1                                       597                                       310 
  # #RED TRASMALLO/ENMALLE/PALANGRE FONDO RED TRASMALLO/ENMALLE/PALANGRE SUPERFICIE 
  # 
  # #most trips are small duration
  # table((e.16_18$date_in-e.16_18$date_out)<0)
  # #FALSE  TRUE 
  # #24259   104 
  # table((e.16_18$date_in-e.16_18$date_out)<90)
  # #FALSE  TRUE 
  # #68 24295 
  # table((e.16_18$date_in-e.16_18$date_out)>90)
  # #FALSE  TRUE 
  # #24296    67 
  
  
  
  e.16_18<-rbind(e.2016,e.2017,e.2018)
  table((e.16_18$date_permit-e.16_18$date_in)<0)
  #FALSE  TRUE 
  #24398   240 
  table((e.16_18$date_permit-e.16_18$date_in)>30)
  #FALSE  TRUE 
  #24556    8
  # most of the trips are done withing one month of the permit emission 
  
  names(ECU.2019)
  e.2019<-ECU.2019%>%select('Fecha de emisión','Arte de Pesca','Nro. individuos','Peso (Kg)')
  table(ECU.2019$`Nombre Común`)
  e.2019$year<-2019
  names(e.2019)<-c("date_permit","gear","number","weight","year")
  e.2019$date_permit<-dmy(e.2019$date_permit)
  e.2019$date_permit[e.2019$date_permit=='2109-12-14']<-'2019-12-14'
  table(e.2019$date_permit)
  
  names(ECU.2020)
  e.2020<-ECU.2020%>%select('Fecha de emisión','Arte de Pesca','Nro. individuos','Peso (Kg)')
  table(ECU.2020$`Nombre Común`)
  e.2020$year<-2020
  names(e.2020)<-c("date_permit","gear","number","weight","year")
  e.2020$date_permit<-ymd(e.2020$date_permit)
  
  e.19_20<-rbind(e.2019,e.2020)
  e.16_18<-e.16_18%>%select(date_permit,gear,number,weight,year)
  
  all.ECU<-rbind(e.16_18,e.19_20)
  all.ECU<-all.ECU%>%mutate(year=year(date_permit),quarter=quarter(date_permit), month=month(date_permit))
  ###see if we can estimate the average weight:
  windows()
  plot(all.ECU$number,all.ECU$weight) #there is one outlier
  all.ECU<-all.ECU%>%mutate(number=ifelse(number>50000, NA,number))
  which(all.ECU$weight==max(all.ECU$weight,na.rm=T))
  #all.ECU[32897,]
  #all.ECU[32890:32900,]
  #other catches by the same vessel are much smaller
  # Nro. individuos	Peso (Kg)
  # 1	104.5
  # 33	272155
  # 4	280
  # 25	1400
  # 11	500
  # 2	45
  # 
  #maybe use median fish weight to avoid outlier problems
  
  #average weight 
  all.ECU<-all.ECU%>%mutate(aveW=weight/number)
  #windows()
  #hist(log(all.ECU$aveW+0.001))
  tmp1<-all.ECU
  unique(tmp1$gear) #too many gear categories
  #replace() ?
  tmp1$gear[which(tmp1$gear %in% c("Palangre","Palangre de fondo","Palangre de superficie","PALANGRE/ESPINEL DE FONDO","PALANGRE DE SUPERFICIE",
                                   "PALANGRE/ESPINEL SUPERFICIE"))]<-"palangre"
  tmp1$gear[which(tmp1$gear %in%c("Trasmallo","RED DE ENMALLE FONDO/SUPERFICIE",
                                  "Red de enmalle de superficie","Red de Enmalle de fondo", 
                                  "Red de enmalle de fondo","Red de trasmallo superficie"))]<-"enmalle"
  tmp1$gear[which(tmp1$gear %in%c( "Red de cerco atunero","Red de cerco atunera" ))]<-"cerco atunero"
  tmp1$gear[which(tmp1$gear %in% c("Trasmallo/Palangre","RED TRASMALLO/ENMALLE/PALANGRE FONDO","RED TRASMALLO/ENMALLE/PALANGRE SUPERFICIE"))]<-"red+palangre"
  
  tmp1$gear
  tmp1<-tmp1%>%group_by(year, quarter,gear)%>%
    summarize(catchW=sum(weight,na.rm=T),
              catchN=sum(number, na.rm=T),
              aveW=median(aveW)) #use median
  
  #get only the main gears
  AveW<-tmp1%>%filter(gear%in%c("enmalle","palangre" ),year%in%seq(2016,2020,1))
  
  p1<-ggplot(tmp1)+geom_point(aes(x=year, y=aveW,color=as.factor(gear)))+
    xlim(2014,2021)+xlab("Year")+ylab("median weight")+
    labs(title="Ecuador, self-reported data")+theme_bw(25)
  
  p1<-ggplot(AveW)+geom_point(aes(x=year, y=aveW,color=as.factor(gear)))+
    xlim(2014,2021)+xlab("Year")+ylab("median weight")+
    labs(title="Ecuador, self-reported data")+theme_bw(25)
  
  ggsave(plot=p1, file=paste0(ResDir,"ECU_AveW_main_gear_2016_2020_self_reported.png"), width=15, height=10)
  
  
  #windows()
  #ggplot(tmp1)+geom_col(aes(x=year, y=catchW,fill=as.factor(quarter)))+
  #facet_wrap(~gear)+xlim(2014,2021)
  
  PropGear<-tmp1          
  tmp2<-tmp1%>%group_by(year, gear)%>%summarize(catchW=sum(catchW,na.rm=T)/1000,catchN=sum(catchN, na.rm=T))
  #windows()
  p1<-ggplot(tmp2)+geom_col(aes(x=year, y=catchW,fill=as.factor(gear)))+
    xlim(2014,2021)+xlab("Year")+ylab("Catch tons")+
    labs(title="Ecuador, self-reported data")+theme_bw(25)
  ggsave(plot=p1, file=paste0(ResDir,"ECU_catchW_gear_2016_2020_self_reported.png"), width=15, height=10)
  
  p2<-ggplot(tmp2)+geom_col(aes(x=year, y=catchN,fill=as.factor(gear)))+
    xlim(2014,2021)+xlab("Year")+ylab("Catch 1000 fish")+
    labs(title="Ecuador, self-reported data")+theme_bw(25)
  
  ggsave(plot=p2, file=paste0(ResDir,"ECU_catchN_gear_2016_2020_self_reported_gear.png"), width=15, height=10)
  
  p3<-ggplot(tmp2)+geom_point(aes(x=year, y=catchW/catchN,fill=as.factor(gear)))+
    xlim(2014,2021)+xlab("Year")+ylab("Average weight per fish")+
    labs(title="Ecuador, self-reported data")+theme_bw(25)
  
  ggsave(plot=p3, file=paste0(ResDir,"ECU_ave_W_gear_2016_2020_self_reported_gear.png"), width=15, height=10)
  #remove cerco atunero since this will be in the PS by catch
  
  ###compare with FSR and FAO
  ##Ecuador
  ECU<-FSR%>%filter(FlagAbv=="ECU")%>%
    group_by(FlagAbv,Year)%>%summarize(CatchFSR=sum(Catch,na.rm=T))%>%
    select(FlagAbv,Year,CatchFSR)
  FAO.Ecu<-SWO.FAO.W%>%filter(Flag=="Ecuador")
  FAO.Ecu$FlagAbv<-"ECU"
  Ecu<-full_join(FAO.Ecu,ECU)
  #self-reported data
  tmp1<-all.ECU%>%group_by(year)%>%
    summarize(catchW=sum(weight,na.rm=T)/1000,catchN=sum(number, na.rm=T))%>%
    mutate(catchW=catchW)%>%select(year,catchW)%>%rename(Year=year)
  Ecu<-full_join(Ecu,tmp1)
  tail(Ecu)
  Ecu<-Ecu%>%filter(Year<2021)
  
  p7<-ggplot(Ecu)+geom_point(aes(x=Year,y=CatchFSR), color="black",fill="black",alpha=1,size=3)+
    theme_bw(25)+ylab("catch in tons")+ 
    geom_col(aes(x=Year,y=Catch),fill="blue",alpha=0.5)+
    geom_point(aes(x=Year,y=catchW),fill="red",color="red",alpha=0.5,size=5)+
    xlab("Year")+
    labs(title="Ecuador, FAO column, FSR black dot,self_reported red dot")+theme_bw(25)+xlim(1985,2021)
  
  windows()
  plot(p7)
  ggsave(plot=p7, file=paste0(ResDir,"Catches_Ecuador_FSR_FAO_self_reported.png"), width=15, height=10)
  
  #use the highest estimate as the catch for the year)
  n<-dim(Ecu)
  n<-n[1]
  for(i in 1:n)
    Ecu$FinalCatchW[i]<-max(Ecu$CatchFSR[i],Ecu$Catch[i],Ecu$catchW[i],na.rm=T)
  head(Ecu)
  tail(Ecu)
  Ecu$FlagAbv[is.na(Ecu$FlagAbv)]<-"ECU"
  
  p7<-ggplot(Ecu)+geom_point(aes(x=Year,y=CatchFSR,fill="IATTC aggregated"), color="black",alpha=1,size=3)+
    theme_bw(25)+ylab("catch in  tons")+ geom_col(aes(x=Year,y=Catch,fill="FAO"),alpha=0.5)+
    geom_point(aes(x=Year,y=catchW,fill="self-reported"),color="green",alpha=1,size=5,shape=24)+
    geom_point(aes(x=Year,y=FinalCatchW,fill="Data used"), color="red",alpha=0.5,size=5,shape=22)+
    xlab("Year")+
    labs(fill = "Data type",    
         title="Ecuador")
  
  ggsave(plot=p7, file=paste0(ResDir,"Catches_Ecuador_FSR_FAO_self_reported_Final.png"), width=15, height=10)
  
  #split 2016-2020 catches according to the proportions of 
  #LL and GN from the special submission
  tmp1<-PropGear%>%group_by(year)%>%summarise(TotalW=sum(catchW,na.rm=T))
  
  PropGear<-full_join(PropGear,tmp1)
  PropGear<-PropGear%>%mutate(Prop=catchW/TotalW)%>%mutate(gear2=ifelse(gear=="cerco atunero","enmalle",ifelse(gear=="Línea de mano","palangre",gear)))
  
  sum(PropGear$catchW)/sum(tmp1$TotalW)
  table(PropGear$gear,PropGear$gear2)
  
  PropGear<-PropGear%>%group_by(gear2,year,quarter)%>%summarise(Prop=sum(Prop,na.rm=T))%>%filter(year<2021)
  windows()
  p8<-ggplot(PropGear)+geom_col(aes(x=year,y=Prop,fill=gear2))
  ggsave(plot=p8, file=paste0(ResDir,"Ecuador_PropGearQuarter.png"), width=15, height=10)
  plot(p8)
  #replace enmalle + palangre by 1/2 for each
  tmp1<-PropGear%>%filter(gear2=="red+palangre")
  tmp2<-tmp1
  tmp1<-tmp1%>%mutate(Prop=0.5*Prop,gear2="LL")%>%rename(GearAbv=gear2)
  tmp2<-tmp2%>%mutate(Prop=0.5*Prop,gear2="GN")%>%rename(GearAbv=gear2)
  
  tmp3<-PropGear%>%filter(gear2!="red+palangre")%>%
    mutate(GearAbv=ifelse(gear2=="enmalle", "GN","LL"))%>%
    select(GearAbv,year,quarter,Prop)
  
  PropGear2<-rbind(tmp1,tmp2,tmp3)%>%
    group_by(GearAbv,year,quarter)%>%
    summarize(Prop=sum(Prop,na.rm=T))
  p9<-ggplot(PropGear2)+geom_col(aes(x=year,y=Prop,fill=GearAbv))
  ggsave(plot=p9, file=paste0(ResDir,"Ecuador_PropGearQuarter_Final.png"), width=15, height=10)
  
  #Let's see if they report catch by Gear for other years
  tmp3<-FSR%>%filter(FlagAbv=="ECU")%>%
    group_by(GearAbv,Year)%>%summarize(CatchFSR=sum(Catch,na.rm=T))%>%
    pivot_wider(names_from= GearAbv, values_from = CatchFSR)
  #only reported: LL LLX and NK
  
  names(Ecu)
  
  Ecu2<-Ecu%>%filter(FlagAbv=="ECU")%>%
    select(FlagAbv,Year,FinalCatchW)
  
  #assume the proportions per quarter for LL from 2016 will 
  # be the same in previous years 
  tmp1<-PropGear2%>%filter(year=="2016",GearAbv=="LL")%>%
    group_by(quarter)%>%summarise(Prop=sum(Prop,na.rm=T))
  tmp1$Prop<-tmp1$Prop/sum(tmp1$Prop)
  #This can be used to split CRI, COL, Panama 
  Prop_Quarter_LL_ECU<-t(tmp1$Prop)
  Year<-unique(Ecu$Year)
  Year<-Year[Year<2016]
  n<-length(Year)
  tmp2<-as.data.frame(matrix(rep(t(tmp1$Prop),n),ncol=4,nrow = n))
  tmp2<-cbind(Year,tmp2)
  colnames(tmp2)<-c("Year","Q1","Q2","Q3","Q4")
  Ecu2<-full_join(Ecu2,tmp2)
  Ecu2<-Ecu2%>%mutate(Q1=FinalCatchW*Q1,
                      Q2=FinalCatchW*Q2,
                      Q3=FinalCatchW*Q3,
                      Q4=FinalCatchW*Q4)
  ## all catches considered LL before 2015
  Ecu2<-pivot_longer(Ecu2,cols=starts_with("Q"),values_to = "SumOfWeight",names_to = "Quarter")
  Ecu2<-Ecu2%>%filter(Year<2016)%>%mutate(GearAbv="LL") #only 2015 and befor
  names(Ecu2)
  Ecu2<-Ecu2%>%select(GearAbv,FlagAbv,Year,Quarter,SumOfWeight)
  #### Use the Prop for 2016 on to split annual catches 
  #per gear and quarter 
  Ecu3<-Ecu%>%filter(Year>2015)%>%rename(year=Year)
  tmp1<-full_join(Ecu3,PropGear2)
  head(tmp1)
  tmp1<-tmp1%>%mutate(SumOfWeight=FinalCatchW*Prop)%>%rename(Year=year)
  
  Ecu3<-tmp1%>%select(GearAbv,FlagAbv,Year,quarter,SumOfWeight)
  #Join early years Ecu2 and late years Ecu3
  Ecu2$quarter<-as.numeric(as.factor(Ecu2$Quarter))
  Ecu2<-Ecu2%>%select(GearAbv,Year,quarter,SumOfWeight)
  Ecu3<-Ecu3%>%select(GearAbv,Year,quarter,SumOfWeight)
  Ecu<-rbind(Ecu2,Ecu3)
  names(Ecu)
  #[1] "Flag"        "GearAbv"     "Year"         "quarter"    
  #[5] "SumOfWeight"
  ##Lat and Lon as Fig 11 in Jimmy-Martinez paper
  # were a lot of the catches of SWO are taken
  #Lat -2
  #Lon -93
  
  Ecu<-Ecu%>%rename(FlagAbv=Flag)%>%mutate(FlagAbv="ECU",Lon=I(-93),Lat=I(-2),SumOfNumber=NA)
  Ecu.grided<-Ecu%>%rename(Year=Year,Quarter=quarter)%>%select(FlagAbv,Lon,Lat,Year,
                                                               Quarter,SumOfWeight,SumOfNumber,GearAbv)
  return(Ecu.grided)  
}

Ecu.grided<-Do_Catches_Ecuador()
#tmp1<-Ecu.grided%>%group_by(Year,GearAbv)%>%summarise(Catch=sum(SumOfWeight,na.rm=T))
#tail(pivot_wider(tmp1, names_from = GearAbv,values_from = Catch))

########### STEP 5 Allocation Ecuador to be used for Central America and Colombia  ###########
Prop_Catches_Ecuador<-function(DataFSR=FSR,DataFAO=SWO.FAO.W,DoPlot=TRUE,ResDir=MyDir){
  #Ecuatorian data 
  FSR<-DataFSR
  SWO.FAO.W<-DataFAO
  require(tidyverse)
  require(lubridate)
  ECU.Dir<-"C:\\Users\\cminte\\Documents\\Carolina2021\\IATTC\\SWO\\DATA_PREP\\Ecuador\\"
  ECU.2016<-read_csv(paste0(ECU.Dir,"ECU_2016.csv"))
  ECU.2017<-read_csv(paste0(ECU.Dir,"ECU_2017.csv"))
  ECU.2018<-read_csv(paste0(ECU.Dir,"ECU_2018.csv"))
  ECU.2019<-read_csv(paste0(ECU.Dir,"ECU_2019.csv"))
  ECU.2020<-read_csv(paste0(ECU.Dir,"ECU_2020.csv"))
  e.2016<-ECU.2016%>%select('Fecha de emisión','Fecha de zarpe','Fecha de arribo','Arte de Pesca','Nro. individuos','Peso (Kg)')
  e.2016$year<-2016
  names(e.2016)<-c("date_permit","date_out","date_in","gear","number","weight","year")
  e.2016$date_out<-mdy(e.2016$date_out)
  e.2016$date_in<-mdy(e.2016$date_in)
  e.2016$date_permit<-mdy(e.2016$date_permit)
  e.2017<-ECU.2017%>%select('Fecha de emisión','Fecha de zarpe','Fecha de arribo','Arte de Pesca','Nro. individuos','Peso (Kg)')
  e.2017$year<-2017
  names(e.2017)<-c("date_permit","date_out","date_in","gear","number","weight","year")
  e.2017$date_out<-mdy(e.2017$date_out)
  e.2017$date_in<-mdy(e.2017$date_in)
  e.2017$date_permit<-mdy(e.2017$date_permit)
  e.2018<-ECU.2018%>%select('Fecha de emisión','Fecha de zarpe','Fecha de arribo','Arte de Pesca','Nro. individuos','Peso (Kg)')
  e.2018$year<-2018
  names(e.2018)<-c("date_permit","date_out","date_in","gear","number","weight","year")
  e.2018$date_out<-mdy(e.2018$date_out)
  e.2018$date_in<-mdy(e.2018$date_in)
  e.2018$date_permit<-mdy(e.2018$date_permit)
  e.16_18<-rbind(e.2016,e.2017,e.2018)
  e.2019<-ECU.2019%>%select('Fecha de emisión','Arte de Pesca','Nro. individuos','Peso (Kg)')
  e.2019$year<-2019
  names(e.2019)<-c("date_permit","gear","number","weight","year")
  e.2019$date_permit<-dmy(e.2019$date_permit)
  e.2019$date_permit[e.2019$date_permit=='2109-12-14']<-'2019-12-14'
  e.2020<-ECU.2020%>%select('Fecha de emisión','Arte de Pesca','Nro. individuos','Peso (Kg)')
  e.2020$year<-2020
  names(e.2020)<-c("date_permit","gear","number","weight","year")
  e.2020$date_permit<-ymd(e.2020$date_permit)
  e.19_20<-rbind(e.2019,e.2020)
  e.16_18<-e.16_18%>%select(date_permit,gear,number,weight,year)
  
  all.ECU<-rbind(e.16_18,e.19_20)
  all.ECU<-all.ECU%>%mutate(year=year(date_permit),quarter=quarter(date_permit), month=month(date_permit))
  all.ECU<-all.ECU%>%mutate(aveW=weight/number)
  tmp1<-all.ECU
  tmp1$gear[which(tmp1$gear %in% c("Palangre","Palangre de fondo","Palangre de superficie","PALANGRE/ESPINEL DE FONDO","PALANGRE DE SUPERFICIE",
                                   "PALANGRE/ESPINEL SUPERFICIE"))]<-"palangre"
  tmp1$gear[which(tmp1$gear %in%c("Trasmallo","RED DE ENMALLE FONDO/SUPERFICIE",
                                  "Red de enmalle de superficie","Red de Enmalle de fondo", 
                                  "Red de enmalle de fondo","Red de trasmallo superficie"))]<-"enmalle"
  tmp1$gear[which(tmp1$gear %in%c( "Red de cerco atunero","Red de cerco atunera" ))]<-"cerco atunero"
  tmp1$gear[which(tmp1$gear %in% c("Trasmallo/Palangre","RED TRASMALLO/ENMALLE/PALANGRE FONDO","RED TRASMALLO/ENMALLE/PALANGRE SUPERFICIE"))]<-"red+palangre"
  
  tmp1<-tmp1%>%group_by(year, quarter,gear)%>%
    summarize(catchW=sum(weight,na.rm=T),
              catchN=sum(number, na.rm=T),
              aveW=median(aveW)) #use median
  #get only the main gears
  PropGear<-tmp1          
  tmp2<-tmp1%>%group_by(year, gear)%>%summarize(catchW=sum(catchW,na.rm=T),catchN=sum(catchN, na.rm=T))
  
  #split 2016-2020 catches according to the propotions of 
  #LL and GN from the special submission
  tmp1<-PropGear%>%group_by(year)%>%summarise(TotalW=sum(catchW,na.rm=T))
  PropGear<-full_join(PropGear,tmp1)
  PropGear<-PropGear%>%mutate(Prop=catchW/TotalW)%>%mutate(gear2=ifelse(gear=="cerco atunero","enmalle",ifelse(gear=="Línea de mano","palangre",gear)))
  PropGear<-PropGear%>%group_by(gear2,year,quarter)%>%summarise(Prop=sum(Prop,na.rm=T))%>%filter(year<2021)
  p8<-ggplot(PropGear)+geom_col(aes(x=year,y=Prop,fill=gear2))
  if(DoPlot) ggsave(plot=p8, file=paste0(ResDir,"Ecuador_PropGearQuarter.png"), width=15, height=10)
  
  #replace enmalle + palangre by 1/2 for each
  tmp1<-PropGear%>%filter(gear2=="red+palangre")
  tmp2<-tmp1
  tmp1<-tmp1%>%mutate(Prop=0.5*Prop,gear2="LL")%>%rename(GearAbv=gear2)
  tmp2<-tmp2%>%mutate(Prop=0.5*Prop,gear2="GN")%>%rename(GearAbv=gear2)
  
  tmp3<-PropGear%>%filter(gear2!="red+palangre")%>%
    mutate(GearAbv=ifelse(gear2=="enmalle", "GN","LL"))%>%
    select(GearAbv,year,quarter,Prop)
  
  PropGear2<-rbind(tmp1,tmp2,tmp3)%>%
    group_by(GearAbv,year,quarter)%>%
    summarize(Prop=sum(Prop,na.rm=T))
  p9<-ggplot(PropGear2)+geom_col(aes(x=year,y=Prop,fill=GearAbv))
  if(DoPlot)ggsave(plot=p9, file=paste0(ResDir,"Ecuador_PropGearQuarter_Final.png"), width=15, height=10)
  
  #assume the proportions per quarter for LL from 2016 will 
  # be the same in previous years 
  tmp1<-PropGear2%>%filter(year=="2016",GearAbv=="LL")%>%
    group_by(quarter)%>%summarise(Prop=sum(Prop,na.rm=T))
  tmp1$Prop<-tmp1$Prop/sum(tmp1$Prop)
  #This can be used to split CRI, COL, Panama 
  Prop_Quarter_LL_ECU<-t(tmp1$Prop)               
  return(Prop_Quarter_LL_ECU)  
}

try(Prop_Ecuador<-Prop_Catches_Ecuador(),silent=T)

########### Optional STEP 5.1:compare FSR with FAO for some fleets ###########
CRI_COL_PAN_FSR_vs_FAO<-function(DataFSR=FSR,DataFAO=SWO.FAO.W, ResDir=MyDir,DoPlot=TRUE)
{
  ##### Optional STEP Inspect Costa Rica, Colombia, Panama
  FSR<-DataFSR
  SWO.FAO.W<-DataFAO
  require(tidyverse)
  FlagAbv<-c("CRI","COL","PAN")
  Flag<-c("Costa Rica" ,"Colombia", "Panama")
  for(i in 1:3){
    MyFlagAbv<-FlagAbv[i]
    MyFlag<-Flag[i]
    tmp1<-FSR%>%filter(FlagAbv==MyFlagAbv)
    tmp2<-SWO.FAO.W%>%filter(Flag==MyFlag)%>%rename(CatchFAO=Catch)
    
    tmp3<-full_join(tmp2,tmp1)
    
    tail(tmp3)
    
    p8<-ggplot(tmp3)+geom_point(aes(x=Year,y=Catch,color="IATTC aggregated data / Data Used"), 
                                fill="black",alpha=1,size=3,shape=19)+
      geom_col(aes(x=Year,y=CatchFAO,fill="FAO data"),alpha=0.5)+
      #geom_point(aes(x=Year,y=Catch,color="Data used",fill="Data used"),alpha=0.5,shape=22)+
      theme_bw(25)+ylab("catch in 1000 tons")+ 
      xlab("Year")+
      labs(title=MyFlag)+theme_bw(25) +
      xlim(1985,2021)
    
    if(DoPlot) ggsave(plot=p8, file=paste0(ResDir,"Catches_",MyFlag,"_FSR_FAO.png"), width=15, height=10)
  }
}

##use FSR for this 3 fleets 
########### Optional STEP 5.2 : look at Catches FSR Peru and Ecuador ###########
PER_ECU_FSR<-function(DataFSR=FSR,ResDir=MyDir,DoPlot=TRUE)
{
  MyFlag<-c("PER","ECU")
  p1<-ggplot(FSR%>%filter(FlagAbv%in%MyFlag))+geom_col(aes(x=Year,y=Catch,fill=FlagAbv), color="grey")+
    facet_wrap(~GearAbv)+
    theme_bw()+ylab("catch in 1000 tons")
  if(DoPlot) ggsave(plot=p1, file=paste0(ResDir,"Catches_",MyFlag,"_FSR.png"), width=15, height=10)
  
}

########### STEP 6 Catches from CHILE from self reported (2000-2020) and FSR (1999 and before) ###########
Do_Catches_Chile<-function(DataFSR=FSR,DoPlot=TRUE,ResDir=MyDir){
  #2000 - 2020 from recent submission
  #1999 and before - FSR
  #03/04/2022
  FSR<-DataFSR
  ### add Chilean catches by quarter and year
  require(tidyverse)
  require(lubridate)
  CHL.Dir<-"C:\\Users\\cminte\\Documents\\Carolina2022\\IATTC\\SWO\\catches\\Chile\\"
  CHL.art<-read_csv(paste0(CHL.Dir, "artisanal_landings.csv"))
  CHL.fac<-read_csv(paste0(CHL.Dir, "factory_vessels.csv"))
  CHL.ind<-read_csv(paste0(CHL.Dir, "industrial_landings.csv"))
  CHL.art$type<-"ART"
  CHL.fac$type<-"FAC"
  CHL.ind$type<-"IND"
  #names(CHL.fac)
  #names(CHL.art)
  #names(CHL.ind)
  CHL<-rbind(CHL.art,CHL.fac,CHL.ind)
  
  CHL.coord<-read_csv(paste0(CHL.Dir,"ports_coords.csv"))
  #names(CHL)
  CHL<-CHL%>%rename( Landing_region='Landing region')
  CHL<-full_join(CHL,CHL.coord)
  CHL<-CHL%>%mutate(quarter=quarter(month),
      GearAbv=ifelse(gear=="ARPON","HAR",ifelse(gear=="ENMALLE","GN",
                  ifelse(gear%in%c("ESPINEL","PALANGRE"),"LL","HAR"))))
  
  
  tmp1<-CHL%>%group_by(type,Year)%>%summarise(whole_weight_kg=sum(whole_weight_kg,na.rm=T))
  p4<-ggplot(tmp1)+geom_col(aes(x=Year,y=whole_weight_kg/1000,fill=type),col="black")+theme_bw(25)
  if(DoPlot) ggsave(plot=p4, file=paste0(CHL.Dir,"CHL_catch_year.png"), width=25, height=15)
  
  tmp1<-CHL%>%group_by(gear,Year)%>%summarise(whole_weight_kg=sum(whole_weight_kg,na.rm=T))
  p5<-ggplot(tmp1)+geom_col(aes(x=Year,y=whole_weight_kg/1000,fill=gear),col="black")+theme_bw(25)
  if(DoPlot) ggsave(plot=p5, file=paste0(CHL.Dir,"CHL_catch_gear.png"), width=25, height=15)
  #tmp1<-CHL%>%group_by(Year)%>%summarize(Catch=sum(whole_weight_kg,na.rm=T)/1000)
  tmp1<-CHL%>%group_by(gear,Year,month)%>%summarize(Catch=sum(whole_weight_kg,na.rm=T)/1000)
  
  write.csv(tmp1, file=paste0(ResDir,"Chile_SWO_catches_in_metric_tons_2000_2020.csv"))
  
  tmp1[tmp1$gear=="CERCO",]
  tmp1[tmp1$gear=="ARPON",]
  
  #FSR<-read_csv("CatchesFSR.csv")
  tmp2<-FSR%>%filter(FlagAbv=="CHL",Year>1999)
  #NEED TO UPDATE FSR for the Chilean Submissions, data sent to data group on 03/08/2022
  #cbind(tmp1,tmp2)
  # Year    Catch FlagAbv Year      LL HAR   NK LLX GN LLD LP OTR
  # 1  2000 2142.288     CHL 2000 1726  NA 1247  NA NA  NA NA  NA
  # 2  2001 3668.034     CHL 2001 1911  NA 1351  NA NA  NA NA  NA
  # 3  2002 4072.210     CHL 2002 1966  NA 1557  NA NA  NA NA  NA
  # 4  2003 3841.641     CHL 2003 3848  NA   NA  NA NA  NA NA  NA
  # 5  2004 3274.770     CHL 2004 3268  NA   NA  NA NA  NA NA  NA
  # 6  2005 3981.335     CHL 2005   NA  NA 3979  NA NA  NA NA  NA
  # 7  2006 3038.116     CHL 2006   NA  NA 3147  NA NA  NA NA  NA
  # 8  2007 3863.893     CHL 2007   NA  NA 3741  NA NA  NA NA  NA
  # 9  2008 2795.788     CHL 2008   NA  NA 2792  NA NA  NA NA  NA
  # 10 2009 3516.733     CHL 2009   NA  NA 3514  NA NA  NA NA  NA
  # 11 2010 4368.153     CHL 2010   NA  NA 4363  NA NA  NA NA  NA
  # 12 2011 4948.769     CHL 2011   NA  NA 4949  NA NA  NA NA  NA
  # 13 2012 6345.438     CHL 2012   NA  NA 6239  NA NA  NA NA  NA
  # 14 2013 4856.300     CHL 2013   NA  NA 4852  NA NA  NA NA  NA
  # 15 2014 5806.572     CHL 2014   NA  NA 5799  NA NA  NA NA  NA
  # 16 2015 6033.197     CHL 2015   NA  NA 5737  NA NA  NA NA  NA
  # 17 2016 7491.109     CHL 2016   NA  NA 6656  NA NA  NA NA  NA
  # 18 2017 7920.972     CHL 2017   NA  NA 5889  NA NA  NA NA  NA
  # 19 2018 6768.099     CHL 2018   NA  NA 4739  NA NA  NA NA  NA
  # 20 2019 8805.558     CHL 2019   NA  NA 8806  NA NA  NA NA  NA
  Chile<-CHL%>%mutate(NS=ifelse(Landing_region>4,"N","S"))%>%
    group_by(GearAbv, Year,NS, quarter)%>%
    summarize(SumOfWeight=sum(whole_weight_kg,na.rm=T)/1000)
  
  p5<-ggplot(Chile)+geom_col(aes(x=Year,y=SumOfWeight,fill=GearAbv),col="black")+
    theme_bw(25)+facet_grid(quarter~NS)
  if(DoPlot) ggsave(plot=p5, file=paste0(CHL.Dir,"CHL_year_landing_region.png"), width=25, height=15)
  
  #no numbers available
  CHL.final<-Chile%>%mutate(Lat=ifelse(NS=="N",-18,-30),
                            Lon=-70, SumOfNumber=NA,FlagAbv="CHL")%>%
    mutate(Quarter=quarter)%>%select(FlagAbv,Lon,Lat,Year,Quarter,SumOfWeight,SumOfNumber,GearAbv)
  
  CHL.final.grided<-CHL.final%>%
    select(FlagAbv,Lon,Lat,Year,
           Quarter,SumOfWeight,SumOfNumber,GearAbv)
  
  #### Allocate FSR Chile before 1999
  
  #Allocation by quarter and region  for each gear Chile before year 2000
  #use the same as 2000 to 2005
  tmp1<-Chile%>%filter(Year>1999, Year<2006,GearAbv%in%c("HAR","HRP","LL","GN"))%>%
    mutate(GearAbv=ifelse(GearAbv=="HRP","HAR",GearAbv))%>%
    group_by(GearAbv,quarter,NS)%>%summarise(Catch=sum(SumOfWeight,na.rm=T))
  n<-dim(tmp1)[1]
  Sum<-tmp1%>%group_by(GearAbv)%>%summarise(CatchS=sum(Catch,na.rm=T))
  tmp1<-left_join(tmp1,Sum)
  tmp1$Prop<-tmp1$Catch/tmp1$CatchS
  #will sum to 1 for each gear. 
  ifelse(sum(tmp1$Prop)==3, print("proportions OK"),print("ERROR"))
  #should sum to 3 (3 gears, 2 regions and 4 quarters= 24)
  ifelse(dim(tmp1)[1]==n,print("number of cases OK"),print("ERROR"))
  
  proportions_CHL_1999_and_before<-tmp1%>%select(GearAbv,quarter,NS,Prop)
  
  p1<-ggplot(tmp1)+geom_col(aes(x=GearAbv,y=Prop,fill=as.factor(quarter)))+facet_wrap(~NS)+
    theme_bw(25)+labs(title="Chile, proportions by gear, quarter, area for Year<2000")
  if(DoPlot) ggsave(plot=p1, file=paste0(ResDir,"CHL_allocation_before_2000.png"), width=25, height=15)
  
  #treat all NK as enmalle GN and LL, LLD, LLX as LL
  
  CHL.1999_and_before<-FSR%>%filter(FlagAbv=="CHL", Year<2000)
  #table(CHL.1999_and_before$FlagAbv,CHL.1999_and_before$GearAbv)
  #do not use gear OTH
  CHL.1999_and_before<-CHL.1999_and_before%>%
    mutate(GearAbv=ifelse(GearAbv=="NK","GN",GearAbv))
  #Assume average HAR catches for years with no Gear (NK)
  TotHar<-CHL.1999_and_before%>%filter(Year>1979,Year<1986,GearAbv=="HAR")%>%
    summarise(Catch=mean(Catch,na.rm=T))
  TotHar$Catch
  HAR<-CHL.1999_and_before%>%filter(Year>1985,GearAbv=="GN")
  HAR$GearAbv<-"HAR"
  HAR$Catch<-TotHar$Catch
    #Assume the rest of NK is GN
  GN<-CHL.1999_and_before%>%filter(Year>1985,GearAbv=="GN")
  GN$Catch<-GN$Catch-TotHar$Catch
  LL<-CHL.1999_and_before%>%filter(Year>1985,GearAbv=="LL")
  CHL.1999_and_before<-CHL.1999_and_before%>%filter(Year<1986)
  CHL.1999_and_before<-rbind(CHL.1999_and_before,HAR,LL,GN)
  ### end                                                   
  CHL.1999_and_before.gridded<-left_join(CHL.1999_and_before,proportions_CHL_1999_and_before)
  CHL.1999_and_before.gridded<-CHL.1999_and_before.gridded%>%mutate(SumOfWeight=Prop*Catch)
  
  p1<-ggplot(CHL.1999_and_before.gridded)+geom_col(aes(x=GearAbv,y=SumOfWeight,fill=as.factor(quarter)))+facet_wrap(~NS)+
    theme_bw(25)+labs(title="Chile, proportions by gear, quarter, area for Year<2000")
  if(DoPlot) ggsave(plot=p1, file=paste0(ResDir,"CHL_Catches_before_2000.png"), width=25, height=15)
  
  p2<-ggplot(CHL.1999_and_before.gridded)+
    geom_col(aes(x=Year,y=SumOfWeight,fill=as.factor(quarter)))+facet_grid(NS~GearAbv)+
    theme_bw(25)+labs(title="Chile, proportions by gear, quarter, area for Year<2000")
  if(DoPlot) ggsave(plot=p2, file=paste0(ResDir,"CHL_Catches_before_2000_by_year.png"), width=25, height=15)
  
  
  
  CHL.1999_and_before.gridded<-CHL.1999_and_before.gridded%>%
    mutate(Lat=ifelse(NS=="N", -15,-27),Lon=I(-90),SumOfNumber=NA)%>%rename(Quarter=quarter)%>%
    select(FlagAbv,Lon,Lat,Year,
           Quarter,SumOfWeight,SumOfNumber,GearAbv)
  
  
  CHL<-rbind(CHL.1999_and_before.gridded,CHL.final.grided[,2:9])
  
  return(CHL)
}

Chile.grided<-Do_Catches_Chile(DataFSR = FSR)
#tmp1<-Chile.grided%>%group_by(Year,GearAbv)%>%summarise(Catch=sum(SumOfWeight,na.rm=T))
#tail(pivot_wider(as_tibble(tmp1), names_from = GearAbv,values_from = Catch))

########### STEP 7 allocate FSR data to Lat Lon (using EEZ) for coastal countries

Allocate_FSR<-function(){
  ###allocate to a Lat and Lon 
  allocate.FSR<-as.data.frame(matrix(c("MEX", 15, -120,"USA",15,-120, 
                                       "COL",	2, -88,
                                       "CRI",	2, -88,
                                       "ECU", -2,-93,
                                       "GTM",	2, -88,
                                       "HND",	2, -88,
                                       "NIC",	2, -88,
                                       "PRT",	-10,-75,
                                       "SLV",	2,-88,
                                       "URY",	-10,-75),ncol=3,byrow=T))
  names(allocate.FSR)<-c("FlagAbv","Lat","Lon")
  return(allocate.FSR)
}

Spatial.allocation<-Allocate_FSR()

###########  STEP 7 Catches Central America ###########
# allocate to to quarter (using Ecuador) 
Do_Catches_Central_America<-function(DataFSR=FSR,DoPlot=TRUE,ResDir=MyDir,Prop=Prop_Ecuador, Allocation=Spatial.allocation)
{
  FSR<-DataFSR
  #all gear considered LL
  CentralAm<-FSR%>%filter(FlagAbv%in%c("CRI","GTM", "HND", "NIC",  "SLV", "COL") )# split using Ecuador
  #table(CentralAm$FlagAbv,CentralAm$GearAbv) #allocate according to ECU LL
  CentralAm<-CentralAm%>%mutate(GearAbv=ifelse(GearAbv=="NK","GN","LL"))
  #LL LLX NK
  #CRI 29   0  0
  #GTM 17   1  2
  #HND  6   0  0
  #NIC 11   0  9
  #SLV  9   0  1
  CentralAm<-CentralAm%>% mutate(GearAbv="LL")
  CentralAm<-CentralAm%>%mutate(Q1=Catch*Prop[1],
                                Q2=Catch*Prop[2],
                                Q3=Catch*Prop[3],
                                Q4=Catch*Prop[4])%>%
    pivot_longer(cols=starts_with("Q"),names_to = "Quarter",values_to = "SumOfWeight")%>%
    mutate(Lat=I(-2),Lon=I(-88))%>% select(FlagAbv,GearAbv,Year,Quarter, SumOfWeight)
  #all gear considered LL
  tmp1<-left_join(CentralAm,Allocation)%>%
    mutate(Quarter=as.numeric(as.factor(Quarter)),SumOfNumber=NA,Lat=as.numeric((Lat),Lon=as.numeric(Lon)))%>%
    select(FlagAbv,Lon,Lat,Year,Quarter,SumOfWeight,SumOfNumber,GearAbv)
  CentralAm.grided<-tmp1
  
  return(CentralAm.grided) 
}

CentralAm.grided<-Do_Catches_Central_America(DataFSR = FSR)
#tmp1<-CentralAm.grided%>%group_by(Year,GearAbv)%>%summarize(Catch=sum(SumOfWeight,na.rm=T))%>%
  #pivot_wider(names_from = GearAbv,values_from = Catch)


###########  STEP 8  PS ALL FLAGS BYCATCH ###########
# NOT ADDED BECAUSE IT IS TOO SMALL
Catches_Purse_seine<-function(DoPlot=TRUE,MyDir=TheDir){
  #purse_seine_catches
  PS_Dir<-"C:\\Users\\cminte\\Documents\\Carolina2022\\IATTC\\SWO\\catches\\Purse_seine\\"
  PS<-read_csv(paste0(PS_Dir,"PublicPSBillfishFlag.csv"))
  PS<-PS%>%mutate(Quarter=quarter(Month))
  tmp3<-PS%>%group_by(Year,Quarter,Flag)%>%summarise(Catch=sum(SWO))
  Flags<-unique(PS$Flag)
  
  p1<-ggplot(tmp3)+geom_col(aes(x=Year,y=Catch,fill=Flag), color="black")+
    facet_wrap(~Quarter)+
    theme_bw(25)+ylab("Purse seiners, catch in number of fish")
  
  if(DoPlot) ggsave(plot=p1, file=paste0(MyDir,"Catches_PS_numbers.png"), width=25, height=15)
  
  tmp4<-PS%>%group_by(Year,LatC1, LonC1)%>%summarise(Catch=sum(SWO))
  
  p2<-ggplot(tmp4)+geom_tile(aes(x=LonC1,y=LatC1,fill=Catch>0, color=Catch>0),alpha=0.7)+
    #scale_color_distiller(palette="Spectral")+
    #scale_fill_distiller(palette="Spectral")+
    facet_wrap(~Year)+
    theme_bw(10)
  
  if(DoPlot) {ggsave(plot=p2, file=paste0(MyDir,"MAP_Catches_PS_numbers.png"), width=25, height=15)
    ggsave(plot=p2, file=paste0(MyDir,"MAP_Catches_PS_numbers.pdf"), width=25, height=15)}
    
    return(PS)
  
  
}
PS.Catch<-Catches_Purse_seine()  #TOO LITTLE, DO NOT ADD




###########  STEP 8  Grided 5 by 5 catches from longline nations EPO ###########
# not corrected for FSR

Do_Catches_Grided<-function(DoPlot=TRUE){
  # ############# Get GRIDDED DATA
  # #Catch estimation from 5 by 5 
  # #Lastest data from:
  # # PO - Swordfish catches 
  # #new download on 2/28/2022
  # #hese data files were last updated on the 23rd Nov 2021.
  ## EPO: August 2021
  ## Data from: https://www.iattc.org/PublicDomainData/IATTC-Catch-by-species1.htm
  DataDir<-"C:\\Users\\cminte\\Documents\\Carolina2022\\IATTC\\SWO\\catches\\PublicLLTunaBillfish\\"
  EPO.LL<-paste0(DataDir,"PublicLLTunaBillfishMt.csv")
  EPOn.LL<-paste0(DataDir,"PublicLLTunaBillfishNum.csv")
  #EPO data:Longline catch data are submitted to the IATTC as numbers of individuals, 
  #weight of catch, or both numbers and weight for the same catch.
  #To eliminate duplication of data within a file when the same catch 
  #is reported in both numbers and weight, catch and effort data are presented 
  #in 2 separate files. Data reported only in numbers or only 
  #in weight are reported identically in both files. However, when the same data 
  #are reported in both number and weight, the Number (PublicLLTunaBillfishNum.csv )
  #file contains number data in the number columns, and the weight data are excluded; likewise, the Weight file (PublicLLTunaBillfishMt.csv ) 
  #contains weight data in the weight columns, and the number data are excluded.
  EPOw<-read_csv(EPO.LL) # if only report weight - weight data in the weigh column, NA in the number colum
  EPOn<-read_csv(EPOn.LL) # same as above with number
  #get weight from the weight file
  EPOw2 <- EPOw %>% mutate(quarter= as.integer(1+floor(Month/3.3)))%>%
    group_by(Flag,Year,quarter,LonC5,LatC5) %>% 
    summarise(swo_w_fromW = sum(SWOmt,na.rm = TRUE),  hooks=sum(Hooks))%>%
    mutate(YrQr=Year+(quarter-1)/4)
  sum(EPOw2$swo_w_fromW)
  #249013.8
  #Numbers from the weight file this is for flags that report both number and weight
  EPOn2 <- EPOw %>% mutate(quarter= as.integer(1+floor(Month/3.3)))%>%
    group_by(Flag,Year,quarter,LonC5,LatC5) %>% 
    summarise(swo_n_fromW = sum(SWOn,na.rm = TRUE),  hooks=sum(Hooks))%>%
    mutate(YrQr=Year+(quarter-1)/4)
  sum(EPOn2$swo_n_fromW)
  #3243725
  
  EPO<-inner_join(EPOw2,EPOn2)
  #Joining, by = c("Flag", "LonC5", "LatC5", "YrQr")
  names(EPO)
  sum(EPO$swo_n_fromW) # 3243725
  sum(EPO$swo_w_fromW) # 249013.8
  
  head(EPO)
  tail(EPO)
  
  #This is for flags that report numbers and weight
  EPOw3 <- EPOn %>% mutate(quarter= as.integer(1+floor(Month/3.3)))%>%
    group_by(Flag,Year,quarter,LonC5,LatC5,) %>% 
    summarise(swo_w_fromN = sum(SWOmt,na.rm = TRUE),
              hooks2=sum(Hooks))%>%
    mutate(YrQr=Year+(quarter-1)/4)
  sum(EPOw3$swo_w_fromN) # 13511.64
  
  #This w3is for Flag that report numbers and weigh and only numbers
  EPOn3 <- EPOn %>% mutate(quarter= as.integer(1+floor(Month/3.3)))%>%
    group_by(Flag,Year,quarter,LonC5,LatC5)%>%
    summarise(swo_n_fromN = sum(SWOn,na.rm = TRUE))%>%
    mutate(YrQr=Year+(quarter-1)/4)
  
  sum(EPOn3$swo_n_fromN) # 7765214
  
  tmp1<-EPOw2%>%filter(LatC5<10.1,LonC5> I(-150.1))%>%group_by(Flag,Year)%>%summarize(CatchW=sum(swo_w_fromW,na.rm=T))
  tmp2<-EPOn3%>%filter(LatC5<10.1,LonC5>I(-150.1))%>%group_by(Flag,Year)%>%summarize(CatchN=sum(swo_n_fromN,na.rm=T))
  tmp<-full_join(tmp1,tmp2)
  write_csv(tmp,"LL_10N_to_the_south_catches_from_gridded.csv")
  #If data in the weight column reported in weight and numbers
  EPO2<-inner_join(EPOw3,EPOn3)
  sum(EPO2$swo_n_fromN)# 7765214
  sum(EPO2$swo_w_fromN) # 13511.64
  
  EPO_all<-inner_join(EPO,EPO2)
  #Joining, by = c("Flag", "Year", "quarter", "LonC5", "LatC5", "YrQr")
  sum(EPO_all$swo_w_fromW) #249013.8
  sum(EPO_all$swo_n_fromW) #3243725
  sum(EPO_all$swo_w_fromN) #13511.64
  sum(EPO_all$swo_n_fromN) #7765214
  
  unique(EPO_all$Flag)
  #"BLZ" "CHN" "ESP" "JPN" "KOR" "MEX" "PAN" "PYF" "TWN" "USA" "VUT"
  names(EPO_all)
  # [1] "Flag"        "Year"        "quarter"     "LonC5"       "LatC5"      
  # [6] "swo_w_fromW" "hooks"       "YrQr"        "swo_n_fromW" "swo_w_fromN"
  # [11] "hooks2"      "swo_n_fromN"
  
  Catch_Grided<-EPO_all%>%select(Flag,LonC5,LatC5,Year,quarter,YrQr,swo_w_fromW,swo_n_fromN)
  Catch_Grided<-as.data.frame(Catch_Grided)
  names(Catch_Grided)<-c("FlagAbv","Lon","Lat","Year","Quarter","YrQr","SumOfWeight","SumOfNumber")
  Catch_Grided$SpeciesAbv<-"SWO"
  #unique(Catch_Grided$Lat)
  
  ####STEP 8.1: Correct China 2006 in grided
  #remove data from CHN for 2006 (observer catches) and replace it by 2005
  Grided.CHN<-Catch_Grided%>%filter(FlagAbv=="CHN")
  Grided.therest<-Catch_Grided%>%filter(FlagAbv!="CHN")
  Grided.CHN.2005<-Grided.CHN%>%filter(Year=="2005")
  Grided.CHN.2006<-Grided.CHN.2005 %>%
    mutate(Year=2006,YrQr=Year+(Quarter-1)/4)
  Grided.CHN.therest<-Grided.CHN%>%filter(Year!="2006")
  Grided.CHN<-rbind(Grided.CHN.therest,Grided.CHN.2006)
  
  #replace the Chinese data for the corrected Chinese data
  Catch_Grided<-Catch_Grided%>%filter(FlagAbv!="CHN")
  Catch_Grided<-rbind(Catch_Grided,Grided.CHN)
  Catch_Grided$GearAbv<-"LL"
  
  Catch_Grided<-Catch_Grided%>%select(FlagAbv,Lon,Lat,Year,
                                      Quarter,SumOfWeight,SumOfNumber,GearAbv)
  
  sum(Catch_Grided$SumOfWeight)#249381.7
  sum(Catch_Grided$SumOfNumber)#7765214
  
  return(Catch_Grided)
}

#
Catch_Longline_grided<-Do_Catches_Grided()
#tmp1<-Catch_Longline_grided%>%group_by(Year,GearAbv)%>%summarise(Catch=sum(SumOfWeight,na.rm=T))%>%
#  pivot_wider(values_from = Catch,names_from = GearAbv)
#tmp1<-Catch_Longline_grided%>%group_by(Year,GearAbv)%>%summarise(Catch=sum(SumOfNumber,na.rm=T))%>%
 # pivot_wider(values_from = Catch,names_from = GearAbv)


########### STEP 9 : PRT and URY ###########
Do_Catches_Portugal_Uruguay<-function(DataFSR=FSR,DataGrided=Catch_Longline_grided,ResPlot=MyDir)
{
  FSR<-DataFSR
  Catch_Grided<-DataGrided
  #allocate according to the Spanish fleet
  Other<-FSR%>%filter(FlagAbv%in%c("PRT","URY"))
  #table(Other$FlagAbv,Other$GearAbv)
  #PRT fishes in FAO area 87:
  #https://www.iattc.org/Meetings/Meetings2017/SAC-08/PDFs/INF/_English/SAC-08-INF-A(g)_European-Union-Portugal-Annual-report-2016-SPO.pdf
  #since not very precise info assume Lat=I(-10), Lon= I(-115)
  Other<-Other%>%group_by(Year,FlagAbv)%>%summarise(Catch=sum(Catch,na.rm=T))
  #assumed Lat 10S and Lon 110W
  Other<-Other%>%mutate(Lat=I(-10),Lon=I(-110))
  minYear<-min(Other$Year)
  maxYear<-max(Other$Year)
  tmp1<-Catch_Grided%>%filter(Year>=minYear,
                              Year<=maxYear,Lat<I(-5),Lat>I(-20),FlagAbv=="ESP")
  tmp1<-tmp1%>%group_by(Year,Quarter)%>%summarise(Catch=SumOfWeight)
  tmp1<-tmp1%>%group_by(Year,Quarter)%>%summarise(Catch=sum(Catch,na.rm=T))
  
  tmp2<-tmp1%>%group_by(Year)%>%summarise(CatchTotal=sum(Catch,na.rm=T))
  tmp1<-full_join(tmp2,tmp1)
  tmp1$Prop<-tmp1$Catch/tmp1$CatchTotal
  tmp1<-tmp1%>%select(Year,Quarter,Prop)
  tmp3<-left_join(Other,tmp1)
  tmp3<-tmp3%>%mutate(SumOfWeight=Catch*Prop,SumOfNumber=NA)
  OTH<-tmp3%>%select(Year,FlagAbv,Lat,Lon,Quarter,SumOfWeight,SumOfNumber)
  OTH.grided<-OTH%>% mutate(GearAbv="LL")%>%select(FlagAbv,Lon,Lat,Year,
                                                   Quarter,SumOfWeight,SumOfNumber,GearAbv)
  return(OTH.grided)
  
}

##Ready to go to grided
Portugal.Uruguay.grided<-Do_Catches_Portugal_Uruguay()
#tmp1<-Portugal.Uruguay.grided%>%group_by(Year,GearAbv)%>%summarise(Catch=sum(SumOfWeight,na.rm = TRUE))
  
########### STEP 10 Central Pacific Catches ###########
Correct.FlagAbv<-function(){
  ###change from WCPFC flag code to IATTC flag code
  correct.FlagAbv<-as.data.frame(matrix(c("JP", "JPN",
                                          "US","USA", 
                                          "BZ",	"BLZ",
                                          "KR",	"KOR",
                                          "CN", "CHN",
                                          "TW", "TWN",
                                          "VU","VUT",
                                          "PF",	"PFY"
  ),ncol=2,byrow=T))
  names(correct.FlagAbv)<-c("flag_id","FlagAbv")
  return(correct.FlagAbv)
}

change_FlagAbv<-Correct.FlagAbv()

Do_Catches_WCPO<-function(DoPlot=TRUE,CorrectFlag=change_FlagAbv,ResDir=MyDir){
  # #WCPO: August/05/2021
  # # 5 by 5 public domain data from WCPFC
  # # available from https://www.wcpfc.int/wcpfc-public-domain-aggregated-catcheffort-data-download-page
  DataDir<-"C:\\Users\\cminte\\Documents\\Carolina2022\\IATTC\\SWO\\catches\\WCPFC_L_PUBLIC_BY_FLAG_MON_3\\"
  #now get the area for the sensitivity analysis:
  #WCPO: 1N to 10S, 170W to 150W 
  WCm.LL<-paste0(DataDir,"WCPFC_L_PUBLIC_BY_FLAG_MON.csv")
  WCmLL<-read_csv(WCm.LL)
  # yy    mm flag_id lat_short lon_short cwp_grid hhooks alb_c alb_n yft_c yft_n bet_c bet_n mls_c mls_n blm_c blm_n
  WCmLL<-WCmLL %>% mutate(lat=ifelse(str_detect(lat_short,"S"),as.numeric(str_sub(lat_short,start=1,end=-2))*-1,
                                     as.numeric(str_sub(lat_short,start=1,end=-2)))) %>%
    mutate(LatC5=lat+2.5) %>%
    mutate(lon=ifelse(str_detect(lon_short,"W"),as.numeric(str_sub(lon_short,start=1,end=-2))*-1,
                      as.numeric(str_sub(lon_short,start=1,end=-2))-360))  %>%
    mutate(LonC5=lon+2.5)
  
  WCmLL<-WCmLL%>%rename(Lat=LatC5,Lon=LonC5)
  WCmLL<-WCmLL%>%mutate(Quarter=quarter(mm),GearAbv="LL")%>%
    rename(Year=yy,SumOfNumber=swo_n,SumOfWeight=swo_c)%>%
    filter(Lat<I(10),Lon<I(-150),Lon>=I(-170))
  WCmLL<-left_join(change_FlagAbv,WCmLL)
  #WCmLL<-WCmLL%>%mutate(FlagAbv=ifelse(is.na(FlagAbv),FlagAbv,flag_id))
  
  #number
  #metric tonnes
  tmp<- WCmLL%>% group_by(FlagAbv,Year,Quarter)%>%
    summarise(numbers = sum(SumOfNumber,na.rm = TRUE),weight=sum(SumOfWeight,na.rm=TRUE))
  p1<-ggplot(tmp) + geom_col(aes(x=Year,y=numbers,fill=FlagAbv),color="grey")+
    facet_wrap(~Quarter)+theme_bw(16)+ylab("catch in numbers")+labs(title="Central Pacific 150W to 170W, South of 10N ")
  if(DoPlot) ggsave(plot=p1, file=paste0(ResDir,"Catches_CPO_numbers.png"), width=25, height=15)
  
  p2<-ggplot(tmp) + geom_col(aes(x=Year,y=weight,fill=FlagAbv),color="grey")+
    facet_wrap(~Quarter)+theme_bw(16)+ylab("catch in weight (metric tonnes)")+labs(title="Central Pacific 150W to 170W, South of 10N ")
  if(DoPlot) ggsave(plot=p2, file=paste0(ResDir,"Catches_CPO_weight.png"), width=25, height=15)
  
  WCmLL.grided<-WCmLL%>% select(FlagAbv,Lon,Lat,Year,Quarter,SumOfWeight,SumOfNumber,GearAbv)
  return(WCmLL.grided) 
  
}

Catch_Longline_Central_PO.grided<-Do_Catches_WCPO()
#tmp1<-Catch_Longline_Central_PO.grided%>%group_by(Year,GearAbv)%>%
 # summarise(Catch=sum(SumOfNumber,na.rm=TRUE))
  
########### STEP 11 Area definitions according to model configuration  ###########
# SWO2011 - base case 2011 Assessment
# SWO2022 - Spatial configuration from tree analysis for the 2022 Assessment

area_code<-function(Lat, Lon, Species) 
{
  Areas <- rep(1, length(Lat))
  if(Species=="SWO2011")
  {
    i2 <- which(Lat <= 10 & Lat > (-5) & Lon >= -150  )
    Areas[i2] <- 2
    i3 <- which(Lat <= (-5) & Lon >= -150 & Lon <(-90))
    Areas[i3] <- 3
    i4 <- which(Lat <= (-5) & Lon >=  -90)
    Areas[i4] <- 4
    
    #WCPO areas
    i5 <- which(Lat <= 10 & Lat > I(-5) & Lon <= (-150) & Lon> -170  )
    Areas[i5] <- 5
    i6 <- which(Lat <= -5 & Lon <= (-150) & Lon> -170 ) 
    Areas[i6] <- 6
    i7 <- which(Lat <= 0 & Lon <= -170) #Aus NZ
    Areas[i7] <- 7
    i8 <- which(Lat > 10 & Lon <= (-150) & Lon > -170 ) #Hawaii
    Areas[i8] <- 8
    i9 <- which(Lat >0 & Lon <= -170) #Japan Taiwan
    Areas[i9] <- 9
  }

  if(Species=="SWO2022")
  {
    i2 <- which(Lat <= 10.1 & Lat > I(-20) & Lon >= -150 & Lon < I(-100))  
    Areas[i2] <- 2
    i3 <- which(Lat <= (-20) & Lon >= I(-150) & Lon <I(-100))
    Areas[i3] <- 3
    i4 <- which(Lat <= 10 & Lat > I(-20) & Lon>=I(-100))
    Areas[i4] <- 4
    
    i5 <- which(Lat <= I(-20) & Lon >=I(-100))
    Areas[i5] <- 5
    
    #WCPO areas
    i6 <- which(Lat < 10 & Lat > (-20) & Lon < (-150) & Lon>= I(-170))
    Areas[i6] <- 6
    i7 <- which(Lat <= I(-20) & Lon < I(-150) & Lon>= I(-170) ) 
    Areas[i7] <- 7
    i8 <- which(Lat < 0 & Lon < I(-170)) #Aus NZ
    Areas[i8] <- 8
    i9 <- which(Lat > 10 & Lon < I(-150) & Lon> I(-170)) #Hawaii
    Areas[i9] <- 9
    i10 <- which(Lat >0 & Lon <= I(-170)) #Japan Taiwan
    Areas[i10] <- 10
  }
  if (Species == "BET") {
    i2 <- which(Lat > -10 & Lat < 10 & Lon > -150 & Lon < (-110))
    Areas[i2] <- 2
    i3 <- which(Lat > -10 & Lon > -110)
    Areas[i3] <- 3
    i4 <- which(Lat < (-10) & Lon < (-110))
    Areas[i4] <- 4
    i5 <- which(Lat < (-10) & Lon > -110)
    Areas[i5] <- 5
    i6 <- which(Lon > -90 & Lat < (-15))
    Areas[i6] <- 6
  }
  if (Species == "YFT") {
    i2 <- which(Lon > -120 & Lon < -90)
    Areas[i2] <- 2
    i3 <- which(Lon > -90)
    Areas[i3] <- 3
  }
  return(Areas)
}

#There is a problem here I will not use the correction:
# ########### STEP 12 Correct catches from STEP 8 to raise to FSR ###########
# Grided_Longline_Catches_Correction<-function(DataGrided=Catch_Longline_grided,
#                                              DataFSR=FSR,ResDir=MyDir,
#                                              SP="SWO2011",Tolerance=0.1,DoPlot=TRUE)
# {
#   require(tidyverse)
#   Results<-list()
#   Catch_Grided<-Catch_Longline_grided
#   #Catch_Grided<-DataGrided
#   FSR<-DataFSR
#   #SP<-"SWO2011"
#   #Tolerance<-0.1
#   Catch_Grided$Area<-area_code(Catch_Grided$Lat,Catch_Grided$Lon,Species=SP)
#   tmp<-Catch_Grided%>%group_by(Year,FlagAbv)%>%
#     summarise(TotalW=sum(SumOfWeight),TotalN=sum(SumOfNumber))
#   sum(tmp$TotalW)#249381.7
#   sum(tmp$TotalN)# 7765214
#   
#   FSR2<-FSR%>%filter(GearAbv=="LL")%>%select(FlagAbv,Year,Catch)
#   tmp2<-right_join(FSR2,tmp)
#   tmp2<-replace_na(tmp2,replace=as.list(c(0,0,0,0,0)))
#   #1% tolerance of error between FSR Catch and W=sum(grided weight) and N=sum(grided numbers)
#   tmp2<-tmp2%>%mutate(WTRUE=ifelse((Catch/TotalW)>(1+Tolerance),"larger",ifelse((Catch/TotalW)<Tolerance,"smaller","same")))%>%
#     mutate(NTRUE=ifelse(TotalN>0,TRUE,FALSE))
#   Results[[1]]<-table(tmp2$WTRUE,tmp2$NTRUE)
#   ##03/03/2022
#   # IS CatchFSR>CatchWgridded   \  FALSE-NO TRUE-YES (P=0.01)#is there data on numbers?
#   #larger -YES                       7**         177* #* use as is; **correct:  only need to correct 7
#   #same                              [old:6]7[added 2006 China]         113* 
#   #smaller -NO                       0            4*
#   #China 2006 grided
#   #tmp2%>%filter(FlagAbv=="CHN" & Year>2004)
#   #which are the cases that need correction?
#   tmp3<-tmp2[tmp2$NTRUE=="FALSE" & tmp2$WTRUE=="larger",]
#   #get the data for this flags and years
#   tmp3<-tmp3[,1:5]
#   tmp3<-tmp3[1:7,]
#   Results[[2]]<-tmp3
#   #03/03/2022
#   # A tibble: 11 x 4
#   #FlagAbv   Year Catch     W
#   #<chr>   <dbl> <dbl> <dbl>
#   #  1 PAN      2016  845.  335.
#   #2 PAN      2018  252   232.
#   #3 KOR      2003  182.  150.
#   #4 KOR      2004 1060   949.
#   #5 KOR      2005  287   275.
#   #6 CHN      2001  328.  316.
#   #7 CHN      2002  852   830.
#   
#   ##LETS correct those cases
#   #get the information for all the cases that need correction
#   #using left_join y to x, which includes all cases in x,
#   #but only the matching cases from y
#   tmp4<-left_join(tmp3,Catch_Grided)
#   
#   tmp4<-tmp4 %>%mutate(NewW=Catch*(SumOfWeight/TotalW))
#   
#   tmp5<-full_join(tmp4,Catch_Grided)
#   tmp5<-tmp5%>%mutate(SumOfW=ifelse(NewW>SumOfWeight,NewW,SumOfWeight))
#   
#   sum(tmp5$SumOfWeight,na.rm=T)#249381.7
#   sum(tmp5$SumOfW,na.rm=T)#2087.29
#   sum(tmp5$SumOfNumber,na.rm=T) #7765214
#   
#   #now use catch in numbers for all except those with NewW
#   getAveW<-tmp2[tmp2$NTRUE=="TRUE" &  tmp2$WTRUE=="same",]
#   getAveW<-left_join(getAveW,Catch_Grided)
#   getAveW<-getAveW%>%group_by(FlagAbv,Year,Quarter,Area)%>%
#     summarize(AveW=sum(SumOfWeight/SumOfNumber))
#   #the sizes are very different BY AREAS AND QUARTER
#   getAveW<-getAveW%>%
#     filter(Area%in%c(1,2,3,4))
#   write.table(getAveW, paste0(ResDir,"AveW_from_gridded_by_area.txt"))
#   
#   p1<-ggplot(getAveW) +geom_point(aes(x=Year,y=AveW,color=FlagAbv,shape=as.factor(Quarter),size=1.2))+
#     facet_wrap(~Area)+  ylim(0,2)+xlim(1989,2020)+theme_bw()
#   if(DoPlot) ggsave(plot=p1, file=paste0(ResDir,"EPOAve_W_from_gridded_data.png"), width=25, height=15)
#   
#   
#   #get overall Ave Weigh  by area and quarter (2010 on)
#   getAveW2<-getAveW%>%filter(Year>2009)%>% 
#     group_by(Quarter,Area)%>%summarize(OveW=mean(AveW,na.rm=T))
#   tmp6<-full_join(getAveW2,getAveW)
#   
#   p2<-ggplot(tmp6) +geom_point(aes(x=Year,y=AveW,color=FlagAbv),size=1.2)+
#     geom_line(aes(x=Year, y=OveW),size=1.2,alpha=0.8)+
#     facet_grid(Quarter~Area)+  ylim(0,2)+xlim(1989,2020)+theme_bw(25)
#   if(DoPlot) ggsave(plot=p2, file=paste0(ResDir,"EPO_Ave_W_from_gridded_data_meanfrom_2010_on.png"), width=25, height=15)
#   
#   #getAveW2
#   Results[[3]]<-pivot_wider(getAveW2,names_from = Area,values_from=OveW)
#   #rather than use NewW, replace those by the NewN=NewWewW/average weight[year previous+year following]/2
#   tmp7<-full_join(tmp5,getAveW2)
#   #Joining, by = c("Quarter", "Area")
#   tmp7<-tmp7%>%mutate(SumOfNumber=ifelse(NewW>SumOfWeight,NewW/OveW,SumOfNumber))
#   #tmp7 has the newN 
#   GetVars<-names(Catch_Grided)
#   tmp7<-tmp7%>%select(all_of(GetVars))
#   
#   #now join tmp7 with the rest of the data:
#   tmp2<-tmp2%>%mutate(WTRUE=ifelse((Catch/TotalW)>1.01,"larger",
#                   ifelse((Catch/TotalW)<0.09,"smaller","same")))%>%
#     mutate(NTRUE=ifelse(TotalN>0,TRUE,FALSE))
#   tmp8<-tmp2[(tmp2$NTRUE=="FALSE" & tmp2$WTRUE=="same")|
#                (tmp2$NTRUE=="TRUE") ,]
#   tmp9<-left_join(tmp8,Catch_Grided)
#   
#   tmp9<-tmp9%>%select(FlagAbv,Lon,Lat,Year,Quarter,
#                       SumOfWeight,SumOfNumber,GearAbv,Area)
#   Grided.Final<-rbind(tmp9,tmp7)
#   sum(Grided.Final$SumOfWeight,na.rm=T)
#   sum(Grided.Final$SumOfNumber,na.rm=T)
#   xx<-Grided.Final%>%filter(Lat<10,Lon>I(-150))%>%group_by(Year,FlagAbv,)%>%
#     summarise(W=sum(SumOfWeight,na.rm=T))%>%pivot_wider(names_from = FlagAbv,values_from = W)
#   
#   p2<-ggplot(tmp6) +geom_point(aes(x=Year,y=AveW,color=FlagAbv),size=1.2)+
#     geom_line(aes(x=Year, y=OveW),size=1.2,alpha=0.8)+
#     facet_grid(Quarter~Area)+  ylim(0,2)+xlim(1989,2020)+theme_bw(25)
#   if(DoPlot) ggsave(plot=p2, file=paste0(ResDir,"EPO_Ave_W_from_gridded_data_meanfrom_2010_on.png"), width=25, height=15)
#   
#   write.table(Grided.Final, paste0(ResDir,"Grided_Longline_Catches_from_5by_b5_IATTC_data.txt"))
#   
#   Results[[4]]<-Grided.Final
#   return(Results)
# }
# #
# #Grided_Longline_Catches_Corrected<-Grided_Longline_Catches_Correction()


#tmp1<-as.data.frame(Grided_Longline_Catches_Corrected[[4]])%>%group_by(FlagAbv,Year)%>%summarise(Catch=sum(SumOfNumber,na.rm=T))
#tmp2<-as.data.frame(Grided_Longline_Catches_Corrected[[4]])%>%summarise(Catch=sum(SumOfNumber,na.rm=T))
#7448273
#sum(Grided_Longline_Catches_Corrected[[4]]$SumOfWeight,na.rm=T)
#[1] 480753.5

######### Integrate all steps ########
#Final_Catches.Grided<-function(SpeciesArea="SWO2011",MyDir=TheDir,nyears=10)
#{ 
  #nyears number of years to check for NAs 
  SpeciesArea="SWO2022"
  nyears=10
  MyDir<-TheDir
  RDir<-paste0(MyDir,SpeciesArea,"\\")
  dir.create(RDir)
  MyDir<-RDir
  FSR<-Get_FSR(ResDir=MyDir,nyears=nyears)
  print("FSR done")
  SWO.FAO.W<-Get_FAO(ResDir=MyDir)
  print("FAO done")
  Per.grided<-Do_Catches_Peru(DataFAO=SWO.FAO.W,DataFSR=FSR,ResDir=MyDir)
  print("Peru done")
  Ecu.grided<-Do_Catches_Ecuador(DataFSR=FSR,DataFAO=SWO.FAO.W,ResDir=MyDir)
  print("Ecuador done")
  #Prop_Ecuador<-Prop_Catches_Ecuador(DataFSR=FSR,DataFAO=SWO.FAO.W,ResDir=MyDir)
  #an error message stopped the execution
  Prop_Ecuador<-Prop_Catches_Ecuador(DataFSR=FSR,DataFAO=SWO.FAO.W,ResDir=MyDir)
  print("Allocation following Ecuador done")
  Spatial.allocation<-Allocate_FSR()
  print("FSR allocation done")
  print("Central America next")
  CentralAm.grided<-Do_Catches_Central_America(ResDir=MyDir,DataFSR=FSR,Prop=Prop_Ecuador, Allocation=Spatial.allocation)
  print("Central America done")
  Chile.grided<-Do_Catches_Chile(ResDir=MyDir,DataFSR=FSR)
  print("Chile done")
  Catch_Longline_grided<-Do_Catches_Grided()
  print("Gridded Catches unraised done")
  Portugal.Uruguay.grided<-Do_Catches_Portugal_Uruguay()
  print("Portugal and Uruguay done")
  change_FlagAbv<-Correct.FlagAbv()
  print("Change Flag Abv done")
  Catch_Longline_Central_PO.grided<-Do_Catches_WCPO(ResDir=MyDir)
  print("Central PO done")
  #Grided_Longline_Catches_Corrected<-Grided_Longline_Catches_Correction(SP=SpeciesArea,ResDir=MyDir)

  
  #Catch_Longline_EPO<-Grided_Longline_Catches_Corrected[[4]]
  #print("Gridded raised done")
  
  Catch_Longline_EPO<-Catch_Longline_grided
  
  CentralAm.grided$Lon<-as.double(CentralAm.grided$Lon)
  CentralAm.grided$Lat<-as.double(CentralAm.grided$Lat)
  CentralAm.grided$SumOfNumber<-as.numeric(CentralAm.grided$SumOfNumber)
  Ecu.grided$SumOfNumber<-as.numeric(Ecu.grided$SumOfNumber)
  Per.grided$SumOfNumber<-as.numeric(Per.grided$SumOfNumber)
  Chile.grided$SumOfNumber<-as.numeric(Chile.grided$SumOfNumber)
  Portugal.Uruguay.grided$SumOfNumber<-as.numeric(Portugal.Uruguay.grided$SumOfNumber)
  Catch_Longline_EPO$SumOfNumber<-as.numeric(Catch_Longline_EPO$SumOfNumber)
  Catch_Longline_Central_PO.grided$SumOfNumber<-as.numeric(Catch_Longline_Central_PO.grided$SumOfNumber)

  SWO.Final.Catches.grided<-rbind(
    as.data.frame(CentralAm.grided),
    as.data.frame(Ecu.grided),
    as.data.frame(Per.grided),
    as.data.frame(Chile.grided),
    as.data.frame(Portugal.Uruguay.grided),
    as.data.frame(Catch_Longline_EPO[,1:8]),
    as.data.frame(Catch_Longline_Central_PO.grided))
  
  SWO.Final.Catches.grided<-SWO.Final.Catches.grided%>%mutate(Lat=as.numeric(Lat),Lon=as.numeric(Lon))
  print("SWO.Final.Catches.grided done")
  
  SWO.Final.Catches.grided$Area<-area_code(Lat= SWO.Final.Catches.grided$Lat,Lon= SWO.Final.Catches.grided$Lon,Species=SpeciesArea)
  print("Added area code")
  tmp1<-SWO.Final.Catches.grided%>%group_by(FlagAbv,GearAbv,Year)%>%
    summarise(Catch=sum(SumOfNumber,na.rm=T))
  #tmp3<-tmp1%>%group_by(GearAbv,FlagAbv)%>% summarise(Catch=sum(Catch,na.rm=T))
  #Gear<-unique(tmp1$GearAbv)
  maxYear<-max(tmp1$Year,na.rm=T)
  tmp2<-tmp1%>%filter(Year>(maxYear-nyears))
  
  tmp3<-tmp2%>%filter(GearAbv=="LL") #only LL is reported in numbers
    #step to add NA when there is no data for the year
    tmp4<-tmp3%>%pivot_wider(names_from = FlagAbv,values_from = Catch)%>%arrange(Year)
    n<-dim(tmp4)[2] #number of columns (flags +1)
    y<-dim(tmp4)[1] #number of years
    #only BLZ 2018 needs to be added with a substitution rule
    #2020 many countries, will stop the assessment in 2019
    write.table(tmp4,file=paste0(MyDir,"aggregated_catch_number.csv"))
    
  #Correct Belize
  tmp1<-SWO.Final.Catches.grided%>%filter(Year==2017,FlagAbv=="BLZ",GearAbv=="LL")
  tmp1$Year<-2018
  SWO.Final.Catches.grided<-rbind(SWO.Final.Catches.grided,tmp1)
  write_csv(x=SWO.Final.Catches.grided,file=paste0(MyDir,"SWO_Final_Catches_grided",SpeciesArea,".csv"),na="NA")

  #return(SWO.Final.Catches.grided)
#}

p1<-ggplot(SWO.Final.Catches.grided%>%filter(GearAbv=="LL"))+
    geom_point(aes(x=SumOfNumber,y=SumOfWeight,color=as.factor(FlagAbv)),shape=19,size=1)+
    facet_wrap(~as.factor(Area))

ggsave(plot=p1,filename=paste0(MyDir,"Numbers_vs_Weight_LL.png"),height=15,width=20)
ggsave(plot=p1,filename=paste0(MyDir,"Numbers_vs_Weight_LL.png"),height=15,width=20)



######## FINAL Catch values gridded data ############
#H1: use the 2011 configuration
#SWO2011<-Final_Catches.Grided(SpeciesArea = "SWO2011")
#H2: use the 2022 configuration
#SWO2022<-Final_Catches.Grided(SpeciesArea = "SWO2022",MyDir=TheDir)
#H3: use the 2022 configuration plus add catches from the Central Pacific Ocean
  #that is areas 6 and 7 



