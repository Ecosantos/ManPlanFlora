##############################################
#	MANAGEMENT PLANS BIAS TOWARD 
#		IN THE ATLANTIC FOREST
#	By Gabriel Santos 24 October 2022
##############################################

#Packages
library(ggplot2)
library("rnaturalearth")
library("rnaturalearthdata")
library("terra")
library(tidyverse)
library(openxlsx)
library(sp)
library(broom)
library(lme4)
library("bbmle")
library("glmmTMB")

#Terminology:
 #Scripts use a slight different terminology than paper, as follow:
 #UCs = Protected areas (PAs)
 #MA  = Atlantic Forest
 #Livro_vermelho_OCCs = Red book of Brazilian Flora occurrences

rm(list=ls())

#==========================================================================
#			Accessory functions
#==========================================================================
		#Pass_through function - for pipes in tidyverse
pass_through <- function(data, fun) {
				fun(data); data}

#==========================================================================
#				DATA
#==========================================================================

#file.edit("Data loading and cleaning.R")


#==========================================================================
# 			LOAD UCs Shapefile
# Please contact Carol Loss to access the full protected areas shapefiles
#==========================================================================

UCsDir<-"C:/Artigos e resumos publicados submetidos ideias/Em desenvolvimento/Endangered flora and Management plans/Script and data"

UCs_cleaned<-readRDS(paste0(UCsDir,"/UCsCleaned.rds"))

UCs_sp=UCs<-UCs_cleaned[[2]]
rm(UCs_cleaned)

#---------------------------------------------------------------------------
#		PAs description
#---------------------------------------------------------------------------
#Total PAs
UCs@data%>%nrow(.)

#RPPNs
filter(UCs@data,CAT_MAN2=="Reserva Particular do Patrimônio Natural")%>%nrow()

#REMAINED PAs
filter(UCs@data,CAT_MAN2!="Reserva Particular do Patrimônio Natural")%>%nrow()

#Prop of remained PAs
filter(UCs@data,CAT_MAN2!="Reserva Particular do Patrimônio Natural")%>%
select(ESF_ADM2)%>%table(.)%>%
pass_through(.,table)%>%print()%>%
prop.table()

#==========================================================================
# 			GLOBAL HUMAN FOOTPRINT
#==========================================================================
databasedir<-"C:/Artigos e resumos publicados submetidos ideias/Em desenvolvimento/INMA/Databases"
#Please find Global Human footprint raster files in: 

HFP1993<-raster::raster(paste0(databasedir,"/Venter 2016 - HFI/HFP1993.tif"))
HFP2009<-raster::raster(paste0(databasedir,"/Venter 2016 - HFI/HFP2009.tif"))

HFP1993<-terra::rast(HFP1993)
HFP2009<-terra::rast(HFP2009)

neotropic_relm<-terra::ext(-8000000, -3000000, -4980000,2000000)
world <- ne_countries(scale = "medium", returnclass = "sf")
newproj <- raster::crs(world)

HFP1993<-HFP1993%>%terra::crop(., neotropic_relm)
HFP2009<-HFP2009%>%terra::crop(., neotropic_relm)

HFP1993<-HFP1993%>%terra::project(., newproj@projargs)	
HFP2009<-HFP2009%>%terra::project(., newproj@projargs)	

UCs2<-sf::st_as_sf(UCs)%>%vect(.)

HFP2009_ext<-terra::extract(HFP2009,UCs2,fun="mean", na.rm=TRUE, touches=TRUE)
HFP1993_ext<-terra::extract(HFP1993,UCs2,fun="mean", na.rm=TRUE, touches=TRUE)


#==========================================================================
# 			LOAD SPECIES RECORD CLEANED
#				- Synonim corrected
#				- Threat status cleaned
#==========================================================================
#file.edit("Data loading and cleaning.R")

#source("threat inference.R")

#---------------------------------------------------------------------------
#			Open data	 - "Flora_data_SynonymFree"
#Metadata:
#	Sci_name_InRecord = scientific Name reported in the CNCFlora baseline records
#	Sci_name_Accepted = Most up-to-date scientific nome accepted according to Flora package
#	IsSynonym = return logic (Sci_name_InRecord ==Sci_name_Accepted)
#	threat.status = Acording to CNCFlora taken from Flora package
#---------------------------------------------------------------------------

Flora_data_cleaned<-readRDS("Flora_data_SynonymFree.Rds")

Flora_data<-Flora_data_cleaned;rm(Flora_data_cleaned)

#Total records in PAs
dim(Flora_data)[1]


#			Remove RPPNs data
Flora_data<-filter(Flora_data, CAT_MAN2!="Reserva Particular do Patrimônio Natural")
length(unique(Flora_data$UC_NAME2))

#Records by species - distribution
Flora_data%>%
group_by(Sci_name_Accepted)%>%
summarise(N=n())%>%
ggplot(aes(N)) + geom_histogram(bins = 100)+ 
theme_bw()+labs(x="Records",y="Number of species")+
scale_x_sqrt(breaks=c(0,1,2,10,50,100,200,300,400,500,600))

#Total records
Flora_data%>%
filter(!(threat.status%in%c("LC","DD","NT")))%>%
filter(!is.na(threat.status))


#---------------------------------------------------------------------------
#		Calculate the Endangered Species richness
#---------------------------------------------------------------------------
Flora_data<-Flora_data%>%
group_by(UC_NAME2)%>%
select(-c(Sci_name_InRecord:family),Sci_name_Accepted,threat.status)%>%
nest(Species_list=c(Sci_name_Accepted,threat.status))%>%
mutate(Species_list = map(Species_list, ~ filter(.,  !(threat.status%in%c("LC","DD","NT")))))%>%
mutate(Species_list = map(Species_list, ~ filter(.,  !is.na(threat.status))))%>%
unnest(Species_list,keep_empty = TRUE)%>%
mutate(Richness=n_distinct(Sci_name_Accepted,na.rm = TRUE))%>%
nest(Species_list=c(Sci_name_Accepted,threat.status))%>%
dplyr::select(c(ID_UC2, UC_NAME2,ESF_ADM2,PLA_MAN2,Area,Richness,Species_list))%>%
ungroup()


#Threatened species
Flora_data%>%unnest(Species_list)%>%distinct(Sci_name_Accepted)


#UCs with records
Flora_data%>%mutate(UC_rec=ifelse(Richness>0,"WITH","WITHOUT"))%>%
select(UC_rec)%>%
table(.)%>%print()%>%prop.table()
#---------------------------------------------------------------------
#		Richness vs. Planos de manejo - ALL
#---------------------------------------------------------------------
Flora_data%>%
group_by(PLA_MAN2)%>%
summarize(Mean=mean(Richness),
		Sd=sd(Richness),
		Max=max(Richness),
			N=n())%>%
mutate(Se = Sd/ sqrt(N),
         lower.ci = Mean - qt(1 - (0.05 / 2), N - 1) * Se ,
         upper.ci = Mean + qt(1 - (0.05 / 2), N - 1) * Se)%>%
#as.data.frame(.)%>%
select(PLA_MAN2,N,Mean,Se,lower.ci, upper.ci)

t.test(Flora_data$Richness~Flora_data$PLA_MAN2)

#---------------------------------------------------------------------
#	Descriptive statistics
#---------------------------------------------------------------------
Flora_data%>%
mutate(PLA_MAN2=factor(PLA_MAN2,levels=c("Sim","Não")))%>%
group_by(ESF_ADM2,PLA_MAN2)%>%
summarize(Mean=mean(Richness),
		Sd=sd(Richness),
		Max=max(Richness),
			N=n())%>%
mutate(Se = Sd/ sqrt(N),
         lower.ci = Mean - qt(1 - (0.05 / 2), N - 1) * Se ,
         upper.ci = Mean + qt(1 - (0.05 / 2), N - 1) * Se)%>%
select(ESF_ADM2,PLA_MAN2,N,Mean,Se,lower.ci, upper.ci)


#Differences
Flora_data%>%
 group_by(ESF_ADM2)%>% 
mutate(PLA_MAN2=factor(PLA_MAN2,levels=c("Sim","Não")))%>%
do(broom::tidy(t.test(.$Richness~.$PLA_MAN2)))%>%
as_tibble(.)%>%
mutate(SE=(conf.high-conf.low)/3.92)%>%	#Standard error from confidence interval.
select(-c(parameter:alternative),statistic,p.value)%>%
rename("Mean_PLAN=No" = "estimate","Mean_PLAN=Yes" = "estimate1","Difference" = "estimate2")


#----------------------------------------------------------------------
Fig1<-Flora_data%>%
#rbind(Flora_data,
#Flora_data%>%mutate(ESF_ADM2="All"))%>%
#mutate(ESF_ADM2=factor(ESF_ADM2, levels = c("All","Federal", "Estadual", "Municipal")))%>%
mutate(Riqueza=as.numeric(replace_na(Richness, 0)))%>%
group_by(ESF_ADM2,PLA_MAN2)%>%
summarize(Mean=mean(Richness),
		Sd=sd(Richness),
		Max=max(Richness),
			N=n())%>%
mutate(Se = Sd/ sqrt(N),
         lower.ci = Mean - qt(1 - (0.05 / 2), N - 1) * Se ,
         upper.ci = Mean + qt(1 - (0.05 / 2), N - 1) * Se)%>%
mutate(PLA_MAN2=case_when(PLA_MAN2=="Sim" ~ "Present",
					TRUE ~ "Absent"))%>%
ggplot(.,aes(x=ESF_ADM2,y=Mean,group=factor(PLA_MAN2)))+
geom_pointrange(
#	aes(ymin=Mean-lower.ci, ymax=Mean+upper.ci,fill=PLA_MAN2),
	aes(ymin=Mean-Se, ymax=Mean+Se,fill=PLA_MAN2),
				size=1,shape=21,position=position_dodge(0.3))+
labs(x="Protected areas domain",
	y="Endangered species richness",
	  fill="Management plan")+
scale_fill_manual(values=c(Present="black",Absent="white"))+
theme_bw(base_size=14)+
theme(aspect.ratio = 6/10,
legend.position="top", 
#legend.position=c(.8,.8),
#legend.background = element_rect(fill="grey",
 #                                 size=0.1, linetype="solid", 
  #                                colour ="black"),
 legend.box="vertical", legend.margin=margin())




Fig1

##ggsave("./Figures/Figure1.svg",Fig1,dpi = 500,width = 140, units="mm")


#---------------------------------------------------------------------------
#		INCORPORATING HUMAN FOOTPRINT TO UCS
#---------------------------------------------------------------------------
UCs2<-UCs
UCs2@data<-cbind(UCs@data,HFP1993=HFP1993_ext[,2],HFP2009=HFP2009_ext[,2])
UCs2@data%>%str()


#---------------------------------------------------------------------------
#		INCORPORATING DISTANCE FROM UNIVERSITIES TO UCs
#---------------------------------------------------------------------------
library(geosphere)

CentroidsUCs<-rgeos::gCentroid(UCs2,byid=TRUE)%>%as.data.frame()

#Load Universities Coordinates
IES<-read.csv("Public IES latlong.csv",sep=",")

IES<-IES%>%filter(complete.cases(longitude,latitude))

dist.matrix <- distm( CentroidsUCs[,c("x","y")], IES[,c("longitude","latitude")], fun=distVincentyEllipsoid )

UCs2@data<-cbind(UCs2@data,MinDist_IES=apply(dist.matrix,1,min))

Flora_data<-Flora_data%>%
left_join(UCs2@data%>%dplyr::select(ID_UC2,UC_CODE2,ANO2,HFP1993,HFP2009,MinDist_IES),by="ID_UC2")%>%
mutate(HFPtrend=HFP2009/HFP1993)%>%
as_tibble()


#=================================================================================
# Rescaling variables
#=================================================================================
Flora_data$ANO2Abs<-Flora_data$ANO2
Flora_data$ANO2<-scale(Flora_data$ANO2)
Flora_data$MinDist_IES<-scale(Flora_data$MinDist_IES)
Flora_data$Area<-log(Flora_data$Area)


#=================================================================================
#  Are PAs with management plans richer in threatened flora species?
#=================================================================================

Flora_stand<-Flora_data%>%
  mutate_at(vars(Area,ANO2:HFPtrend),scale)

Modelo_stand<-glmmTMB(Richness~
                        Area+ANO2+PLA_MAN2+HFP2009+HFPtrend+MinDist_IES+(1|ESF_ADM2), 
                      ziformula = ~Area, family="genpois",na.action="na.fail", data = Flora_stand)

Modelo_stand%>%summary()

Dredge_res_stand<-MuMIn::dredge(Modelo_stand,
                                fixed = c("cond(Area)","zi(Area)"),
                                trace=2,rank = "AIC")


DHARMa::simulateResiduals(Modelo_stand)%>%
  DHARMa::testResiduals(.)%>%plot(.)
#---------------------------------------------------------------------------
#	TRANSFORM TABLE FROM MuMIn INTO A STANDARDIZED TABLE WITH MODEL NAMES
#---------------------------------------------------------------------------

model_i<-Dredge_names<-NULL

for(i in 1:dim(Dredge_res_stand)[1]){
  Dredge_names[i]<- Dredge_res_stand[i]%>%coef()%>%
    dimnames()%>%magrittr::extract2(2)%>%
    reformulate()%>%as.character()%>%
    magrittr::extract2(2)
}



cbind(Dredge_names,Dredge_res_stand%>%as_tibble()%>%dplyr::select(df:weight))


#Create a standardized excel file with model selection
cbind(Dredge_names,Dredge_res_stand%>%as_tibble()%>%dplyr::select(df:weight))%>%
write.xlsx(., "Table 1_preliminary.xlsx", sheetName = "Models")


#=================================================================================
#  Figure 2
#=================================================================================

Fig2<-Modelo_stand%>%parameters::parameters()%>%
as_tibble()%>%
filter(!Parameter=="(Intercept)")%>%
filter(Component=="conditional" & Effects=="fixed")%>%
mutate(Parameter=case_when(Parameter=="ANO2"~"PA's creation year",
					Parameter=="Area"~"log(Area)",
					Parameter=="HFPtrend"~"Trend human pressure",
					Parameter=="HFP2009"~"Current human pressure",
					Parameter=="PLA_MAN2Sim"~"Management Plan present",
					Parameter=="MinDist_IES"~"Research institute distance"))%>%
mutate(Parameter=fct_reorder(Parameter, .$Coefficient))%>%
mutate(significance=ifelse(p<0.05,"Significant","Non-significant"))%>%
ggplot(.,aes(x=Parameter,y=Coefficient))+
geom_pointrange(aes(x=Parameter,ymin=CI_low,ymax=CI_high,color=significance),size=.9)+
#viridis::scale_color_viridis(discrete=TRUE,option="grey")+
scale_colour_manual(values = c("grey70", "black"))+
geom_hline(yintercept=0,color="tomato",linetype=2)+
labs(x=NULL,y="Relative effect size",color=NULL)+
coord_flip()+
theme_bw(base_size=16)+
theme(legend.position="top")

Fig2


#=================================================================================
#  Are the management plan determined by our cofounding variables?
#=================================================================================
#---------------------------------------------------------------------------------
#Prepare dataset - standardized variables
#---------------------------------------------------------------------------------
Flora_data_bin_stand<-Flora_data%>%
mutate(PLA_MAN2=case_when(PLA_MAN2=="Sim" ~ 1,
						TRUE ~ 0))%>%
mutate(Richness=log(Richness+1))%>%
mutate_at(vars(Area,ANO2:HFPtrend),scale)


#---------------------------------------------------------------------------------
# GLMM Binomial distributed
#---------------------------------------------------------------------------------
model_area_plan_stand<-lme4::glmer(PLA_MAN2~Area+ANO2+HFP2009+HFPtrend+MinDist_IES+(1|ESF_ADM2), 
family=binomial,data=Flora_data_bin_stand)

model_area_plan_stand%>%ggeffects::ggpredict(.,terms=c("ANO2[[all]]"))%>%plot()

DHARMa::simulateResiduals(model_area_plan_stand)%>%
DHARMa::testResiduals(.)%>%plot(.)

#---------------------------------------------------------------------------------
#		Figure 3
#---------------------------------------------------------------------------------
####Fig3A
Fig3A<-model_area_plan_stand%>%parameters::parameters()%>%
as_tibble()%>%
filter(!Parameter=="(Intercept)")%>%
filter(Effects=="fixed")%>%
mutate(Parameter=case_when(Parameter=="ANO2"~"PA's creation year",
					Parameter=="Richness"~"log(Richness)",
					Parameter=="Area"~"log(Area)",
					Parameter=="HFPtrend"~"Trend human pressure",
					Parameter=="HFP2009"~"Current human pressure",
					Parameter=="MinDist_IES"~"Research institute distance"))%>%
mutate(significance=ifelse(p<0.05,"Significant","Non-significant"))%>%
mutate(Parameter=fct_reorder(Parameter, -.$Coefficient))%>%
ggplot(.,aes(x=Parameter,y=Coefficient))+
geom_pointrange(aes(x=Parameter,ymin=CI_low,ymax=CI_high,color=significance),size=.9)+
#viridis::scale_color_viridis(discrete=TRUE,option="grey")+
scale_colour_manual(values = c("grey70", "black"))+
geom_hline(yintercept=0,color="tomato",linetype=2)+
labs(color=NULL,x=NULL,y=bquote(Odds~ratio[~log]))+
coord_flip()+
theme_bw(base_size=16)+
theme(legend.position="top")

Fig3A
	
####Fig3B
Flora_data_bin<-Flora_data%>%
mutate(PLA_MAN2=case_when(PLA_MAN2=="Sim" ~ 1,
						TRUE ~ 0))%>%
mutate(Richness=log(Richness+1))


model_area_plan<-lme4::glmer(PLA_MAN2~Richness+Area+ANO2Abs+HFP2009+HFPtrend+MinDist_IES+(1|ESF_ADM2), 
family=binomial,data=Flora_data_bin)

model_area_plan%>%summary()

Fig3B<-model_area_plan%>%
sjPlot::plot_model(., type = "pred", terms = "ANO2Abs[[all]]")+
theme_bw(base_size=16)+
labs(title =NULL,
x="PA's creation year",
y="Predicted probability of \n having a management plan")

Fig3B

#library(cowplot)
cowplot::plot_grid(ncol=1,align = "hv",
Fig3A+theme(legend.position="right"),Fig3B)

ggpubr::ggarrange(Fig3A,Fig3B,common.legend = T,nrow = 2,labels="AUTO")


#=================================================================================
#  			Supplementary material S3
#=================================================================================

#---------------------------------------------------------------------------
#	Preliminary model - non-zero-inflated
#---------------------------------------------------------------------------
model_pre<-glmer(Richness~
                   Area+ANO2+PLA_MAN2+HFP2009+HFPtrend+MinDist_IES+(1|ESF_ADM2), 
                 family="poisson",na.action="na.fail", data = Flora_stand)

model_pre%>%performance::check_overdispersion()
model_pre%>%performance::check_zeroinflation()

#---------------------------------------------------------------------------
#	Comparing different zero-structured models
#---------------------------------------------------------------------------

model_zi_area<-glmmTMB(Richness~
                         Area+ANO2+PLA_MAN2+HFP2009+HFPtrend+MinDist_IES+(1|ESF_ADM2), 
                       ziformula = ~Area, family="genpois",na.action="na.fail", data = Flora_stand)

model_zi_equal<-glmmTMB(Richness~
                          Area+ANO2+PLA_MAN2+HFP2009+HFPtrend+MinDist_IES+(1|ESF_ADM2), 
                        ziformula = ~1, family="genpois",na.action="na.fail", data = Flora_stand)

model_NOzi<-glmmTMB(Richness~
                      Area+ANO2+PLA_MAN2+HFP2009+HFPtrend+MinDist_IES+(1|ESF_ADM2), 
                    ziformula = ~0 , family="genpois",na.action="na.fail", data = Flora_stand)

cbind(AIC(model_zi_area,model_zi_equal,model_NOzi), 
      dAIC=AICtab(model_zi_area,model_zi_equal,model_NOzi)$dAIC)

#---------------------------------------------------------------------------
#	TRANSFORM TABLE FROM MuMIn INTO A STANDARDIZED TABLE WITH MODEL NAMES
#---------------------------------------------------------------------------

model_area_plan_stand_A<-model_area_plan_stand

model_area_plan_stand_B<-lme4::glmer(PLA_MAN2~Richness+Area+ANO2+HFP2009+HFPtrend+MinDist_IES+(1|ESF_ADM2), 
                                     family=binomial,data=Flora_data_bin_stand)

rbind(
  model_area_plan_stand_A%>%parameters::parameters()%>%as_tibble()%>%mutate(Model="Without richness"),
  model_area_plan_stand_B%>%parameters::parameters()%>%as_tibble()%>%mutate(Model="With richness"))%>%
  filter(!Parameter=="(Intercept)")%>%
  filter(Effects=="fixed")%>%
  mutate(Parameter=case_when(Parameter=="ANO2"~"PA's creation year",
                             Parameter=="Richness"~"log(Richness)",
                             Parameter=="Area"~"log(Area)",
                             Parameter=="HFPtrend"~"Trend human pressure",
                             Parameter=="HFP2009"~"Current human pressure",
                             Parameter=="MinDist_IES"~"Research institute distance"))%>%
  mutate(significance=ifelse(p<0.05,"Significant","Non-significant"))%>%
  mutate(Parameter=fct_reorder(Parameter, -.$Coefficient))%>%
  ggplot(.,aes(x=Parameter,y=Coefficient,group=Model))+
  geom_pointrange(aes(shape=Model,x=Parameter,ymin=CI_low,ymax=CI_high,color=significance),
                  size=.9, stat="identity",position=position_dodge(width=.3))+
  #viridis::scale_color_viridis(discrete=TRUE,option="grey")+
  scale_colour_manual(values = c("grey70", "black"))+
  geom_hline(yintercept=0,color="tomato",linetype=2)+
  labs(color=NULL,x=NULL,y=bquote(Odds~ratio[~log]))+
  coord_flip()+
  theme_bw(base_size=20)+
  theme(legend.position="top")


#=================================================================================
#  		Data making to Supplementary material S1
#=================================================================================
#Predicted richness
Richness_pred<-predict(Modelo_stand,type = "response")%>%as.vector(.)

#Calculate the centroids

FinalData<-cbind(UCs@data,rgeos::gCentroid(UCs,byid=TRUE))%>%
  select(UC_NAME2,IUCN_CAT2,x,y)%>%
  #Merge Centroids and Flora_data
  left_join(Flora_data,.,by="UC_NAME2")%>%
  select(c(ID_UC2,UC_NAME2,IUCN_CAT2,ESF_ADM2,PLA_MAN2,Richness,x,y))%>%
  #Merge all data with predicted richness 
  left_join(.,
            select(data.frame(
              Flora_data,Richness_pred),
              c(UC_NAME2,Richness_pred)),by="UC_NAME2")

FinalData<-FinalData%>%
  as_tibble()%>%
  mutate(ID_UC2=as.numeric(ID_UC2))

#Change last encoding error manually "N?o"
FinalData<-FinalData%>%
  mutate(PLA_MAN2=
           case_when(PLA_MAN2=="Não" ~ "NO",
                     PLA_MAN2=="Sim" ~ "YES"))

names(FinalData)[c(7,8)]<-c("Long","Lat")

#Write the supplementary material in CSV format
#write.csv(FinalData,"Supplementary material2 Data.csv")

#===================================================================================
#			READ AS LEAFLET
# Centroid is not a good option for Protected areas with two or more different shapes
#	Dani suggested to check  "singlepart to multipart" conversion
#	check this: https://gis.stackexchange.com/questions/305734/splitting-multipart-polygons-to-single-part-in-r
#===================================================================================

library(leaflet)
library(leaflegend)

FinalData$bins<-cut(FinalData$Richness,
                    breaks=c(-1,4,10,50,100,500),
                    labels=c("<5","5-10","10-49","50-99",">100"))

Rich_pal <- colorFactor(c("pink","blue","yellow", "orange" ,"red", "black"), domain = FinalData$bins)


FinalData<-FinalData%>%
  mutate(Richness=as.numeric(Richness))%>%
  mutate(Radius = case_when(Richness  < 5  ~  5,
                            Richness  >= 5  ~  10,
                            Richness  >= 10  ~ 30,
                            Richness  > 50  ~  50))


pal_man<- colorFactor(c("tomato","royalblue2"), domain = FinalData$PLA_MAN2)



leaflet(FinalData, height=500, width=1000)%>%
  addMiniMap()%>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~Long, lat = ~Lat,
                   #                 clusterOptions = markerClusterOptions(),
                   #                  color = ~ifelse(PLA_MAN2=="Sim","Blue","Red"),
                   radius = ~Radius,
                   color = ~pal_man(PLA_MAN2),
                   #                       color = ~Rich_pal(bins),
                   #                       color = ~pal(na.omit(Richness+1)),
                   popup = ~paste0(UC_NAME2,
                                   "<br/><strong>Jurisprudence</strong>:",ESF_ADM2,
                                   "<br/><strong>Management plan.:</strong>",PLA_MAN2,
                                   "<br/><strong>Endangered Species Observed:</strong>",Richness,
                                   "<br/><strong>Endangered Species Predicted:</strong>",round(Richness_pred),0))%>%
  addLegendSize(
    values = FinalData$Radius,
    #   labels = FinalData$bins,
    color = 'black',
    strokeWidth = 2,
    title = 'Endangered species richness',
    shape = 'circle',
    orientation = 'horizontal',
    opacity = .5,
    fillOpacity = .3,
    breaks = 5,
    position = 'topright') %>%
  addLegend("topright", pal=pal_man, values = ~PLA_MAN2,
            title = "Management plan",
            #  orientation = 'horizontal',
            labFormat = labelFormat(prefix = ""),
            opacity = 1)






