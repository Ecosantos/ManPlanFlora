############################################################
#	   	   DATA LOADING AND CLEANING
#
#	An integrated part of the MANAGEMENT PLANS BIAS TOWARD 
#		IN THE ATLANTIC FOREST paper 
#		 By Gabriel Santos 20 Jan 2022
############################################################

#require(cleangeo)
#require(raster)
#require(rnaturalearth)

#Diretory for main resources in INMA project
MainDiretory<- "C:/Artigos e resumos publicados submetidos ideias/Em desenvolvimento/INMA/Databases"

#Diretory for this session
setwd("C:/Artigos e resumos publicados submetidos ideias/Em desenvolvimento/Endangered flora and Management plans/Script and data")
#============================================================================
# 			SHAPEFILES
#============================================================================
#----------------------------------------------------------------------------
# 				World data 
#	It'll be used to reproject coordinates in the next steps
#----------------------------------------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")
newproj <- raster::crs(world)

#----------------------------------------------------------------------------
# 	Atlantic Forest limits sensu Law - according to Loss et al. in press
#----------------------------------------------------------------------------

MA_sp=MA<-rgdal::readOGR(
	dsn = paste0(MainDiretory,"/UCs Brasil _Carol Loss"), 
			layer = "mata_atlantica_bosque_bioma2019_dissolved")
MA<-sf::st_as_sf(MA)
MA<-MA%>%
	as(., "Spatial")%>%
		terra::vect(.)

#----------------------------------------------------------------------------
# 	Protected Areas limits - according to Loss et al. in press
#----------------------------------------------------------------------------
UCs_sp=UCs<-rgdal::readOGR(
	dsn = paste0(MainDiretory,"/UCs Brasil _Carol Loss"), 
		layer = "uc_bosque-bioma_mar_2020-2_final_v02")

UCs%>%str()
UCs_sp@data

#----------------------------------------------------------------------------
# 	Metadata in Loss et al. in Press encoding compatible
#----------------------------------------------------------------------------
UCmetadata<-read.xlsx(paste0(MainDiretory,"/UCs Brasil _Carol Loss/uc_bosque-bioma_mar_2020-2_final.xlsx"),sheet="Sheet1")

UCmetadata<-UCmetadata%>%as_tibble()%>%
mutate(ID_UC2=as.character(ID_UC2))

UCs@data<-UCs_sp@data<-UCs_sp@data%>%
as_tibble()%>%
select(ID_UC2, UC_CODE2)%>%
left_join(.,UCmetadata,by=c("ID_UC2","UC_CODE2"))

#----------------------------------------------------------------------------
# 	Brazilian's landsurface limits
#----------------------------------------------------------------------------

Brasil<-ne_countries(country = 'brazil',scale = "medium", returnclass = "sf")
Brasil<-sf::st_as_sf(Brasil[1])
Brasil<-Brasil%>%
	as(., "Spatial")

#----------------------------------------------------
#Check for problems with holes in the UCs 
#----------------------------------------------------
report <- cleangeo::clgeo_CollectionReport(UCs)
summary <- cleangeo::clgeo_SummaryReport(report)

summary 

UCs_problematic<-UCs[as.numeric(rownames(report[report$valid == FALSE,])),]
UCs_perfect<-UCs[as.numeric(rownames(report[report$valid == TRUE,])),]

#-------------------------------------------------------------------------------------------
# Remove area over ocean from the protected areas
#	We are interested only the terrestrial surface
#-------------------------------------------------------------------------------------------
UCs_terrestrial<-raster::intersect(UCs_perfect, Brasil)

plot(MA_sp,col="grey90",lty = 1,border=NA)
plot(Brasil[1],col=NA,add=T)
plot(UCs,col="red",lty = 1,border=NA,add=T)
plot(UCs_terrestrial,
	col="blue",lty = 1,border=NA,add=T)
legend("bottomright", 
  legend = c("Protected area retained", "Protected area excluded"), 
  col = c("blue","red"),  pch = c(15,15),   bty = "n", 
  pt.cex = 2,   cex = 1.2,   text.col = "black", 
  horiz = F ,   inset = c(0.1, 0.1))


UCs_area_MA<-raster::intersect(UCs_perfect, MA_sp)


UCs_Loss_et_al<-UCs

UCs_cleaned<-list()
UCs_cleaned[[1]]<- UCs_area_MA
	 names(UCs_cleaned)[[1]]<-"Atlantic Forest covered by Protected Areas"
UCs_cleaned[[2]]<- UCs_terrestrial
	 names(UCs_cleaned)[[2]]<-"Terrestrian surface covered by Brazilian Protected Areas"
UCs_cleaned[[3]]<- UCs_Loss_et_al
	 names(UCs_cleaned)[[3]]<-"UCs according to Loss et al. 2022"

names(UCs_cleaned)



#---------------------------------------------------------------------------------------
#			SAVE THE NEW SHAPEFILE 
#---------------------------------------------------------------------------------------
#saveRDS(UCs_cleaned,file="UCsCleaned.Rds")
#
#############################################################################
#			SHAPEFILE FILE PROCESSING END
#		STARTING PREPARING THE DATASET FOR ANALYSES
#############################################################################

#============================================================================
#			CHOOSING UCs SHAPEFILE
#============================================================================
UCs_cleaned<-readRDS("UCsCleaned.rds")
UCs_sp=UCs<-UCs_cleaned[[2]]

#============================================================================
#			CALCULATING UCs SIZE
#============================================================================
UCs_Area_total<-as.data.frame(UCs)
UCs_Area_total$Area<-terra::expanse(vect(UCs))
UCs_Area_total$Area<-UCs_Area_total$Area/10000	#Transform size in hectare
#============================================================================
# 			Brazilian Flora Occurrence
#============================================================================
Livro_vermelho_OCCs<-read.xlsx(
	"Ocorrencias CNCFlora.xlsx" ,
			sheet="Planilha1",startRow = 1,  
				detectDates = TRUE, colNames = TRUE)


#============================================================================
# 		Filtering Atlantic Forest occurrences
#============================================================================

UCs_limits<-bbox(UCs_sp)

Livro_vermelho_OCCs<-Livro_vermelho_OCCs%>%
 as_tibble()%>%
mutate(decimalLatitude=as.numeric(decimalLatitude))%>%
mutate(decimalLongitude=as.numeric(decimalLongitude))%>%
	#Remove o que não se encaixa dentro dos limites (como box) da mata atlântica
filter(decimalLongitude>UCs_limits[1,1],decimalLongitude<UCs_limits[1,2])%>%
filter(decimalLatitude>UCs_limits[2,1],decimalLatitude<UCs_limits[2,2])%>%
select(id,
    collectionCode,
	year,locality,
	  decimalLatitude,decimalLongitude,
	      identifiedBy,scientificName,family)

#============================================================================
# 		Filtering occurrences inside protected areas 
#============================================================================

# Transform occurrences in a spatial object
coordinates(Livro_vermelho_OCCs)= ~ decimalLongitude+decimalLatitude
#plot(Livro_vermelho_OCCs)

#Reproject occurreces using UCs_sp coordinates
sp::proj4string(Livro_vermelho_OCCs)<-sp::proj4string(UCs_sp)
sp::proj4string(UCs_sp)<-sp::proj4string(Livro_vermelho_OCCs)

#plot(Livro_vermelho_OCCs)


UCs_occ<-over(Livro_vermelho_OCCs,UCs_sp)

#Low performance using Raster package
#UCs_occ<-raster::intersect(Livro_vermelho_OCCs,UCs_sp)


#Filtering only those records that match a UC
UCs_Flora<-cbind(
	Livro_vermelho_OCCs,
			UCs_occ)%>%
				as_tibble()%>%
					filter(!is.na(UC_NAME2))


#============================================================================
# 			FINISHING DATASET PREPARATION
# This step does:
#	- Add the protected areas where no record exists
#	- Estimate richness
#============================================================================

Flora_data<-full_join(
select(UCs_Flora,c("year","locality","scientificName","family",ID_UC2)),
	select(UCs_Area_total,c(ID_UC2,UC_NAME2,  ESF_ADM2, CAT_MAN2, PLA_MAN2,MATA_ARE2,Area)),
			by="ID_UC2")%>%
mutate(ESF_ADM2=factor(ESF_ADM2, levels = c("Federal", "Estadual", "Municipal")))%>%
select(scientificName,family,ID_UC2,UC_NAME2,  ESF_ADM2, CAT_MAN2, PLA_MAN2,MATA_ARE2,Area)

#---------------------------------------------------------------------------------------
#Confirm if protected area size is corrected
#---------------------------------------------------------------------------------------
#plot(Flora_data$Area~Flora_data$MATA_ARE2,xlim=c(0,10^6),ylim=c(0,10^6))
#abline(a=0,b=1)
#---------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------
#Confirm if UCs are correct! THEY ARE NOT! CHECK BELOW
#	Yet, only 5 in ~800 UCs are duplicated
#			4/5 are RPPNs, which are not included in our manuscript, 
#				there is nothing to worry about
#---------------------------------------------------------------------------------------
TEST_Flora<-Flora_data%>%distinct(ID_UC2,.keep_all = TRUE)%>%select(-c(scientificName,family))

#There are duplicated UC's names! check them!
TEST_Flora[duplicated(TEST_Flora$UC_NAME2),]%>%print(n=10)


#These duplicated UCs have different ID_UCs? YES!
#	So, there are no duplicated IDs
TEST_Flora[duplicated(TEST_Flora$ID_UC2),]%>%print(n=10)

#Check where duplicated UCs are
Dupli<-TEST_Flora[duplicated(TEST_Flora$UC_NAME2),]

plot(UCs)
UCs[(UCs@data$UC_NAME2 %in% Dupli$UC_NAME2),]%>%plot(.,col="red",lwd=5,border="red",add=T)


#All duplicated UCs profile!
TEST_Flora%>%
distinct(ID_UC2,.keep_all = TRUE)%>%
filter(UC_NAME2 %in% Dupli$UC_NAME2)


#---------------------------------------------------------------------------------------
# 				SAVE DATA
#---------------------------------------------------------------------------------------
#saveRDS(Flora_data,file="UCsFlora.Rds")
#---------------------------------------------------------------------------------------

Flora_data<-readRDS("UCsFlora.rds")

Flora_data

#---------------------------------------------------------------------------------------
#	Acessory function - create binomial scientific name
#---------------------------------------------------------------------------------------
Create_binomial<-function(X){
 X2<-vector()
	for (i in 1:length(X)){
		X2[i]<-paste(
 			str_split(X[i], " ", n = 3, simplify = TRUE)[,1],
	 		str_split(X[i], " ", n = 3, simplify = TRUE)[,2])} 

X2[is.na(X)]<-NA

	return(X2)
}
#---------------------------------------------------------------------------------------
Flora_data$Binomial<-Create_binomial(Flora_data$scientificName)

#Check if Create_binomial function worked right 
# 	ScientificName and Binomial should be both "NA"
Flora_data[is.na(Flora_data$Binomial),]
Flora_data[!is.na(Flora_data$Binomial),]

#Collect the most up-to-date scientific name, check for synonyms and get threat level according to CNCFlora
Sci_names<-flora::get.taxa(unique(Flora_data$Binomial))%>%as_tibble(.)

#Open threat inference function
source("threat inference.R")

Sci_names$threat.status<-infer_threat(Sci_names$threat.status.cnc,method="min")

Flora_data_cleaned<-full_join(Flora_data,Sci_names,
	by=c("Binomial"="original.search"), suffix = c("", ".new"))%>%
ungroup()%>%
mutate(Sci_name_InRecord=Create_binomial(scientificName))%>%
mutate(Sci_name_Accepted=Create_binomial(scientific.name))%>%
mutate(IsSynonym=(Sci_name_Accepted!=Sci_name_InRecord))%>%
select(Sci_name_InRecord,Sci_name_Accepted,IsSynonym,threat.status,family,ID_UC2,UC_NAME2,ESF_ADM2,CAT_MAN2,PLA_MAN2,Area)

#Check synonims
Flora_data_cleaned%>%distinct(Sci_name_InRecord,.keep_all=TRUE)%>%
select(Sci_name_InRecord,Sci_name_Accepted,IsSynonym,threat.status,family)%>%
group_by(Sci_name_Accepted) %>% 
  filter(n()>1)%>%
arrange(Sci_name_Accepted)%>%print(n=10)

#Accepted species
Flora_data_cleaned%>%distinct(Sci_name_Accepted,.keep_all=TRUE)

#Species we failed to find synonym and threat level as well.
Flora_data_cleaned%>%filter(is.na(Sci_name_Accepted))%>%
distinct(Sci_name_InRecord,.keep_all=TRUE)


#---------------------------------------------------------------------------------------
# 				SAVE DATA SYNONIM FREE
#---------------------------------------------------------------------------------------
#saveRDS(Flora_data_cleaned,file="Flora_data_SynonymFree.Rds")
#---------------------------------------------------------------------------------------



