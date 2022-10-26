infer_threat<-function(X,method=method,boot=100,collapse=FALSE){
	method=substitute(method)
	New<-replicate(length(X),"NA")
	categories<-as.factor(seq(length(c("EX","CR","EN","VU","NT","LC","DD"))))
	levels(categories)<-c("EX","CR","EN","VU","NT","LC","DD")

#Take minimum threat status

if(method=="min"){
for(i in 1:length(X)){
New[[i]]=as.character(
	categories[min(na.rm=T,
		match(str_split(X[i],"\\|")[[1]],
							categories))])
	}
}

#Take maximum threat status

if(method=="max"){
for(i in 1:length(X)){
New[[i]]=as.character(
	categories[max(na.rm=T,
		match(str_split(X[i],"\\|")[[1]],
							categories))])
	}
}

#Random threat status

if(method=="sort"){
New<-data.frame(matrix("NA",nrow=length(X),ncol=boot))
for(i in 1:length(X)){
New[i,]<-sample(
categories[
match(str_split(X[i],"\\|")[[1]],categories)
],boot, replace = TRUE)
	}
}

if (collapse==TRUE){
	New<-New[,1]}

return(New)
}


#USAGES
# Data Example
# Threat status according to Flora Package "https://github.com/gustavobio/flora"

#Download raw data of threat status according to CNC Flora
#urlfile="https://raw.githubusercontent.com/gustavobio/flora/07a96f4b38222f7bbf127811be1877d9a77b5064/data-raw/status_cnc.csv"
#data.test<-read_csv(url(urlfile))

#Select MAXIMUM threat status among synonim 
#infer_threat(data.test$threat.status.cnc,method="max")

#Select MINIMUM threat status among synonim 
#infer_threat(data.test$threat.status.cnc,method="min")

#Select RANDOM threat status among synonim 
#infer_threat(data.test$threat.status.cnc,method="sort",boot=5,collapse=TRUE)

#Select RANDOM threat status among synonim - return all samples
#infer_threat(data.test$threat.status.cnc,method="sort",boot=10)

print(c(
	"You have loaded the 'infer_threat' function.",
		"Please use as 'infer_threat(c('CR','LC'),method='min')'",
			"Other methods are also available, please check usage inside the function using 'file.edit('infer_threat.R')'"))




