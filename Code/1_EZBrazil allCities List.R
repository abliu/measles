
#MIKE
setwd("~/Dropbox/academic_life/Harvard/Graduate Students/HSPH/2019/Andrew Liu/Brazil/")


source("./Code/to Actually Use/getDzzChoices fxn.R")
source("./Code/to Actually Use/EZBrazil fxn.R")

library("car")  #for vif()


#available arguments (and their default values) for the EZBrazil function.  
#Note: use getDzzChoices() to get the options for mortality (i.e. dzz) types to explore

pathToBrazil<-normalizePath("./../") #pathToBrazil should be the path leading up to, but not including the Brazil folder

gender="both" #male female both
age="1-9"  # must be one of: "1-9", "0-9","1-9","1-4",or "5-9"
useRate=TRUE #will give data as disease per 100,000 measles per 100,000
useCaseCalculations=TRUE #use MV cases calculated from CFR vs. FALSE - provides actual measles mortality case numbers
writeCSV=FALSE #can have it automatically output results as .csv file
fileName=NA #can name the file whatever you want
fileLoc=NA #can specify a location for the csv file
dzzLabel="Disease" #title of .csv file only if saving as a .csv file


#Check out which diseases (mortality data for specific infections) are available to use 
dzzTouse <- getDzzChoices(cityName = "Brazil",noMeasles = FALSE)

cityList<-list()
citiesVec<- c("Brazil", "Rio de Janeiro", "Sau Paulo", "Recife", 
"Manaus", "Petrolina", "Piracicaba", "Fortaleza", "Cariri")


dzzChoices <- 
  getDzzChoices(
    cityName = "Brazil",
    noMeasles = TRUE)

#see what classes of diseases are available to choose. 
#if just noMeasles = TRUE, will give all but measles back as the dzz mortality data.
args(getDzzChoices)


data<-EZBrazil(
  cityName="Brazil", 
  gender=gender,
  age=age,
  dzzToUse=dzzChoices,
  useRate=useRate,
  pathLeadingToBrazil=pathToBrazil,
  giveDzzChoicesOnly=FALSE)



#below, just loop through all the cities and compbine them into a list called
#cityList... and then below the loop, merge the list into a single data.frame.


for(i in 1: length(citiesVec)){

	city=citiesVec[i]
	dzzChoices<-EZBrazil(cityName=city,
		pathLeadingToBrazil=pathToBrazil,
		giveDzzChoicesOnly=TRUE)

	if(dzzChoices[1]=="#N/A") dzzChoices=dzzChoices[-1]
		
		data<-EZBrazil(
			cityName=city, 
			gender=gender,
			age=age,
			dzzToUse=dzzChoices,
			useRate=useRate,
			pathLeadingToBrazil=pathToBrazil,
			giveDzzChoicesOnly=FALSE)

		data$city<-city
	cityList[[i]]<-data	
}

#merge list into a dataframe.
allCityData<-data.table::rbindlist(cityList)




#mixed effects model with city as random effect.
library("lmerTest")

a<-lmer(dzz~meV + year + (1|city), data=allCityData)
summary(a)

b<-lm(dzz~meV+year, data=allCityData)
summary(b)



cityListSC<-cityList
cityListSC[[1]][,c(2:3)]<-scale(cityList[[1]][,c(2:3)])		
cityListSC[[2]][,c(2:3)]<-scale(cityList[[2]][,c(2:3)])		
cityListSC[[3]][,c(2:3)]<-scale(cityList[[3]][,c(2:3)])		

allCityDataSC<-
  data.frame(rbind(cityListSC[[1]],cityListSC[[2]],cityListSC[[3]]))
plot(allCityDataSC[,2]~allCityDataSC[,3])

c<-lm(dzz~meV+year, data=allCityDataSC)
summary(c)





d<-lm(dzz~meV+year, data=data)
summary(d)
vif(d)


