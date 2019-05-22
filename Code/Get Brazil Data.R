####**********************************************************************************
####**********************************************************************************
#To help you figure out what's going on
#When all is said and done, this script gathers up all the data you want from Brazil
#based on your criteraia that you input below and makes a csv at the very very end.

#This script will allow you to get all of the required data from brazil by running
#a few functions that I have written previously and are stored at:
#"~/Dropbox/Cricket/Brazil/Code"
#the files (including this one) that we will actually run are in the folder
#"to Actually Use" which is simply to state that these are the ones that you should
#mess around with because, messing around the function scripts might prevent them from 
#working should something be mis-coded.
#nevertheless, should that happen, you can always rescue previously saved files using 
#dropbox.

#OK, onto the show.






####**********************************************************************************
####**********************************************************************************
#IF YOU WANT TO START WITH A CLEAN SLATE - REMOVE ALL OF THE R OBJECTS THAT MAY BE
#LINGERING FROM AN EARLIER SESSION.  YOU CAN SEE WHAT IS STORED IN MEMORY BY 
#TYPEING IN ls()
#to remove everything in ls()...run the next line of code.
####**********************************************************************************
	rm(list=ls())


####**********************************************************************************
#Set the workding directory.  If you want to switch between mine and yours, just 
#comment, or uncomment the appropriate working directory.  By doing this
#we can then keep all the other folders the same and just change this.
####**********************************************************************************
	setwd("~/Dropbox/Cricket/Brazil")
	#setwd("~/Dropbox/academic life/Post doc/princeton/undergrads/Cricket/Brazil")


####**********************************************************************************
####**********************************************************************************
#load the libraries that will be required for this script
####**********************************************************************************
	library(dplyr)
	library(reshape2)
	library(plotrix)




####**********************************************************************************
####**********************************************************************************
#The following are the choices you have to partition out the brazil data.
#I'll go through each in turn
####**********************************************************************************

####**********************************************************************************
#whether you want to get a rate (per 100,000) or just simply the raw number of cases
#of your chosen disease.  Probably we'll always want to keep this as true, but 
#there might be instances that come up that we will want to not use a rate
####**********************************************************************************
	useRate=TRUE

####**********************************************************************************
#Wether you want to get the number (or rate) of measles mortality or the 
#measles cases.  If true, this will give you the incidence of measles cases
#as determined by dividing the mortality (which is the original data) by the case
#fatality rate for each year - which I collected from other literature on pubmed.
####**********************************************************************************
	useCaseCalculations=TRUE

####**********************************************************************************
#which sex to analyze.  "both", "female", or "male".  I put three rows there 
#so instead of typing it out if you want to change it, you just change which row does
#not have the pound sign in front of it.
	#note, when writing out texts in R, you need to surround it with quotes - this is 
	#true for categorical factors as well as, for example, column names or rownames.
	#the only time this isn't true, as far as I know, is if you're using one of 
	#the packages "reshape2" or "dplyr" - which we do use here... and you'll see 
	#where we leave the quotes out. Or if you attach a dataset using the attach() 
	#function but I would urge not to do that and to write out the myDataSet$columnName  
	#rather than doing attache(myDataSet).....   columnName blah blah blah.  This can 
	#be a bit dangerous as you might forget to detach that dataset later and then 
	#wonder why nothing is working as you explect
####**********************************************************************************
	sex1<-sex<-"both" #female, male
	#sex1<-sex<-"female"
	#sex1<-sex<-"male"


####**********************************************************************************
#Which city or location in Brazil... either the whole country, or one of the two 
#major metropolitan cities in Brazil.
#note, the data set has others if you want to go and get more data.
####**********************************************************************************
	cityName<-"Brazil"
	#cityName<-"Rio de Janeiro"
	#cityName<-"Sau Paulo"


####**********************************************************************************
#Which age groups to use. Uncomment first row for 0-9.
#uncomment second row for 1-9, etc....
	#note, I was able to put two distinct commands for R on the same line
	#ie: I defined "ageToUse" as a variable and "ageForLabel" as a variable
	#I was able to do this by putting a semicolon between then.  Which R treats like 
	#a new line
####**********************************************************************************
	#ageToUse<-"age0_1"; ageForLabel="0-1"
  ageToUse<- c("age0_1","age1_4","age5_9")   ; ageForLabel="0 - 9"
	#ageToUse<- c("age1_4","age5_9") 			; ageForLabel="1 - 9"
	#ageToUse<- c("age1_4") 					; ageForLabel="1 - 4"
	#ageToUse<- c("age5_9")			 			; ageForLabel="5 - 4"





####**********************************************************************************
####**********************************************************************************
####**********************************************************************************
#Choose Which diseases you would like to analyze
####**********************************************************************************
	#to See disease choices, run this line and then type in dzzChoices
	dzzChoices <- levels(data.frame(read.csv(paste("./Brazil/cities/",cityName,
										"/all data.csv",sep=""),header=TRUE))[,"dzz"])
	dzzChoices  #this line just allows you to see what disease choices you have 

	
	####*******************************************************************************
	#These are just some cateogries that I made from the choices available
	####*******************************************************************************
		bacterialDzz  <- c("TB","URT","bacterial mening","bronchopneumonia","mening unsp",
							"pneumococcal pneumonia","pneumonia other","pneumonia unsp", 
							"septicemia") 
		pneumoniaSep  <- c("bronchopneumonia","pneumococcal pneumonia","pneumonia other",
							"pneumonia unsp","septicemia")  
		LRT			  <- c("bronchopneumonia","pneumococcal pneumonia","pneumonia other",
							"pneumonia unsp")
		intestinal	  <- c("helminths","intestinal") 
		meningitis	  <- c("bacterial mening","mening unsp")  
		####**************************************************************************




####**********************************************************************************
####**********************************************************************************
#Now is where we are going to actually define the disease that we want to use.	
	#because of how I designed this, you need to store it as the variable called dzzToUse
	#you can define these either by using the predefined objects above that we made 
	#simply for convenience... for example
		#dzzToUse <- bacterialDzz  ; dzzLab="bacterial disease"
		#dzzToUse <- LRT           ; dzzLab="lower respiratory tract disease"
		
##or by stringing together any of the choices from dzzChoices into a vector using c()
	#for example:
		#dzzToUse <- c("TB","helminths","intestinal") ; dzzLab="TB & intestinal parasites"

#or you can use 'all except' from the dzzChoices by just putting 
#      dzzToUse<-dzzChoices[-(index of dzzChoices, I, want, to, exclude)]
	#this is what I do below... which  why I am going to make a vector (with one entry)
	#that tells me the index number (ie: location in the vector) of the measles cases
	#because I want to remove them from the analysis and I want to use everything else
	#so I'm going to go with 'all except measles' dzzChoices as follows:
####**********************************************************************************
####**********************************************************************************		
	mevColIndex<-grep("measles",dzzChoices)	#this gives me the index of measles
	
	#and now I'm going make dzzToUse equal to all the dzzChoices except measles
	#dzzToUse <-dzzChoices[-mevColIndex];  dzzLab="non-measles infections"
	dzzToUse<-"measles"
####**********************************************************************************
#Just some other options useing the prescribed categories we made above
####**********************************************************************************
	#dzzToUse<-bacterialDzz        ;  dzzLab="bacterial disease"
	#dzzToUse=pneumoniaSep         ;  dzzLab = "invasive bacterial disease"
	#dzzToUse <-intestinal         ;  dzzLabe = "intestinal disease"
####**********************************************************************************

####**********************************************************************************
#also note that I also defined a label called 
	#dzzLab that can be used for plotting if you want to, for instance, remind yourself
	#of what diseases are being aggregated for that particular plot
####**********************************************************************************


	

####**********************************************************************************
#Read in the population data that will be used to get the denominators to convert 
#the disease mortality into rates
#note the use of the period just before the forward slash in the read.csv
#this simply means.... 'append working directory here'
#if you put two periods, it would go back one folder from the current working
#director, three periods would go back two folders, etc...
####**********************************************************************************
	populations<-data.frame(read.csv("./Brazil/cities/Brazil populations.csv",header=TRUE))
	
	####******************************************************************************
	#Give new column names - these are necessary to be exactly as I'm making them here
	#in order for the functions to read them below
	####******************************************************************************
		colnames(populations)<-c("city","sex","year","age0_4","age0_1","age1_4","age5_9","age10_14")
	


	
	####******************************************************************************
	####******************************************************************************
	#Now we're going to use a technique called 'piping' (ie: the %>% operator) from 
	#the dplyr package.  This package (along with reshape2) are all you need for 
	#data cleaning and reshaping data for use in analysis!!  They're so great together!
	
	#OK...
	#Make a new dataframe called 'pop' by taking the 'populations' datafrmae that we 
	#just made (note that I wrapped the called to read.csv in a data.frame() because 
	#the following procedure using the %>% symbols requires data.frame() instead of 
	#matrix and it can be finicky somtimes... I could also explicity wrap it in 
	#data.frame here as well (ie:  populations<-data.frame(populations))
	#OK now we are going to make pop out of populations by taking the 'populations'
	#data frame and filtering by the cityname that we want (note the double == signs for
	#a conditional argument) and by the sex that we chose above.
	#then we're going to take that filtered data frame and 'pipe' it over (using the %>%)
	#and use select() to choose which age columns I want.  Here I'm saying...
	#"select the columns with the column names in population that are also in the vector 
	# c("year, ageToUse)"
	#try it out by just looking first at colnames(populations) and c("year",agesToUse)
	#by using the 'which()' and making it a conditional statement, you're only going to 
	#get those column names within the populations data.frame that are also in the 
	#c("year",agesToUse) vector
	####******************************************************************************	
		pop<-populations %>% 
			filter(city==cityName & sex==sex1) %>%
			select(which(colnames(populations) %in% c("year",ageToUse)))
		

	####******************************************************************************	
	####******************************************************************************	
	#Make a new dataframe called popDat (ie: population data) that will have two 
	#columns (year and popTot).
	
	#year is going to equal the first column from pop that we just made 
	#(it should be year) by doing: cbind(year=pop[,1]....) 
	#and then we're also going to make a column popTot where, each element row of this 
	#column is going to equal the sum of all the columns (except this first... which is year
	#we wouldn't want to add 1980 or 1985 to the number of fatalities!) for the respective row
	#by using the built in function rowSums 
	####******************************************************************************	
		popDat<-cbind(year=pop[,1], popTot=rowSums(pop[,-1]))




			



	#************************************************************************************
	#************************************************************************************
	#OK, now we're going to bring a lot of this stuff together.  now that we know which
	#diseases we want to look at (recall we defined dzzToUse up above) and we know which 
	#city, sex and ages (ie: via ageToUse) we can go ahead and get all the disease mortality
	#we are interested in.			
	#************************************************************************************
		
		#Read in the data for the disease mortality... (this is the same thing we read in before
		#just to get the choices in dzzChoices... but there we only stored the column names)

		city2 <-data.frame(read.csv(paste(getwd(),"/Brazil/cities/",cityName,"/all data.csv",sep=""),header=TRUE))
		
		#filter the data for the correct gender and the correct year.  the year isn't essential since it's the
		#full range of the possible years, but still it's good to define for housekeeping purposes
		
		city1 <- city2 %>% filter(gender==sex & year %in% c(1979:1995)) 
		
		#this next thing is just to take care of these random empty first columns that sometimes show up
		#with the column of simply "X"... it was a quick fix to a stupid problem.  
		if(colnames(city1)[1]=="X") city1 <- city1[,-1]
		
		#Just store as city, instead of city1 for no good reason I suppose....
		city <-city1
		
		yearsInData<-as.numeric(levels(as.factor(city1$year)))
		
		#rename all the columns by first creating a vector of all the names 
		fullColnamesToUse <-c("city","dzz","sex","year","age0_1","age1_4","age5_9","age10_14",
			"age15_19","age20_29","age30_39","age40_49","age50_59","60age_69","age70_79","age80_","noAge","total")
		
		
		####******************************************************************************	
		#and then renaming it as below... but since the city data set only gues up to age 
		#15_19... only use the fullColnamesToUse indexes from 1 to the number of columns 
		#in city... 
		####******************************************************************************	
	colnames(city) <-fullColnamesToUse[1:ncol(city)]
			#lets look at what we have...
			head(city)  #and compare to 

			skeleton<-city[which(city$dzz==dzzToUse[1] & city$sex==sex),ageToUse[1]]

			#Make a matrix that will hold the numbers of disease for each age group of interest
			#should have the same number of rows as the measles data 'measlesDat' does above.
			mevByAgeSums<-dzzByAgeSums<-matrix(ncol=length(ageToUse),nrow=length(skeleton))

			#give it column names = to the ages of interest stored in 'ageToUse'
			colnames(dzzByAgeSums)<-colnames(mevByAgeSums)<-ageToUse
			
			#for each column do....
			#well, you can work through this one :-)
			for(j in 1:length(ageToUse)){
			
				dzzDat<-matrix(ncol=length(dzzToUse),nrow=length(skeleton))
				mevDat<-matrix(ncol=length(c("measles")),nrow=length(skeleton))
				colnames(dzzDat)<-dzzToUse
				colnames(mevDat)<-"measles"
				for(i in 1:ncol(dzzDat)){
					dzzDat[,i]<-city[which(city$dzz==dzzToUse[i] & city$sex==sex),ageToUse[j]]
				}
				mevDat<-city[which(city$dzz=="measles" & city$sex==sex),ageToUse[j]]
				
				dzzDatTemp<-data.frame(dzzDat) %>% mutate(zeros=0)
				dzzByAgeSums[,j]<-rowSums(dzzDatTemp)
				mevByAgeSums[,j]<-mevDat
			}				
	



	####******************************************************************************	
	####******************************************************************************	
	####******************************************************************************	
	#put the data sets together by 'binding' columns together using cbind()
	#and call this both dataRate, and data
	####******************************************************************************	
	#Adding a column of zeros here because 'rowSums' doesn't like it if there happens
	#to be only a single column....
	dzzByAgeSums<-data.frame(dzzByAgeSums) %>% mutate(zeros=0)
	mevByAgeSums<-data.frame(mevByAgeSums) %>% mutate(zeros=0)
	data<-dataRate<-data.frame(cbind(
					year=yearsInData,
					totMev=rowSums(mevByAgeSums),
					dzz=rowSums(dzzByAgeSums)))

	#If we want to get a count of measles incidence rather than measles deaths we divide by the 
			#case fatality rate (CFR) divided by 100... because the CFR's are given as percent and we want as 
			#ratio... ie: CFR of 2%... we want to divide not by 2 but by .02.
			if(useCaseCalculations==TRUE){
				measlesCFR<-data.frame(read.csv(paste(getwd(),"/Brazil/cities/measles case fatality rates.csv",sep="")))
				data[,"totMev"]<-dataRate[,"totMev"]  /  (measlesCFR[,"cfr"]/100)
				
			}

		dataRate<-data
	####******************************************************************************	
	####******************************************************************************	
	#Make sure the popData (ie: population data) is the same year range as the disease 
	#data in data or dataRate... and filter OUT anything that isn't.. while keeping the
	#rows of popDat$year that are in the first column for dataRate or dataRate$year
	####******************************************************************************	
	popData <-as.data.frame(popDat) %>% filter(year %in% dataRate[,"year"])


	####******************************************************************************	
	#Convert all these normal values to rates, per 100,000 people.
	#firstLets just see what we have to start... 
	####******************************************************************************	
	head(dataRate)

	if(useRate==TRUE){
		####******************************************************************************
		#makre sure the years are all aligned with each other by getting rid 
		#of any ears that are in popData$year but are not in dataRate$year
		dataRate1<-dataRate %>% filter(year %in% popData[,"year"])
		
		#convert to per 100,000 persons
		dataRate1$totMev<-100000*dataRate1$totMev/popData[,"popTot"]
		dataRate1$dzz<-100000*dataRate1$dzz/popData[,"popTot"]
		data<-dataRate1
	}
	
	#get rid of any negative total diseases that may have come up - this shouldnt happen
	data <- data %>% filter(totMev >0)
	
	#select only the year, the dotal (ie: total measles) and the dzz total
	dat<-data %>% select(year,totMev,dzz)

	#rename the columns
	colnames(dat) <-c("year","meV","dzz")

	#now we have a usable dataset called 'dat'
	dat

plot(dzz~meV, data=dat)
  abline(lm(dzz~meV, data=dat))
x<-lm(dzz~meV, data=dat)
  summary(x)

 #export it as a .csv to a folder called "Cleaned Data" under Brazil in the dropbox
	#you might want to keep the actual 'write.csv...' line commented out so that you 
	#only make new csv files when it's very deliberate - ie: you go and remove the comment
	#and then run the line.... as I have below.
	fileName<-paste(cityName,sex,"sex ages",ageForLabel,dzzLab)
	#write.csv(dat,file=paste("./Cleaned Data/",fileName,sep=""))
