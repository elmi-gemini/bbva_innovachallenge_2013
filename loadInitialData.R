path1 = "geografia_pcodes.csv";
path2 = "poblacion_zipcode.csv";
path3 = "age_catal.csv";
path4 = "gender_catal.csv";

g= c(rep("character", 2));
catalogue = read.table(path1, colClasses=g, sep=",", dec=".", header=TRUE);

o = c(rep("character", 8));
poblacion = read.csv(path2, colClasses=o, sep=",", header=TRUE);
poblacion_all <- poblacion[, c("Zipcode", "NOMBRE", "POB12")];

t <- c(rep("character",4));
age_catal <- read.table(path3, sep=";", dec=".", header=TRUE, colClasses=t)
gender_catal <- read.table(path4, sep=";", dec=".", header=TRUE, colClasses=t)


path5 = "ZipCodes.CSV";
path6 = "CardsCube.CSV";
path7 = "hourly1.csv";
path8 = "Daily.CSV";

v <- c(rep("character", 7))
data <- read.table(path5, sep=",", dec=".", header=TRUE,colClasses=v)
colnames(data) <- c("postal_code", "commerce_type", "week", "zip_label", "number_of_cards", "number_of_payments", "income")

v <- c(rep("character", 7))
cardsdata <- read.table(path6,sep=",", dec=".", header=TRUE, colClasses=v)

P <- c(rep("character", 12))
hourlydata <- read.table(path7, sep=",", header=TRUE, dec=".", colClasses=P)


y <- c(rep("character", 11))
dailydata <- read.table(path8, sep=",", header=TRUE, dec=".", colClasses=y)


# "fetch 75% percentile" function:      
percentil75 <- function(Data)  {
  return(quantile(Data, .75))      
}

# "fetch 90% percentile" function:      
percentil90 <- function(Data)  {
 return(quantile(Data, .9))      
}

# "fetch 95% percentile" function:      
percentil95 <- function(Data)  {
 return(quantile(Data, .95))      
}  

card_pcode <- as.factor(cardsdata$postal_code)
card_comtype <- as.factor(cardsdata$commerce_type)
card_week <- as.factor(cardsdata$week) 
card_segment <- as.factor(cardsdata$client_segment) 

card_numpayms <- as.numeric(cardsdata$num_payms)
card_avgpaym <- as.numeric(cardsdata$avg_paym_eur)
card_numcards <- as.numeric(cardsdata$num_cards)

##################################   Hotspot calculations for every combination pcode_commerce:  ##################################

avravr_numpaym <- aggregate(card_numpayms, by=list(pcode = card_pcode, comtype = card_comtype, segment = card_segment), 
						   FUN=mean, na.rm=TRUE)  # avr income for every client segment at every postal_code_comtype over all weeks.

pcode_n <- avravr_numpaym$pcode
comtype_n <- avravr_numpaym$comtype
segment_n <- avravr_numpaym$segment
avrnum <- avravr_numpaym$x

perc90data_npaym <- aggregate(avrnum, by=list(segment=segment_n), 
							 FUN=percentil90)  # 90 percentile   for combination of comtype and pcode

perc90data_npaym  <- data.frame(perc90data_npaym, row.names=NULL)
							 
colnames(perc90data_npaym) = c("segment", "percentil")

merge = merge(avravr_numpaym, perc90data_npaym, by="segment", all.x=TRUE)							 
hotspot_preplist = merge[merge$x >= merge$percentil, ]	

hotspot_preplist <- data.frame(hotspot_preplist, row.names=NULL)

####################################    PeakHour calculations for every combination pcode_commerce   #####################################

daily_numpaym <- as.numeric(dailydata$wd_num_paym) 
d_pcode <- dailydata$postal.code
d_comtype <- dailydata$commerce_type
d_day <- dailydata$weekday

avravr_daily <- aggregate(daily_numpaym, by=list(pcode=d_pcode,comtype=d_comtype, day=d_day), FUN=mean, na.rm=TRUE)

avravr_daily <- data.frame(avravr_daily, row.names=NULL)

tex <- avravr_daily$x
pcode_tex <- avravr_daily$pcode
comtype_tex <- avravr_daily$comtype
day_tex <- avravr_daily$day

maxavr_daily <- aggregate(tex, by=list(pcode=pcode_tex, comtype=comtype_tex), FUN=max, na.rm=TRUE)

maxavr_daily <- data.frame(maxavr_daily, row.names=NULL)

maxavr_daily$lookup <- paste(sep="", maxavr_daily$pcode, maxavr_daily$comtype, maxavr_daily$x) 
avravr_daily$lookup <- paste(sep="", avravr_daily$pcode, avravr_daily$comtype, avravr_daily$x) 
maxday <- merge(maxavr_daily, avravr_daily, by = "lookup", all=FALSE)

maxday = maxday[, c("pcode.x", "comtype.x", "x.x", "day")] # max day for every combination

maxday <- data.frame(maxday, row.names=NULL)

# now hours! 
hourly_numpaym <- as.numeric(hourlydata$hourly_num_paym)

merge_hourly <- merge(hourlydata, maxday, by.x=c("postal_code", "commerce_type", "weekday"),
					 by.y=c("pcode.x","comtype.x","day"), all=FALSE)
				
merge_hourly <- data.frame(merge_hourly, row.names=NULL)
					 
npaym <- as.numeric(merge_hourly$hourly_num_paym)
pcoder = merge_hourly$postal_code
comtyper = merge_hourly$commerce_type

max_npaym <- aggregate(npaym, by=list(pcoder, comtyper), FUN=max, na.rm=TRUE)
colnames(max_npaym) = c("postal_code", "commerce_type", "max_npaym")

max_npaym <- data.frame(max_npaym, row.names=NULL)

maxhour = merge(merge_hourly, max_npaym, by.x=c("postal_code", "commerce_type", "hourly_num_paym"),
			   by.y=c("postal_code", "commerce_type", "max_npaym"), all=FALSE) 
maxhour = maxhour[, 1:6]

maxhour  <- data.frame(maxhour, row.names=NULL)

###################################################################
     
	 
#SACADO DE BUZONEO
   pcode <- as.factor(data$postal_code)
   comtype <- as.factor(data$commerce_type)
   target <- as.factor(data$zip_label)
   
   week <- as.factor(data$week)
   
   incomes <- as.numeric(data$income)
   numpayms <- as.numeric(data$number_of_payments)
   numcards <- as.numeric(data$number_of_cards)
   
   income_means <- aggregate(incomes, by=list(pcode=pcode,comtype=comtype, target=target), 
                             FUN=mean, na.rm=TRUE)
   
   income_means <- data.frame(income_means, row.names=NULL)
   
   income_mean <- income_means$x    
   x <- income_means$x
   comtype_mean <-  income_means$comtype
   pcode_mean <- income_means$pcode
   target_mean <- income_means$target
   
   sorted_inc_mean <- income_means[order(comtype_mean, pcode_mean, -x),]  
   pcode_sort <- sorted_inc_mean$pcode
   comtype_sort <- sorted_inc_mean$comtype
   target_sort <- sorted_inc_mean$target
   income_sort <- sorted_inc_mean$x
   
   # 95 percentile   for combination of comtype and pcode:      
   perc95 <- aggregate(x, by=list(comtype=comtype_mean, pcode=pcode_mean), FUN=percentil90)
   #FIN SACADO DE BUZONEO	 
	 
	 #SACADO DE TOPSEGMENT
	 
	   
  cardsdata$card_avgincome <- card_numpayms*card_avgpaym
  card_avginc <- cardsdata$card_avgincome
  
  avravr_income <- aggregate(card_avginc, by=list(pcode=card_pcode,comtype=card_comtype, segment=card_segment), 
                             FUN=mean, na.rm=TRUE)  # avr income for every segment at every postal_code_comtype over all weeks.
  
  pcode_ <- avravr_income$pcode
  comtype_ <- avravr_income$comtype
  segment_ <- avravr_income$segment
  avrinc <- avravr_income$x
  
  perc95data <- aggregate(avrinc, by=list(pcode=pcode_, comtype=comtype_), 
                          FUN=percentil95)  # 95 percentile   for combination of comtype and pcode
  
  
	 #FIN SACADO DE TOPSEGMENT


#NUEVO CODE PARA EVITAR EL MERGE

data = merge(data, poblacion_all, by.x="zip_label", by.y="Zipcode", all=FALSE)
data = merge(data, catalogue, by.x="zip_label", by.y="postal_code", all=FALSE)	 
	 
Buzoneo <- function(pcode_f, comtype_f)
 {  
   
   # NEXT STEP: to fetch the targets and x-s from table "sorted_inc_mean" which exceed the 95 percentile     (table perc95):
   
   target_x =  perc95[which(perc95$pcode %in% pcode_f & perc95$comtype %in% comtype_f), ] 
   
   result <- sorted_inc_mean[which(sorted_inc_mean$pcode %in% pcode_f & sorted_inc_mean$comtype %in% comtype_f), ] 
   
   result = result[result$x > target_x$x, ]  
   
   billboard = matrix(as.character(result$target), nrow=length(result$target))
   colnames(billboard) = c("top_zip_code")
   
   
   
   top_zip_code <- as.vector(billboard)  
   billboard_prueba = data[which(data$zip_label %in% top_zip_code & data$postal_code %in% pcode_f & data$commerce_type %in% comtype_f), ]
   billboard_prueba = billboard_prueba[, c("zip_label", "postal_code", "commerce_type", "NOMBRE", "POB12", "locality")]
   billboard_prueba = unique(billboard_prueba)
   billboard = billboard_prueba[, c("zip_label", "NOMBRE", "locality", "POB12")]
 
	 # to delete!!
	#   billboard = merge(billboard, catalogue, by.x="top_zip_code", by.y="postal_code", all.x=TRUE)
	#   billboard = unique(billboard)  
	#   billboard = merge(billboard, poblacion_all, by.x="top_zip_code", by.y="Zipcode", all.x=TRUE)
	#   billboard = unique(billboard)    
	  
   colnames(billboard) = c("postal code", "province", "municipality", "residents in the area")  
   billboard <- data.frame(billboard, row.names=NULL)

   cps = billboard[,"postal.code"]
   cps[sapply(cps, is.null)] <- NULL
   
   cpslist <- paste(cps, collapse=', ' )
   cpslist[sapply(cpslist, is.null)] <- NULL
   
   #Mostrar CP
   message1 = paste(sep="", "Your best potential clients live in these areas: ",  as.character(cpslist), '. Place your ads on their mailboxes ;)')

   result_buzoneo_list <- as.character(message1)
 }
 
 TOPsegmentHotSpot <- function(pcode_f, comtype_f)
{  
  # to find out the best clients, who bring 95+ percentile of the average income per client -->

  target_inc =  perc95data[which(perc95data$pcode %in% pcode_f & perc95data$comtype %in% comtype_f), ] 
  
  TOP_SEGMENT <- avravr_income[which(avrinc >= target_inc$x & pcode_ %in% pcode_f & comtype_ %in% comtype_f), ] 
  
  genero_c = substr(TOP_SEGMENT$segment, 1, 1)
  edad_c = substr(TOP_SEGMENT$segment, 3, 3)
  
  genero = gender_catal[which(gender_catal$lable %in% genero_c,) ]$description
  edad = age_catal[which(age_catal$lable %in% edad_c), ]$description
  
  if(length(edad) == 0)
	edad = "36-45"
	
  if(length(genero) == 0)
  {
	genero = "Female";
  }
  if(length(genero) == 1)
  {
	if (length(genero) == 1 & genero == 'Unknown')
		genero = "Female"
  }
  message2 = paste(sep="", "Your best client segment is : ",genero," with age of ", edad, "!")
  
  recoed_top_segment = paste(sep="", genero_c,"#",edad_c)	
  
  hotspot_for_recoed <- hotspot_preplist[which(hotspot_preplist$segment %in% recoed_top_segment), ]
  hotspot_for_recoed$hotspot <- paste(sep="", hotspot_for_recoed$comtype, "_", hotspot_for_recoed$pcode)
  myspot <- paste(sep="", comtype_f,"_",pcode_f)	
  
  reco_for_top_recoed <- hotspot_for_recoed[hotspot_for_recoed$hotspot != myspot, ]
  reco_data <- reco_for_top_recoed[, c("pcode", "comtype")]
  
  postal <- reco_for_top_recoed$pcode
  commerce <- reco_for_top_recoed$comtype
  
#  max_day <- avravr_daily[pcode_tex == postal & comtype_tex == commerce & avravr_daily$x == maxavr_daily$x, ]
  max_day <- avravr_daily[which(pcode_tex %in% postal & comtype_tex %in% commerce & avravr_daily$x %in% maxavr_daily$x), ]
  
  day = as.character(max_day$day)
  
  max_hour = maxhour[which(maxhour$postal_code %in% postal & maxhour$commerce_type %in% commerce), ]
  
  horaMax <- max_hour$dayhour
  if(length(horaMax)==0)
  {
	horaMax = 18
  }
  HOUR = paste(sep="", horaMax, ":00-", as.numeric(horaMax) + 1, ":00")
  DAY = max_hour$weekday
  
  cpTarget = as.character(postal[[1]])
  commerceTarget = as.character(commerce[[1]])

  if(length(DAY) == 0)
  {
	DAY = 'Saturday'
  }
	
  if(length(DAY) > 3)
  {
	DAY = DAY[1:3]
  }	
  
   if(length(HOUR) > 3)
  {
	HOUR = HOUR[1:3]
  }	
  
  message3 = paste(sep="", "Try to reach them in the areas with postal code ", cpTarget ," at the exit of the commerces: ",commerceTarget ," on ", DAY,"s at this hour ", HOUR,"!")
  
  
   message3 <- unique(message3)
   first <- message3
   if(length(message3) > 3)
		first <- message3[1:3]
  
  result_top_hotspot <- paste(sep="", message2, ". ", first)
   
  result_buzoneo_list <- Buzoneo(pcode_f, comtype_f) 
  
  result = paste(sep="", result_top_hotspot, "XXX", result_buzoneo_list)
  return(result)
}
 
Chosen_Segment_HotSpot <- function(pcode_f, comtype_f, chosen_sex, chosen_age)
{  
  chosen_sex = chosen_sex
  chosen_age = chosen_age
  users_comtype = comtype_f 
  users_pcode = pcode_f  
  users_spot = paste(sep="", users_comtype,"_",users_pcode)
  
  chosen_age_code = age_catal$lable[which(age_catal$description %in% chosen_age)]
  chosen_segment = paste(sep="", substr(chosen_sex,1,1),"#",chosen_age_code)
  
  hotspot_for_chosen <- hotspot_preplist[which(hotspot_preplist$segment %in% chosen_segment), ]
  hotspot_for_chosen$hotspot <- paste(sep="", hotspot_for_chosen$comtype, "_", hotspot_for_chosen$pcode)
  
  reco_for_chosen <- hotspot_for_chosen[hotspot_for_chosen$hotspot != users_spot, ]
  
  hs_comtype <- reco_for_chosen$comtype
  hs_comtype[1]
  hs_pcode <- reco_for_chosen$pcode
  hs_pcode[1]
  
  #max_day <- avravr_daily[pcode_tex == hs_pcode[1] & comtype_tex == hs_comtype[1] & avravr_daily$x == maxavr_daily$x, ]
  
  max_day <- avravr_daily[which(pcode_tex %in% hs_pcode & comtype_tex %in% hs_comtype & avravr_daily$x %in% maxavr_daily$x), ]
  
  day = as.character(max_day$day)
  
  max_hour1 = maxhour[which(maxhour$postal_code %in% hs_pcode[1] & maxhour$commerce_type %in% hs_comtype[1]), ]
  max_hour2 = maxhour[which(maxhour$postal_code %in% hs_pcode[2] & maxhour$commerce_type %in% hs_comtype[2]), ]
  
  HOUR1 = paste(sep="", max_hour1$dayhour, ":00-", as.numeric(max_hour1$dayhour) + 1, ":00")
  DAY1 = max_hour1$weekday
  
  
  if(length(DAY1) == 0)
  {
	DAY1 = 'Friday'
  }
	
  if(length(DAY1) > 3)
  {
	DAY1 = DAY1[1:3]
  }	
  
   if(length(HOUR1) > 3)
  {
	HOUR1 = HOUR1[1:3]
  }	
  
  # HOUR2 = paste(sep="", max_hour2$dayhour, ":00-", as.numeric(max_hour2$dayhour) + 1, ":00")
  # DAY2 = max_hour2$weekday
  
  choice = paste(sep="", "You chose ", chosen_sex, " in the age of ", chosen_age)
  
  txths_comtype = as.character(hs_comtype[1])
  txt2hs_pcode = as.character(hs_pcode[1])
  
  g <- paste(sep="", "Try to place your commercial near the businesses: ", txths_comtype," in the area with postal code: ",txt2hs_pcode, " on ", DAY1," in between " ,HOUR1 ," hours!")

  result_chosen_list <- paste(sep="", choice, ". ",g)
  
  result_buzoneo_list <- Buzoneo(pcode_f, comtype_f)
  
  result = paste(sep="", result_chosen_list,"XXX",result_buzoneo_list)
  return(result)
  
}
 
GLOBALFunctionElmira <- function(comtype_f, pcode_f, input3, chosen_sex, chosen_age)
 {  

   if (input3 == "No")
   {
		return(cat(TOPsegmentHotSpot(pcode_f, comtype_f)))
   }
   else
   {
		return(cat(Chosen_Segment_HotSpot(pcode_f, comtype_f, chosen_sex, chosen_age)))  
   }
 }
 
#rm(list=ls());

#source("D:\\datathon\\eclipse\\ws\\AdRecommenderGood\\RBueno\\loadInitialData.R");

#save(list = ls(), file = "../war/all.RData", version= 2);


# INPUT PARAMETERS:

#GLOBALFunctionElmira(comtype_f,pcode_f, "No", chosen_sex,chosen_age)

#pcode_f = "28003"
#comtype_f = "Books and press"
#comtype_f = "Technology" 
#input3 = "Yes"
#chosen_sex = "Female"
#chosen_age = "56-65"
# 
#
#resultcheck <- GLOBALFunctionElmira(comtype_f, pcode_f, input3, chosen_sex, chosen_age)
#print(resultcheck)
#
#
#buzoneores <- Buzoneo(pcode_f, comtype_f)    # OK!
#print(buzoneores)
#
#topsegmentres <- TOPsegmentHotSpot(pcode_f, comtype_f)  # OK!!
#print(topsegmentres)
#
#chisensegmentres <- Chosen_Segment_HotSpot(pcode_f, comtype_f, chosen_sex, chosen_age) # OK!!
#print(chisensegmentres)