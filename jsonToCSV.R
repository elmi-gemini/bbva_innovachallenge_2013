jsonToCSVCube <- function (jsonText, codigopostal1, negocio1)
{
	out <- tryCatch(
	{
		biglist1 <- fromJSON(paste(jsonText, collapse=""))          
		seclist1 <- biglist1[2]  # removing the element "result" of a big list
		seclist1$data$size
		#[1] 26
		seclist1$data$last_key
		# [1] "201317"       
		# to create tables for the statistics for each period-gender-age interval:
		statslist <- seclist1$data$stats
		statistics = unlist(statslist)
		unique(names(statistics))
		# [1] "date"      "cube.hash"  "cube.num_payments" "cube.avg"   [5] "cube.num_cards" 
		# to vectors:
		dates <- statistics[names(statistics)=="date"]
		segment <- statistics[names(statistics)=="cube.hash"]
		num_paym <- statistics[names(statistics)=="cube.num_payments"]
		avg_paym <- statistics[names(statistics)=="cube.avg"]
		num_cards <- statistics[names(statistics)=="cube.num_cards"] 
		
		dates_all = NULL
		for( i in 1:seclist1$data$size)
		{
			dates_all = c(dates_all, rep(dates[[i]],length(sapply(statslist[[i]][[2]], length))));
		}

		first = data.frame(rep(as.character(codigopostal1), length(num_paym)), negocio1, dates_all, segment, num_paym, avg_paym, num_cards)
		colnames(first) = c("postal_code", "commerce_type", "week", "client_segment", "num_payms", "avg_paym_eur", "num_cards")
		
		write.table(first, sep=",", row.name=FALSE, append=TRUE, file = "CardsCube.CSV", col.names=FALSE)
	},
		error=function(cond)
		{
				message(cond)
				# Choose a return value in case of error
				return(NA)
		}
	)
	return(out)
}

jsonToCSVZipCodes <- function (jsonText, codigopostal1, negocio1)
{
	out <- tryCatch(
		{
			biglist1 <- fromJSON(paste(jsonText, collapse=""))
			seclist1 <- biglist1[2]  # removing the element "result" of a big list
			seclist1$data$size
			#[1] 26  # weeks
			seclist1$data$last_key
			# to create tables for the statistics:
			statslist <- seclist1$data$stats
			statistics = unlist(statslist)
			
			# unlisted data to vectors:
			date1 <- statistics[names(statistics)==unique(names(statistics))[1]]
			zipcodes.label1 <- statistics[names(statistics)==unique(names(statistics))[2]]
			zipcodes.num_cards1 <- statistics[names(statistics)==unique(names(statistics))[3]]
			zipcodes.num_payments1 <- statistics[names(statistics)==unique(names(statistics))[4]]
			zipcodes.incomes1 <- statistics[names(statistics)==unique(names(statistics))[5]]
			
			dates_all1 = NULL
			for( i in 1:seclist1$data$size)
			{
				dates_all1 = c(dates_all1, rep(date1[[i]],length(sapply(statslist[[i]][[2]], length))));
			}

			# writing it into the table:
			data = data.frame(rep(as.character(codigopostal1), length(zipcodes.incomes1)), negocio1, dates_all1, zipcodes.label1, zipcodes.num_cards1,
						   zipcodes.num_payments1, zipcodes.incomes1)
						   
			colnames(data) = c("postal code","commerce_type", "week", "zip_label", "zip_numcards", "zip_numpayms", "zip_incomes")
							
			#return(write.table(data, sep=",", row.name=FALSE));
			write.table(data, sep=",", row.name=FALSE, append=TRUE, file = "ZipCodes.CSV", col.names=FALSE)
		},
		error=function(cond)
		{
				message(cond)
				# Choose a return value in case of error
				return(NA)
		}
	)
	return(out)
}

jsonToCSVConsumptionHourlyData <- function (jsonText, codigopostal1, negocio1)
{
	out <- tryCatch(
	{
		biglist1 <- fromJSON(paste(jsonText, collapse=""))                    
		seclist1 <- biglist1[2]  # removing the element "result" of a big list
		seclist1$data$size
		#[1] 6
		seclist1$data$last_key
		# [1] "201304" --> = last period in the dataset
		# to create tables for the statistics for each period-gender-age interval:
		statslist <- seclist1$data$stats                    
		statistics = unlist(statslist)
		# unlisted data to vectors:
		month <- statistics[names(statistics)=="date"]
		weekday <- statistics[names(statistics)=="days.day"]

		# HOURLY stats:
		dayhour <- statistics[names(statistics)=="days.hours.hour"]
		hourly_avr_paym <- statistics[names(statistics)=="days.hours.avg"]
		hourly_max_paym <- statistics[names(statistics)=="days.hours.max"]
		hourly_min_paym <- statistics[names(statistics)=="days.hours.min"]
		hourly_stdev_paym <- statistics[names(statistics)=="days.hours.std"]
		hourly_mode_paym <- statistics[names(statistics)=="days.hours.mode"]
		hourly_num_cards <- statistics[names(statistics)=="days.hours.num_cards"]
		hourly_num_paym <- statistics[names(statistics)=="days.hours.num_payments"]

		# creating the column with month numbers to merge with an hourly dataset:
		
		months_all = NULL;
		totalNumDias = 0;
		for(i in 1: seclist1$data$size)
		{
			numDias = length(sapply(statslist[[i]][[2]], length));
			totalNumDias = totalNumDias + numDias;
			suma = 0;
			for(j in 1: numDias)
			{
				suma = suma + length(sapply(statslist[[i]][[2]][[j]][[9]], length));
			}
			months_all = c(months_all, rep(month[[i]], suma));
		}

		wdays_all = NULL;
		k = 1;
		for(i in 1: seclist1$data$size)
		{
			numDias = length(sapply(statslist[[i]][[2]], length));
			for(j in 1: numDias)
			{
				wdays_all = c(wdays_all, rep(weekday[[k]], length(sapply(statslist[[i]][[2]][[j]][[9]], length))));
				k = k+1;
			}
		}	

		# adding here month name and weekday name too...

		first_hourly = data.frame(rep(as.character(codigopostal1), length(hourly_num_paym)), negocio1, months_all, wdays_all, dayhour, hourly_avr_paym ,hourly_max_paym ,hourly_min_paym ,
								  hourly_stdev_paym, hourly_mode_paym, hourly_num_cards, hourly_num_paym)

		colnames(first_hourly) = c("postal_code", "commerce_type", "month", "weekday", "dayhour", "hourly_avr_paym", "hourly_max_paym" ,"hourly_min_paym" ,
							   "hourly_stdev_paym", "hourly_mode_paym", "hourly_num_cards", "hourly_num_paym")
							   
		#return(write.table(first_hourly, sep=",", row.name=FALSE));
		write.table(first_hourly, sep=",", row.name=FALSE, append=TRUE, file = "Hourly.CSV", col.names=FALSE)
	},
		error=function(cond)
		{
				message(cond)
				# Choose a return value in case of error
				return(NA)
		}
	)
	return(out)		
}

jsonToCSVConsumptionDailyData <- function (jsonText, codigopostal1, negocio1)
{
	out <- tryCatch(
	{
		biglist1 <- fromJSON(paste(jsonText, collapse=""))                    
		seclist1 <- biglist1[2]  # removing the element "result" of a big list
		seclist1$data$size
		#[1] 6
		seclist1$data$last_key
		# [1] "201304" --> = last period in the dataset
		# to create tables for the statistics for each period-gender-age interval:
		statslist <- seclist1$data$stats                    
		statistics = unlist(statslist)
		# unlisted data to vectors:
		month <- statistics[names(statistics)=="date"]
		weekday <- statistics[names(statistics)=="days.day"]

		# daily statistics :
		weekday_avr_paym <- statistics[names(statistics)=="days.avg"]
		weekday_max_paym <- statistics[names(statistics)=="days.max"]
		weekday_min_paym <- statistics[names(statistics)=="days.min"]
		weekday_stdev_paym <- statistics[names(statistics)=="days.std"]
		weekday_mode_paym <- statistics[names(statistics)=="days.mode"]
		weekday_num_cards <- statistics[names(statistics)=="days.num_cards"]
		weekday_num_paym <- statistics[names(statistics)=="days.num_payments"]

		months_all_daily = NULL
		for(i in 1: seclist1$data$size)
		{
			months_all_daily = c(months_all_daily, rep(month[[i]],length(sapply(statslist[[i]][[2]], length))));
		}
						 
		# writing this into the table (r object)
		dailydata = data.frame(rep(as.character(codigopostal1), length(weekday_num_paym)), negocio1, months_all_daily, weekday, weekday_avr_paym, weekday_max_paym, weekday_min_paym, weekday_stdev_paym,
						   weekday_mode_paym, weekday_num_cards, weekday_num_paym)

		colnames(dailydata) = c("postal code", "commerce_type", "month", "weekday", "wd_avr_paym", "wd_max_paym", "wd_min_paym", "wd_stdev_paym",
						"wd_mode_paym", "wd_num_cards", "wd_num_paym")

						
		#return(write.table(dailydata, sep=",", row.name=FALSE));
		write.table(dailydata, sep=",", row.name=FALSE, append=TRUE, file = "Daily.CSV", col.names=FALSE)
	},
		error=function(cond)
		{
				message(cond)
				# Choose a return value in case of error
				return(NA)
		}
	)
	return(out)			
}