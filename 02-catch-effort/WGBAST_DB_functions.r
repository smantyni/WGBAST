## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Functions for calculating country and year specific catches and efforts

# R-file:		   WGBAST_DB_functions.r

# input: 		
# output:  	

# R ver:	  	  2.8.0

# programmed:		2009 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


###############################################################################
# Functions:

# This builds columns by half yearly data
GatherHalfYears<-function(V1,V2,nY){
temp1<-NULL;temp2<-NULL;
for(i in 1:nY){
  temp1<-rbind(V1[i],V2[i])
  temp2<-rbind(temp2, temp1)
}
return(temp2)
}

################################################################################

Catch_QUART<-function(dat){
  Catch1<-vector()
  Catch2<-vector()
  
  for(y in min_year:max_year){
    temp1<-0
    temp2<-0
    
    for(i in 1:dim(dat)[1]){
      
      if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]<3){
        if(is.na(dat$NUMB[i])==F){temp1<-dat$NUMB[i]+temp1}
      }
      if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]>2){
        if(is.na(dat$NUMB[i])==F){temp2<-dat$NUMB[i]+temp2}
      }
    }
    Catch1[(y-min_year+1)]<-temp1
    Catch2[(y-min_year+1)]<-temp2
  }
  Catch1<-round(Catch1,0); Catch2<-round(Catch2,0)
  catch<-cbind(years,Catch1,Catch2)
  return(catch)
}


###################################

Effort_HYR<-function(dat){
  Effort1<-vector()
  Effort2<-vector()

  for(y in min_year:max_year){
	 temp1<-0
	 temp2<-0

	 for(i in 1:dim(dat)[1]){
     if(dat$TP_TYPE[i]=="HYR"){
       if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]==1){
			  if(is.na(dat$EFFORT[i])==F){temp1<-dat$EFFORT[i]+temp1}
		    }
		    if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]==2){
			   if(is.na(dat$EFFORT[i])==F){temp2<-dat$EFFORT[i]+temp2}
		    }
      }
	 }
	 Effort1[(y-min_year+1)]<-temp1
	 Effort2[(y-min_year+1)]<-temp2
  }
  Effort1<-round(Effort1,0); Effort2<-round(Effort2,0)
  eff<-cbind(years,Effort1,Effort2)
  return(eff)
}
#########################

Catch_HYR<-function(dat){
  Catch1<-vector()
  Catch2<-vector()

  for(y in min_year:max_year){
	 temp1<-0
	 temp2<-0

	 for(i in 1:dim(dat)[1]){
     if(dat$TP_TYPE[i]=="HYR"){
        if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]==1){
          if(is.na(dat$NUMB[i])==F){temp1<-dat$NUMB[i]+temp1}
		    }
		    if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]==2){
          if(is.na(dat$NUMB[i])==F){temp2<-dat$NUMB[i]+temp2}
		    }
      }
	 }
	 Catch1[(y-min_year+1)]<-temp1
	 Catch2[(y-min_year+1)]<-temp2
  }
  Catch1<-round(Catch1,0); Catch2<-round(Catch2,0)
  catch<-cbind(years,Catch1,Catch2)
  return(catch)
}


################################################################################
Effort_MON<-function(dat){

  Effort1<-vector()
  Effort2<-vector()

  for(y in min_year:max_year){
	 temp1<-0
	 temp2<-0

  	for(i in 1:dim(dat)[1]){

		  if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]<7){
			 if(is.na(dat$EFFORT[i])==F){temp1<-dat$EFFORT[i]+temp1}
		  }
		  if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]>6){
			 if(is.na(dat$EFFORT[i])==F){temp2<-dat$EFFORT[i]+temp2}
		  }
	 }
	 Effort1[(y-min_year+1)]<-temp1
	 Effort2[(y-min_year+1)]<-temp2
  }
  Effort1<-round(Effort1,0); Effort2<-round(Effort2,0)
  eff<-cbind(years,Effort1,Effort2)
  return(eff)
}

################################################################################

Catch_MON<-function(dat){

  Catch1<-vector()
  Catch2<-vector()

  for(y in min_year:max_year){
	 temp1<-0
	 temp2<-0

	 for(i in 1:dim(dat)[1]){

	 	 if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]<7){
	 	  if(is.na(dat$NUMB[i])==F){temp1<-dat$NUMB[i]+temp1}
   	  }
	     	if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]>6){
		  	if(is.na(dat$NUMB[i])==F){temp2<-dat$NUMB[i]+temp2}
		  }
	 }
	 Catch1[(y-min_year+1)]<-temp1
	 Catch2[(y-min_year+1)]<-temp2
  }
  Catch1<-round(Catch1,0); Catch2<-round(Catch2,0)
  catch<-cbind(years,Catch1,Catch2)
  return(catch)
}


################################################################################

Effort_MONandHYR<-function(dat){

Effort1<-vector()
Effort2<-vector()


for(y in min_year:max_year){
	temp1<-0
	temp2<-0

	for(i in 1:dim(dat)[1]){

  if(dat$TP_TYPE[i]=="MON"){
		if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]<7){
			if(is.na(dat$EFFORT[i])==F){temp1<-dat$EFFORT[i]+temp1}
		}
		if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]>6){
			if(is.na(dat$EFFORT[i])==F){temp2<-dat$EFFORT[i]+temp2}
		}
	}

  if(dat$TP_TYPE[i]=="HYR"){
	if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]==1){
			if(is.na(dat$EFFORT[i])==F){temp1<-dat$EFFORT[i]+temp1}
		}
		if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]==2){
			if(is.na(dat$EFFORT[i])==F){temp2<-dat$EFFORT[i]+temp2}
		}

  }

	}
	Effort1[(y-min_year+1)]<-temp1
	Effort2[(y-min_year+1)]<-temp2
}
  Effort1<-round(Effort1,0); Effort2<-round(Effort2,0)
  eff<-cbind(years,Effort1,Effort2)
  return(eff)
}

################################################################################

Catch_MONandHYR<-function(dat){

Catch1<-vector()
Catch2<-vector()

for(y in min_year:max_year){
	temp1<-0
	temp2<-0

	for(i in 1:dim(dat)[1]){

  if(dat$TP_TYPE[i]=="MON"){
		if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]<7){
			if(is.na(dat$NUMB[i])==F){temp1<-dat$NUMB[i]+temp1}
		}
		if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]>6){
			if(is.na(dat$NUMB[i])==F){temp2<-dat$NUMB[i]+temp2}
		}
  }
  if(dat$TP_TYPE[i]=="HYR"){

		if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]==1){
			if(is.na(dat$NUMB[i])==F){temp1<-dat$NUMB[i]+temp1}
		}
		if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]==2){
			if(is.na(dat$NUMB[i])==F){temp2<-dat$NUMB[i]+temp2}
		}
  }
	}
	Catch1[(y-min_year+1)]<-temp1
	Catch2[(y-min_year+1)]<-temp2
}
  Catch1<-round(Catch1,0); Catch2<-round(Catch2,0)
  catch<-cbind(years,Catch1,Catch2)
  return(catch)
}


################################################################################
Catch_HYRandYR<-function(dat){
  Catch1<-vector()
  Catch2<-vector()
  propC_HYR1<-vector()

  for(y in min_year:max_year){
	 temp1<-0
	 temp2<-0
	 temp3<-0

	 for(i in 1:dim(dat)[1]){
       if(dat$TP_TYPE[i]=="MON"){
		    if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]<7){
			  if(is.na(dat$NUMB[i])==F){temp1<-dat$NUMB[i]+temp1}
		    }
		    if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]>6){
			   if(is.na(dat$NUMB[i])==F){temp2<-dat$NUMB[i]+temp2}
		    }
      }
      if(dat$TP_TYPE[i]=="HYR"){
    	 if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]==1){
			   if(is.na(dat$NUMB[i])==F){temp1<-dat$NUMB[i]+temp1}
		    }
		    if (dat$YEAR[i]==y && dat$TIME_PERIOD[i]==2){
			   if(is.na(dat$NUMB[i])==F){temp2<-dat$NUMB[i]+temp2}
		    }
      }
  	}
	 # Count proportion of catch on first half year
	 propC_HYR1[(y-min_year+1)]<-temp1/(temp1+temp2)

	 # Count the sum of catch reported as yearly..
	 for(i in 1:dim(dat)[1]){
     if(dat$TP_TYPE[i]=="YR" && dat$YEAR[i]==y){
       	        if(is.na(dat$NUMB[i])==F){temp3<-dat$NUMB[i]+temp3}
      }
    }
    # ... divide it based on the half yearly proportion and
    # add it to the half yearly catches
    temp1<-temp1 + temp3 * propC_HYR1[(y-min_year+1)]
    temp2<-temp2 + temp3 * (1-propC_HYR1[(y-min_year+1)])

   ifelse(is.na(temp1)==F,
	   Catch1[(y-min_year+1)]<-temp1,
	   Catch1[(y-min_year+1)]<-0)
	 ifelse(is.na(temp1)==F,
	   Catch2[(y-min_year+1)]<-temp2,
	   Catch2[(y-min_year+1)]<-0)	 
  }
  Catch1<-round(Catch1,0); Catch2<-round(Catch2,0)
  catch<-cbind(years,Catch1,Catch2)
  return(catch)
}
#####################################
