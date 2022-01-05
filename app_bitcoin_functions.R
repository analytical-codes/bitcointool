# All functions used in our web tool are defined in here


######################################################
# this is the main function that caluculates the probability of price movement in the next day
# within this function we used some other functions for data preparation, the explanation of 
# other functions are provided after this main function.
prob_cal<-function(entry,style){
  
  require(pacman)
  pacman::p_load(shiny,dplyr,tidyverse,magrittr, TTR)
  
  # library(shiny)
  # library(dplyr)
  # library(tidyverse)
  # library(magrittr)
  # library(TTR)
  
  # here we enter the developed model from Github
  result_model <-readRDS(url("https://raw.githubusercontent.com/analytical-codes/bitcointool/main/ct-model.rds","rb"))
  # result_model <- readRDS("ct-model.rds")
  
  # Here are all the variables used for data preparation
  # we entered more than 6 variables that the final decision tree used, because of the requirements
  # of the R package, however eventually the decision treee just used the 6 variables
  vars=c("BTC_USD_close_diff", "dollar2yuan_Open", "dji_close_sma3_diff", 
         "BTC_USD_Open_ema5", "stock_Adjusted")
  extra_cat=c("gold_open_move_1", "BTC_USD_close_rsi_move_0", "BTC_USD_Open_ema5_move_0", "BTC_USD_Close_sma3_move_0")
  extra_num=c("gold_open_rsi", "oil_Volume", "dollar2euro_Adjusted", "dollar2yuan_Close", 
              "BTC_USD_Close_sma3_diff")
  
  err_check<-"NO"
  #error if in the manual part still the user did not input some variables
  error_manual<-""
  #error for no of rows equal zero
  error_empty<-""
  # check if the variables are provided
  error_vars<-""
  # error if the numerical variables are out of range
  error_range<-""
  # error if the value of categorical variables are valid
  error_cat<-""
  error_message<-""
  
  # if we have manual data entry this section will be operated and does the data preparation
  if(style=="manual"){
    error_empty_m<-entry$error_empty_m
    if(error_empty_m=="NO"){
      file1<-entry$file1
      for(i in extra_cat){
        file1%<>%mutate(!!as.name(i):= as.factor(1))}
      for(i in extra_num){
        file1%<>%mutate(!!as.name(i):= 1)}
      file1[,c("BTC_USD_open_move")]=as.factor(c("1"))
      probs=predict(result_model, newdata=file1, type="prob")[,2]
      
    }else{
      err_check<-"YES"
      error_message<- c("  Please enter values of all the variables in order to get survival probabilities! ")
    }}
  
  
  
  
  # if we have table data entry "i.e. CSV" this section will be operated and does the data preparation
  if(style=="automated"){
    file1<-entry
    if(nrow(file1) %in% apply(file1, 2,function(x) sum(is.na(x)))|
    nrow(file1) %in% apply(file1, 2,function(x) sum(x==""))){
      err_check="YES"
      error_message="Error: at leaset one of you columns is entirely empty!"
    }

    # In here, we need to produce all calculated variables "i.e. diff, sma3, and move"
    # please study the paper for provided explainations
      if(err_check=="NO"){
      file1 <- entry
      if (nrow(file1)==0){error_empty<- c("  Your file is empty  ")
      err_check<-"YES"}
      file1=ts_interpolate(file1,method="nocb")
      file1 %<>%mutate(BTC_USD_open_move=func_move(c(NA,diff(file1$BTC_USD_Open))))%>%
      mutate(BTC_USD_Open_ema5=EMA(file1$BTC_USD_Open,5))%>%
      mutate(dji_close_sma3=SMA(file1$DJI.Close,3))%>%
      mutate(dji_close_sma3_diff=c(NA,diff(file1$dji_close_sma3)))%>%
      mutate(dji_close_sma3_diff=c(NA,diff(file1$BTC_USD_Close)))
      file1$BTC_USD_close_diff=c(NA,diff(file1$BTC_USD_Close))
      file1=file1[complete.cases(file1$BTC_USD_open_move),]

      file1[,c("BTC_USD_open_move")] <- c(file1[,c("BTC_USD_open_move")])
      file1[,c("BTC_USD_open_move")]=as.factor(ifelse(file1[,c("BTC_USD_open_move")]== 1,"down", "up"))

      # if there is no error in the above sections of data entry and data preparation
      # the following section calculates the prediction
      for(i in extra_cat){
        file1%<>%mutate(!!as.name(i):= as.factor(1))}
      for(i in extra_num){
        file1%<>%mutate(!!as.name(i):= 1)}
      # for(i in vars){
      #   file1[,i]=as.numeric(file1[,i])
      # }
      file1=file1[nrow(file1),]
      probs=predict(result_model, newdata=file1, type="prob")[,2]}
  }
  
  # the rest will run only if the err_check=="NO"
  
  if(err_check=="NO"){
    
    # the survival probability matrix after isotonic regression is applied
    
    outcome<-list()
    outcome$prob_cal<-probs
    outcome$threshold<-"The typical threshold is 0.5 anything above that refres to
          increase in the bitcoin open price (up), otherwise decrease (down). Based on the results
          of the study for a higher accuracy disregards probabilities with the (0.4,0.6) range."
    outcome$error_message<-error_message
    outcome$err_check<-err_check
    return(outcome)
  }
  
  if(err_check=="YES"){
    outcome<-list()
    outcome$prob_cal<-""
    outcome$error_message<-error_message
    outcome$err_check<-err_check
    return(outcome)
  }
  
}
# this section is for changing the format of manual data entry from shiny-friendly to
# a friendly version for the packages that do prediction
# it calculates the 
manual2df<-function(parameters){
  file1 <- read.csv("https://raw.githubusercontent.com/analytical-codes/bitcointool/main/template.csv")
  
  file1$dollar2yuan_Open<-parameters$dollar2yuan_Open
  file1$stock_Adjusted<- parameters$stock_Adjusted
  file1$dji_close_sma3_diff<- parameters$dji_close_sma3_diff
  file1$BTC_USD_Open_ema5<- parameters$BTC_USD_Open_ema5
  file1$BTC_USD_close_diff<- parameters$BTC_USD_close_diff
  
  error_empty_m<-"NO"
  if((sum(which(file1==""))+sum(is.na(file1)))>0){
    error_empty_m<-"YES"
  }
  
  out_put<-list()
  out_put$file1<-file1
  out_put$error_empty_m<-error_empty_m
  #out_put=file1
  return(out_put)
  
}


# the next function is for interpolating a dataframe as the time series. In case if it is needed
# this packeg could interpolate indipendent variables

ts_interpolate<-function(df,method="nocb"){
  NA_Col_Rate <- col_missing_function(df)
  NA_Col_Rate$varname <- rownames(NA_Col_Rate)
  for(i in 1:nrow(NA_Col_Rate)){
    if(NA_Col_Rate$na_count_col[i]>0){
      df[,NA_Col_Rate$varname[i]]<-imputeTS::na_locf(as.numeric(df[,NA_Col_Rate$varname[i]]), option = method)
    }
  }
  return(df)
}

## col_missing_function() counts number of nas in each column in a given data object
## input_data: a array, including a matrix, dataframe
## Return Values: a data frame object that reconds percentage of missing values in each column in the input data
col_missing_function <- function(input_data){
  # first, we count nas in each column
  na_count_col <- apply(input_data, 2, function(y) length(which(is.na(y)))/nrow(input_data)) 
  # we saved the object into a data_frame
  #names_df<-names(na_count_col)
  na_count_col <- data.frame(na_count_col)
  #na_count_col$varname <-names_df
  return(na_count_col)
}


# move function, this function calculates the movement of price

func_move<-function(data_in){
  data_in[data_in<0]<-0 
  data_in[data_in>0]<-1 
  data_in<-as.factor(data_in)
  return(data_in)
}

# Panel_div Function used in the Shiny LP Package within the user interface (UI) part of the app
panel_div <- function (class_type, panel_title, content){
  shiny::div(class = sprintf("panel panel-%s", class_type), 
             shiny::div(class = "panel-heading", 
                        shiny::h3(class = "panel-title", panel_title)), 
             shiny::div(class = "panel-body", content))
}
