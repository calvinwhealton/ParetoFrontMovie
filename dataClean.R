# function to read-in and clean the data----
# data is assumed to have
#     6 columns (1 for each objective)
#     Pareto front at different NFEs separated by a consistent character
#     Pareto fronts sequentially in file, starting with the earliest
# output is a data frame with
#     6 objectives
#     additional column for the plot number, represented a Pareto Front generation
cleanData <- function(fname            # name of file with data
                      ,separator='#'   # character separator of the Pareto fronts
                      ,objNames = NULL # list of objectives names in order of column
){
  
  # initializing data frame to store values
  # objectives and plot number
  # if objectives names are not defined, then using default values
  df <- as.data.frame(list("Obj1"=NA
                           ,"Obj2"=NA
                           ,"Obj3"=NA
                           ,"Obj4"=NA
                           ,"Obj5"=NA
                           ,"Obj6"=NA
                           ,"PtNo"=NA))

  # changing column names if specified
  if(is.null(objNames) == F){
    for(i in 1:6){
      names(df)[i] <- objNames[i]
    }
  }

  # opening "read" connection to file
  conn <- file(fname,open="r")
  # determining number of lines in file
  linn <-readLines(conn)
  
  # counter for indexing which plot points will appear in
  plot_counter <- 1
  
  # loop over number of lines in file
  for (i in 1:length(linn)){
    if(linn[i] == separator){
      plot_counter <- plot_counter + 1
    }else{
      df[i,seq(1,6,1)] <- c(as.numeric(strsplit(linn[i],split=' ')[[1]]))
      df[i,7] <- plot_counter
    }
  }
  
  # closing connection to file
  close(conn)
  
  # at this point the dataframe should have
  # seven columns (6 objectives, 1 plot number)
  # and NAs for each row where there was a '#'
  
  # removing NAs from dataframe
  dfComp <- df[complete.cases(df),]
  
  # returning completing data frame
  return(dfComp)
}