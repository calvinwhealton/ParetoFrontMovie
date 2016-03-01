###########################################
# functions for assigning:
# color, size, and transparency objectives
###########################################

# function to assign color, size, and transparency----
# for objectives
assignColSizeTrans <- function(df               # data frame processed through cleanData
                               ,colObj          # color objective, index of column
                               ,sizeObj         # size objective, index of column
                               ,transObj        # transparency objective, index of column
                               ,ideal=NULL      # ideal point for objectives, NULL implies using best values for objectives (color,size,transparency)
                               ,nCols=15        # number of colors
                               ,colPal=NULL     # color palette to use
                               ,sizePow=1/3     # power scaling for the point size
                               ,sizeMin=0.1     # minimum size of points
                               ,transMin=0.2    # minimum transparency used, very faint
                               ,transMax=1      # maximum transparency used, very saturaged
){
  
  # assigning the colors
  dfcalc <- assignCols(df=df
                       ,colObj=colObj
                       ,ideal=ideal[1]
                       ,nCols=nCols
                       ,colPal=colPal)
  
  # assigning the sizes
  dfcalc <- assignSize(df=dfcalc
                       ,sizeObj=sizeObj
                       ,ideal=ideal[2]
                       ,sizePow=sizePow
                       ,sizeMin=sizeMin)
  
  # assigning the transparency
  dfcalc <- assignTrans(df=dfcalc
                       ,transObj=transObj
                       ,ideal=ideal[3]
                       ,transMin=transMin
                       ,transMax=transMax)
  
  # returning the data frame
  return(dfcalc)
}

# function to generate colors for input----
# data is assumed to have
#     1 column for color
#     objective is assumed to be maximization
assignCols <- function(df            # data frame processed through cleanData
                       ,colObj       # color objective, index of column
                       ,ideal=NULL   # ideal point for objective, NULL implies using best value for objective used
                       ,nCols=15     # number of colors
                       ,colPal=NULL  # color palette to use
){
  
  # setting color palette if none specified
  # goes red-orange-green-red
  if(is.null(colPal)){
    colPal <- rev(colorRampPalette(c("midnightblue","green3","orange","firebrick1"))(nCols))
    nc <- nCols
  }else{
    nc <- length(colPal)
  }
  
  # calculating ideal point
  if(is.null(ideal)){
    ideal <- max(df[,colObj])
  }
  
  # assigning the color scheme
  colMax <- ideal # maximum color value
  colMin <- min(df[,colObj]) # maximum color value, assumed to be minimum of plotted
  
  # indices for color palettes
  # scale all variables to [0,1]
  # multiply by nCol-1 and then add 1 to get to range [1,nCol]
  indsColPal <- round((nc-1)*(df[,colObj] - colMin)/(colMax - colMin))+1
  
  # assigning colors
  df$colors <- colPal[indsColPal]
  
  # returning data frame
  return(df)
}

# function to generate size for input----
# data is assumed to have
#     1 size objective column, greater than zero
#     objective is assumed to be maximization
#     (higher values are larger in size)
assignSize <- function(df            # data frame processed through cleanData
                       ,sizeObj      # size objective, index of column
                       ,ideal=NULL   # ideal point for objective, NULL implies using best value is used
                       ,sizePow=1/3  # power scaling for the point size
                       ,sizeMin=0.1  # minimum size of points
){
  # scaling all variables to the required size
  df$size <- df[,sizeObj]^sizePow
  
  # printing error warning if there is a value less than sizeMin
  if(min(df$size) < sizeMin){
    print('Minimum size value is less than sizeMin')
    print('Will set all size values less than sizeMin to sizeMin')
    
    df$size[which(df$size  < sizeMin)] <- sizeMin
  }
  
  # returning data frame
  return(df)
}

# function to generate transparency for input----
# data is assumed to have
#     1 transparency objective column
#     objective is assumed to be maximization
#     (higher values are less transparent)
assignTrans <- function(df             # data frame processed through cleanData
                        ,transObj      # transparency objective, index of column
                        ,ideal=NULL    # ideal point for objective, NULL implies using best values for each objective used
                        ,transMin=0.2  # minimum transparency used, very faint
                        ,transMax=1    # maximum transparency used, very saturaged
){
  
  # calculating ideal point
  if(is.null(ideal)){
    ideal <- max(df[,transObj])
  }
  
  # scaling all transparency values
  df$trans <- (df[,transObj]-min(df[,transObj]))*(transMax - transMin)/(max(df[,transObj]) - min(df[,transObj])) + transMin
  
  # returning data frame
  return(df)
}
