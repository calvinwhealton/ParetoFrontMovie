# setting working directory for file
# note: Windows needs to use \\ because \ is an escape character
setwd('/Users/calvinwhealton/Desktop')

# libraries
library(scatterplot3d) # for 3-d scatter plot
library(scales) # for transparency in plots
library(grDevices) # some color palettes
library(animation) # animation of plots
library(fields) # for color bar
library(aqfig)# for color bar
library(phytools)# for color bar

# initializing data frame to store values
# objectives and plot number
df <- as.data.frame(list("Obj1"=NA
                         ,"Obj2"=NA
                         ,"Obj3"=NA
                         ,"Obj4"=NA
                         ,"Obj5"=NA
                         ,"Obj6"=NA
                         ,"PtNo"=NA))

######################################
# reading-in data & initial formatting
######################################

# file name
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CHANGE
fileName <- "Susquehanna_1000MC_S1_M2.obj"

# opening "read" connection to file
conn <- file(fileName,open="r")
# determining number of lines in file
linn <-readLines(conn)

# counter for indexing which plot points will appear in
plot_counter <- 1

# loop over number of lines in file
for (i in 1:length(linn)){
  if(linn[i] == '#'){
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

######################################
# scaling values for individual plots
######################################

# objectives for each of the "axes"
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! CHANGE
x_obj <- "Obj1"
y_obj <- "Obj2"
z_obj <- "Obj3"
size_obj <- "Obj4"
col_obj <- "Obj5"
trans_obj <- "Obj6"

# limits for the "axes"
# can be manually specified, if desired
xlims <- c(min(dfComp[x_obj]),max(dfComp[x_obj]))
ylims <- c(min(dfComp[y_obj]),max(dfComp[y_obj]))
zlims <- c(min(dfComp[z_obj]),max(dfComp[z_obj]))
size_upper <- c(min(dfComp[size_obj]))
col_lims <- c(min(dfComp[col_obj]),max(dfComp[col_obj]))
trans_lims <- c(min(dfComp[trans_obj]),max(dfComp[trans_obj]))

### creating data frame columns for size, color, and transparency
# size component scaled so that is upper bound, radius is 1
# square root is used so that area will be proportional
# do not use square root if width should be proportional
dfComp$size <- unlist(sqrt(dfComp[size_obj]/size_upper))

# color bounds
# color palette can be changed, reversed, etc
n_cols <- 100
col_pal <- topo.colors(n_cols,alpha=1)
dfComp$col <- col_pal[unlist(round((n_cols-1)*((dfComp[col_obj] - col_lims[1])/(col_lims[2] - col_lims[1])))+1)]

# transparency scaling
dfComp$trans <- unlist(dfComp[trans_obj] - trans_lims[1])/(trans_lims[2]-trans_lims[1])

######################################
# creating movie as .gif file
######################################
# pch multiplier
pch_mult <- 5
tsp_diff <- trans_lims[2]-trans_lims[1]
transp_leg_values <- c(0.1*tsp_diff,0.3*tsp_diff,0.95*tsp_diff)/(trans_lims[2]-trans_lims[1])
col_bar_lwd <- 10
col_bar_length <- 2
saveGIF({
  for(i in 1:length(unique(dfComp$PtNo))){
    
    # indices in matrix to use
    inds <- which(dfComp$PtNo %in% unique(dfComp$PtNo)[i])
    
    # making the 3-d scatter plot
    scatterplot3d(dfComp[x_obj][[1]][inds] # x axis
                           ,dfComp[y_obj][[1]][inds] # y axis
                           ,dfComp[z_obj][[1]][inds] # z axis
                           ,xlim = xlims # x limits
                           ,ylim = ylims # y limits
                           ,zlim = zlims # z limits
                           ,color = alpha(dfComp$col[inds],dfComp$trans[inds]) # color and transparency
                           ,pch = 16 # shape of marker
                           ,cex.symbols = pch_mult*dfComp$size[inds] # scale of points, change multiplier
                           ,xlab = x_obj # label for x axis
                           ,ylab = y_obj # label for y axis
                           ,zlab = z_obj # label for z axis
                           ,main = 'The WEx'
                           ,mar=c(5,5,5,7)
                  ,xpd=T
                  )
    par(xpd=T)
    add.color.bar(leg=col_bar_length
                  ,cols=alpha(col_pal,transp_leg_values[1])
                  ,title=col_obj
                  ,lims=col_lims
                  ,digits=2
                  ,prompt=F
                  ,x=2
                  ,y=-3.5
                  ,subtitle=paste(trans_obj,'=',round(transp_leg_values[1]*tsp_diff,2),sep=' ')
                  ,lwd=col_bar_lwd)
    add.color.bar(leg=col_bar_length
                  ,cols=alpha(col_pal,transp_leg_values[2])
                  ,title=NA
                  ,lims=col_lims
                  ,digits=2
                  ,prompt=F
                  ,x=2
                  ,y=-4
                  ,subtitle=paste(trans_obj,'=',round(transp_leg_values[2]*tsp_diff,2),sep=' ')
                  ,lwd=col_bar_lwd)
    add.color.bar(leg=col_bar_length
                  ,cols=alpha(col_pal,transp_leg_values[3])
                  ,title=NA
                  ,lims=col_lims
                  ,digits=2
                  ,prompt=F
                  ,x=2
                  ,y=-4.5
                  ,subtitle=paste(trans_obj,'=',round(transp_leg_values[3]*tsp_diff,2),sep=' ')
                  ,lwd=col_bar_lwd)
    legend(x=3
           ,y=3
           ,legend=round(c(0.1,0.5,0.9)/abs(size_upper),2)
           ,col=c('gray48')
           ,pch=16
           ,pt.cex=pch_mult*sqrt(c(0.1,0.5,0.9)/abs(size_upper))
           ,bty='n'
           ,title=size_obj
           ,cex=1
           ,y.intersp=2
           ,horiz=F
    )
    
  }
}
,movie.name='paretoFun.gif',ani.height=800,ani.width=800)