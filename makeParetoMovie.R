# function to create Pareto front movie
# assumes that there is a data frame with six objectives
# all objectives are assumed to be maximization
# objectives are plotted in 3-d space, size, color, and transparency
makeParetoMovie <- function(fname                # data name for file
                            ,datSep='#'          # seperator of parteo fronts in file
                            ,mnmx=rep('max',6)   # are objectives minimization or maximazation
                            ,objNames=NULL       # names for objectives
                            ,objs=c(1,2,3,4,5,6) # column of objective for x,y,z,size,color,transparency
                            ,ideal=NULL          # ideal point
                            ,idealAdd=F          # should ideal point be added to plot
                            ,idealPch=8          # pch for the ideal point
                            ,idealMult=10        # multiplier for the ideal point
                            ,colN=15             # number of colors
                            ,colPal=NULL         # color palette
                            ,sizePow=1/3         # power used in scaling size
                            ,sizeMin=0.1         # minimum size
                            ,transMin=0.05       # minimum transparency (very faint)
                            ,transMax=1          # maximum transparency (very saturated)
                            ,pchOutline=T        # should points be outlined
                            ,pchType='circle'    # type of pch symbol
                            ,pchMult=5           # multiplier for the plot symbols
                            ,aniH=800            # height of animation
                            ,aniW=800            # width of animation
                            ,aniName='ParetoFront.mp4' # name for animation
                            ,aniInt=0.1            # interval for animation in seconds
                            ,aniFramEx=10          # number of extra frames at end of rotation
                            ,aniRot='last'         # rotate all or only last Pareto Front
                            ,aniRotFrames=20       # number of frames for the non-rotated Pareto fronts
                            ,aniNonRotAng=40      # angle of non-rotated plots
                            ,ptMain='Pareto Front' # title for plot
                            ,ptMainCex=4         # character expansion for title of plot
                            ,ptMainLine=-7       # line location of plot
                            ,ptAngle=seq(9,81,1) # angles of axis in degrees, rotates through theta and phi in scatter3D()
                            ,ptAxis = T          # should axes be plotted
                            ,ptTick=T            # should tick marks be drawn
                            ,ptLabTick=F         # should tick marks be labeled
                            ,ptCexLab=1          # character expansion for plot labels
                            ,legSizeVals = NULL  # legend values for size
                            ,legLoc=c(0.5,0.7)   # x and y location of the legend
                            ,legCol='black'      # color of the size legend point
                            ,legCex=2            # character expansion for size legend
                            ,legyinsp=2          # vertial spacing of entries
                            ,legHoriz=F          # vertical or horizontal orientation
                            ,cbLen = 0.6         # color bar length
                            ,cbx=0.2             # x location of the color bars
                            ,cby=0.7             # y location of the first color bar
                            ,cbdy=-0.1           # spacing difference between the color bars
                            ,cblwd=10            # line weight of the color bar
                            ,cbout=F             # outline the color bar
                            ,cbDig=2             # digits for color bar
                            ,cbCex=1.5           # character expansion for color bar
                            ,tspCex=1.5          # text expansion for the transparency
                            ,tspLevs=NULL        # transparency levels, in the objective value
){

  # printing error warning if columns for objectives not defined
  if(length(unique(objs)) != 6){
    print('Not 6 unique objectives')
  }

  # reading-in file and cleaning it
  pf <- cleanData(fname
                  ,separator=datSep
                  ,objNames=objNames)

  # converting all values to maximization
  for(i in 1:length(mnmx)){
    if(mnmx[i] == 'min'){
      pf[,i] <- pf[,i]*-1
    }
  }

  # assigning color, size, and transparency values
  pf <- assignColSizeTrans(df=pf
                           ,colObj=objs[4]
                           ,sizeObj=objs[5]
                           ,transObj=objs[6]
                           ,ideal=ideal[c(4,5,6)]
                           ,nCols=colN
                           ,colPal=colPal
                           ,sizePow=sizePow
                           ,sizeMin=sizeMin
                           ,transMin=transMin
                           ,transMax=transMax)
  
  # assigning limits for axis, if no ideal point defined
  # will assign ideal as maximum if it is defined
  if(is.null(ideal)){
    xlims <- c(min(pf[,objs[1]]),max(pf[,objs[1]]))
    ylims <- c(min(pf[,objs[2]]),max(pf[,objs[2]]))
    zlims <- c(min(pf[,objs[3]]),max(pf[,objs[3]]))
  }else{
    xlims <- c(min(pf[,objs[1]]),ideal[objs[1]])
    ylims <- c(min(pf[,objs[2]]),ideal[objs[2]])
    zlims <- c(min(pf[,objs[3]]),ideal[objs[3]])
  }

  # other limits
  translims <- c(min(pf$trans),max(pf$trans))
  collims <- c(min(pf$colors),max(pf$colors))
  sizelims <- c(min(pf$size),max(pf$size))

  # changing point representation if there is an outline
  if(pchOutline==T){
    if(pchType=='circle'){
      pc  <- 21
    }else if(pchType=='square'){
      pc <- 22
    }else if(pchType=='diamond'){
      pc <- 23
    }else if(pchType=='triangle'){
      pc <- 24
    }else{
      print('No valid pchType selected')
    }
  }else{
    if(pchType=='circle'){
      pc  <- 16
    }else if(pchType=='square'){
      pc <- 15
    }else if(pchType=='diamond'){
      pc <- 18
    }else if(pchType=='triangle'){
      pc <- 17
    }else{
      print('No valid pchType selected')
    }
  }
  
  # legend size values and labels
  if(is.null(legSizeVals)){
    leg_sizes <- round(quantile(pf$sizes,probs=c(0.1,0.5,0.9)),2)
  }else{
    leg_sizes <- legSizeVals
  }

  # legend labels
  leg_sizes_labs <- paste(round(quantile(pf[,objs[5]],probs=c(0.1,0.5,0.9)),2),sep='')

  # colorbar values
  if(is.null(colPal)){
    cb_cols <- rev(colorRampPalette(c("midnightblue","green3","orange","firebrick1"))(colN))
  }else{
    cb_cols <- colPal
  }

  # color bar limits
  if(is.null(ideal)){
    cbLim <- c(min(pf[,objs[4]]),max(pf[,objs[4]]))
  }else{
    cbLim <- c(min(pf[,objs[4]]),ideal[4])
  }
  
  # transparency values and levels plotted
  if(is.null(ideal[6])){
    ideal <- max(pf[,objs[6]])
  }
  
  # scaling all transparency values
  if(is.null(tspLevs)){
    tspVals <- (quantile(pf[,objs[6]],c(0.01,0.25,0.5,0.75,0.99))-min(pf[,objs[6]]))*(transMax - transMin)/(max(pf[,objs[6]]) -min(pf[,objs[6]]))+transMin
    tspLevsUse <- quantile(pf[,objs[6]],probs=c(0.01,0.25,0.5,0.75,0.99))
  }else{
    tspVals <-  (tspLevs-min(pf[,objs[6]]))*(transMax - transMin)/(max(pf[,objs[6]]) -min(pf[,objs[6]]))+transMin
    tspLevsUse <- tspLevs
  }
  
  # tick types
  if(ptTick){
    ttype <- "detailed"
  }else{
    ttype=NA
  }
  
  # making vector of angles and number of frames for movie
  # case of all plots rotated first
  if(aniRot == 'all'){
    
    # initializing angle vector
    angs <- NULL
    pfNo <- NULL
    
    # loop through the number of Pareto fronts
    for(h in 1:length(unique(pf$PtNo))){
      # looping through the plot angles
      for(i in 1:length(ptAngle)){
        
        # setting values to reverse if necessary
        if((h %% 2) == 1){
          an <- ptAngle
        }else{
          an <- rev(ptAngle)
        }
        
        # adding extra frames to "pause" the movie at extreme rotation
        if(i == 1 | i == length(ptAngle)){
          angs <- c(angs,rep(an[i],aniFramEx))
          pfNo <- c(pfNo,rep(unique(pf$PtNo)[h],aniFramEx))
        }else{
          angs <- c(angs,an[i])
          pfNo <- c(pfNo,unique(pf$PtNo)[h])
        } # end of if/else for extreme angle
      } # end of for loop for angles
    } # end of loop through Pareto fronts
  }else if(aniRot == 'last'){
    
    # initializing angle vector
    angs <- NULL
    pfNo <- NULL
    
    # loop through the number of Pareto fronts
    for(h in 1:length(unique(pf$PtNo))){
      
      # condition for the last Pareto Front
      if(h == length(unique(pf$PtNo))){
        # looping through the plot angles
        for(i in 1:length(ptAngle)){
          # adding extra frames to "pause" the movie at extreme rotation
          if(i == 1 | i == length(ptAngle)){
            angs <- c(angs,rep(ptAngle[i],aniFramEx))
            pfNo <- c(pfNo,rep(unique(pf$PtNo)[h],aniFramEx))
          }else{
            angs <- c(angs,ptAngle[i])
            pfNo <- c(pfNo,unique(pf$PtNo)[h])
          } # end of if/else for extreme angle
        } # end of for loop for angles
      }else{
        # adding the nonrotation angles for the specified number of frames
        angs <- c(angs,rep(aniNonRotAng,aniRotFrames))
        pfNo <- c(pfNo,rep(unique(pf$PtNo)[h],aniRotFrames))
      } # end if/else for whether the last frame or not
    }# end of loop through Pareto fronts
  }else{
    print('Select a valid input for aniRot (\'all\' or \'last\')')
  } # end if if/else for the type of rotation in the plots
  
  # ideal point for plotting if not defined
  if(is.null(ideal)){
    idPlot <- apply(hi[,c(1,2,3,4,5,6)],2,max)
  }else{
    idPlot <- ideal
    for(i in 1:6){
      if(mnmx[i] == 'min'){
        idPlot[i] <-ideal[i]*-1
      }
    }
  }
  
  # making the movie
  saveVideo({
    
    # loop to create individual plots
    for(h in 1:length(angs)){
      
      # indices in matrix to use
      inds <- which(pf$PtNo %in% pfNo[h])
      
      # layout matrix
      # plot1 is is in the 1 locations, 2 and 3 are used for legends
      # 1 1 | 2
      # 1 1 | 3
      layout(matrix(c(1,1,2,1,1,3),2,3,byrow=T))
      
      # making the 3-d scatter plot
      scatter3D(pf[inds,objs[1]]   # x axis
                ,pf[inds,objs[2]]  # y axis
                ,pf[inds,objs[3]]  # z axis
                ,xlim = xlims      # x limits
                ,ylim = ylims      # y limits
                ,zlim = zlims      # z limits
                ,col = alpha.col(col=pf$colors[inds],alpha=pf$trans[inds])
                ,pch = pc          # shape of marker
                ,bg = alpha.col(col=pf$colors[inds],alpha=pf$trans[inds]) # color and transparency when using an outline point
                ,cex.symbols = pchMult*pf$size[inds] # scale of points, change multiplier
                ,xlab = colnames(pf)[objs[1]] # label for x axis
                ,ylab = colnames(pf)[objs[2]] # label for y axis
                ,zlab = colnames(pf)[objs[3]] # label for z axis
                ,axes=ptAxis                 # should axis be drawn
                ,ticktype=ttype              # should tick marks be drawn
                ,label.tick.marks=ptLabTick  # should tick marks be labeled
                ,cex.lab=ptCexLab            # character expansion for labels
                ,theta=angs[h]               # angle of plot
                ,phi=angs[h]                 # angle of plot
                ,colkey=NULL                 # removing color bar
                ,colvar=NA                   # no color variable
      )
      
      # adding the ideal point
      if(idealAdd){
        scatter3D(x=idPlot[objs[1]]   # x ideal location
                  ,y=idPlot[objs[2]]  # y ideal location
                  ,z=idPlot[objs[3]]  # z ideal location
                  ,col=alpha.col(col=cb_cols[length(cb_cols)],alpha=1)
                  ,pch=idealPch
                  ,add=T)
      }

      # adding title for 3d plot
      title(ptMain
            ,line=ptMainLine
            ,cex.main=ptMainCex)
      
      # plotting outside of the graph, in margins
      # might be vestigial
      par(xpd=T)  
     
      # empty plot for where legend will be (plot area 2)
      plot(NA,NA,bty='n'
           ,xlim=c(0,1)
           ,ylim=c(0,1)
           ,axes=F
           ,xlab=''
           ,ylab='')
      
      # adding legend
      legend(x=legLoc[1]                  # x location
             ,y=legLoc[2]                 # y location
             ,legend= leg_sizes_labs      # legend entries rounded
             ,col=alpha.col(legCol,1)     # transparency              # color of points
             ,pch=pc                      # symbol type
             ,bg = alpha.col(col=legCol,alpha=1)    # fill for color
             ,pt.cex=pchMult*round(quantile(pf$size,probs=c(0.1,0.5,0.9)),2)
             ,bty='n'                     # no box
             ,title=colnames(pf)[objs[5]] # legend title
             ,cex=legCex                  # character expansion
             ,y.intersp=legyinsp          # vertial spacing of entries
             ,horiz=legHoriz # vertical or horizontal orientation
      )
      
      # empty plot for where color bar legend will be
      plot(NA,NA
           ,bty='n'
           ,xlim=c(0,1)
           ,ylim=c(0,1)
           ,axes=F
           ,xlab=''
           ,ylab=''
      )
      
      # color bars
      for(j in 1:length(tspLevsUse)){
        if(j == 1){
          colTitle <- colnames(pf)[objs[4]]
        }else{
          colTitle <- ''
        }
        add.color.bar(leg=cbLen                       # length of color bar
                      ,cols=alpha.col(col=cb_cols,alpha=tspVals[j]) # colors, with alpha for transparency
                      ,title=''                       # title, only for top color bar
                      ,lims=cbLim                     # limits for color bar
                      ,digits=cbDig                   # number of digits in rounding entries
                      ,prompt=F                       # do not prompt user for location
                      ,x=cbx                          # x location
                      ,y=cby + (j-1)*cbdy             # y location            # y location
                      ,subtitle=''                    # subtitle for plot
                      ,lwd=cblwd                      # width of the color bar
                      ,outline=cbout                  # outline the color bar
                      ,cex=cbCex                      # 
        )
        # adding header text
        text(x=cbx+cbLen/2      # x location
             ,y=cby-2*cbdy     # y location
             ,pos=2     # above
             ,labels=colTitle # title
             ,cex=cbCex # character expansion
        )
        
        # adding a text for the transparency
        text(x=cbx
             ,y=cby+(j-1)*cbdy
             ,labels=paste(colnames(pf)[objs[6]],' = ',round(tspLevsUse[j],2),sep='')
             ,cex=tspCex
             ,pos=2)
      } # close of color bar loop
    } # close for angles
  } # close expression for plotting in video
  
  ,video.name=aniName # video name
  ,ani.height=aniH    # video height (pixels)
  ,ani.width=aniW     # video width (pixels)
  ,interval=aniInt    # interval between frames
  ) # end of saveVideo()

  # returning data frame with calculations
  return(pf)
  
} # close of function