##### Auswertungs-Skript StaLAgmite_ICPMS #####
##### do not change this file #####
# set working directory
 setwd(path)

# truncate time in rawData
 column.IS       <- column.IS - 1
 Line.of.Header  <- Line.of.Header - 1
 Line.of.Signal  <- Line.of.Signal - 1
  
# subtract line.of.Signal due to header of file
  number.sweeps.Refs   <- number.sweeps.Refs   - Line.of.Signal
  number.sweeps.sample <- number.sweeps.sample - Line.of.Signal
  
  
  first.blankValue <- first.blankValue - Line.of.Signal
  last.blankValue  <- last.blankValue - Line.of.Signal
  
  first.sampleValue <- first.sampleValue - Line.of.Signal
  last.sampleValue  <- last.sampleValue - Line.of.Signal
  
  first.blankValue.linescan   <- first.blankValue.linescan - Line.of.Signal
  last.blankValue.linescan    <- last.blankValue.linescan - Line.of.Signal
  
  first.sampleValue.linescan  <- first.sampleValue.linescan - Line.of.Signal
  last.sampleValue.linescan   <- last.sampleValue.linescan - Line.of.Signal
  
  m <- m/100
  
  ########################################################################
  #### INPUT SECTION for Data files line scan section reference material #
  ########################################################################
  # all variables in the INPUT SECTION should be formated ################
  # like the original ones to fit the data from other MASS SPECTROMETERS #
  ########################################################################
  
  #################################################
  # check the second input further down this file #
  #################################################

# Read data from the reference material files (long version)
 alleDatenRefs <- list()
 
  for (i in seq(along=Reference.Material)) {
   alleDatenRefs[[i]] <- list.files(sprintf("%s%s",path,path.rawData.RefMat),full.names=TRUE,pattern=Reference.Material[i])
 }


# read data from the reference material files (only filenames)
 alleDatenRefsCount <- matrix()
  
 for (i in seq(along=Reference.Material)) {
  alleDatenRefsCount <- matrix(c(alleDatenRefsCount,list.files(sprintf("%s%s",path,path.rawData.RefMat),full.names=FALSE,pattern=Reference.Material[i])))
 }

 alleDatenRefsCount <- alleDatenRefsCount[-1, ] 

 
 #########################
 #### READ Data files ####
 #########################
 # check seperator for other MASS SPECTROMETER
 
 # fill LISTE with all the datafiles
 LISTE<-list()
 
 if (machine=="Element2") {
   for (j in seq(along=Reference.Material)) {
     for (i in 1:length(alleDatenRefs[[j]])) {
       LISTE[length(LISTE)+1] <- list(read.table(alleDatenRefs[[j]][i],header=FALSE,skip=Line.of.Signal,sep="\t",nrow=number.sweeps.Refs)[,2:(measured.isotopes+1)])
     }
   }
 }  else {
   for (j in seq(along=Reference.Material)) {
     for (i in 1:length(alleDatenRefs[[j]])) {
       LISTE[length(LISTE)+1] <- list(read.table(alleDatenRefs[[j]][i],header=FALSE,skip=Line.of.Signal,sep=",",nrow=number.sweeps.Refs)[,2:(measured.isotopes+1)])
     }
   }
 }
 
 
 #####################
 #### READ Header ####
 #####################
 # check seperator for other MASS SPECTROMETERS
 
 # Header einlesen
 if (machine=="Element2") {
   Kopf <- as.matrix(read.table(alleDatenRefs[[1]][1],skip=Line.of.Header,header=FALSE,sep="\t")[1,2:(measured.isotopes+1)])
   Kopf <- gsub(resolution,"",Kopf,fixed=TRUE)
 } else {
   Kopf <- as.matrix(read.table(alleDatenRefs[[1]][1],header=FALSE,skip=Line.of.Header,sep=",")[1,2:(measured.isotopes+1)])
 }
 

# Einlesen der Standards nach der GeoReM-Database
  Standards_all <- read.table(sprintf("%s%s",path.notChange,"Standards_GeoReM.csv"),header=TRUE,sep=",",row.names=1)


# Einlesen der Atomgewichte und der Isotopenhäufigkeit
  IsotopWerte_all <- read.table(sprintf("%s%s",path.notChange,"AtomGewIsoAbund_NIST.csv"),header=TRUE,sep="\t")

  
  ##########################################
  #### END INPUT SECTION for Data files ####
  ##########################################
  

  Kopf.rows <- c(alleDatenRefsCount)
  
  r <- 3				# MPI Standard				# number of referenceglass-meassurements per referenceglass-meassurement
  
  
# extract elementname from isotope  
  Element <- sub("^([[:alpha:]]*).*", "\\1", Kopf)
  
  
# extraction of isotopic values and reference materials
  IsotopWerte <- subset(IsotopWerte_all,select=Kopf)
  Standards <- subset(Standards_all,select=Kopf)


# eliminates E-values introduced during data saving process (Element2)
  EWerte <- list()
      
  for (i in seq(along=LISTE)) {
	  EWerte[[i]] <- sapply(LISTE[[i]],function(x) {as.numeric(gsub("[^E0-9.0-9]", "NA", x))})
  }
  
  LISTE <- EWerte


# calculation of the medianGasBlank
  medianGasBlank <- list()
  
  if (background.correction=="mean") {
   for (i in seq(along=LISTE)) {
	  medianGasBlank[[i]] <- matrix(c(colMeans(LISTE[[i]][first.blankValue:last.blankValue,],na.rm=TRUE)),ncol=measured.isotopes,nrow=last.sampleValue,byrow=TRUE)
	  }
   } else {
    for (i in seq(along=LISTE)) {
	   medianGasBlank[[i]] <- matrix(c(colMedians(LISTE[[i]][first.blankValue:last.blankValue,],na.rm=TRUE)),ncol=measured.isotopes,nrow=last.sampleValue,byrow=TRUE)
	 }
  }


# cps Probe - medianGasBlank 
  X <- matrix(0,nrow=last.sampleValue,ncol=measured.isotopes)
  Y <- list()

  for( i in seq(along=LISTE)) {
	  X[first.sampleValue:last.sampleValue,] <- as.matrix(LISTE[[i]][first.sampleValue:last.sampleValue,] - medianGasBlank[[i]][first.sampleValue:last.sampleValue,])
	  Y[[i]] <- as.data.frame(X[first.sampleValue:last.sampleValue,])
	}

  
  # calculate the LoD
  LoD <- list()
  
  for (i in seq(along=LISTE)) {
    LoD[[i]] <- matrix(c(
      ifelse(3*colSds(LISTE[[i]][first.blankValue:last.blankValue,],na.rm=TRUE)==0,1,3*colSds(LISTE[[i]][first.blankValue:last.blankValue,],na.rm=TRUE))
                       /
                         as.numeric(colMeans(Y[[i]]))
                       * 
                         as.numeric((matrix(Standards[eval(RefMat1), ],nrow=1,byrow=TRUE)))
                       )
                       ,ncol=measured.isotopes,nrow=last.sampleValue,byrow=TRUE)
  }
  
  
  # extract the Limit of Detection for the reference material (RefMat1) only  
  w.LoD <- matrix(NA,nrow=(length(Kopf.rows)-length(alleDatenRefs)),ncol=measured.isotopes)
  
  for (i in   (length(Kopf.rows) - (length(Kopf.rows) - length(alleDatenRefs))+1):length(Kopf.rows)) {
    w.LoD[i - (length(Kopf.rows) - (length(Kopf.rows) - length(alleDatenRefs))),] <- LoD[[i]][1, ]
  }
  
  
  # plot the Limit of Detection    
  pdf(sprintf("%s%s%s.%s",path.corrData.results,"LoD_ReferenceMaterial_",sample.name,"pdf"),width=12)
  par(las=1)
  plot(colMedians(w.LoD,na.rm=TRUE),type="p",log="y",ylab="LoD",xlab="Element",xaxt="n")
  axis(1, at=seq(1,measured.isotopes,1),labels=Element)
  dev.off()
  
  
  # save the Limit of Detection    
  write.table(round(colMedians(w.LoD,na.rm=TRUE),digits=5),sprintf("%s%s%s.%s",path.corrData.results,"LoD_ReferenceMaterial_",sample.name,"csv"),sep="\t",row.names=Element,col.names="LoD")
  detach("package:matrixStats", unload=TRUE)
  

# calculates values relative to IS
  ISNormSample <- list()
  
  for (i in seq(along=LISTE)) {
	 ISNormSample[[i]] <- Y[[i]] / (LISTE[[i]][first.sampleValue:last.sampleValue,column.IS] - medianGasBlank[[i]][first.sampleValue:last.sampleValue,column.IS])
	}
  ISN <- ISNormSample   


# is just used if outlier.Test == "N"
  ISN.noOutlier <- ISNormSample
  

# calculates the median of values relative to IS
  medianISNormSample <- list()
  for (i in seq(along=ISNormSample)) {
	 medianISNormSample[[i]] <- sapply(ISNormSample[[i]],median,na.rm=TRUE)
	}

	  
# defines the upper boundary of the outlier test
  FilterA <- list()
      for (i in seq(along=medianISNormSample)) {
	  FilterA[length(FilterA)+1] <- list(matrix(c(medianISNormSample[[i]] + medianISNormSample[[i]] * m),ncol=measured.isotopes,nrow=last.sampleValue,byrow=TRUE))
	  }


# defines the lower boundary of the outlier test
  FilterB <- list()

  for (i in seq(along=medianISNormSample)) {
	 FilterB[length(FilterB)+1] <- list(matrix(c(medianISNormSample[[i]] - medianISNormSample[[i]] * m),ncol=measured.isotopes,nrow=last.sampleValue,byrow=TRUE))
	}


# eliminates outliers
  gef <- matrix(0,ncol=measured.isotopes,nrow=last.sampleValue-first.sampleValue+1)
  
  gefilterteWerte <- list()    

  for (i in seq(along=ISNormSample)) {
	  gef[1:(last.sampleValue-first.sampleValue+1),] <- as.matrix({ISNormSample[[i]][1:(last.sampleValue-first.sampleValue+1),][ISNormSample[[i]][1:(last.sampleValue-first.sampleValue+1),] > FilterA[[i]][1:(last.sampleValue-first.sampleValue+1),]] <- NA;
	  ISNormSample[[i]][1:(last.sampleValue-first.sampleValue+1),][ISNormSample[[i]][1:(last.sampleValue-first.sampleValue+1),] < FilterB[[i]][1:(last.sampleValue-first.sampleValue+1),]] <- NA;
	  ISNormSample[[i]][1:(last.sampleValue-first.sampleValue+1),]})
	  gefilterteWerte[length(gefilterteWerte)+1]<- list(as.data.frame(gef))
  }
  
  
  ISF <- gefilterteWerte
# override the  outlier Test if intended
  if (outlier.Test=="N") { 
	  gefilterteWerte <- ISN.noOutlier
	  } else {
	  gefilterteWerte <- gefilterteWerte
	}
  
 
  # plot outlier test
  pdf(sprintf("%s%s%s.%s",path.corrData.results,"Outlier_Test_",sample.name,"pdf"))
  par(mfrow=c(2,2),mar=c(5.5,5,1,1),las=1,mgp=c(4,1,0))
  
  for (j in seq(along=ISN))  {
    for (i in 1:measured.isotopes)    {
	plot(ISN[[j]][ ,i],type="b",ylab=c(paste(Kopf[i],"rel_to_IS")),xlab=Kopf.rows[j])
	abline(h=(median(ISN[[j]][ ,i],na.rm=TRUE)+median(ISN[[j]][ ,i],na.rm=TRUE)*m),col="black")
	abline(h=(median(ISN[[j]][ ,i],na.rm=TRUE)-median(ISN[[j]][ ,i],na.rm=TRUE)*m),col="black")
      
	points(ISF[[j]][ ,i],type="p",col="red",pch=16)
	legend("topleft",inset=0.02,c(paste("boundary outlier test. m=",m),"incorporated values"),col=c("black","red"),pch=c(NA,16),lty=c(1,1,1))
    }
  }
  dev.off()
  
  
   
# mean of the filtered values
  Auswertung <- list()
  
  for (i in seq(along=gefilterteWerte)) {
	  Auswertung[length(Auswertung)+1] <- list(colMeans(gefilterteWerte[[i]],na.rm=TRUE))
	}


# (isotopAbundance(Ca)/isotopAbundace(Element))*(isotopWeigth(Element)/IsotopWeigth(Ca)))
  IsotopBerechnung <- matrix(IsotopWerte[2,column.IS] / IsotopWerte[2,1] * IsotopWerte[1,1] / IsotopWerte[1,column.IS])
  
  for (i in 2:measured.isotopes) {
	  IsotopBerechnung[length(IsotopBerechnung)+1] <- matrix(IsotopWerte[2,column.IS] / IsotopWerte[2,i] * IsotopWerte[1,i] / IsotopWerte[1,column.IS])
	}

  
# isotopic abundance relative to IS * measured values
  AbundanzAuswertung <- matrix(0,ncol=measured.isotopes,nrow=length(LISTE),byrow=TRUE)
  
  for (i in seq(along=Auswertung)) {
	  AbundanzAuswertung[i,] <- Auswertung[[i]] * IsotopBerechnung
	  }


##### Referenzgläser und mean RSF 
# helpvalues
  helpValue.1 <- matrix(0,ncol=measured.isotopes)
  
#if(machine=="Element2") {
  for ( i in seq(along=alleDatenRefs)) {
	  helpValue.1 <- rbind(helpValue.1,matrix(Standards[eval(parse(text=paste("RefMat",i,sep=""))),column.IS],ncol=measured.isotopes,nrow=length(alleDatenRefs[[i]]),byrow=TRUE))
	  }

helpValue.1 <- helpValue.1[-1, ]

# uncorrected concentration
  unCorrPpm.all <- AbundanzAuswertung * helpValue.1
  unCorrPpm.Refs <- unCorrPpm.all #unCorrPpm.all[-1:-(length(alleDaten)),]	# Auftrennen nach Referenzgläser


# helpvalues for the reference materials 
  helpValue.2<-matrix(0,ncol=measured.isotopes,nrow=1)
  
for (i in seq(along=alleDatenRefs)) {
 helpValue.2 <- rbind(helpValue.2,matrix(as.matrix(Standards[eval(parse(text=paste("RefMat",i,sep=""))),]),ncol=measured.isotopes,nrow=length(alleDatenRefs[[i]]),byrow=TRUE))
}
  
 helpValue.2 <- helpValue.2[-1,]
  

# calcualation for corrected reference materials
  CorrPpm.Refs <- ifelse((unCorrPpm.Refs / helpValue.2)==0,NA, (unCorrPpm.Refs / helpValue.2))


# plot RSF
if (length(alleDatenRefs)>1)  {
  helpValue.RSF <- matrix(length(alleDatenRefs[[1]]),nrow=length(alleDatenRefs))
      for (i in 2:length(alleDatenRefs)) {
	helpValue.RSF[i,] <- helpValue.RSF[i-1,]+length(alleDatenRefs[[i]])
	}; helpValue.RSF <- helpValue.RSF[-length(alleDatenRefs),]
	} else {
  helpValue.RSF <- matrix(length(alleDatenRefs[[1]]),nrow=(length(alleDatenRefs)))
  }


pdf(sprintf("%s%s%s.%s",path.corrData.results,"RSFused_",sample.name,"pdf"))
  par(mfrow=c(2,2),mar=c(4,4,1,1),las=1)
      for (i in 1:measured.isotopes) if (all(is.na(CorrPpm.Refs[,i]))==FALSE)
	  {
	  plot(CorrPpm.Refs[,i],type="b",xlab=paste(paste(Reference.Material,collapse=" | ")),ylab=paste("mean RSF",Element[i]))
	  abline(h=mean(CorrPpm.Refs[,i],na.rm=TRUE),lty=3,lwd=2)
	  abline(v=helpValue.RSF + 0.5,lty=2)
	  } else {next}
  dev.off()
#  }


# calculate the mean of the RSF of all reference materials
  RSFused <- colMeans(CorrPpm.Refs,na.rm=TRUE)

  
write.table(round(RSFused,digits=2),(sprintf("%s%s%s.%s",path.corrData.results,"RSFused_",sample.name,"csv")),sep="\t",row.names=Element)


## read the RSFused for linescans calculated
RSFused.LS <- as.matrix(RSFused)


########################################################
#### INPUT SECTION for Data files line scan section ####
########################################################################
# all variables in the INPUT SECTION should be formated ################
# like the original ones to fit the data from other MASS SPECTROMETERS #
########################################################################


Daten.linescan <- list.files(sprintf("%s%s",path,path.rawData.samples),full.names=TRUE)

if (machine=="Element2"){
	sample.linescan <- read.table(Daten.linescan,sep="\t",skip=Line.of.Signal)[,2:(measured.isotopes+1)]
	sample.time <- as.numeric(as.character(read.table(Daten.linescan,sep="\t",skip=Line.of.Signal)[1:(last.sampleValue.linescan-first.sampleValue.linescan+1),1]))
	} else{
		sample.linescan <- read.table(Daten.linescan,sep=",",skip=Line.of.Signal)[,2:(measured.isotopes+1)]
		sample.time <- as.numeric(as.character(read.table(Daten.linescan,sep=",",skip=Line.of.Signal)[1:(last.sampleValue.linescan-first.sampleValue.linescan+1),1]))
	}

line.length <- sample.time*laser.speed/1000


# read Header
if (machine=="Element2"){
	  Kopf <- as.matrix(read.table(Daten.linescan,header=FALSE,skip=Line.of.Header,sep="\t")[1,2:(measured.isotopes+1)])
	  } else {
	  Kopf <- as.matrix(read.table(Daten.linescan,header=FALSE,skip=Line.of.Header,sep=",")[1,2:(measured.isotopes+1)])
	} 

  Kopf <- gsub("(LR)","",Kopf,fixed=TRUE)

  
  ##########################################
  #### END INPUT SECTION for Data files ####
  ##########################################
  
  
# change names according to the Elements
names(sample.linescan) <- Kopf


# eliminate potential E-values
EWerte <- list()
EWerte[[1]] <- sapply(sample.linescan,function(x) {as.numeric(gsub("[^E0-9.0-9]", "NA", x))})
sample.linescan <- as.data.frame(EWerte[[1]])


## plot cps raw Data
pdf(sprintf("%s%s%s.%s",path.corrData.results,"rawCountrate_",sample.name,"pdf"))

df.liste <- as.data.frame(sample.linescan)
af <- rep(1:length(sample.linescan[,1]),measured.isotopes)
names(df.liste) <- c(Kopf)
df.liste.long <- melt(df.liste)
df_l <- cbind(af,df.liste.long)

p <- ggplot(df_l,aes(x=af,y=value))+
  geom_line() +
  scale_y_log10()+
  facet_wrap(~variable)+labs(x=paste("No. of sweeps - linescan:",sample.name),y="cps")+
  theme_bw()

print(p)


for(i in seq(along=LISTE)){
  
  df.liste <- as.data.frame(LISTE[[i]])
  af <- rep(1:length(LISTE[[1]][,1]),measured.isotopes)
  names(df.liste) <- c(Kopf)
  df.liste.long <- melt(df.liste)
  df_l <- cbind(af,df.liste.long)
  
  p <- ggplot(df_l,aes(x=af,y=value))+
    geom_line() +
    scale_y_log10()+
    facet_wrap(~variable)+labs(x=paste("No. of sweeps - spot:",Kopf.rows[i]),y="cps")+
    theme_bw()
  
  print(p) 
}
dev.off()


## background Median
background <- matrix(colMedians(sample.linescan[first.blankValue.linescan:last.blankValue.linescan,]),nrow=sampleValue.all.linescan,ncol=measured.isotopes,byrow=TRUE)
colnames(background) <- Kopf


## background corrected cps  
sample.linescan.bg <- sample.linescan[first.sampleValue.linescan:last.sampleValue.linescan,]-background
names(sample.linescan.bg) <- Kopf

sample.linescan.Ca <- sample.linescan.bg / sample.linescan.bg[,column.IS]  
names(sample.linescan.Ca) <- Kopf


IsotopBerechnung <- matrix(IsotopWerte[2,column.IS] / IsotopWerte[2,1] * IsotopWerte[1,1] / IsotopWerte[1,column.IS])
for (i in 2:measured.isotopes){
	  IsotopBerechnung[length(IsotopBerechnung)+1] <- matrix(IsotopWerte[2,column.IS] / IsotopWerte[2,i] * IsotopWerte[1,i] / IsotopWerte[1,column.IS])
	  }


## matrix for Abundance of isotopes and weight
IsoB <- matrix(IsotopBerechnung,nrow=sampleValue.all.linescan,ncol=measured.isotopes,byrow=TRUE)
colnames(IsoB) <- Kopf


## IS-nomalized sample multiplicate with isotopic abundances
sample.linescan.CaIsoNorm <- sample.linescan.Ca * IsoB


## RSFused calculations for line scans
RSFused.sample.linescan <- matrix(RSFused.LS[,1],nrow=sampleValue.all.linescan,ncol=measured.isotopes,byrow=TRUE)
colnames(RSFused.sample.linescan) <- Kopf


## uncorrected concentration / RSFused
CorrConc <- sample.linescan.CaIsoNorm*IS/RSFused.sample.linescan


# apply Limit of Detection
limit <- matrix(colMedians(w.LoD,na.rm=TRUE),nrow=length(CorrConc[,1]),ncol=measured.isotopes,byrow=TRUE)
	CorrConc.limit <- ifelse(as.matrix(CorrConc)>=limit,as.matrix(CorrConc),NA)
	CorrConc.limit <- ifelse(CorrConc.limit>=0,CorrConc.limit,NA)


# write the concentrations of the linescan
  write.table(cbind(line.length,CorrConc.limit),(sprintf("%s%s%s.%s",path.corrData.results,"Results_",sample.name,"csv")),sep="\t",row.names=FALSE,col.names=as.matrix(cbind("length",Element)))

  
  # plot the concentrations of the linescan  
  pdf(sprintf("%s%s%s.%s",path.corrData.results,"Results_",sample.name,"pdf"),useDingbats=FALSE)
  par(mfrow=c(2,2),mar=c(5,5,1,1),las=1,mgp=c(3.5,1,0))
  for (i in 1:measured.isotopes)
    if(i != column.IS) { 
      if (all(is.na(CorrConc[,i]))==FALSE)
        if (all(is.na(CorrConc.limit[,i]))==FALSE){
          plot(CorrConc.limit[,i]~line.length,type="l",xlab=paste("Distance - ",sample.name),ylab=paste(Element[i]," [µg/g]"))
        } else {next}
    } else {next}
  dev.off()
  
##### End of the script TERMITE #####
