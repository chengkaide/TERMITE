# ######################################################################### #
# ######################################################################### #
#									                                                          #
#			                  	TERMITE                                          	#
#								                                                      	    #
#			An R-Script for fast reduction of LA_ICPMS data and                   #
#       its application to trace element measurements                       #
#                                                                           #
#             			      Version 1_rev                                     #
#                                                                           #
#			                	Simon Mischel				                                #
#									                                                          #
#		Speleothem Research Group, University Mainz		                          #
#									                                                          #
#			eMail: simon.mischel@uni-mainz.de		                                  #
#									                                                          #
# ######################################################################### #
# ######################################################################### #
### find . -name "*.csv" -type f -exec cp {} ~/your_folder \;

# installs all required packages
rm(list=ls(all=TRUE))                                                             # delete the internal R memory to avoid duplicates
if (!require("miscTools"))   install.packages("miscTools", dependencies = TRUE)	  # on first run package will be installed if not available
if (!require("matrixStats")) install.packages("matrixStats", dependencies = TRUE)	# on first run package will be installed if not available
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)	# on first run package will be installed if not available
if (!require("reshape2")) install.packages("reshape2", dependencies = TRUE)	# on first run package will be installed if not available

# Directory section 
path			            <- "~/your_main_directory/"	                                # defining the working-directory of the script
path.rawData.samples	<- "Rawdata_linescan"   	      	                          # defining the directory of the raw-data-files
path.rawData.RefMat 	<- "ReferenceMaterial_linescan"                             # defining the directory of the reference material
path.corrData.results	<- "Results/"          		                                  # defining the directory for the overview-plots

sample.name	        	<- "your_sample_name_linescan"	                            # is printed in the file-names
  
##### before starting the script check the HelpValue Sections !! #####

##### general HelpValue Section #####
Mode.of.Scan                <- "line.scan"        # spot.scan or line.scan 
Line.of.Header              <- 3                  # the line where the measured isotopes are listed
Line.of.Signal              <- 4                  # the line where the raw data start
measured.isotopes	          <- 12			          	# number of analysed isotopes
column.IS		                <- 5				          # column of internal standard in the rawdata-file
IS 		          	          <- 400003.81	        # [µg/g] internal standard
outlier.Test                <- "Y"                # "Y" or "N"
m              		          <- 30 				        # percentage for the range of the outlier test
number.sweeps.sample        <- 6996				        # No. of sweeps (e.g. overall number of rows)
number.sweeps.Refs          <- 550                # No. of sweeps (e.g. overall number of rows)
resolution		              <- "(LR)"		        	# Resolution "(LR)""(MR)""(HR)" (needed for Thermo Element2 ICP-MS)
machine                     <- "Agilent"          # Element2 or Agilent

background.correction	      <- "median"		  	    # method for background.correction (mean <-> median)


##### HelpValue for Linescans Section #####
first.blankValue.linescan   <- 5			            # first Blank-Value (e.g. line number) used for background-correction
last.blankValue.linescan		<- 124				        # last  Blank-Value (e.g. line number) used for background-correction

first.sampleValue.linescan  <- 143				        # first Laser-on-sample-Value (e.g. line number) used for analysis
last.sampleValue.linescan	  <- 6890			          # last  Laser-on-sample-Value (e.g. line number) used for analysis

laser.speed                 <- 5                  # laser scan speed in µm/s

##### HelpValues for spot scans and/or reference material Section #####
first.blankValue	          <- 5			            # first Blank-Value (e.g. line number) used for background-correction
last.blankValue		          <- 124				        # last  Blank-Value (e.g. line number) used for background-correction

first.sampleValue	          <- 130				        # first Laser-on-sample-Value (e.g. line number) used for analysis
last.sampleValue	          <- 545		        		# last  Laser-on-sample-Value (e.g. line number) used for analysis



##### Reference material Section

##### Values from the GeoReM database (www.http://georem.mpch-mainz.gwdg.de)
##### MPI-DING ATHO-G (ATHO), MPI-DING KL2-G (KL2-G)
##### MPI-DING StHs6/80-G (StHs), MPI-DING T1-G (T1-G)
##### BAM-S005B (BAM-B)
##### USGS MACS-1 (MACS1), USGS MACS-3 (MACS3), USGS GSD-G1 (GSD)
##### NIST SRM 610 (NIST610), NIST SRM 612 (NIST612)

RefMat1     <- "NIST612"	; Reference.Material <- c(RefMat1)		 		            # 1. referencematerial
#RefMat2     <- "MACS3"	  ; Reference.Material <- c(Reference.Material,RefMat2)	# 2. referencematerial
#RefMat3    <- "GSD"  	  ; Reference.Material <- c(Reference.Material,RefMat3)	# 3. referencematerial
#RefMat4    <- "KL2-G"	  ; Reference.Material <- c(Reference.Material,RefMat4)	# 4. referencematerial
#RefMat5    <- "BAM-B"	  ; Reference.Material <- c(Reference.Material,RefMat5)	# 5. referencematerial
#RefMat6    <- "T1-G"	    ; Reference.Material <- c(Reference.Material,RefMat6)	# 6. referencematerial
#RefMat7    <- "MACS1"	  ; Reference.Material <- c(Reference.Material,RefMat7)	# 7. referencematerial
#RefMat8    <- "StHs"	    ; Reference.Material <- c(Reference.Material,RefMat8)	# 8. referencematerial
#RefMat9    <- "ATHO-G"	  ; Reference.Material <- c(Reference.Material,RefMat9)	# 9. referencematerial
#RefMat10  <- "NIST610" 	; Reference.Material <- c(Reference.Material,RefMat10)	# 10. referencematerial

##### End HelpValues #####


##### do not change #####
sampleValues		<- (last.sampleValue-first.sampleValue)+1			# ???
sampleValue.all.linescan <- (last.sampleValue.linescan-first.sampleValue.linescan)+1
path.notChange		<- sprintf("%s%s",path,"TERMITEScriptFolder/")				# the directory containing the script-files


if (Mode.of.Scan == "spot.scan") {
 source(sprintf("%s%s",path.notChange,"Correction_Script_spot_scan_TERMITE.r"))
} else {
    source(sprintf("%s%s",path.notChange,"Correction_Script_line_scan_TERMITE.r"))
}

#### end skript
