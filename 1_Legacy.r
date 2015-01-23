# Comments JF March 3 2014):
# 1) Nicholas Taper file is not reading in my script, JF; can it be deleted from this script?
# 2) GW, sp. gr. MC... 12 new columns added including an indicator column for abnormal cases (i.e Bridge data is DW_TOT ind 1 means; 
#	DW_TOT to 1"; most of these fields are not checked yet;
#	MC is only added for Bridge whole tree MC; might be better to just reserved the MC field for stem wood; or multiple MC fields:
# 	bark, branch, stemwood, probably excessive?
# 3) Lykins_1995 added
# 4) MC field added, but only entered for Bridge; will MC just be for stem wood? or should we have
# sep MC for all components? or just calculate when we have GW and DW
# 
#Change basepath to location of legacy data
library(plyr)
library(dplyr)
basepath <- "/home/dwalker/Documents/Work/Data/Legacy_Data_Sets/Legacy3/000_All/"
#basepath <- "C:\\FIA\\LegacyDataBase\\LegacyDatasets\\"
Legacy<-data.frame()

###############
# BRIGGS 1989 #
###############

Briggs<-read.csv(paste(basepath,"Briggs_1989.csv",sep=""),header=T, as.is= T)
#str(Briggs)
#names(Briggs)
Briggs$AUTHOR<-'Briggs'			# AUTHOR + LOC links the TREE table with the LEGACY LOCATION TABLE
Briggs$LOC<-1				# 
Briggs$SPCD<-Briggs$FIA.SPCD
Briggs$TREENO<-Briggs$Tree
Briggs$CCLCD<-"NA"
Briggs$CW<-"NA"
Briggs$H_STUMP<-0.5				# 6 inches assumed in Jenkins; not reported in Briggs
Briggs$DBH<-Briggs$DBH_cm/2.54						# Table 3, p. 9
Briggs$HT<-Briggs$Total_Height_m*3.28084					# Table 3 p. 9
Briggs$HL_BRANCH<-"NA"
Briggs$DW_SW<-Briggs$Bole_wood_kg*2.20462					# DW:FW ratio, p. 3; 4 in Jenkins
Briggs$DW_SB<-Briggs$Bole_Bark_kg*2.20462					# DW:FW, p. 3; 5 in Jenkins
Briggs$DW_STEM<-Briggs$DW_SB+Briggs$DW_SW					# stem dissected into 3 sections of equal length 1 disk/section at random point withing section
Briggs$DEAD_BRANCH_DW<-NA
Briggs$LIVE_BRANCH_DW<-NA
Briggs$DW_BRANCH<-Briggs$Branch_kg*2.20462		# all woody material < 10 cm = above ~ 4" top			
# Branch assumed to be live and dead in Jenkins (13); DW/GW ratio (ratio of means); 
# p. 3; 10 branch samples on Beech; based on analysis on beech 5 branch samples sufficient (+/- 10%) and used for maple
Briggs$DW_TOT<-Briggs$DW_STEM+Briggs$DW_BRANCH
Briggs$DW_FOL<-Briggs$Foliage_kg*2.20462					# 100% sample of foliage; p. 3; 18 in Jenkins
Briggs$BIOMASS<-Briggs$DW_STEM+Briggs$DW_BRANCH+Briggs$DW_FOL 	# Jenkins comp 3 yes
Briggs$STUMP<-NA
Briggs$ABG<-NA
Briggs$ORIGIN<-"1"								# Assumed
Briggs$TEMP<-60									# 60 degrees C until constant weight was obtained
Briggs$DGL<-NA
Briggs$DSTEM_MIN<-4			# close enough to 4, p. 2 "branches =all woody material < 10 cm in diameter 
Briggs$DBLC<-NA
Briggs$DW_CROWN<-Briggs$DW_BRANCH+Briggs$DW_FOL
Briggs$ST_METH<-'1'
Briggs$BR_METH<-'5'
Briggs$FOL_METH<-'1'
Briggs$REGION<-'NE'
Briggs$Indicator <- 'unchecked'
Briggs$GW_STEM<- NA
Briggs$GW_BRANCH<-'unchecked'
Briggs$GW_FOL<-'unchecked'
Briggs$GW_CROWN<-'unchecked'
Briggs$GW_BIOMASS<-'unchecked'
Briggs$GW_ABG<-'unchecked';Briggs$GW_TOT<-'unchecked'
Briggs$M.C.<-'unchecked'
Briggs$SP.GR<-'unchecked'
Briggs$Bark.Fraction<-'unchecked'; 
Briggs$Age<-'unchecked'
names(Briggs)

Briggs1989<-Briggs[,c(11:53)]		

Legacy<-rbind(Legacy,Briggs1989)
#plot(Legacy$DBH,Legacy$DW_STEM)

#dim(Legacy) # 30
#names(Legacy)
#######################
# Carter & Smith 1971 # # Populus deltoides; Alabama; Should be Carter_Smith, did not change yet, until change both TREE and LOC sheets
#######################

C_S_71<-read.csv(paste(basepath,"Carter_Smith_1971.csv",sep=""),header=T, as.is= T)
#str(C_S_71)
#(C_S_71)
#names(C_S_71)
C_S_71$AUTHOR<-'Carter_Smith'
C_S_71$LOC<-1							# there are multiple stands but they are geographically close
C_S_71$SPCD<-C_S_71$FIA.SPCD
C_S_71$TREENO<-c(1:length(C_S_71[,1]))
C_S_71[C_S_71$Crown.Class=="D","CCLCD"]<-2		# no code for open grown or intermediate
C_S_71[C_S_71$Crown.Class=="S","CCLCD"]<-5
C_S_71[C_S_71$Crown.Class=="CD","CCLCD"]<-3
C_S_71$CW<-"NA"
C_S_71$H_STUMP<-0			# 0 inches assumed in Jenkins and reported in C_S_71
C_S_71$DBH<-C_S_71$DBHOB_in
C_S_71$HT<-C_S_71$Height
C_S_71$HL_BRANCH<-"NA"
C_S_71$DW_SW<-C_S_71$Bole.Wood..lb.
C_S_71$DW_SB<-C_S_71$Bole.Bark..lb.
C_S_71$DW_STEM<-C_S_71$DW_SB+C_S_71$DW_SW
C_S_71$DEAD_BRANCH_DW<-NA
C_S_71$LIVE_BRANCH_DW<-NA
C_S_71$DW_BRANCH<-C_S_71$Branches..lb.				# branches are both live and dead (as per Jenkins table)
C_S_71$DW_TOT<-C_S_71$DW_BRANCH+C_S_71$DW_STEM
C_S_71$DW_FOL<-C_S_71$Foliage..lb.
C_S_71$BIOMASS<-C_S_71$DW_TOT+C_S_71$DW_FOL
C_S_71$STUMP<-NA
C_S_71$ABG<-C_S_71$BIOMASS
C_S_71[C_S_71$Stand=="1"|C_S_71$Stand=="2","ORIGIN"]<-"2"	# plantations = 1& 2 N; 3 + one other natural (could not determine which 
C_S_71[C_S_71$Stand=="3"|C_S_71$Stand=="4"|C_S_71$Stand=="5"|C_S_71$Stand=="6"|C_S_71$Stand=="7"|C_S_71$Stand=="8","ORIGIN"]<-"1"	
C_S_71$TEMP<-65
C_S_71$DGL<-NA
C_S_71$DSTEM_MIN<-'Unchecked'
C_S_71$DBLC<-NA
C_S_71$DW_CROWN<-C_S_71$DW_BRANCH+C_S_71$DW_FOL
C_S_71$ST_METH<-'1'
C_S_71$BR_METH<-'1'					# no mention of subsampling branches and foliage, assume all weighed
C_S_71$FOL_METH<-'1'
C_S_71$REGION<-'SE'
C_S_71$Indicator <- 'unchecked'
C_S_71$GW_STEM<-'unchecked'
C_S_71$GW_BRANCH<-'unchecked'
C_S_71$GW_FOL<-'unchecked'
C_S_71$GW_CROWN<-'unchecked'
C_S_71$GW_BIOMASS<-'unchecked'
C_S_71$GW_ABG<-'unchecked'
C_S_71$GW_TOT<-'unchecked'
C_S_71$M.C.<-'unchecked'
C_S_71$SP.GR<-'unchecked'
C_S_71$Bark.Fraction<-'unchecked'
C_S_71$Age<-'unchecked'

#names(Legacy)
#names(C_S_71)
Carter_Smith_1971<-C_S_71[,c(12:54)]		
#head(Carter_Smith_1971)
#tail(Carter_Smith_1971)
#names(Carter_Smith_1971)
#summary(Carter_Smith_1971)
#unique(Carter_Smith_1971$SPCD)
#plot(Carter_Smith_1971$DBH[Carter_Smith_1971$SPCD=='742'],Carter_Smith_1971$DW_BRANCH[Carter_Smith_1971$SPCD=='742'])
#plot(Carter_Smith_1971$DBH[Carter_Smith_1971$SPCD=='742'],Carter_Smith_1971$DW_TOT[Carter_Smith_1971$SPCD=='742'])
#plot(Carter_Smith_1971$DBH[Carter_Smith_1971$SPCD=='742'],Carter_Smith_1971$DW_FOL[Carter_Smith_1971$SPCD=='742'])
#plot(Carter_Smith_1971$DBH[Carter_Smith_1971$SPCD=='742'],Carter_Smith_1971$DW_SW[Carter_Smith_1971$SPCD=='742'])
#plot(Carter_Smith_1971$DBH[Carter_Smith_1971$SPCD=='742'],Carter_Smith_1971$DW_SB[Carter_Smith_1971$SPCD=='742'])
#plot(Carter_Smith_1971$DBH[Carter_Smith_1971$SPCD=='742'],Carter_Smith_1971$BIOMASS[Carter_Smith_1971$SPCD=='742'])

Legacy<-rbind(Legacy,Carter_Smith_1971)
#dim(Legacy) #78

#########
# Clark # Southeastern States
#########

# Small trees (< 5" DBH)

Clark.Small<-read.csv(paste(basepath,"Clark_Small.csv",sep=""),header=T, as.is= T)

#(CLARK)
#head(CLARK)
Clark.Small$AUTHOR<-'Clark'
Clark.Small$LOC<-Clark.Small$LOCATION
Clark.Small$SPCD<-Clark.Small$SPECIES  # see Clar_colnames.csv				
Clark.Small$TREENO<-Clark.Small$TREENUMB
Clark.Small$CCLCD<-Clark.Small$CCL		# see Clar_colnames.csv		
Clark.Small$CW<-"NA"
Clark.Small$H_STUMP<-Clark.Small$STHT # Stump height in feet
Clark.Small$DBH<-Clark.Small$DBH		# see Clar_colnames.csv; assume inches 
Clark.Small$HT<-Clark.Small$THT		# see Clar_colnames.csv	
Clark.Small$HL_BRANCH<-Clark.Small$HTBC		# see Clar_colnames.csv	
Clark.Small$DW_SW<-Clark.Small$WT1D # Stem to a 1" top
Clark.Small$DW_SB<-Clark.Small$BK1D # Stem to a 1" top
Clark.Small$DW_STEM<-Clark.Small$DW_SB + Clark.Small$DW_SW
Clark.Small$DEAD_BRANCH_DW<-Clark.Small$DEADD	# see Clar_colnames.csv
Clark.Small$LIVE_BRANCH_DW<-Clark.Small$BCHWBD	# see Clar_colnames.csv
Clark.Small$DW_BRANCH<-rowSums(Clark.Small[, c("DEADD","BCHWBD")], na.rm= T)	# see Clar_colnames.csv
Clark.Small$DW_TOT<-Clark.Small$DW_STEM+Clark.Small$DW_BRANCH	# see Clar_colnames.csv
Clark.Small$DW_FOL<-Clark.Small$FOLD	# see Clar_colnames.csv
Clark.Small[Clark.Small$DW_FOL == 0 &
              !is.na(Clark.Small$DW_FOL), "DW_FOL"] <- NA
Clark.Small$BIOMASS<-Clark.Small$DW_TOT+Clark.Small$DW_FOL	# see Clar_colnames.csv
Clark.Small$STUMP<-NA
Clark.Small$ABG<-NA
Clark.Small$ORIGIN<-NA 
Clark.Small$TEMP<-NA
Clark.Small$DGL<-NA
Clark.Small$DSTEM_MIN<-1 # Small trees 1" top
Clark.Small$DBLC<-Clark.Small$DBC
Clark.Small$DW_CROWN<-Clark.Small$DW_BRANCH+Clark.Small$DW_FOL
Clark.Small$DSTEM_MIN[is.na(Clark.Small$DSTEM_MIN)]<-0
Clark.Small$ST_METH<-'1'
Clark.Small$BR_METH<-'2'
Clark.Small$FOL_METH<-'2'
Clark.Small$REGION<-'S'
summary(Clark.Small)
dim(Clark.Small)
Clark.Small$Indicator <- 'unchecked'
Clark.Small$GW_STEM<-'unchecked'
Clark.Small$GW_BRANCH<-'unchecked'
Clark.Small$GW_FOL<-'unchecked'
Clark.Small$GW_CROWN<-'unchecked'
Clark.Small$GW_BIOMASS<-'unchecked'
Clark.Small$GW_ABG<-'unchecked'
Clark.Small$GW_TOT<-'unchecked'
Clark.Small$M.C.<-'unchecked'
Clark.Small$SP.GR<-'unchecked'
Clark.Small$Bark.Fraction<-'unchecked'
Clark.Small$Age<-'unchecked'

Clark.Small <- (Clark.Small[,c(1,6,18:58)])

# Some clark data uses old species codes
Clark.Small[Clark.Small$SPCD == 899, "SPCD"] <- 800
Clark.Small[Clark.Small$SPCD == 60, "SPCD"] <- 68


Legacy<-rbind.fill(Legacy,Clark.Small)
names(Legacy)
Legacy<-Legacy[,c(1:43)]
dim(Legacy) # 1229
head(Legacy)
# Large Trees (> 5" DBH)

head(Legacy)

Clark.Large<-read.csv(paste(basepath,"Clark_Large.csv",sep=""),header=T, as.is= T)

# remove trees with foliage problems
Clark.Large[Clark.Large$LOC == 187291, "FOLD"] <- NA
Clark.Large[Clark.Large$LOC == 25900, "FOLD"] <- NA
Clark.Large[Clark.Large$LOC == 12876, "FOLD"] <- NA
Clark.Large[Clark.Large$LOC == 6580, "FOLD"] <- NA
Clark.Large[Clark.Large$LOC == 6364, "FOLD"] <- NA
Clark.Large[Clark.Large$LOC == 4371, "FOLD"] <- NA
Clark.Large[Clark.Large$LOC == 5525 & Clark.Large$SPECIES == 621, "FOLD"] <- NA
Clark.Large[Clark.Large$LOC == 11492 & Clark.Large$SPECIES == 802, "FOLD"] <- NA
Clark.Large[Clark.Large$LOC == 11492 & Clark.Large$SPECIES == 611, "FOLD"] <- NA


Clark.Large$AUTHOR<-'Clark'
Clark.Large$LOC<-Clark.Large$LOCATION
Clark.Large$SPCD<-Clark.Large$SPECIES  # see Clar_colnames.csv				
Clark.Large$TREENO<-Clark.Large$TREENUMB
Clark.Large$CCLCD<-Clark.Large$CCL		# see Clar_colnames.csv		
Clark.Large$CW<-"NA"
#mean(Clark.Large$STHT,na.rm=T)	# mean STHT is .456 (feet?)
Clark.Large$H_STUMP<-Clark.Large$STHT	# assume that STHT is in feet; convert to inches 	
Clark.Large$DBH<-Clark.Large$DBH		# see Clar_colnames.csv; assume inches 
Clark.Large$HT<-Clark.Large$THT		# see Clar_colnames.csv	
Clark.Large$HL_BRANCH<-Clark.Large$HTBC		# see Clar_colnames.csv	
Clark.Large$DW_SW<-Clark.Large$WT2D+Clark.Large$WDTIPD		# see Clar_colnames.csv
Clark.Large$DW_SB<-Clark.Large$BK2D+Clark.Large$BKTIPD		# see Clar_colnames.csv
Clark.Large$DW_STEM<-Clark.Large$WTB2D+Clark.Large$WBTIPD	# see Clar_colnames.csv	
Clark.Large$DEAD_BRANCH_DW<-Clark.Large$DEADD	# see Clar_colnames.csv
Clark.Large$LIVE_BRANCH_DW<-Clark.Large$BCHWBD	# see Clar_colnames.csv
Clark.Large$DW_BRANCH<-rowSums(Clark.Large[, c("DEADD","BCHWBD")], na.rm= T)	# see Clar_colnames.csv
Clark.Large$DW_TOT<-Clark.Large$DW_STEM+Clark.Large$DW_BRANCH	# see Clar_colnames.csv
Clark.Large$DW_FOL<-Clark.Large$FOLD	# see Clar_colnames.csv
Clark.Large[Clark.Large$DW_FOL == 0 &
              !is.na(Clark.Large$DW_FOL), "DW_FOL"] <- NA
Clark.Large$BIOMASS<-Clark.Large$DW_TOT+Clark.Large$DW_FOL	# see Clar_colnames.csv
Clark.Large$STUMP<-NA
Clark.Large$ABG<-NA
Clark.Large$ORIGIN<-NA 
Clark.Large$TEMP<-NA
Clark.Large$DGL<-NA
Clark.Large$DSTEM_MIN<-0 
Clark.Large$WDTIPD<-as.numeric(Clark.Large$WDTIPD)
Clark.Large$DBLC<-Clark.Large$DBC
Clark.Large$DW_CROWN<-Clark.Large$DW_BRANCH+Clark.Large$DW_FOL
Clark.Large$ST_METH<-'1'
Clark.Large$BR_METH<-'2'
Clark.Large$FOL_METH<-'2'
Clark.Large$REGION<-'SE'
		
Clark.Large$Indicator <- 'unchecked'
Clark.Large$GW_STEM<-'unchecked'
Clark.Large$GW_BRANCH<-'unchecked'
Clark.Large$GW_FOL<-'unchecked'
Clark.Large$GW_CROWN<-'unchecked'
Clark.Large$GW_BIOMASS<-'unchecked'
Clark.Large$GW_ABG<-'unchecked'
Clark.Large$GW_TOT<-'unchecked'
Clark.Large$M.C.<-'unchecked'
Clark.Large$SP.GR<-'unchecked'
Clark.Large$Bark.Fraction<-'unchecked'
Clark.Large$Age<-'unchecked'

Clark.Large[Clark.Large$SPCD == 899, "SPCD"] <- 800
Clark.Large[Clark.Large$SPCD == 60, "SPCD"] <- 68

#subset(Clark.Large,subset = SPCD == 899)
Clark.Large<-Clark.Large[,c(1,6,21:61)]
Legacy<-rbind.fill(Legacy,Clark.Large)
dim(Legacy) # 3245
Legacy<-Legacy[,c(1:43)]

head(Legacy)
###########################
# Clebsch and DeSelm 1963 # Tennessee
###########################
# White Pine only

C_D_63<-read.csv(paste(basepath,"Clebsch_DeSelm_1963.csv",sep=""),header=T, as.is= T)
C_D_63$AUTHOR<-'Clebsch_DeSelm'
C_D_63$LOC<-1			
C_D_63$SPCD<-C_D_63$FIA.SPCD
C_D_63$TREENO<-C_D_63$TREE.NO
C_D_63$CCLCD<-NA
C_D_63$CW<-NA
C_D_63$H_STUMP<-NA						# not reported 
C_D_63$DBH<-C_D_63$DBH
C_D_63$HT<-C_D_63$TOTAL.HEIGHT..ft.
C_D_63$HL_BRANCH<-NA
C_D_63$DW_SW<-NA
C_D_63$DW_SB<-NA
C_D_63$DW_STEM<-NA
C_D_63$DEAD_BRANCH_DW<-NA
C_D_63$LIVE_BRANCH_DW<-NA
C_D_63$DW_BRANCH<-NA
C_D_63$DW_TOT<-NA
C_D_63$DW_FOL<-NA
C_D_63$BIOMASS<-C_D_63$TREE.WEIGHT..kg. #Weights recorded in pounds, not KG (need to change header)
C_D_63$STUMP<-NA
C_D_63$ABG<-NA
#C_D_63[C_D_63$LOC=="1","ORIGIN"]<-"P"	# plantations = 1& 2 N; 3 + one other natural (could not determine which 
#C_D_63[C_D_63$LOC=="2","ORIGIN"]<-"P"
#C_D_63[C_D_63$LOC=="3","ORIGIN"]<-"N"	
C_D_63$ORIGIN<-1
C_D_63$TEMP<-65
C_D_63$DGL<-NA
C_D_63$DSTEM_MIN<-NA
C_D_63$DBLC<-NA
C_D_63$DW_CROWN<-NA
C_D_63$ST_METH<-'2'
C_D_63$BR_METH<-'4'
C_D_63$FOL_METH<-'4'
C_D_63$REGION<-'SE'
names(Legacy)
names(C_D_63)
C_D_63$Indicator <- 'unchecked'
C_D_63$GW_STEM<-'unchecked'
C_D_63$GW_BRANCH<-'unchecked'
C_D_63$GW_FOL<-'unchecked'
C_D_63$GW_CROWN<-'unchecked'
C_D_63$GW_BIOMASS<-'unchecked'
C_D_63$GW_ABG<-'unchecked'
C_D_63$GW_TOT<-'unchecked'
C_D_63$M.C.<-'unchecked'
C_D_63$SP.GR<-'unchecked'
C_D_63$Bark.Fraction<-'unchecked'
C_D_63$Age<-'unchecked'



Clebsch_DeSelm_1963<-C_D_63[,c(8:12,3,13:49)]		
#head(Clebsch_DeSelm_1963)
#tail(Clebsch_DeSelm_1963)


Legacy<-rbind(Legacy,Clebsch_DeSelm_1963)
dim(Legacy) # 3279

#####################
# Cunia Briggs 1984 # # Sugar Maple
#####################

# There are no methods related to how where, when, the sugar maples were sampled??
#todo: request Briggs' dissertation to see methods
C_B_84<-read.csv(paste(basepath,"Cunia_Briggs_1984.csv",sep=""),header=T, as.is= T)
#names(C_B_84)

C_B_84$AUTHOR<-'Cunia_Briggs'
C_B_84$LOC<-1
C_B_84$SPCD<-C_B_84$FIA.SPCD
#unique(C_B_84$SPCD)
C_B_84$CCLCD<-NA
C_B_84$TREENO<-c(1:length(C_B_84[,1]))
C_B_84$CW<-NA
C_B_84$H_STUMP<-NA			# did not see any mention of stump height ?assume 6 inches?
C_B_84$DBH<-C_B_84$DBH..cm./2.54
C_B_84$HT<-NA
C_B_84$HL_BRANCH<-NA
C_B_84$DW_SW<-C_B_84$Bole.Wood.Biomass..kg.*2.20462
C_B_84$DW_SB<-C_B_84$Bole.Bark.Biomass..kg.*2.20462
C_B_84$DW_STEM<-C_B_84$DW_SB+C_B_84$DW_SW
C_B_84$DEAD_BRANCH_DW<-NA
C_B_84$LIVE_BRANCH_DW<-NA
C_B_84$DW_BRANCH<-C_B_84$Branch.Biomass..kg.*2.20462				# branches are both live and dead (as per Jenkins table)
C_B_84$DW_TOT<-C_B_84$DW_BRANCH+C_B_84$DW_STEM
C_B_84$DW_FOL<-C_B_84$Leaf.Biomass..kg.*2.20462
C_B_84$BIOMASS<-C_B_84$DW_TOT+C_B_84$DW_FOL
C_B_84$STUMP<-NA
C_B_84$ABG<-NA
C_B_84$ORIGIN<-NA
C_B_84$TEMP<-65
C_B_84$DGL<-NA
C_B_84$DSTEM_MIN<-NA									# probably 4" top as in Briggs 1989?
C_B_84$DBLC<-NA
C_B_84$DW_CROWN<-C_B_84$DW_FOL+C_B_84$DW_BRANCH
C_B_84$ST_METH<-'NA'									# not well described but likely the same as Briggs 1989?
C_B_84$BR_METH<-'NA'
C_B_84$FOL_METH<-'NA'
C_B_84$REGION<-'NE'

C_B_84$Indicator <- 'unchecked'
C_B_84$GW_STEM<-'unchecked'
C_B_84$GW_BRANCH<-'unchecked'
C_B_84$GW_FOL<-'unchecked'
C_B_84$GW_CROWN<-'unchecked'
C_B_84$GW_BIOMASS<-'unchecked'
C_B_84$GW_ABG<-'unchecked'
C_B_84$GW_TOT<-'unchecked'
C_B_84$M.C.<-'unchecked'
C_B_84$SP.GR<-'unchecked'
C_B_84$Bark.Fraction<-'unchecked'
C_B_84$Age<-'unchecked'


Cunia_Briggs_1984<-C_B_84[,c(8:50)]		
#head(Cunia_Briggs_1984)
#tail(Cunia_Briggs_1984)
#names(Carter_Smith_1971);names(Briggs1989);names(Cunia_Briggs_1984)

Legacy<-rbind(Legacy,Cunia_Briggs_1984)
dim(Legacy) # 3308
#####################
# Denny Siccama 1998# # Beech and Striped maple	
#####################
#http://www.hubbardbrook.org/research/longterm/calcium/w1_overview/sap98.htm
#http://www.yale.edu/fes519b/totoket/allom/allom.htm visit for sassafrass, not yet entered
D_S_98<-read.csv(paste(basepath,"DennySiccama_1998.csv",sep=""),header=T, as.is= T)
#names(D_S_98)
D_S_98$AUTHOR<-'Denny_Siccama'
D_S_98[D_S_98$elevation == "Low","LOC"]<-"Low"												# sites are divided by elevation; however, 
D_S_98[D_S_98$elevation == "Upper","LOC"]<-"Upper"
D_S_98[D_S_98$species=="Beech","SPCD"]<-531
D_S_98[D_S_98$species=="Stripped Maple","SPCD"]<-315
D_S_98$CCLCD<-NA
D_S_98$TREENO<-D_S_98$TREE_ID
D_S_98$CW<-NA
D_S_98$H_STUMP<-0				# most tree measured at base = small trees probably at 0"
D_S_98$DBH<-D_S_98$DBH_cm/2.54
D_S_98$HT<-D_S_98$height_m * 3.28
D_S_98$HL_BRANCH<-NA
D_S_98$DW_SW<-NA
D_S_98$DW_SB<-NA
D_S_98$DW_STEM<-D_S_98$Bole_g/1000*2.20462
D_S_98$DEAD_BRANCH_DW<-D_S_98$dead.br_g/1000*2.20462
D_S_98$LIVE_BRANCH_DW<-D_S_98$Live.br_g/1000*2.20462
D_S_98$DW_BRANCH<-D_S_98$LIVE_BRANCH_DW+D_S_98$DEAD_BRANCH_DW	
D_S_98$DW_TOT<-D_S_98$DW_BRANCH+D_S_98$DW_STEM
D_S_98$DW_FOL<-D_S_98$leaves_g/1000*2.20462
D_S_98$BIOMASS<-D_S_98$DW_TOT+D_S_98$DW_FOL
D_S_98$STUMP<-NA
D_S_98$ABG<-D_S_98$BIOMASS
D_S_98$ORIGIN<-NA
D_S_98$TEMP<-NA
D_S_98$DGL<-D_S_98$Basal.Diam_cm/2.54    		# basal diameter
D_S_98$DSTEM_MIN<-NA					# does not say; probably to top? small trees
D_S_98$DBLC<-NA
D_S_98$DW_CROWN<-D_S_98$DW_FOL+D_S_98$DW_BRANCH
D_S_98$ST_METH<-'5'
D_S_98$BR_METH<-'1'
D_S_98$FOL_METH<-'1'
D_S_98$REGION<-'NE'

D_S_98$Indicator <- 'unchecked'
D_S_98$GW_STEM<-'unchecked'
D_S_98$GW_BRANCH<-'unchecked'
D_S_98$GW_FOL<-'unchecked'
D_S_98$GW_CROWN<-'unchecked'
D_S_98$GW_BIOMASS<-'unchecked'
D_S_98$GW_ABG<-'unchecked'
D_S_98$GW_TOT<-'unchecked'
D_S_98$M.C.<-'unchecked'
D_S_98$SP.GR<-'unchecked'
D_S_98$Bark.Fraction<-'unchecked'
D_S_98$Age<-'unchecked'



#names(D_S_98)
Denny_Siccama_1998<-D_S_98[,c(13:55)]		
#names(Denny_Siccama_1998)
#tail(Denny_Siccama_1998)
#Denny_Siccama_1998
#names(Carter_Smith_1971);names(Briggs1989);names(Cunia_Briggs_1984);names(Denny_Siccama_1998)

Legacy<-rbind(Legacy,Denny_Siccama_1998)
dim(Legacy)
###################
# FATEMI 2007     # red maple, sugar maple, 371, 375, 531, 761
################### # table 12 p. 78, Fatemi 2007

F_07<-read.csv(paste(basepath,"Fatemi_2007.csv",sep=""),header=T, as.is= T)
#names(F_07)
#head(F_07)
F_07$AUTHOR<-'Fatemi'
F_07$LOC<-1					# there are multiple stands, but just one set of coords used because they were all in relatively close proximity
#F_07$SPCD<-F_07$SPCD			
#unique(F_07$SPCD)				
F_07$CCLCD<-NA;
F_07$TREENO<-F_07$Tree..
F_07$CW<-NA;
F_07$H_STUMP<-0		
F_07$DBH<-F_07$dbh_cm/2.54
F_07$HT<-NA
F_07$HL_BRANCH<-NA
F_07$DW_SW<-F_07$stem_wood_kg*2.20462
F_07$DW_SB<-F_07$stem_bark_kg*2.20462
F_07$DW_STEM<-F_07$DW_SW+F_07$DW_SB
F_07$DEAD_BRANCH_DW<-NA
F_07$LIVE_BRANCH_DW<-NA
F_07$DW_BRANCH<-F_07$branch_kg*2.20462				# assume live and dead; not specified in dissertation
F_07$DW_TOT<-F_07$DW_BRANCH+F_07$DW_STEM
F_07$DW_FOL<-F_07$fol_kg*2.20462					# one foliage = 0 value? sampled in winter??
F_07$BIOMASS<-F_07$DW_TOT+F_07$DW_FOL				# likely natural = clearcut 14-16 yrs prior, not specified
F_07$STUMP<-NA
F_07$ABG<-F_07$BIOMASS
F_07$ORIGIN<-NA
F_07$TEMP<-60
F_07$DGL<-NA
F_07$DSTEM_MIN<-0.8
F_07$DBLC<-NA
F_07$DW_CROWN<-F_07$DW_FOL+F_07$DW_BRANCH
F_07$ST_METH<-'1'
F_07$BR_METH<-'5'					# changed 1/28
F_07$FOL_METH<-'5'				# changed 1/28
F_07$REGION<-'NE'

F_07$Indicator <- 'unchecked'
F_07$GW_STEM<-'unchecked'
F_07$GW_BRANCH<-'unchecked'
F_07$GW_FOL<-'unchecked'
F_07$GW_CROWN<-'unchecked'
F_07$GW_BIOMASS<-'unchecked'
F_07$GW_ABG<-'unchecked'
F_07$GW_TOT<-'unchecked'
F_07$M.C.<-'unchecked'
F_07$SP.GR<-'unchecked'
F_07$Bark.Fraction<-'unchecked'
F_07$Age<-'unchecked'


  
Fatemi_2007<-F_07[,c(11,12,1,13:52)]
#names(Fatemi_2007)

#plot(Fatemi_2007$DBH[Fatemi_2007$SPCD=='316']*2.54,Fatemi_2007$DW_BRANCH[Fatemi_2007$SPCD=='316']/2.2)
#plot(Fatemi_2007$DBH[Fatemi_2007$SPCD=='316']*2.54,Fatemi_2007$DW_TOT[Fatemi_2007$SPCD=='316']/2.2)
#plot(Fatemi_2007$DBH[Fatemi_2007$SPCD=='316']*2.54,Fatemi_2007$DW_FOL[Fatemi_2007$SPCD=='316']/2.2)
#plot(Fatemi_2007$DBH[Fatemi_2007$SPCD=='316']*2.54,Fatemi_2007$DW_SW[Fatemi_2007$SPCD=='316']/2.2)
#plot(Fatemi_2007$DBH[Fatemi_2007$SPCD=='316']*2.54,Fatemi_2007$DW_SB[Fatemi_2007$SPCD=='316']/2.2)
#plot(Fatemi_2007$DBH[Fatemi_2007$SPCD=='316']*2.54,Fatemi_2007$BIOMASS[Fatemi_2007$SPCD=='316']/2.2)
# July 4, data checks out with Fatemi_2007 Table 12

Legacy<-rbind(Legacy,Fatemi_2007)
dim(Legacy) # 3403

##########
# FERNOW #	# I have no document for Fernow; most entries are NAs
##########

FERN<-read.csv(paste(basepath,"Fernow.csv",sep=""),header=T, as.is= T) # foliage only
#names(FERN)
#head(FERN)
FERN$AUTHOR<-'Fernow'
FERN$LOC<-1
FERN$SPCD<-FERN$FIA.SPP
#unique(FERN$SPCD)
FERN$CCLCD<-NA
FERN$TREENO<-1:length(FERN[,1])
FERN$CW<-NA
FERN$H_STUMP<-NA			
FERN$DBH<-FERN$DBH..in.
FERN$HT<-FERN$Total.Height..ft.
FERN$HL_BRANCH<-NA
FERN$DW_SW<-NA
FERN$DW_SB<-NA
FERN$DW_STEM<-(FERN$Dry.Stem.WT..kg. + FERN$Dry.Top.WT..kg.) * 2.20462	# to a x inch top?
FERN$DEAD_BRANCH_DW<-NA
FERN$LIVE_BRANCH_DW<-NA
FERN$DW_BRANCH<-(FERN$Dry.Small.Branch....0.5.in..WT..kg.+FERN$Dry.Large.Branch...0.5.in..WT..kg.)*2.20462
FERN$DW_TOT<-FERN$DW_STEM + FERN$DW_BRANCH
FERN$DW_FOL<-(FERN$Dry.Leaves..kg)*2.20462
FERN$BIOMASS<-FERN$DW_TOT+FERN$DW_FOL
FERN$STUMP<-NA
FERN$ABG<-NA
FERN$ORIGIN<-NA				
FERN$TEMP<-100
FERN$DGL<-NA
FERN$DSTEM_MIN<-NA		# "to where crown began"	
FERN$DBLC<-NA
FERN$DW_CROWN<-FERN$DW_FOL+FERN$DW_BRANCH
FERN$ST_METH<-'1'
FERN$BR_METH<-'5'
FERN$FOL_METH<-'5'
FERN$REGION<-'NE'

FERN$Indicator <- 'unchecked'
FERN$GW_STEM<-'unchecked'
FERN$GW_BRANCH<-'unchecked'
FERN$GW_FOL<-'unchecked'
FERN$GW_CROWN<-'unchecked'
FERN$GW_BIOMASS<-'unchecked'
FERN$GW_ABG<-'unchecked'
FERN$GW_TOT<-'unchecked'
FERN$M.C.<-'unchecked'
FERN$SP.GR<-'unchecked'
FERN$Bark.Fraction<-'unchecked'
FERN$Age<-'unchecked'

Fernow<-FERN[,c(21:63)]
#names(Fernow)
#head(Fernow)

Legacy<-rbind(Legacy,Fernow)
dim(Legacy) # 3491
###################
# LEVIA 2008      #	# White Pine foliage only
###################
L_08<-read.csv(paste(basepath,"Levia_2008.csv",sep=""),header=T, as.is= T) # foliage only
#names(L_08)
#head(L_08)
L_08$AUTHOR<-'Levia'
L_08$SPCD<-L_08$FIA.SPCD
#unique(L_08$SPCD)
L_08$CCLCD<-NA;
L_08$TREENO<-L_08$Tree.Label
L_08$CW<-NA;
L_08$H_STUMP<-NA			
L_08$DBH<-L_08$dbh_cm/2.54
L_08$HT<-NA
L_08$HL_BRANCH<-NA
L_08$DW_SW<-NA
L_08$DW_SB<-NA
L_08$DW_STEM<-NA
L_08$DEAD_BRANCH_DW<-NA
L_08$LIVE_BRANCH_DW<-NA
L_08$DW_BRANCH<-NA
L_08$DW_TOT<-NA
L_08$DW_FOL<-L_08$fol_kg*2.20462
L_08$BIOMASS<-NA
L_08$STUMP<-NA
L_08$ABG<-NA
L_08$ORIGIN<-NA				# not mentioned in Levia 2008
L_08$TEMP<-105
L_08$DGL<-NA
L_08$DSTEM_MIN<-NA
L_08$DBLC<-NA
L_08$DW_CROWN<-NA
L_08$ST_METH<-'NA'
L_08$BR_METH<-'NA'
L_08$FOL_METH<-'1'
L_08$REGION<-'NE'

L_08$Indicator <- 'unchecked'
L_08$GW_STEM<-'unchecked'
L_08$GW_BRANCH<-'unchecked'
L_08$GW_FOL<-'unchecked'
L_08$GW_CROWN<-'unchecked'
L_08$GW_BIOMASS<-'unchecked'
L_08$GW_ABG<-'unchecked'
L_08$GW_TOT<-'unchecked'
L_08$M.C.<-'unchecked'
L_08$SP.GR<-'unchecked'
L_08$Bark.Fraction<-'unchecked'
L_08$Age<-'unchecked'




Levia_2008<-L_08[,c(7,3,8:48)]
Legacy<-rbind(Legacy,Levia_2008)
dim(Legacy) #3500
###################
# MARTIN 1998     # # Cowheeta NC
###################
M_89<-read.csv(paste(basepath,"Martin_1998.csv",sep=""),header=T, as.is= T)
#names(M_89)
#head(M_89)
#unique(M_89$site)
M_89$AUTHOR<-'Martin_et_al'
M_89$LOC<-1				# there are multiple sites in the data, but they are close. only one set of coords

M_89[M_89$species =="acru","SPCD"] <- 316
M_89[M_89$species =="bele","SPCD"] <- 372
M_89[M_89$species =="caov","SPCD"] <- 400
M_89[M_89$species =="cofl","SPCD"] <- 491
M_89[M_89$species =="litu","SPCD"] <- 621
M_89[M_89$species =="oxar","SPCD"] <- 711
M_89[M_89$species =="qual","SPCD"] <- 802
M_89[M_89$species =="quco","SPCD"] <- 806
M_89[M_89$species =="qupr","SPCD"] <- 832
M_89[M_89$species =="quru","SPCD"] <- 833

M_89[M_89$canopycl =="s","CCLCD"] <- 5
M_89[M_89$canopycl =="i","CCLCD"] <- 4
M_89[M_89$canopycl =="c","CCLCD"] <- 3
M_89[M_89$canopycl =="d","CCLCD"] <- 2
M_89$TREENO<-1:length(M_89[,1])
M_89$CW<-NA;
M_89$H_STUMP<-NA			# as noted in Jenkins, stump height not recorded, but height from base recorded
M_89$DBH<-M_89$DBH./2.54	# cm in article; #kg for biomass in article
M_89$HT<-M_89$height*3.28084
M_89$HL_BRANCH<-NA
M_89$DW_SB<-M_89$barkmass*2.20462
M_89$DW_SW<-M_89$woodmass*2.20462
M_89$DW_STEM<-M_89$stemmass*2.20462	# checked stem mass = woodmass+barkmass
M_89$DEAD_BRANCH_DW<-NA
M_89$LIVE_BRANCH_DW<-NA
M_89$DW_BRANCH<-M_89$brcmass*2.20462	# live and dead equation 13 in Jenkins
M_89$DW_TOT<-M_89$DW_BRANCH+M_89$DW_STEM
M_89$DW_FOL<-M_89$folmass*2.20462		# used foliage ratios #
M_89$BIOMASS<-M_89$DW_TOT+M_89$DW_FOL
M_89$STUMP<-NA
M_89$ABG<-M_89$BIOMASS			# stump = 0; above stump = abg
M_89$ORIGIN<-NA				# not clear from skimming article
M_89$TEMP<-65
M_89$DGL<-NA
M_89$DSTEM_MIN<-'Unchecked'
M_89$DBLC<-NA
M_89$DW_CROWN<-M_89$DW_BRANCH+M_89$DW_FOL
M_89$ST_METH<-'1'
M_89$BR_METH<-'2'		# page 1650 lab analysis 
M_89$FOL_METH<-'2'	# multiplying the ratio of dried foliage or dried branches to the total dried mass of the crown section subsample by the total crown section dry mass. 
M_89$REGION<-'SE'

M_89$Indicator <- 'unchecked'
M_89$GW_STEM<-'unchecked'
M_89$GW_BRANCH<-'unchecked'
M_89$GW_FOL<-'unchecked'
M_89$GW_CROWN<-'unchecked'
M_89$GW_BIOMASS<-'unchecked'
M_89$GW_ABG<-'unchecked'
M_89$GW_TOT<-'unchecked'
M_89$M.C.<-'unchecked'
M_89$SP.GR<-'unchecked'
M_89$Bark.Fraction<-'unchecked'
M_89$Age<-'unchecked'


#names(M_89)
Martin_1989<-M_89[,c(47:89)]

Legacy<-rbind(Legacy,Martin_1989)
dim(Legacy) # 3587

########################
# MITSCH and EWEL 1979 # # sp. 222 cypress
########################

M_E_79<-read.csv(paste(basepath,"Mitsch_Ewel_1979.csv",sep=""),header=T, as.is= T)
#names(M_E_79)
#head(M_E_79)
M_E_79$AUTHOR<-'Mitsch_Ewel'
M_E_79$LOC<-1					
M_E_79$SPCD<-222
M_E_79$CCLCD<-NA;
M_E_79$TREENO<-M_E_79$Tree.number
M_E_79$CW<-NA;						# branch length reported in table 1 (longer than tree is tall?)
M_E_79$H_STUMP<-2					# article specifies 0.6 meters; ~ 24 inches
M_E_79$DBH<-M_E_79$dbh_cm/2.54
M_E_79$HT<-M_E_79$ht_m*3.28084
M_E_79$HL_BRANCH<-NA
M_E_79$DW_SW<-NA
M_E_79$DW_SB<-NA
M_E_79$DW_STEM<-M_E_79$stem_kg*2.20462
M_E_79$DEAD_BRANCH_DW<-NA
M_E_79$LIVE_BRANCH_DW<-NA
M_E_79$DW_BRANCH<-M_E_79$branch_kg*2.20462
M_E_79$DW_TOT<- M_E_79$DW_STEM+M_E_79$DW_BRANCH
M_E_79$DW_FOL<- M_E_79$fol_kg*2.20462
M_E_79$BIOMASS<-M_E_79$DW_TOT+M_E_79$DW_FOL
M_E_79$STUMP<-M_E_79$stump_kg*2.20462				# check all prior for stump root; need to rationalize total above; total tree; and above stump
M_E_79$ABG<-M_E_79$BIOMASS+M_E_79$STUMP
M_E_79$ORIGIN<-"1"
M_E_79$TEMP<-100
M_E_79$DGL<-NA
M_E_79$DSTEM_MIN<-NA
M_E_79$DBLC<-NA
M_E_79$DW_CROWN<-M_E_79$DW_FOL+M_E_79$DW_BRANCH
M_E_79$ST_METH<-'1'
M_E_79$BR_METH<-'6'
M_E_79$FOL_METH<-'6'
M_E_79$REGION<-'SE'

M_E_79$Indicator <- 'unchecked'
M_E_79$GW_STEM<-'unchecked'
M_E_79$GW_BRANCH<-'unchecked'
M_E_79$GW_FOL<-'unchecked'
M_E_79$GW_CROWN<-'unchecked'
M_E_79$GW_BIOMASS<-'unchecked'
M_E_79$GW_ABG<-'unchecked'
M_E_79$GW_TOT<-'unchecked'
M_E_79$M.C.<-'unchecked'
M_E_79$SP.GR<-'unchecked'
M_E_79$Bark.Fraction<-'unchecked'
M_E_79$Age<-'unchecked'


#names(M_E_79)
Mitsch_Ewel_1979<-M_E_79[,c(24:66)]
#names(Mitsch_Ewel_1979)
#head(Mitsch_Ewel_1979)

Legacy<-rbind(Legacy,Mitsch_Ewel_1979)
dim(Legacy) # 3597

###################
# NELSON 2013     #	Maine
###################
N_13<-read.csv(paste(basepath,"Nelson_2013.csv",sep=""),header=T, as.is= T)
#names(N_13)
#head(N_13)
N_13$AUTHOR<-'Nelson'
N_13$LOC<-1					# note multiple sites within the PEF with different treatments 1 location considered for now (i.e. PEF)
#unique(N_13$Spp)
N_13[N_13$Spp == "ACRU","SPCD"] <- 316
N_13[N_13$Spp == "BEPA","SPCD"] <- 375
N_13[N_13$Spp == "BEPO","SPCD"] <- 379
N_13[N_13$Spp == "D51","SPCD"]  <- 740 
N_13[N_13$Spp == "DN10","SPCD"] <- 740
N_13[N_13$Spp == "DN70","SPCD"] <- 740
N_13[N_13$Spp == "NM6","SPCD"]  <- 740
N_13[N_13$Spp == "POGR","SPCD"]  <- 743
N_13[N_13$Spp == "POTR","SPCD"] <- 746
N_13[N_13$Spp == "IPIGL","SPCD"]<- 94
#N_13
#N_13$SPCD
N_13$CCLCD<-NA
N_13$TREENO<-1:length(N_13[,1])
N_13$CW<-NA
N_13$H_STUMP<-0
N_13$DBH<-N_13$dbh/2.54
N_13$HT<-N_13$ht/100*3.28084
N_13$HL_BRANCH<-(N_13$ht-N_13$llc)/100*3.28084
N_13$DW_SW<-NA
N_13$DW_SB<-NA
N_13$DW_STEM<-N_13$Stem/1000*2.20462
N_13$DEAD_BRANCH_DW<-NA
N_13$LIVE_BRANCH_DW<-NA
N_13$DW_BRANCH<-N_13$Branch/1000*2.20462
N_13$DW_TOT<- N_13$DW_STEM+N_13$DW_BRANCH
N_13$DW_FOL<- N_13$Leaf/1000*2.20462
N_13$BIOMASS<-N_13$DW_TOT+N_13$DW_FOL
N_13$STUMP<-NA
N_13$ABG<-N_13$BIOMASS
N_13$ORIGIN<-"2"
N_13$TEMP<-65
N_13$DGL<-N_13$bdia/2.54
N_13$DSTEM_MIN<-0
N_13$DBLC<-NA
N_13$DW_CROWN<-N_13$DW_FOL+N_13$DW_BRANCH
N_13$ST_METH<-'5'
N_13$BR_METH<-'1'
N_13$FOL_METH<-'1'
N_13$REGION<-'NE'

N_13$Indicator <- 'unchecked'
N_13$GW_STEM<-'unchecked'
N_13$GW_BRANCH<-'unchecked'
N_13$GW_FOL<-'unchecked'
N_13$GW_CROWN<-'unchecked'
N_13$GW_BIOMASS<-'unchecked'
N_13$GW_ABG<-'unchecked'
N_13$GW_TOT<-'unchecked'
N_13$M.C.<-'unchecked'
N_13$SP.GR<-'unchecked'
N_13$Bark.Fraction<-'unchecked'
N_13$Age<-'unchecked'


Nelson_2013<-N_13[,c(14:56)]

Legacy<-rbind(Legacy,Nelson_2013)
dim(Legacy) # 3702
################
# NICHOLAS 1992 #	Virginia
################

N_92<-read.csv(paste(basepath,"Nichols_1992_3.csv",sep=""),header=T, as.is= T)

#N_Taper<-read.csv(paste(basepath,"Nichols_Spruce_Fir_STEM_Table.csv",sep=""),header=T, as.is= T)
#by.2<-N_Taper$TREE_ID
#N_STEMMIN<-as.data.frame(aggregate(N_Taper$DIA_CALIP,list(by.2),min))
#names(N_STEMMIN)<-c("TREE.NO","DSTEM.MIN.cm")
#N_STEMMIN$DSTEM.MIN.in<-N_STEMMIN$DSTEM.MIN.cm/2.54
#N_92_M<-merge(x=N_92,y=N_STEMMIN,by.x=("TREE.ID"),by.y=("TREE.NO"),all.x=T)
#N_92<-N_92_M

N_92$AUTHOR<-'Nicholas'
N_92[N_92$LOC=="BM","LOC"]<-1
N_92[N_92$LOC=="MTR","LOC"]<-2
names(N_92)
N_92[,c(1,2,3,4,5)]
# N_92$SPCD<- already coded
N_92$CCLCD<-NA;
N_92$TREENO<-N_92$TREE_ID
N_92$CW<-NA						
N_92$H_STUMP<-NA				# Nichols P. 45 at or below 30 cm				
N_92$DBH<-N_92$DBH
N_92$HT<-N_92$HT_TAPED
N_92$HL_BRANCH<-NA
N_92$DW_SW<-N_92$DW_STEM_WOOD
N_92$DW_SB<-N_92$DW_STEM_BARK
N_92$DW_STEM<-N_92$DW_STEM_TOTAL
N_92$DEAD_BRANCH_DW<-NA
N_92$LIVE_BRANCH_DW<-NA
N_92$DW_BRANCH<-N_92$DW_BRANCH
N_92$DW_TOT<- N_92$DW_STEM+N_92$DW_BRANCH
N_92$DW_FOL<- N_92$DW_FOL
N_92$BIOMASS<-N_92$DW_TOT+N_92$DW_FOL
N_92$STUMP<-NA				
N_92$ABG<-NA
N_92$ORIGIN<-1
N_92$TEMP<-65				# Nichols P. 46 65 degree C
N_92$DGL<-NA
N_92$DSTEM_MIN<-N_92$DSTEM_MIN
N_92$DBLC<-NA
N_92$DW_CROWN<-N_92$DW_FOL+N_92$DW_BRANCH
N_92$ST_METH<-'1'
N_92$BR_METH<-'2'
N_92$FOL_METH<-'2'
N_92$REGION<-'SE'

N_92$Indicator <- NA
N_92$GW_STEM<-N_92$GW_STEM.lb
N_92$GW_BRANCH<-N_92$GW_BRANCH.lb
N_92$GW_FOL<-N_92$GW_FOL.lb
N_92$GW_CROWN<-N_92$GW_BRANCH + N_92$GW_FOL
N_92$GW_BIOMASS<-N_92$GW_STEM + N_92$GW_BRANCH + N_92$GW_FOL
N_92$GW_ABG<-NA
N_92$GW_TOT<-N_92$GW_BRANCH + N_92$GW_STEM
N_92$M.C.<-'unchecked'
N_92$SP.GR<-'unchecked'
N_92$Bark.Fraction<-'unchecked'
N_92$Age<-'unchecked'


names(N_92)
Nicholas_1992<-N_92[,c("AUTHOR","LOC","SPCD","TREENO","CCLCD","CW",
                       "H_STUMP","DBH","HT","HL_BRANCH","DW_SW","DW_SB",
                       "DW_STEM","DEAD_BRANCH_DW","LIVE_BRANCH_DW","DW_BRANCH","DW_TOT","DW_FOL",
                       "BIOMASS","STUMP","ABG","ORIGIN","TEMP","DGL",
                       "DSTEM_MIN","DBLC","DW_CROWN","ST_METH","BR_METH","FOL_METH",
                       "REGION","Indicator","GW_STEM","GW_BRANCH","GW_FOL","GW_CROWN",
                       "GW_BIOMASS","GW_ABG","GW_TOT","M.C.","SP.GR","Bark.Fraction",
                       "Age")]

Legacy<-rbind(Legacy,Nicholas_1992)
dim(N_92)
dim(Legacy) # 3790
############
# NOWAK ND # SPCD 125 only; Red Pine; Warrensburg NY
############

N_ND<-read.csv(paste(basepath,"Nowak_et_al_nd.csv",sep=""),header=T, as.is= T)
#names(N_ND)
#head(N_ND)
N_ND$AUTHOR<-'Nowak'
N_ND$LOC<-1					
N_ND$SPCD<-N_ND$FIA_SPCD
#unique(N_ND$SPCD)
N_ND$CCLCD<-NA
N_ND$TREENO<-1:length(N_ND[,1])
N_ND$CW<-NA;
N_ND$H_STUMP<-0				# p. 4; paragraph 1 = ground level
N_ND$DBH<-N_ND$DBH_cm/2.54
N_ND$HT<-N_ND$HT_m * 3.28084
N_ND$HL_BRANCH<-NA
N_ND$DW_SW<-N_ND$Stem_wood_kg*2.20462
N_ND$DW_SB<-N_ND$Stem_bark_kg*2.20462
N_ND$DW_STEM<-N_ND$DW_SW+N_ND$DW_SB
N_ND$DEAD_BRANCH_DW<-N_ND$Dead_br_kg*2.20462
N_ND$LIVE_BRANCH_DW<-N_ND$Live_Br_kg*2.20462
N_ND$DW_BRANCH<-N_ND$DEAD_BRANCH_DW+N_ND$LIVE_BRANCH_DW
N_ND$DW_TOT<- N_ND$DW_STEM+N_ND$DW_BRANCH
N_ND$DW_FOL<- N_ND$Foliage_kg/1000*2.20462
N_ND$BIOMASS<-N_ND$DW_TOT+N_ND$DW_FOL
N_ND$STUMP<-NA
N_ND$ABG<-N_ND$BIOMASS
N_ND$ORIGIN<-"2"
N_ND$TEMP<-80
N_ND$DGL<-NA
N_ND$DSTEM_MIN<-4						# says definitions follow Nowak 1985; assuming methodsa and bole definitions are same a Briggs 1989 
N_ND$DBLC<-NA
N_ND$DW_CROWN<-N_ND$DW_FOL+N_ND$DW_BRANCH
N_ND$ST_METH<-'1'
N_ND$BR_METH<-'2'
N_ND$FOL_METH<-'2'
N_ND$REGION<-'NE'

N_ND$Indicator <- 'unchecked'
N_ND$GW_STEM<-'unchecked'
N_ND$GW_BRANCH<-'unchecked'
N_ND$GW_FOL<-'unchecked'
N_ND$GW_CROWN<-'unchecked'
N_ND$GW_BIOMASS<-'unchecked'
N_ND$GW_ABG<-'unchecked'
N_ND$GW_TOT<-'unchecked'
N_ND$M.C.<-'unchecked'
N_ND$SP.GR<-'unchecked'
N_ND$Bark.Fraction<-'unchecked'
#N_ND$Age<-'unchecked'
names(N_ND)
Nowak_ND<-N_ND[,c(13:54,4)]

Legacy<-rbind(Legacy,Nowak_ND)

##########
# RIBE   # Ribe 1979 Table 27, page 103
##########
R_79_316<-read.csv(paste(basepath,"Ribe_1979_316.csv",sep=""),header=T, as.is= T)
R_79_740<-read.csv(paste(basepath,"Ribe_1979_740.csv",sep=""),header=T, as.is= T)
#names(R_79_316);names(R_79_740)
R_79<-rbind(R_79_316[,c(1:6)],R_79_740[,c(1,2,4:7)])
#names(R_79)
#head(R_79)
R_79$AUTHOR<-'Ribe'
R_79$LOC<-1					
R_79$SPCD<-R_79$FIA.SPCD
R_79$CCLCD<-NA;
R_79$TREENO<-1:length(R_79[,1])
R_79$CW<-NA;
R_79$H_STUMP<-(3/12)				# p. 23; Dimensional Analysis; paragraph 1 
R_79$DBH<-R_79$DBH..in.
R_79$HT<-NA
R_79$HL_BRANCH<-NA
R_79$DW_SW<-NA
R_79$DW_SB<-NA
R_79$DW_STEM<-R_79$Stem.Wt..g./1000*2.20462
R_79$DEAD_BRANCH_DW<-NA
R_79$LIVE_BRANCH_DW<-NA
R_79$DW_BRANCH<-R_79$Branch.Wt..g./1000*2.20462
R_79$DW_TOT<- R_79$DW_STEM+R_79$DW_BRANCH
R_79$DW_FOL<- R_79$Leaf.Wt..g./1000*2.20462
R_79$BIOMASS<-R_79$DW_TOT+R_79$DW_FOL
R_79$STUMP<-NA
R_79$ABG<-NA
R_79$ORIGIN<-"1"
R_79$TEMP<-105
R_79$DGL<-NA
R_79$DSTEM_MIN<-0					# Assumed to tip since small trees
R_79$DBLC<-NA
R_79$DW_CROWN<-R_79$DW_FOL+R_79$DW_BRANCH
R_79$ST_METH<-'1'
R_79$BR_METH<-'2'
R_79$FOL_METH<-'2'
R_79$REGION<-'NE'

R_79$Indicator <- 'unchecked'
R_79$GW_STEM<-'unchecked'
R_79$GW_BRANCH<-'unchecked'
R_79$GW_FOL<-'unchecked'
R_79$GW_CROWN<-'unchecked'
R_79$GW_BIOMASS<-'unchecked'
R_79$GW_ABG<-'unchecked'
R_79$GW_TOT<-'unchecked'
R_79$M.C.<-'unchecked'
R_79$SP.GR<-'unchecked'
R_79$Bark.Fraction<-'unchecked'
R_79$Age<-'unchecked'

Ribe_1979<-R_79[,c(7:49)]


Legacy<-rbind(Legacy,Ribe_1979)
dim(Legacy) # 3867
##########
# ROTH   # # southern pine spcd 111, 131; FL and GA
##########
R_12<-read.csv(paste(basepath,"Roth_2010.csv",sep=""),header=T, as.is= T)

R_12$dupcheck<-paste(R_12$HT,R_12$DBH,R_12$TREE,R_12$BIDCK,R_12$FAM,sep = "_")
#R_12$dupcheck
R_12$dups<-duplicated(R_12$dupcheck)		# 60 slash and 70 loblolly does not add up even with dups removed
R_12_sub<-subset(R_12,subset = dups == "FALSE") # there are duplicates in the tables (i.e. table D6,D7,D8,D9,D10,D11, dups start at row 10)
#dim(R_12);dim(R_12_sub)
#head(R_12_sub)
#unique(R_12$LOCATION)

R_12_sub$AUTHOR<-'Roth'
#names(R_12_sub)
R_12_sub[R_12_sub$LOCATION == "Perry_FL","LOC"]<- 1
R_12_sub[R_12_sub$LOCATION == "Waldo_FL","LOC"]<- 2
R_12_sub[R_12_sub$LOCATION == "Sanderson_FL","LOC"]<- 3
R_12_sub[R_12_sub$LOCATION == "Waverly_GA","LOC"]<- 4
R_12_sub[R_12_sub$LOCATION == "Bunnel_FL","LOC"]<-5
		
R_12_sub[R_12_sub$LOCATION == "Perry_FL","SPCD"]<- 111
R_12_sub[R_12_sub$LOCATION == "Waldo_FL","SPCD"]<- 111
R_12_sub[R_12_sub$LOCATION == "Sanderson_FL","SPCD"]<- 131
R_12_sub[R_12_sub$LOCATION == "Waverly_GA","SPCD"]<- 131
R_12_sub[R_12_sub$LOCATION == "Bunnel_FL","SPCD"]<-131
	
R_12_sub$CCLCD<-NA;
R_12_sub$TREENO<-1:length(R_12_sub[,1])
R_12_sub$CW<-((R_12_sub$CWAL+R_12_sub$CWAC)/2)*3.28084
R_12_sub$H_STUMP<-0					# small trees assumed at 0?			
R_12_sub$DBH<-R_12_sub$DBH_cm/2.54			# all weights are in kg, converted to pounds here
R_12_sub$HT<-R_12_sub$HT_m*3.28084
R_12_sub$HL_BRANCH<-R_12_sub$HTLC*3.28084
R_12_sub$DW_SW<-R_12_sub$BOLE*2.20462
R_12_sub$DW_SB<-R_12_sub$BARK*2.20462
R_12_sub$DW_STEM<-R_12_sub$DW_SW+R_12_sub$DW_SB
R_12_sub$DEAD_BRANCH_DW<-NA
R_12_sub$LIVE_BRANCH_DW<-NA
R_12_sub$DW_BRANCH<-R_12_sub$BRANCH*2.20462		# assumed all branches
R_12_sub$DW_TOT<- R_12_sub$DW_STEM+R_12_sub$DW_BRANCH
R_12_sub$DW_FOL<- R_12_sub$FOLIAGE*2.20462
R_12_sub$BIOMASS<-R_12_sub$DW_TOT+R_12_sub$DW_FOL
R_12_sub$STUMP<-NA
R_12_sub$ABG<-R_12_sub$BIOMASS
R_12_sub$ORIGIN<-"2"
R_12_sub$TEMP<-70							# p. 26 Roth 2010
R_12_sub$DGL<-R_12_sub$GID/2.54				 
R_12_sub$DSTEM_MIN<-'Unchecked'
R_12_sub$DBLC<-NA
R_12_sub$DW_CROWN<-R_12_sub$DW_BRANCH+R_12_sub$DW_FOL
R_12_sub$ST_METH<-'1'
R_12_sub$BR_METH<-'4'
R_12_sub$FOL_METH<-'4'
R_12_sub$REGION<-'SE'
R_12_sub$Indicator <- 'unchecked'
R_12_sub$GW_STEM<-'unchecked'
R_12_sub$GW_BRANCH<-'unchecked'
R_12_sub$GW_FOL<-'unchecked'
R_12_sub$GW_CROWN<-'unchecked'
R_12_sub$GW_BIOMASS<-'unchecked'
R_12_sub$GW_ABG<-'unchecked'
R_12_sub$GW_TOT<-'unchecked'
R_12_sub$M.C.<-'unchecked'
R_12_sub$SP.GR<-'unchecked'
R_12_sub$Bark.Fraction<-'unchecked'


names(R_12_sub)
Roth_2010<-R_12_sub[,c(23:64,19)]
#names(Roth_2010)

Legacy<-rbind(Legacy,Roth_2010)
dim(Legacy) # 4057
dim(Roth_2010)

###########
# SABATIA # spcd: 110; SE Oklahoma
###########

# in some cases branch + stem + fol != total tree; but there is general agreement
S_07<-read.csv(paste(basepath,"Sabatia_2007.csv",sep=""),header=T, as.is= T)
#names(S_07)
S_07$AUTHOR<-'Sabatia'
S_07$LOC<-1						# there are multiple plots mentioned of varying densities in close proximity but only 1 set of coords given in paper
S_07$SPCD<-S_07$FIA.SPP				# SE Oklahoma
#unique(S_07$SPCD)
S_07$CCLCD<-NA
S_07$TREENO<-1:length(S_07[,1])
S_07$CW<-S_07$Crown.Width..m.*3.28084
S_07$H_STUMP<-(0.14*3.28084)					#Reported as 0.14 meters in methods
S_07$DBH<-S_07$DBH..cm./2.54			# all weights are in kg, converted to pounds here
S_07$HT<-S_07$Total.Height..m.*3.28084
S_07$HL_BRANCH<-S_07$Crown.Height..m.*3.28084
S_07$DW_SW<-S_07$Bolewood.Biomass..kg.*2.20462
S_07$DW_SB<-S_07$Bole.Bark.Biomass..kg.*2.20462
S_07$DW_STEM<-S_07$DW_SW+S_07$DW_SB
S_07$DEAD_BRANCH_DW<-NA
S_07$LIVE_BRANCH_DW<-NA
S_07$DW_BRANCH<-S_07$Branch.Biomass..kg.*2.20462
S_07$DW_TOT<- S_07$DW_STEM+S_07$DW_BRANCH
S_07$DW_FOL<- S_07$Foliage.Biomass..kg.*2.20462
S_07$BIOMASS<-S_07$DW_TOT+S_07$DW_FOL
S_07$STUMP<-NA
S_07$ABG<-NA
S_07$ORIGIN<-"1"						# assumed not fount in Sab 2007
S_07$TEMP<-60						# p. 13 Sabatia 2007
S_07$DGL<-NA
S_07$DSTEM_MIN<-1
S_07$DBLC<-NA
S_07$DW_CROWN<-S_07$DW_BRANCH+S_07$DW_FOL
S_07$ST_METH<-'1'
S_07$BR_METH<-'3'
S_07$FOL_METH<-'3'
S_07$REGION<-'SE'
S_07$Indicator <- 'unchecked'
S_07$GW_STEM<-'unchecked'
S_07$GW_BRANCH<-'unchecked'
S_07$GW_FOL<-'unchecked'
S_07$GW_CROWN<-'unchecked'
S_07$GW_BIOMASS<-'unchecked'
S_07$GW_ABG<-'unchecked'
S_07$GW_TOT<-'unchecked'
S_07$M.C.<-'unchecked'
S_07$SP.GR<-'unchecked'
S_07$Bark.Fraction<-'unchecked'
S_07$Age<-'unchecked'


#names(S_07)
Sabatia_2007<-S_07[,c(16:58)]
#names(Sabatia_2007)

Legacy<-rbind(Legacy,Sabatia_2007)
dim(Legacy) #4093
##############
# Smith 1984 # balsam fir and red spruce; Weymouth PT, ME
##############
S_84<-read.csv(paste(basepath,"Smith1984.csv",sep=""),header=T, as.is= T)
# p. 166 (fir) - p. 167 (spruce) Tattersall Smith 1984
#names(S_84);head(S_84)
S_84$AUTHOR<-'Smith'
S_84$LOC<-1	
S_84$CCLCD<-NA
#length(S_84$TreeNum);length(unique(S_84$TreeNum))
S_84$TREENO<-S_84$TreeNum
S_84$CW<-NA
S_84$H_STUMP<-0.5						# assumed; not mentioned in thesis										
S_84$DBH<-S_84$DBH_in					
S_84$HT<-S_84$Ht_m*3.28084						 # p. 166/7 Total Height (m)
S_84$HL_BRANCH<-S_84$HBLC_m*3.28084				 # p. 166/7 ht to base of live crown (m)
S_84$DW_SW<-NA
S_84$DW_SB<-NA

S_84$DW_STEM<-(S_84$Stem_above4in_top+S_84$Stem_stump_4in_TOP)*2.20462 # added stem above 4" to stem below 4"; 
# not entirely clear if the top was weighed with branches or if branches were removed					
S_84$DEAD_BRANCH_DW<-S_84$Dead_Br*2.20462 			  # p. 166/7 Dead Branches
S_84$LIVE_BRANCH_DW<-NA						  # foliage not separated	
S_84$DW_BRANCH<-NA					        # foliage not separated	
S_84$DW_TOT<-NA							  # foliage not separated		
S_84$DW_FOL<- NA							  # foliage not separated					
S_84$BIOMASS<-S_84$Total.Above.Ground.Weight*2.20462		  # Total above ground weight
S_84$STUMP<-NA							  # not given	
S_84$ABG<-NA							  # no stump weight	
S_84$ORIGIN<-"1"							  # assumed
S_84$TEMP<-105							  # given page 55 24 hours
S_84$DGL<-NA	
S_84$DSTEM_MIN<-0
S_84$DBLC<-NA
S_84$DW_CROWN<-S_84$Branches_Fol_sm+S_84$Branches_Fol_med+S_84$Branches_Fol_lg+S_84$Dead_Br
S_84$ST_METH<-'1'
S_84$BR_METH<-NA
S_84$FOL_METH<-NA
S_84$REGION<-'NE'
S_84$Indicator <- 'unchecked'
S_84$GW_STEM<-'unchecked'
S_84$GW_BRANCH<-'unchecked'
S_84$GW_FOL<-'unchecked'
S_84$GW_CROWN<-'unchecked'
S_84$GW_BIOMASS<-'unchecked'
S_84$GW_ABG<-'unchecked'
S_84$GW_TOT<-'unchecked'
S_84$M.C.<-'unchecked'
S_84$SP.GR<-'unchecked'
S_84$Bark.Fraction<-'unchecked'
S_84$Age<-'unchecked'


#names(S_84)
Smith_1984<-S_84[,c(16,17,2,18:57)]
#names(Smith_1984)


Legacy<-rbind(Legacy,Smith_1984)
dim(Legacy) # 4135
########################
# Sollins And Anderson # 	Southeast
########################
# Sollins and Anderson not calculating correctly #

# Variables not measured entered as 0
Sollins_Large<-read.csv(paste(basepath,"Sollins_Anderson_Large.csv",sep=""),header=T, as.is= T, na.strings= 0)
Sollins_Small<-read.csv(paste(basepath,"Sollins_Anderson_Small.csv",sep=""),header=T, as.is= T, na.strings= 0)

Sollins_Large$dgl<-NA
Sollins_Small$dbh<-NA
Sollins_Small$h_stump<-NA
Sollins_Large$h_stump<-NA

S_A_71 <- rbind(Sollins_Large, Sollins_Small)

S_A_71$AUTHOR<-'Sollins_Anderson'
S_A_71$LOC<-S_A_71$dsn						
S_A_71$SPCD<-S_A_71$FIA.SPCD
#unique(S_A_71$SPCD)
S_A_71$CCLCD<-NA
S_A_71$TREENO<-1:length(S_A_71[,1])
S_A_71$CW<-NA
S_A_71$H_STUMP<-S_A_71$h_stump					# will vary between studies; assume 6" for large; 0" for small								
S_A_71$DBH<-S_A_71$dbh/2.54			
S_A_71$HT<-S_A_71$ht*3.28084
S_A_71$HL_BRANCH<-(S_A_71$ht-S_A_71$crdepth)*3.28084		# crdepth in Sollins Desc = tree ht-ht lowest branch
S_A_71$DW_SW<-NA					
S_A_71$DW_SB<-NA
S_A_71$DW_STEM<-as.double(S_A_71$bolewt)*2.20462					# Sollins Desc - weight of bole kg
#S_A_71$DW_STEM
S_A_71$DEAD_BRANCH_DW<-NA
S_A_71$LIVE_BRANCH_DW<-NA
S_A_71$DW_BRANCH<-S_A_71$brwt*2.20462					# Sollins Desc - dry weight of branches kg; assume live + dead		
#summary(S_A_71)
#str(S_A_71$bolewt)
S_A_71$DW_TOT<-S_A_71$totwt*2.20462					# Sollins Desc - stemwt = dry weight of branches plus bole kg;
S_A_71$DW_FOL<- S_A_71$leafwt*2.20462					# Sollins Desc - dry weight of leaves
S_A_71$BIOMASS<-S_A_71$DW_TOT+S_A_71$DW_FOL
S_A_71$STUMP<-NA
S_A_71$ABG<-NA
S_A_71$ORIGIN<-NA						
S_A_71$TEMP<-NA					
S_A_71$DGL<-S_A_71$dgl/2.54						# sollins Desc - dbh/dgl diameter bh or ground level cm
S_A_71$DSTEM_MIN<-NA
S_A_71$DBLC<-NA
S_A_71$DW_CROWN<-S_A_71$DW_BRANCH+S_A_71$DW_FOL
S_A_71$ST_METH<-'UnChecked'
S_A_71$BR_METH<-'UnChecked'
S_A_71$FOL_METH<-'UnChecked'
S_A_71$REGION<-'SE'
S_A_71$Indicator <- 'unchecked'
S_A_71$GW_STEM<-'unchecked'
S_A_71$GW_BRANCH<-'unchecked'
S_A_71$GW_FOL<-'unchecked'
S_A_71$GW_CROWN<-'unchecked'
S_A_71$GW_BIOMASS<-'unchecked'
S_A_71$GW_ABG<-'unchecked'
S_A_71$GW_TOT<-'unchecked'
S_A_71$M.C.<-'unchecked'
S_A_71$SP.GR<-'unchecked'
S_A_71$Bark.Fraction<-'unchecked'
S_A_71$Age<-'unchecked'


#names(S_A_71)
#S_A_71[,c(37,33,36,10)]
#S_A_71$DW_TOT
#S_A_71_2<-subset(S_A_71,subset = DBH != 0)			# there a are a number of dbh = 0 in the Sollins Large file; 
Sollins_Anderson_1971<-S_A_71[,c(23:65)]

Legacy<-rbind(Legacy,Sollins_Anderson_1971)

##################
# Whittaker 1974 #	# Hubbard Brook, NY # edited
##################

W_74<-read.csv(paste(basepath,"Whittaker_1974.csv",sep=""),header=T, as.is= T)
head(W_74)
W_74$AUTHOR<-'Whittaker_et_al'
W_74$LOC<-1						
W_74[W_74$SPEC=="BEAL","SPCD"]<-371  
W_74[W_74$SPEC=="FAGR","SPCD"]<- 531
W_74[W_74$SPEC=="PIRU","SPCD"]<- 97 
W_74[W_74$SPEC=="ACSA","SPCD"]<- 318 
W_74[W_74$SPEC=="ACPE","SPCD"]<- 315 
 
W_74$CCLCD<-NA
W_74$TREENO<-1:length(W_74[,1])
W_74$CW<-NA
W_74$H_STUMP<-0					# p. 	235; stump just above ground level or root flare				
W_74$DBH<-W_74$DBH_cm/2.54			# dbh in cm
W_74$HT<-W_74$HT_cm/100*3.28084			# height in cm
W_74$HL_BRANCH<-NA
W_74$DW_SW<-W_74$WOOD/1000*2.20462
W_74$DW_SB<-W_74$BARK/1000*2.20462
W_74$DW_STEM<-W_74$DW_SW+W_74$DW_SB
W_74[W_74$DEADWD < 0,"DEADWD"]<- NA		# -7777 entered for ACPE so no branch or crown values 
W_74$DEAD_BRANCH_DW<-W_74$DEADWD/1000*2.20462
W_74$LIVE_BRANCH_DW<-NA			# branch DW is NA since twigs are included with foliage
W_74$DW_BRANCH<-NA		
W_74$DW_TOT<- W_74$DW_STEM+W_74$DW_BRANCH
W_74$DW_FOL<- NA					# problems getting foliage biomass to match equations
W_74$BIOMASS<-W_74$ABOVE/1000*2.20462		# total above groun as per Siccama Table	
W_74$STUMP<-NA
W_74$ABG<-W_74$ABOVE/1000*2.20462			# = biomass since stump is 0
W_74$ORIGIN<-"N"					# assumed 
W_74$TEMP<-105					# p.234
W_74$DGL<-NA
W_74$DSTEM_MIN<-0					# Fairly confident
W_74$DBLC<-NA
W_74[W_74$OLDNDLS < 0,"OLDNDLS"]<-0
W_74$DW_CROWN<-W_74$BRDW/1000*2.20462 + W_74$DEAD_BRANCH_DW + W_74$CTLGR/1000*2.20462+
							W_74$OLDNDLS/1000*2.20462
plot(W_74$DBH,W_74$DW_CROWN)
W_74$ST_METH<-'1'
W_74$BR_METH<-'3'
W_74$FOL_METH<-'3'
W_74$REGION<-'NE'
#W_74[W_74==0]<-NA
W_74$Indicator <- 'unchecked'
W_74$GW_STEM<-'unchecked'
W_74$GW_BRANCH<-'unchecked'
W_74$GW_FOL<-'unchecked'
W_74$GW_CROWN<-'unchecked'
W_74$GW_BIOMASS<-'unchecked'
W_74$GW_ABG<-'unchecked'
W_74$GW_TOT<-'unchecked'
W_74$M.C.<-'unchecked'
W_74$SP.GR<-'unchecked'
W_74$Bark.Fraction<-'unchecked'
W_74$Age<-'unchecked'

Whittaker_1974<-W_74[,c(28:70)]
Legacy<-rbind(Legacy,Whittaker_1974) 
dim(Legacy);dim(Whittaker_1974)#5313 # 93
#############################
# Whittaker & Woodwell 1968 # edited
#############################

W_W_68<-read.csv(paste(basepath,"Whittaker_1968.csv",sep=""),header=T, as.is= T)
W_W_68$AUTHOR<-'Whittaker_Woodwell'
W_W_68$LOC<-1
#unique(W_W_68$Species)
W_W_68[W_W_68$Species=="PIRI","SPCD"]<- 126
W_W_68[W_W_68$Species=="QUAL","SPCD"]<- 802
W_W_68[W_W_68$Species=="QUCO","SPCD"]<- 806

W_W_68$CCLCD<-NA;
W_W_68$TREENO<-1:length(W_W_68[,1])  					
W_W_68$CW<-NA
W_W_68$H_STUMP<-0									
W_W_68$DBH<-W_W_68$DBH_cm/2.54			# dbh in cm
W_W_68$HT<-W_W_68$Height_dm/10*3.28084			# height in cm
#W_W_68
W_W_68$HL_BRANCH<-NA
W_W_68$DW_SW<-W_W_68$stemwood/1000*2.20462
W_W_68$DW_SB<-W_W_68$stembark/1000*2.20462
W_W_68$DW_STEM<-W_W_68$DW_SW+W_W_68$DW_SB
W_W_68$DEAD_BRANCH_DW<-NA
W_W_68$LIVE_BRANCH_DW<-NA
W_W_68$DW_BRANCH<-NA	# branch weight also given, but 		
W_W_68$DW_TOT<- NA
W_W_68$DW_FOL<- NA					# problems getting foliage biomass to match equations
W_W_68$BIOMASS<-NA	
W_W_68$STUMP<-NA
W_W_68$ABG<-W_W_68$DW_STEM+
		W_W_68$branches/1000*2.20462 + 
		W_W_68$currlftw/1000*2.20462	
W_W_68$ORIGIN<-"1"					# assumed 
W_W_68$TEMP<-105
W_W_68$DGL<-NA
W_W_68$DSTEM_MIN<-0
W_W_68$DBLC<-NA
W_W_68$DW_CROWN<-W_W_68$branches/1000*2.20462 + W_W_68$currlftw/1000*2.20462	
W_W_68$ST_METH<-'1'
W_W_68$BR_METH<-'3'
W_W_68$FOL_METH<-'3'
W_W_68$REGION<-'NE'

W_W_68$Indicator <- 'unchecked'
W_W_68$GW_STEM<-'unchecked'
W_W_68$GW_BRANCH<-'unchecked'
W_W_68$GW_FOL<-'unchecked'
W_W_68$GW_CROWN<-'unchecked'
W_W_68$GW_BIOMASS<-'unchecked'
W_W_68$GW_ABG<-'unchecked'
W_W_68$GW_TOT<-'unchecked'
W_W_68$M.C.<-'unchecked'
W_W_68$SP.GR<-'unchecked'
W_W_68$Bark.Fraction<-'unchecked'
W_W_68$Age<-'unchecked'


Whittaker_Woodwell_1968<-W_W_68[,c(12:54)]
summary(Whittaker_Woodwell_1968)
dim(W_W_68)
names(Whittaker_Woodwell_1968)
#head(Whittaker_Woodwell_1968)

Legacy<-rbind(Legacy,Whittaker_Woodwell_1968)
dim(Whittaker_Woodwell_1968)
dim(Legacy)		#5360
#############################
# Witherspoon 1972	    # # Oak Ridge
#############################

W_72<-read.csv(paste(basepath,"Witherspoon_1962.csv",sep=""),header=T, as.is= T)
#names(W_72)
#head(W_72)
W_72$AUTHOR<-'Witherspoon'
W_72$LOC<-1					# a coule of diff sites, dolomitic/shale; wet/dry			

W_72$SPCD<- 802
W_72$CCLCD<-NA;
W_72$TREENO<-1:4
W_72$CW<-NA
W_72$H_STUMP<-0				# small trees; assumed to be 0									
W_72$DBH<-W_72$DBH..in			
W_72$HT<-W_72$Height..ft.		# height in ft
W_72$HL_BRANCH<-NA
W_72$DW_SW<-NA
W_72$DW_SB<-NA
W_72$DW_STEM<-W_72$Trunk/1000*2.20462	# appear to be grams
W_72$DEAD_BRANCH_DW<-NA
W_72$LIVE_BRANCH_DW<-NA
W_72$DW_BRANCH<-W_72$Branches/1000*2.20462		
W_72$DW_TOT<- W_72$DW_STEM+W_72$DW_BRANCH
W_72$DW_FOL<- W_72$Leaves/1000*2.20462
W_72$BIOMASS<-W_72$DW_BRANCH+W_72$DW_TOT	
W_72$STUMP<-NA
W_72$ABG<-NA
W_72$ORIGIN<-NA 
W_72$TEMP<-NA
W_72$DGL<-NA
W_72$DSTEM_MIN<-'UnChecked'
W_72$DBLC<-NA
W_72$DW_CROWN<-W_72$DW_FOL+W_72$DW_BRANCH
W_72$ST_METH<-'4'
W_72$BR_METH<-'4'
W_72$FOL_METH<-'4'
W_72$REGION<-'SE'

W_72$Indicator <- 'unchecked'
W_72$GW_STEM<-'unchecked'
W_72$GW_BRANCH<-'unchecked'
W_72$GW_FOL<-'unchecked'
W_72$GW_CROWN<-'unchecked'
W_72$GW_BIOMASS<-'unchecked'
W_72$GW_ABG<-'unchecked'
W_72$GW_TOT<-'unchecked'
W_72$M.C.<-'unchecked'
W_72$SP.GR<-'unchecked'
W_72$Bark.Fraction<-'unchecked'
W_72$Age<-'unchecked'



#dim(Witherspoon_1972)
Witherspoon_1972<-W_72[,c(13:55)]
#names(Witherspoon_1972)
#head(Witherspoon_1972)

Legacy<-rbind(Legacy,Witherspoon_1972)
dim(Legacy)#5364
##############
# Baker_1971 #
##############

Baker_1971<-read.csv(paste(basepath,"Baker_1971_Data.csv",sep=""),header=T, as.is= T)
Baker_1971[is.na(Baker_1971)]<-0
Baker_1971$AUTHOR<-"Baker_1971"
Baker_1971$LOC<-1
Baker_1971$SPCD.1<-Baker_1971$SPCD
Baker_1971$SPCD<-NULL
Baker_1971$SPCD<-Baker_1971$SPCD.1
Baker_1971$SPCD.1<-NULL
Baker_1971$CCLCD<-NA
Baker_1971$TREENO<-1:length(Baker_1971[,1])
Baker_1971$CW<-NA
Baker_1971$H_STUMP<-NA  												
Baker_1971$DBH<-Baker_1971$DBH.CM/2.54 #convert to inches
Baker_1971$HT<-Baker_1971$TH.M*3.28084		# height in ft
Baker_1971$HL_BRANCH<-NA
Baker_1971$DW_SW<-Baker_1971$STEM.WOOD.WT.G/453.592
Baker_1971$DW_SB<-Baker_1971$STEM.BARK.WT.G/453.592
Baker_1971$DW_STEM<-Baker_1971$DW_SW+Baker_1971$DW_SB
Baker_1971$DEAD_BRANCH_DW<-Baker_1971$DEAD.BRANCH.WT.G/453.592
Baker_1971$LIVE_BRANCH_DW<-(Baker_1971$CURRENT.BRANCH.WT.G+Baker_1971$OLD.FOL.BEARING.WT.G+Baker_1971$OLD.BRANCH.WT.G)/453.592 
Baker_1971$DW_BRANCH<-Baker_1971$DEAD_BRANCH_DW+Baker_1971$LIVE_BRANCH_DW		
Baker_1971[Baker_1971==0]<-NA
Baker_1971$DW_TOT<- Baker_1971$DW_STEM+Baker_1971$DW_BRANCH
Baker_1971$DW_FOL<- (Baker_1971$CURRENT.FOL.WT.G+Baker_1971$OLD.FOL.WT.G)/453.592
Baker_1971$BIOMASS<-Baker_1971$DW_FOL+Baker_1971$DW_TOT
Baker_1971$STUMP<-NA
Baker_1971$ABG<-NA
Baker_1971$ORIGIN<-2 
Baker_1971$TEMP<-NA
Baker_1971$DGL<-NA
Baker_1971$DSTEM_MIN<-NA
Baker_1971$DBLC<-NA
Baker_1971$DW_CROWN<-Baker_1971$DW_BRANCH+Baker_1971$DW_FOL

Baker_1971[Baker_1971==0]<-NA
Baker_1971$ST_METH<-4			
Baker_1971$BR_METH<-4
Baker_1971$FOL_METH<-4
Baker_1971$REGION<-'SE'
Baker_1971$Indicator <- 'unchecked'
Baker_1971$GW_STEM<-'unchecked'
Baker_1971$GW_BRANCH<-'unchecked'
Baker_1971$GW_FOL<-'unchecked'
Baker_1971$GW_CROWN<-'unchecked'
Baker_1971$GW_BIOMASS<-'unchecked'
Baker_1971$GW_ABG<-'unchecked'
Baker_1971$GW_TOT<-'unchecked'
Baker_1971$M.C.<-'unchecked'
Baker_1971$SP.GR<-'unchecked'
Baker_1971$Bark.Fraction<-'unchecked'
Baker_1971$Age<-'unchecked'

Baker_1971<-Baker_1971[,14:56]
names(Baker_1971)
#names(LegacyData.2)
LegacyData<-rbind(Legacy,Baker_1971)
#plot(LegacyData$DBH,LegacyData$BIOMASS)
dim(LegacyData) # 5449
##############
# Baker_1962 # # loblolly pine
##############

Baker_1962<-read.csv(paste(basepath,"Baker_1962_Data.csv",sep=""),header=T, as.is= T)
Baker_1962[is.na(Baker_1962)]<-0
Baker_1962$AUTHOR<-"Baker_1962"
Baker_1962$LOC<-1
Baker_1962$SPCD.1<-Baker_1962$SPCD
Baker_1962$SPCD<-NULL
Baker_1962$SPCD<-Baker_1962$SPCD.1
Baker_1962$SPCD.1<-NULL
Baker_1962$CCLCD<-NA
Baker_1962$TREENO<-Baker_1962$TREE.NO
Baker_1962$CW<-NA
Baker_1962$H_STUMP<-NA    		
Baker_1962$DBH<-Baker_1962$DBH.IN
Baker_1962$HT<-Baker_1962$TH.FT
Baker_1962$HL_BRANCH<-NA
Baker_1962$DW_SW<-NA
Baker_1962$DW_SB<-NA
Baker_1962$DW_STEM<-Baker_1962$WEIGHT.STEM.LB
Baker_1962$DEAD_BRANCH_DW<-NA
Baker_1962$LIVE_BRANCH_DW<-NA
Baker_1962$DW_BRANCH<-Baker_1962$WEIGHT.BRANCH.LB
Baker_1962[Baker_1962==0]<-NA
Baker_1962$DW_TOT<- Baker_1962$DW_STEM+Baker_1962$DW_BRANCH
Baker_1962$DW_FOL<- Baker_1962$WEIGHT.FOLIAGE.LB
Baker_1962$BIOMASS<-Baker_1962$DW_FOL+Baker_1962$DW_TOT	
Baker_1962$STUMP<-NA
Baker_1962$ABG<-NA
Baker_1962$ORIGIN<-2 
Baker_1962$TEMP<-NA
Baker_1962$DGL<-NA
Baker_1962$DSTEM_MIN<-NA
Baker_1962$DBLC<-NA
Baker_1962$DW_CROWN<-Baker_1962$DW_FOL+Baker_1962$DW_BRANCH
Baker_1962$ST_METH<-5			# small trees
Baker_1962$BR_METH<-1
Baker_1962$FOL_METH<-1
Baker_1962$REGION<-'SE'
Baker_1962$Indicator <- 'unchecked'
Baker_1962$GW_STEM<-'unchecked'
Baker_1962$GW_BRANCH<-'unchecked'
Baker_1962$GW_FOL<-'unchecked'
Baker_1962$GW_CROWN<-'unchecked'
Baker_1962$GW_BIOMASS<-'unchecked'
Baker_1962$GW_ABG<-'unchecked'
Baker_1962$GW_TOT<-'unchecked'
Baker_1962$M.C.<-'unchecked'
Baker_1962$SP.GR<-'unchecked'
Baker_1962$Bark.Fraction<-'unchecked'
Baker_1962$Age<-'unchecked'


Baker_1962<-Baker_1962[,c(8:50)]
#Baker_1962[Baker_1962==0]<-NA

LegacyData<-rbind(LegacyData,Baker_1962)

#####################
# Storey et al 1955 #
#####################

Storey_et_al_1955<-read.csv(paste(basepath,"Storey_et_al_1955_Data.csv",sep=""),header=T, as.is= T)
names(Storey_et_al_1955)
Storey_et_al_1955$AUTHOR<-"Storey_et_al_1955"
Storey_et_al_1955$LOC<-Storey_et_al_1955$LOCATION
Storey_et_al_1955[Storey_et_al_1955$SPECIES=="ponderosa pine","SPCD"]<-122
Storey_et_al_1955[Storey_et_al_1955$SPECIES=="sugar pine","SPCD"]<-117
Storey_et_al_1955[Storey_et_al_1955$SPECIES=="western white pine","SPCD"]<-119
Storey_et_al_1955[Storey_et_al_1955$SPECIES=="lodgepole pine","SPCD"]<-108
Storey_et_al_1955[Storey_et_al_1955$SPECIES=="white fir","SPCD"]<-94
Storey_et_al_1955[Storey_et_al_1955$SPECIES=="loblolly pine","SPCD"]<-131
Storey_et_al_1955[Storey_et_al_1955$SPECIES=="grand fir","SPCD"]<-17
Storey_et_al_1955[Storey_et_al_1955$SPECIES=="Douglas-fir","SPCD"]<-202
Storey_et_al_1955[Storey_et_al_1955$SPECIES=="Engelmann spruce","SPCD"]<-93
Storey_et_al_1955[Storey_et_al_1955$SPECIES=="western hemlock","SPCD"]<-263
Storey_et_al_1955[Storey_et_al_1955$SPECIES=="western redcedar","SPCD"]<-242
Storey_et_al_1955[Storey_et_al_1955$SPECIES=="western larch","SPCD"]<-73
Storey_et_al_1955[Storey_et_al_1955$SPECIES=="incense cedar","SPCD"]<-81
Storey_et_al_1955$CCLCD<-NA
#length(Storey_et_al_1955$TREE.NO);length(unique(Storey_et_al_1955$TREE.NO))
Storey_et_al_1955$TREENO<-Storey_et_al_1955$TREE.NO
Storey_et_al_1955$CW<-NA
Storey_et_al_1955$H_STUMP<-NA      	
Storey_et_al_1955$DBH<-Storey_et_al_1955$DBH.IN
Storey_et_al_1955$HT<-Storey_et_al_1955$TH.FT
Storey_et_al_1955$HL_BRANCH<-Storey_et_al_1955$HT-Storey_et_al_1955$HC.FT
Storey_et_al_1955$DW_SW<-NA
Storey_et_al_1955$DW_SB<-NA
Storey_et_al_1955$DW_STEM<-NA
Storey_et_al_1955$DEAD_BRANCH_DW<-NA
Storey_et_al_1955$LIVE_BRANCH_DW<-NA
Storey_et_al_1955$DW_BRANCH<-Storey_et_al_1955$WT.DW.LB
Storey_et_al_1955$DW_TOT<- NA
Storey_et_al_1955$DW_FOL<- Storey_et_al_1955$W.DF.LB
Storey_et_al_1955$BIOMASS<-NA
Storey_et_al_1955$STUMP<-NA
Storey_et_al_1955$ABG<-NA
Storey_et_al_1955$ORIGIN<-2 
Storey_et_al_1955$TEMP<-NA
Storey_et_al_1955$DGL<-NA
Storey_et_al_1955$DSTEM_MIN<-NA
Storey_et_al_1955$DBLC<-Storey_et_al_1955$DIAM.BC.IN
Storey_et_al_1955$DW_CROWN<-Storey_et_al_1955$WT.DC.LB
Storey_et_al_1955$ST_METH<-'NA'
Storey_et_al_1955$BR_METH<-'NA'
Storey_et_al_1955$FOL_METH<-'4'
Storey_et_al_1955$REGION<-'IM,PSW'

Storey_et_al_1955$Indicator <- 'unchecked'
Storey_et_al_1955$GW_STEM<-'unchecked'
Storey_et_al_1955$GW_BRANCH<-'unchecked'
Storey_et_al_1955$GW_FOL<-'unchecked'
Storey_et_al_1955$GW_CROWN<-'unchecked'
Storey_et_al_1955$GW_BIOMASS<-'unchecked'
Storey_et_al_1955$GW_ABG<-'unchecked'
Storey_et_al_1955$GW_TOT<-'unchecked'
Storey_et_al_1955$M.C.<-'unchecked'
Storey_et_al_1955$SP.GR<-'unchecked'
Storey_et_al_1955$Bark.Fraction<-'unchecked'
Storey_et_al_1955$Age<-'unchecked'
names(Storey_et_al_1955)
names(LegacyData)
Storey_et_al_1955<-Storey_et_al_1955[,c(12:54)]


LegacyData<-rbind(LegacyData,Storey_et_al_1955)
dim(LegacyData) # 5665

###############
# Storey 1969 # # Southwestern U.S.
###############
#names(Storey_1969)
Storey_1969<-read.csv(paste(basepath,"Storey_1969_Data.csv",sep=""),header=T, as.is= T)
Storey_1969$AUTHOR<-"Storey_1969"
Storey_1969$LOC<-1
Storey_1969[Storey_1969$SPECIES=="pinyon pine","SPCD"]<-133
Storey_1969[Storey_1969$SPECIES=="Utah juniper","SPCD"]<-65
Storey_1969$CCLCD<-NA
Storey_1969$TREENO<-1:length(Storey_1969[,1])
Storey_1969$CW<-Storey_1969$CROWN.DIA.AVG.FT
Storey_1969$H_STUMP<-0
Storey_1969$DBH<-NA
Storey_1969$HT<-Storey_1969$TH.FT
Storey_1969$HL_BRANCH<-NA
Storey_1969$DW_SW<-NA
Storey_1969$DW_SB<-NA
Storey_1969$DW_STEM<-NA
Storey_1969$DEAD_BRANCH_DW<-NA
Storey_1969$LIVE_BRANCH_DW<-NA
Storey_1969$DW_BRANCH<-NA
Storey_1969$DW_TOT<- NA
Storey_1969$DW_FOL<- NA
Storey_1969$BIOMASS<-Storey_1969$TREE.WT.LB
Storey_1969$STUMP<-0
Storey_1969$ABG<-Storey_1969$BIOMASS
Storey_1969$ORIGIN<-1 
Storey_1969$TEMP<-NA
Storey_1969$DGL<-Storey_1969$D.AT.1FT.IN
Storey_1969$DSTEM_MIN<-NA
Storey_1969$DBLC<-NA
Storey_1969$DW_CROWN<-NA
Storey_1969$ST_METH<-'4'
Storey_1969$BR_METH<-'4'
Storey_1969$FOL_METH<-'4'
Storey_1969$REGION<-'IMW'

Storey_1969$Indicator <- 'unchecked'
Storey_1969$GW_STEM<-'unchecked'
Storey_1969$GW_BRANCH<-'unchecked'
Storey_1969$GW_FOL<-'unchecked'
Storey_1969$GW_CROWN<-'unchecked'
Storey_1969$GW_BIOMASS<-'unchecked'
Storey_1969$GW_ABG<-'unchecked'
Storey_1969$GW_TOT<-'unchecked'
Storey_1969$M.C.<-'unchecked'
Storey_1969$SP.GR<-'unchecked'
Storey_1969$Bark.Fraction<-'unchecked'
Storey_1969$Age<-'unchecked'
names(Storey_1969)
Storey_1969<-Storey_1969[,c(8:50)]


LegacyData<-rbind(LegacyData,Storey_1969)

#plot(LegacyData$DBH,LegacyData$BIOMASS)


###############
# Krumlik 1974# # West Coast
###############

Krumlik_1974<-read.csv(paste(basepath,"Krumlik_1974_Data.csv",sep=""),header=T, as.is= T)
Krumlik_1974$AUTHOR<-"Krumlik"
Krumlik_1974[Krumlik_1974$SPECIES=="Abam"|Krumlik_1974$SPECIES=="Tsme","LOC"]<-1 
#Mountain hemlock and pacific silver fir harvested at Squamish plot (LOC=1)
Krumlik_1974[Krumlik_1974$SPECIES=="Thpl"|Krumlik_1974$SPECIES=="Tshe"|Krumlik_1974$SPECIES=="Chno","LOC"]<-2 
#Western hemlock, western redcedar, and Alaksa yellow cedar harvest at Haney plot (LOC=2)
Krumlik_1974[Krumlik_1974$SPECIES=="Abam","SPCD"]<-11
Krumlik_1974[Krumlik_1974$SPECIES=="Chno","SPCD"]<-42
Krumlik_1974[Krumlik_1974$SPECIES=="Thpl","SPCD"]<-242
Krumlik_1974[Krumlik_1974$SPECIES=="Tshe","SPCD"]<-263
Krumlik_1974[Krumlik_1974$SPECIES=="Tsme","SPCD"]<-264
Krumlik_1974[is.na(Krumlik_1974)]<-0 #Temporarily convert 'NA' to 0 in order to combine branch weight columns
Krumlik_1974$CCLCD<-NA
Krumlik_1974$TREENO<-1:length(Krumlik_1974[,1])
Krumlik_1974$CW<-Krumlik_1974$CROWN.DIAM.M*3.28084
Krumlik_1974$H_STUMP<-1
Krumlik_1974$DBH<-Krumlik_1974$DBH.M*39.3701
Krumlik_1974$HT<-Krumlik_1974$TH.M*3.2084
Krumlik_1974$HL_BRANCH<-Krumlik_1974$HT-(Krumlik_1974$CROWN.LENGTH.M*3.2084)
Krumlik_1974$DW_SW<-Krumlik_1974$SUM.WOOD.KG*2.20462
Krumlik_1974$DW_SB<-Krumlik_1974$SUM.BARK.KG*2.20462
Krumlik_1974$DW_STEM<-Krumlik_1974$DW_SW+Krumlik_1974$DW_SB
Krumlik_1974$DEAD_BRANCH_DW<-NA
Krumlik_1974$LIVE_BRANCH_DW<-NA
Krumlik_1974$DW_BRANCH<-(Krumlik_1974$LARGE.BRANCH.KG+Krumlik_1974$SMALL.BRANCH.KG+Krumlik_1974$CONES.KG+Krumlik_1974$TWIGS.KG)*2.20462
Krumlik_1974[Krumlik_1974==0]<-NA
Krumlik_1974$DW_TOT<-Krumlik_1974$DW_STEM+Krumlik_1974$DW_BRANCH
Krumlik_1974$DW_FOL<-Krumlik_1974$FOLIAGE.KG*2.20462
Krumlik_1974$BIOMASS<-Krumlik_1974$DW_STEM+Krumlik_1974$DW_FOL
Krumlik_1974$STUMP<-NA
Krumlik_1974$ABG<-NA
Krumlik_1974$ORIGIN<-1 
Krumlik_1974$TEMP<-105
Krumlik_1974$DGL<-NA
Krumlik_1974$DSTEM_MIN<-1
Krumlik_1974$DBLC<-NA
Krumlik_1974$DW_CROWN<-Krumlik_1974$DW_BRANCH+Krumlik_1974$TWIG.FOLIAGE.KG
Krumlik_1974$ST_METH<-'4'
Krumlik_1974$BR_METH<-'NA'
Krumlik_1974$FOL_METH<-'NA'
Krumlik_1974$REGION<-'PNW'

Krumlik_1974$Indicator <- 'unchecked'
Krumlik_1974$GW_STEM<-'unchecked'
Krumlik_1974$GW_BRANCH<-'unchecked'
Krumlik_1974$GW_FOL<-'unchecked'
Krumlik_1974$GW_CROWN<-'unchecked'
Krumlik_1974$GW_BIOMASS<-'unchecked'
Krumlik_1974$GW_ABG<-'unchecked'
Krumlik_1974$GW_TOT<-'unchecked'
Krumlik_1974$M.C.<-'unchecked'
Krumlik_1974$SP.GR<-'unchecked'
Krumlik_1974$Bark.Fraction<-'unchecked'
Krumlik_1974$Age<-'unchecked'


Krumlik_1974<-Krumlik_1974[,c(21:63)]
#names(Krumlik_1974)
#names(LegacyData.2)
LegacyData<-rbind(LegacyData,Krumlik_1974)
dim(LegacyData) #5740
###############
# Brown 1978  # # Western species 
############### # Need new posting 1/22/14
#names(Brown_1978)
dim(LegacyData)
Brown_1978<-read.csv(paste(basepath,"Brown_1978_Data_ALL.csv",sep=""),header=T,as.is = T, na.strings = c("NA","--"))
head(Brown_1978)
plot(Brown_1978$DBH,as.numeric(Brown_1978$FOL))
Brown_1978$AUTHOR<-"Brown"
Brown_1978$LOC<- Brown_1978$PLOT
Brown_1978[Brown_1978$SPECIES=="Douglas-fir","SPCD"]<-202
Brown_1978[Brown_1978$SPECIES=="Engelmann Spruce","SPCD"]<-93
Brown_1978[Brown_1978$SPECIES=="grand fir","SPCD"]<-17
Brown_1978[Brown_1978$SPECIES=="lodgepole pine","SPCD"]<-108
Brown_1978[Brown_1978$SPECIES=="ponderosa pine","SPCD"]<-122
Brown_1978[Brown_1978$SPECIES=="subalpine fir","SPCD"]<-19
Brown_1978[Brown_1978$SPECIES=="western hemlock","SPCD"]<-263
Brown_1978[Brown_1978$SPECIES=="western larch","SPCD"]<-73
Brown_1978[Brown_1978$SPECIES=="western redcedar","SPCD"]<-242
Brown_1978[Brown_1978$SPECIES=="western white pine","SPCD"]<-119
Brown_1978[Brown_1978$SPECIES=="whitebark pine","SPCD"]<-101
Brown_1978[is.na(Brown_1978)]<-0 #Temporarily convert 'NA' to 0 in order to combine branch weight columns
Brown_1978$TREENO<-1:length(Brown_1978[,1])
Brown_1978$CCLCD<-Brown_1978$CC
Brown_1978$CW<-Brown_1978$CROWN.WIDTH
Brown_1978$H_STUMP<-NA
Brown_1978$DBH<-Brown_1978$DBH.IN
Brown_1978$HT<-Brown_1978$TH.FT
Brown_1978$HL_BRANCH<-Brown_1978$HT-Brown_1978$LIVE.CROWN.LENGTH
Brown_1978$DW_SW<-NA
Brown_1978$DW_SB<-NA
Brown_1978$DW_STEM<-NA
Brown_1978$DEAD_BRANCH_DW<-Brown_1978$BRANCH.DEAD.LB
Brown_1978$BRANCH.025.LB<-as.numeric(Brown_1978$BRANCH.025.LB)
Brown_1978$BRANCH.1.LB<-as.numeric(Brown_1978$BRANCH.1.LB)
Brown_1978$BRANCH.3.LB<-as.numeric(Brown_1978$BRANCH.3.LB)
Brown_1978$BRANCH.3..LB<-as.numeric(Brown_1978$BRANCH.3..LB)
Brown_1978$TIP.WT.LB<-as.numeric(Brown_1978$TIP.WT.LB)
Brown_1978$LIVE_BRANCH_DW<-Brown_1978$BRANCH.025.LB+Brown_1978$BRANCH.1.LB+Brown_1978$BRANCH.3.LB+
                           Brown_1978$BRANCH.3..LB+Brown_1978$TIP.WT.LB
Brown_1978$DEAD_BRANCH_DW<-as.numeric(Brown_1978$DEAD_BRANCH_DW)
Brown_1978$DW_BRANCH<-Brown_1978$LIVE_BRANCH_DW+Brown_1978$DEAD_BRANCH_DW
Brown_1978[Brown_1978==0]<-NA
Brown_1978$DW_TOT<-NA
Brown_1978$DW_FOL<-Brown_1978$FOL
Brown_1978$BIOMASS<-NA
Brown_1978$STUMP<-NA
Brown_1978$ABG<-NA
Brown_1978$ORIGIN<-1 
Brown_1978$TEMP<-100
Brown_1978$DGL<-NA
Brown_1978$DSTEM_MIN<-Brown_1978$DIA.TIP.BASE.IN
Brown_1978$DBLC<-Brown_1978$DIA.CROWN.BASE.IN
Brown_1978$DW_FOL<-as.numeric(Brown_1978$DW_FOL)
Brown_1978$DW_CROWN<-Brown_1978$DW_BRANCH+Brown_1978$DW_FOL
Brown_1978$ST_METH<-'1'
Brown_1978$BR_METH<-'5'
Brown_1978$FOL_METH<-'5'
Brown_1978$REGION<-'IMW'

Brown_1978$Indicator <- 'unchecked'
Brown_1978$GW_STEM<-'unchecked'
Brown_1978$GW_BRANCH<-'unchecked'
Brown_1978$GW_FOL<-'unchecked'
Brown_1978$GW_CROWN<-'unchecked'
Brown_1978$GW_BIOMASS<-'unchecked'
Brown_1978$GW_ABG<-'unchecked'
Brown_1978$GW_TOT<-'unchecked'
Brown_1978$M.C.<-'unchecked'
Brown_1978$SP.GR<-'unchecked'
Brown_1978$Bark.Fraction<-'unchecked'
Brown_1978$Age<-'unchecked'
names(Brown_1978)
names(LegacyData)
Brown_1978<-Brown_1978[,c(24:66)]
head(Brown_1978)
plot(Brown_1978$DBH,Brown_1978$DW_FOL)
LegacyData<-rbind(LegacyData,Brown_1978)
dim(LegacyData) # 6114



############################
# Fraser and McGuire 1969  #
############################

Fraser_and_McGuire_1969<-read.csv(paste(basepath,"Fraser_and_McGuire_1969_Data.csv",sep=""),header=T, as.is= T)

Fraser_and_McGuire_1969$AUTHOR<-"Fraser_and_McGuire"
Fraser_and_McGuire_1969$LOC<-1
Fraser_and_McGuire_1969$SPCD.1<-Fraser_and_McGuire_1969$SPCD
Fraser_and_McGuire_1969$SPCD<-NULL
Fraser_and_McGuire_1969$SPCD<-Fraser_and_McGuire_1969$SPCD.1
Fraser_and_McGuire_1969$SPCD.1<-NULL
Fraser_and_McGuire_1969$CCLCD<-NA
Fraser_and_McGuire_1969$TREENO<-1
Fraser_and_McGuire_1969$CW<-NA
Fraser_and_McGuire_1969$H_STUMP<-0
Fraser_and_McGuire_1969$DBH<-NA
Fraser_and_McGuire_1969$HT<-Fraser_and_McGuire_1969$TH.FT
Fraser_and_McGuire_1969$HL_BRANCH<-NA
Fraser_and_McGuire_1969$DW_SW<-NA
Fraser_and_McGuire_1969$DW_SB<-NA
Fraser_and_McGuire_1969$DW_STEM<-Fraser_and_McGuire_1969$MAIN.STEM.DRY.WEIGHT.LB
Fraser_and_McGuire_1969$DEAD_BRANCH_DW<-NA
Fraser_and_McGuire_1969$LIVE_BRANCH_DW<-NA
Fraser_and_McGuire_1969$DW_BRANCH<-Fraser_and_McGuire_1969$BRANCH.DRY.WEIGHT.LB
Fraser_and_McGuire_1969$DW_TOT<-Fraser_and_McGuire_1969$DW_BRANCH+Fraser_and_McGuire_1969$DW_STEM
Fraser_and_McGuire_1969$DW_FOL<-Fraser_and_McGuire_1969$FOLIAGE.DRY.WEIGHT.LB
Fraser_and_McGuire_1969$BIOMASS<-Fraser_and_McGuire_1969$DW_FOL+Fraser_and_McGuire_1969$DW_TOT
Fraser_and_McGuire_1969$STUMP<-NA
Fraser_and_McGuire_1969$ABG<-Fraser_and_McGuire_1969$BIOMASS
Fraser_and_McGuire_1969$ORIGIN<-1 
Fraser_and_McGuire_1969$TEMP<-80
Fraser_and_McGuire_1969$DGL<-Fraser_and_McGuire_1969$DGL.IN
Fraser_and_McGuire_1969$DSTEM_MIN<-0				# Likely assumption
Fraser_and_McGuire_1969$DBLC<-NA
Fraser_and_McGuire_1969$DW_CROWN<-Fraser_and_McGuire_1969$DW_BRANCH+Fraser_and_McGuire_1969$DW_FOL
Fraser_and_McGuire_1969$ST_METH<-'4'
Fraser_and_McGuire_1969$BR_METH<-'1'
Fraser_and_McGuire_1969$FOL_METH<-'1'
Fraser_and_McGuire_1969$REGION<-'NE'

Fraser_and_McGuire_1969$Indicator <- 'unchecked'
Fraser_and_McGuire_1969$GW_STEM<-'unchecked'
Fraser_and_McGuire_1969$GW_BRANCH<-'unchecked'
Fraser_and_McGuire_1969$GW_FOL<-'unchecked'
Fraser_and_McGuire_1969$GW_CROWN<-'unchecked'
Fraser_and_McGuire_1969$GW_BIOMASS<-'unchecked'
Fraser_and_McGuire_1969$GW_ABG<-'unchecked'
Fraser_and_McGuire_1969$GW_TOT<-'unchecked'
Fraser_and_McGuire_1969$M.C.<-'unchecked'
Fraser_and_McGuire_1969$SP.GR<-'unchecked'
Fraser_and_McGuire_1969$Bark.Fraction<-'unchecked'
Fraser_and_McGuire_1969$Age<-'unchecked'


Fraser_and_McGuire_1969<-Fraser_and_McGuire_1969[,c(9:51)]

LegacyData<-rbind(LegacyData,Fraser_and_McGuire_1969)

#plot(LegacyData$DBH,LegacyData$BIOMASS)


############################
# Fraser and McGuire 1969  #
############################
#names(Fraser_et_al_1964)
Fraser_et_al_1964<-read.csv(paste(basepath,"Fraser_et_al_1964_Data.csv",sep=""),header=T, as.is= T)
Fraser_et_al_1964$AUTHOR<-"Fraser_et_al"
Fraser_et_al_1964$LOC<-1
Fraser_et_al_1964$SPCD.1<-Fraser_et_al_1964$SPCD
Fraser_et_al_1964$SPCD<-NULL
Fraser_et_al_1964$SPCD<-Fraser_et_al_1964$SPCD.1
Fraser_et_al_1964$SPCD.1<-NULL
Fraser_et_al_1964$CCLCD<-NA
Fraser_et_al_1964$TREENO<-1
Fraser_et_al_1964$CW<-NA
Fraser_et_al_1964$H_STUMP<-0
Fraser_et_al_1964$DBH<-NA
Fraser_et_al_1964$HT<-Fraser_et_al_1964$TH.FT
Fraser_et_al_1964$HL_BRANCH<-NA
Fraser_et_al_1964$DW_SW<-NA
Fraser_et_al_1964$DW_SB<-NA
Fraser_et_al_1964$DW_STEM<-Fraser_et_al_1964$MAIN.STEM.DRY.WEIGHT.LB
Fraser_et_al_1964$DEAD_BRANCH_DW<-NA
Fraser_et_al_1964$LIVE_BRANCH_DW<-NA
Fraser_et_al_1964$DW_BRANCH<-Fraser_et_al_1964$BRANCH.DRY.WEIGHT.LB
Fraser_et_al_1964$DW_TOT<-Fraser_et_al_1964$DW_BRANCH+Fraser_et_al_1964$DW_STEM
Fraser_et_al_1964$DW_FOL<-Fraser_et_al_1964$FOLIAGE.DRY.WEIGHT.LB
Fraser_et_al_1964$BIOMASS<-Fraser_et_al_1964$DW_FOL+Fraser_et_al_1964$DW_TOT
Fraser_et_al_1964$STUMP<-NA
Fraser_et_al_1964$ABG<-Fraser_et_al_1964$BIOMASS
Fraser_et_al_1964$ORIGIN<-1 
Fraser_et_al_1964$TEMP<-80
Fraser_et_al_1964$DGL<-Fraser_et_al_1964$DGL.IN
Fraser_et_al_1964$DSTEM_MIN<-0				# Likely assumption
Fraser_et_al_1964$DBLC<-NA
Fraser_et_al_1964$DW_CROWN<-Fraser_et_al_1964$DW_BRANCH+Fraser_et_al_1964$DW_FOL
Fraser_et_al_1964$ST_METH<-'4'
Fraser_et_al_1964$BR_METH<-'1'
Fraser_et_al_1964$FOL_METH<-'1'
Fraser_et_al_1964$REGION<-'NE'

Fraser_et_al_1964$Indicator <- 'unchecked'
Fraser_et_al_1964$GW_STEM<-'unchecked'
Fraser_et_al_1964$GW_BRANCH<-'unchecked'
Fraser_et_al_1964$GW_FOL<-'unchecked'
Fraser_et_al_1964$GW_CROWN<-'unchecked'
Fraser_et_al_1964$GW_BIOMASS<-'unchecked'
Fraser_et_al_1964$GW_ABG<-'unchecked'
Fraser_et_al_1964$GW_TOT<-'unchecked'
Fraser_et_al_1964$M.C.<-'unchecked'
Fraser_et_al_1964$SP.GR<-'unchecked'
Fraser_et_al_1964$Bark.Fraction<-'unchecked'
Fraser_et_al_1964$Age<-'unchecked'

Fraser_et_al_1964<-Fraser_et_al_1964[,c(8:50)]

#names(LegacyData.2)
LegacyData<-rbind(LegacyData,Fraser_et_al_1964)


##############
# Mead 1971  # # Florida
##############
#names(Mead_1971)
Mead_1971<-read.csv(paste(basepath,"Mead_1971_Data.csv",sep=""),header=T, as.is= T)
Mead_1971
Mead_1971$AUTHOR<-"Mead"
Mead_1971$LOC<-1
Mead_1971$SPCD.1<-Mead_1971$SPCD
Mead_1971$SPCD<-NULL
Mead_1971$SPCD<-111 #Mead_1971$SPCD.1
Mead_1971$SPCD.1<-NULL
Mead_1971$CCLCD<-3
Mead_1971$TREENO<-Mead_1971$TREE.ID
Mead_1971$CW<-NA
Mead_1971$H_STUMP<-NA
Mead_1971$DBH<-Mead_1971$DBH.IN
Mead_1971$HT<-Mead_1971$TH.FT
Mead_1971$HL_BRANCH<-NA
Mead_1971$DW_SW<-Mead_1971$STEM.WOOD.DW/459.592
Mead_1971$DW_SB<-Mead_1971$STEM.BARK.DW/459.592
Mead_1971$DW_STEM<-Mead_1971$DW_SW+Mead_1971$DW_SB
Mead_1971$DEAD_BRANCH_DW<-NA
Mead_1971$LIVE_BRANCH_DW<-NA
Mead_1971$DW_BRANCH<-Mead_1971$BRANCH.DW/459.592
Mead_1971$DW_TOT<-Mead_1971$DW_BRANCH+Mead_1971$DW_STEM
Mead_1971$DW_FOL<-Mead_1971$FOLIAGE.DW/459.592
Mead_1971$BIOMASS<-Mead_1971$DW_FOL+Mead_1971$DW_TOT
Mead_1971$STUMP<-Mead_1971$STUMP.DW/453.592
Mead_1971$ABG<-NA
Mead_1971$ORIGIN<-2
Mead_1971$TEMP<-60
Mead_1971$DGL<-NA
Mead_1971$DSTEM_MIN<-NA
Mead_1971$DBLC<-NA
Mead_1971$DW_CROWN<-Mead_1971$DW_BRANCH+Mead_1971$DW_FOL
Mead_1971$ST_METH<-'1'
Mead_1971$BR_METH<-'1'
Mead_1971$FOL_METH<-'1'
Mead_1971$REGION<-'SE'

Mead_1971$Indicator <- 'unchecked'
Mead_1971$GW_STEM<-'unchecked'
Mead_1971$GW_BRANCH<-'unchecked'
Mead_1971$GW_FOL<-'unchecked'
Mead_1971$GW_CROWN<-'unchecked'
Mead_1971$GW_BIOMASS<-'unchecked'
Mead_1971$GW_ABG<-'unchecked'
Mead_1971$GW_TOT<-'unchecked'
Mead_1971$M.C.<-'unchecked'
Mead_1971$SP.GR<-'unchecked'
Mead_1971$Bark.Fraction<-'unchecked'
Mead_1971$Age<-'unchecked'


Mead_1971<-Mead_1971[,c(10:52)]

#names(LegacyData.2)
LegacyData<-rbind(LegacyData,Mead_1971)


########################
# Metz and Wells 1965  #
########################
Metz_and_Wells_1965<-read.csv(paste(basepath,"Metz_and_Wells_1965_Data.csv",sep=""),header=T, as.is= T)
names(Metz_and_Wells_1965)
Metz_and_Wells_1965$AUTHOR<-"Metz_and_Wells"
Metz_and_Wells_1965$LOC<-1
Metz_and_Wells_1965$SPCD.1<-Metz_and_Wells_1965$SPCD
Metz_and_Wells_1965$SPCD<-NULL
Metz_and_Wells_1965$SPCD<-Metz_and_Wells_1965$SPCD.1
Metz_and_Wells_1965$SPCD.1<-NULL
Metz_and_Wells_1965$TREENO<-Metz_and_Wells_1965$TREE.NO
Metz_and_Wells_1965$CCLCD<-3
Metz_and_Wells_1965$CW<-NA
Metz_and_Wells_1965$H_STUMP<-0
Metz_and_Wells_1965$DBH<-Metz_and_Wells_1965$DBH.IN	# assuming DBH is in inches
Metz_and_Wells_1965$HT<-Metz_and_Wells_1965$TH
Metz_and_Wells_1965$HL_BRANCH<-NA
Metz_and_Wells_1965$DW_SW<-Metz_and_Wells_1965$DRY.WEIGHT.STEMWOOD.LB
Metz_and_Wells_1965$DW_SB<-Metz_and_Wells_1965$DRY.WEIGHT.STEM.BARK.LB
Metz_and_Wells_1965$DW_STEM<-Metz_and_Wells_1965$DW_SB+Metz_and_Wells_1965$DW_SW
Metz_and_Wells_1965$DEAD_BRANCH_DW<-NA
Metz_and_Wells_1965$LIVE_BRANCH_DW<-NA
Metz_and_Wells_1965$DW_BRANCH<-Metz_and_Wells_1965$DRY.WEIGHT.BRANCHES.LB
Metz_and_Wells_1965$DW_TOT<-Metz_and_Wells_1965$DW_BRANCH+Metz_and_Wells_1965$DW_STEM
Metz_and_Wells_1965$DW_FOL<-Metz_and_Wells_1965$DRY.WEIGHT.FOLIAGE.LB
Metz_and_Wells_1965$BIOMASS<-Metz_and_Wells_1965$DW_FOL+Metz_and_Wells_1965$DW_TOT
Metz_and_Wells_1965$STUMP<-NA
Metz_and_Wells_1965$ABG<-Metz_and_Wells_1965$BIOMASS
Metz_and_Wells_1965$ORIGIN<-2
Metz_and_Wells_1965$TEMP<-60
Metz_and_Wells_1965$DGL<-NA
Metz_and_Wells_1965$DSTEM_MIN<-NA
Metz_and_Wells_1965$DBLC<-NA
Metz_and_Wells_1965$DW_CROWN<-Metz_and_Wells_1965$DW_BRANCH+Metz_and_Wells_1965$DW_FOL
Metz_and_Wells_1965$ST_METH<-'1'
Metz_and_Wells_1965$BR_METH<-'2'
Metz_and_Wells_1965$FOL_METH<-'2'
Metz_and_Wells_1965$REGION<-'SE'

Metz_and_Wells_1965$Indicator <- 'unchecked'
Metz_and_Wells_1965$GW_STEM<-'unchecked'
Metz_and_Wells_1965$GW_BRANCH<-'unchecked'
Metz_and_Wells_1965$GW_FOL<-'unchecked'
Metz_and_Wells_1965$GW_CROWN<-'unchecked'
Metz_and_Wells_1965$GW_BIOMASS<-'unchecked'
Metz_and_Wells_1965$GW_ABG<-'unchecked'
Metz_and_Wells_1965$GW_TOT<-'unchecked'
Metz_and_Wells_1965$M.C.<-'unchecked'
Metz_and_Wells_1965$SP.GR<-'unchecked'
Metz_and_Wells_1965$Bark.Fraction<-'unchecked'
Metz_and_Wells_1965$Age<-'unchecked'


Metz_and_Wells_1965<-Metz_and_Wells_1965[,c(10:52)]
LegacyData<-rbind(LegacyData,Metz_and_Wells_1965)
#plot(LegacyData$DBH,LegacyData$BIOMASS)


################
# Santee 1970  # # Cowheeta
################
Santee_1970<-read.csv(paste(basepath,"Santee_1970_Data.csv",sep=""),header=T, as.is= T)
Santee_1970$AUTHOR<-"Santee"
Santee_1970$LOC<-1
Santee_1970$SPCD<-261
Santee_1970$CCLCD<-NA
Santee_1970$TREENO<-1:length(Santee_1970[,1])
Santee_1970$CW<-NA
Santee_1970$H_STUMP<-NA
Santee_1970$DBH<-Santee_1970$dbh.cm/2.54
Santee_1970$HT<-Santee_1970$height.m*3.28084
Santee_1970$HL_BRANCH<-NA
Santee_1970$DW_SW<-(Santee_1970$dry.bole.wt.kg-Santee_1970$dry.bark.wt.kg)/2.20462
Santee_1970$DW_SB<-Santee_1970$dry.bark.wt.kg/2.20462
Santee_1970$DW_STEM<-Santee_1970$DW_SB+Santee_1970$DW_SW
Santee_1970$DEAD_BRANCH_DW<-NA
Santee_1970$LIVE_BRANCH_DW<-NA
Santee_1970$DW_BRANCH<-Santee_1970$dry.branch.wt.kg/2.20462
Santee_1970$DW_TOT<-Santee_1970$DW_BRANCH+Santee_1970$DW_STEM
Santee_1970$DW_FOL<-Santee_1970$dry.needle.wt.kg
Santee_1970$BIOMASS<-Santee_1970$DW_FOL+Santee_1970$DW_TOT
Santee_1970$STUMP<-NA
Santee_1970$ABG<-NA
Santee_1970$ORIGIN<-1
Santee_1970$TEMP<-80
Santee_1970$DGL<-Santee_1970$basal.diameter.cm/2.54
Santee_1970$DSTEM_MIN<-NA
Santee_1970$DBLC<-NA
Santee_1970$DW_CROWN<-Santee_1970$DW_BRANCH+Santee_1970$DW_FOL
Santee_1970$ST_METH<-'1'
Santee_1970$BR_METH<-'5'
Santee_1970$FOL_METH<-'5'
Santee_1970$REGION<-'SE'

Santee_1970$Indicator <- 'unchecked'
Santee_1970$GW_STEM<-'unchecked'
Santee_1970$GW_BRANCH<-'unchecked'
Santee_1970$GW_FOL<-'unchecked'
Santee_1970$GW_CROWN<-'unchecked'
Santee_1970$GW_BIOMASS<-'unchecked'
Santee_1970$GW_ABG<-'unchecked'
Santee_1970$GW_TOT<-'unchecked'
Santee_1970$M.C.<-'unchecked'
Santee_1970$SP.GR<-'unchecked'
Santee_1970$Bark.Fraction<-'unchecked'
Santee_1970$Age<-'unchecked'


Santee_1970<-Santee_1970[,c(10:52)]
#names(Santee_1970)
#names(LegacyData.2)
LegacyData<-rbind(LegacyData,Santee_1970)

################
# Heilman 1961 #	
################
Heilman_1961<-read.csv(paste(basepath,"Heilman_1961_data.csv",sep=""),header=T, as.is= T)
Heilman_1961$AUTHOR<-"Heilman"
Heilman_1961[Heilman_1961$site=="upper pack","LOC"]<-3
Heilman_1961[Heilman_1961$site=="lower pack","LOC"]<-3
Heilman_1961[Heilman_1961$site=="whidbey","LOC"]<-1
Heilman_1961[Heilman_1961$site=="darrington","LOC"]<-2
Heilman_1961[Heilman_1961$site=="matlock","LOC"]<-4
Heilman_1961$SPCD<-202
Heilman_1961[Heilman_1961$crown.class=="dom","CCLCD"]<-2    #dom=2,codom=3,inter=4,sup=5
Heilman_1961[Heilman_1961$crown.class=="codom","CCLCD"]<-3
Heilman_1961[Heilman_1961$crown.class=="inter","CCLCD"]<-4
Heilman_1961[Heilman_1961$crown.class=="sup","CCLCD"]<-5
Heilman_1961$TREENO<-1:length(Heilman_1961[,1])
Heilman_1961$CW<-NA
Heilman_1961$H_STUMP<-0   #page 57
#str(Heilman_1961)
Heilman_1961$DBH<-Heilman_1961$dbh.in
Heilman_1961$HT<-Heilman_1961$tht.ft
Heilman_1961$HL_BRANCH<-NA
#head(Heilman_1961)
Heilman_1961$DW_SW<-Heilman_1961$stem.wood.dw.g*0.00220462
Heilman_1961$DW_SB<-Heilman_1961$stem.bark.dw.g*0.00220462
Heilman_1961$DW_STEM<-Heilman_1961$DW_SB+Heilman_1961$DW_SW
Heilman_1961$DEAD_BRANCH_DW<-Heilman_1961$dead.branch.dw.g*0.00220462
Heilman_1961$LIVE_BRANCH_DW<-Heilman_1961$live.branch.dw.g*0.00220462
Heilman_1961$DW_BRANCH<-Heilman_1961$DEAD_BRANCH_DW+Heilman_1961$LIVE_BRANCH_DW
Heilman_1961$DW_TOT<-Heilman_1961$DW_BRANCH+Heilman_1961$DW_STEM
Heilman_1961$DW_FOL<-Heilman_1961$needle.dw.g*0.00220462
Heilman_1961$BIOMASS<-Heilman_1961$DW_FOL+Heilman_1961$DW_TOT
Heilman_1961$STUMP<-NA
Heilman_1961$ABG<-Heilman_1961$BIOMASS
Heilman_1961$ORIGIN<-1
Heilman_1961$TEMP<-70
Heilman_1961$DGL<-NA
Heilman_1961$DSTEM_MIN<-NA
Heilman_1961$DBLC<-NA
Heilman_1961$DW_CROWN<-Heilman_1961$DW_BRANCH+Heilman_1961$DW_FOL
Heilman_1961$ST_METH<-'1'
Heilman_1961$BR_METH<-'2'
Heilman_1961$FOL_METH<-'2'
Heilman_1961$REGION<-'PNW'

Heilman_1961$Indicator <- 'unchecked'
Heilman_1961$GW_STEM<-'unchecked'
Heilman_1961$GW_BRANCH<-'unchecked'
Heilman_1961$GW_FOL<-'unchecked'
Heilman_1961$GW_CROWN<-'unchecked'
Heilman_1961$GW_BIOMASS<-'unchecked'
Heilman_1961$GW_ABG<-'unchecked'
Heilman_1961$GW_TOT<-'unchecked'
Heilman_1961$M.C.<-'unchecked'
Heilman_1961$SP.GR<-'unchecked'
Heilman_1961$Bark.Fraction<-'unchecked'
Heilman_1961$Age<-'unchecked'


Heilman_1961<-Heilman_1961[,c(14:56)]

#names(LegacyData.2)
#head(Heilman_1961)
#names(LegacyData);names(Heilman_1961)
#plot(Heilman_1961$DBH,Heilman_1961$BIOMASS)
LegacyData<-rbind(LegacyData,Heilman_1961)
#plot(LegacyData$DBH,LegacyData$BIOMASS)
dim(LegacyData) #6228

###################
# Reid et al 1974 # Colorado
###################
#names(Reid_et_al_1974)
Reid_et_al_1974<-read.csv(paste(basepath,"Reid_et_al_1974_Data.csv",sep=""),header=T, as.is= T)
Reid_et_al_1974$AUTHOR<-"Reid_et_al"
Reid_et_al_1974$LOC<-1
Reid_et_al_1974$SPCD<-108
Reid_et_al_1974$CCLCD<-NA
Reid_et_al_1974$TREENO<-Reid_et_al_1974$TREE.NO
Reid_et_al_1974$CW<-NA
Reid_et_al_1974$H_STUMP<-NA
Reid_et_al_1974$DBH<-Reid_et_al_1974$DBH.CM/2.54
Reid_et_al_1974$HT<-NA
Reid_et_al_1974$HL_BRANCH<-NA
Reid_et_al_1974$DW_SW<-NA
Reid_et_al_1974$DW_SB<-NA
Reid_et_al_1974$DW_STEM<-Reid_et_al_1974$STEM.BIOMASS.KG*2.20462
Reid_et_al_1974$DEAD_BRANCH_DW<-NA
Reid_et_al_1974$LIVE_BRANCH_DW<-Reid_et_al_1974$LIVING.BRANCH.BIOMASS*2.20462
Reid_et_al_1974$DW_BRANCH<-Reid_et_al_1974$LIVE_BRANCH_DW
Reid_et_al_1974$DW_TOT<-Reid_et_al_1974$DW_BRANCH+Reid_et_al_1974$DW_STEM
Reid_et_al_1974$DW_FOL<-Reid_et_al_1974$GREEN.NEEDLE.BIOMASS.KG*2.20462
Reid_et_al_1974$BIOMASS<-Reid_et_al_1974$DW_FOL+Reid_et_al_1974$DW_TOT
Reid_et_al_1974$STUMP<-NA
Reid_et_al_1974$ABG<-NA
Reid_et_al_1974$ORIGIN<-NA
Reid_et_al_1974$TEMP<-70
Reid_et_al_1974$DGL<-NA
Reid_et_al_1974$DSTEM_MIN<-NA
Reid_et_al_1974$DBLC<-NA
Reid_et_al_1974$DW_CROWN<-Reid_et_al_1974$DW_BRANCH+Reid_et_al_1974$DW_FOL
Reid_et_al_1974$ST_METH<-'1'
Reid_et_al_1974$BR_METH<-'5'
Reid_et_al_1974$FOL_METH<-'5'
Reid_et_al_1974$REGION<-'IMW'

Reid_et_al_1974$Indicator <- 'unchecked'
Reid_et_al_1974$GW_STEM<-'unchecked'
Reid_et_al_1974$GW_BRANCH<-'unchecked'
Reid_et_al_1974$GW_FOL<-'unchecked'
Reid_et_al_1974$GW_CROWN<-'unchecked'
Reid_et_al_1974$GW_BIOMASS<-'unchecked'
Reid_et_al_1974$GW_ABG<-'unchecked'
Reid_et_al_1974$GW_TOT<-'unchecked'
Reid_et_al_1974$M.C.<-'unchecked'
Reid_et_al_1974$SP.GR<-'unchecked'
Reid_et_al_1974$Bark.Fraction<-'unchecked'
Reid_et_al_1974$Age<-'unchecked'


Reid_et_al_1974<-Reid_et_al_1974[,c(10:52)]

#names(LegacyData)
LegacyData<-rbind(LegacyData,Reid_et_al_1974)
dim(LegacyData)   # 6247
################
# Hutnik 1964  #  PA
################
#names(Hutnik_1964)
Hutnik_1964<-read.csv(paste(basepath,"Hutnik_1964_Data.csv",sep=""),header=T, as.is= T)
Hutnik_1964$AUTHOR<-"Hutnik"
Hutnik_1964$LOC<-1
Hutnik_1964$SPCD<-125
Hutnik_1964$CCLCD<-NA
Hutnik_1964$TREENO<-Hutnik_1964$TREE.ID
Hutnik_1964$CW<-NA
Hutnik_1964$H_STUMP<-0.5
Hutnik_1964$DBH<-Hutnik_1964$DBH.IN
Hutnik_1964$HT<-Hutnik_1964$TH.FT
Hutnik_1964$HL_BRANCH<-NA
Hutnik_1964$DW_SW<-Hutnik_1964$STEM.WOOD.DW.LB
Hutnik_1964$DW_SB<-Hutnik_1964$STEM.BARK.DW.LB
Hutnik_1964$DW_STEM<-Hutnik_1964$DW_SB+Hutnik_1964$DW_SW
Hutnik_1964$DEAD_BRANCH_DW<-NA
Hutnik_1964$LIVE_BRANCH_DW<-NA
Hutnik_1964$DW_BRANCH<-Hutnik_1964$BRANCH.DW.LB
Hutnik_1964$DW_TOT<-Hutnik_1964$DW_BRANCH+Hutnik_1964$DW_STEM
Hutnik_1964$DW_FOL<-Hutnik_1964$FOL.DW.LB
Hutnik_1964$BIOMASS<-Hutnik_1964$DW_FOL+Hutnik_1964$DW_TOT
Hutnik_1964$STUMP<-NA
Hutnik_1964$ABG<-NA
Hutnik_1964$ORIGIN<-2
Hutnik_1964$TEMP<-80
Hutnik_1964$DGL<-NA
Hutnik_1964$DSTEM_MIN<-NA
Hutnik_1964$DBLC<-NA
Hutnik_1964$DW_CROWN<-Hutnik_1964$DW_BRANCH+Hutnik_1964$DW_FOL
Hutnik_1964$ST_METH<-'4'							# not entirely clear, semms more likely to be method 3? there is no talk of taking sections but some methods describing sp. gravity
Hutnik_1964$BR_METH<-'3'
Hutnik_1964$FOL_METH<-'3'
Hutnik_1964$REGION<-'NE'

Hutnik_1964$Indicator <- 'unchecked'
Hutnik_1964$GW_STEM<-'unchecked'
Hutnik_1964$GW_BRANCH<-'unchecked'
Hutnik_1964$GW_FOL<-'unchecked'
Hutnik_1964$GW_CROWN<-'unchecked'
Hutnik_1964$GW_BIOMASS<-'unchecked'
Hutnik_1964$GW_ABG<-'unchecked'
Hutnik_1964$GW_TOT<-'unchecked'
Hutnik_1964$M.C.<-'unchecked'
Hutnik_1964$SP.GR<-'unchecked'
Hutnik_1964$Bark.Fraction<-'unchecked'
Hutnik_1964$Age<-'unchecked'


Hutnik_1964<-Hutnik_1964[,c(18:60)]
#names(Hutnik_1964)
#names(LegacyData)
LegacyData<-rbind(LegacyData,Hutnik_1964)

################
# Vaidya 1961  # Durham NC
################
#names(Vaidya_1961)
Vaidya_1961<-read.csv(paste(basepath,"Vaidya_1961_Data.csv",sep=""),header=T, as.is= T)
Vaidya_1961$AUTHOR<-"Vaidya"
Vaidya_1961$LOC<-1
Vaidya_1961$SPCD<-110
Vaidya_1961$CCLCD<-NA
Vaidya_1961$TREENO<-Vaidya_1961$TREE.ID
Vaidya_1961$CW<-NA
Vaidya_1961$H_STUMP<-0
Vaidya_1961$DBH<-Vaidya_1961$DBH.IN
Vaidya_1961$HT<-Vaidya_1961$TH.FT
Vaidya_1961$HL_BRANCH<-NA
Vaidya_1961$DW_SW<-NA
Vaidya_1961$DW_SB<-NA
Vaidya_1961$DW_STEM<-Vaidya_1961$STEM.DW.LB
Vaidya_1961$DEAD_BRANCH_DW<-NA
Vaidya_1961$LIVE_BRANCH_DW<-NA
Vaidya_1961$DW_BRANCH<-Vaidya_1961$BRANCH.DW.LB
Vaidya_1961$DW_TOT<-Vaidya_1961$DW_BRANCH+Vaidya_1961$DW_STEM
Vaidya_1961$DW_FOL<-Vaidya_1961$FOLIAGE.DW.LB
Vaidya_1961$BIOMASS<-Vaidya_1961$DW_FOL+Vaidya_1961$DW_TOT
Vaidya_1961$STUMP<-NA
Vaidya_1961$ABG<-Vaidya_1961$BIOMASS
Vaidya_1961$ORIGIN<-NA
Vaidya_1961$TEMP<-70
Vaidya_1961$DGL<-NA
Vaidya_1961$DSTEM_MIN<-NA
Vaidya_1961$DBLC<-NA
Vaidya_1961$DW_CROWN<-Vaidya_1961$DW_BRANCH+Vaidya_1961$DW_FOL
Vaidya_1961$ST_METH<-'1'
Vaidya_1961$BR_METH<-'1'
Vaidya_1961$FOL_METH<-'1'
Vaidya_1961$REGION<-'SE'

Vaidya_1961$Indicator <- 'unchecked'
Vaidya_1961$GW_STEM<-'unchecked'
Vaidya_1961$GW_BRANCH<-'unchecked'
Vaidya_1961$GW_FOL<-'unchecked'
Vaidya_1961$GW_CROWN<-'unchecked'
Vaidya_1961$GW_BIOMASS<-'unchecked'
Vaidya_1961$GW_ABG<-'unchecked'
Vaidya_1961$GW_TOT<-'unchecked'
Vaidya_1961$M.C.<-'unchecked'
Vaidya_1961$SP.GR<-'unchecked'
Vaidya_1961$Bark.Fraction<-'unchecked'
Vaidya_1961$Age<-'unchecked'


Vaidya_1961<-Vaidya_1961[,c(8:50)]

#names(LegacyData)
LegacyData<-rbind(LegacyData,Vaidya_1961)

################
# Garbett 1977  # FL
################
#names(Garbett_1977)
Garbett_1977<-read.csv(paste(basepath,"Garbett_1977_Data.csv",sep=""),header=T, as.is= T)
Garbett_1977$AUTHOR<-"Garbett"
Garbett_1977$LOC<-1
Garbett_1977[Garbett_1977$spp=="slash pine","SPCD"]<-111
Garbett_1977[Garbett_1977$spp=="longleaf pine","SPCD"]<-121
Garbett_1977$CCLCD<-NA
Garbett_1977$TREENO<-1:length(Garbett_1977[,1])
Garbett_1977$CW<-NA
Garbett_1977$H_STUMP<-0
Garbett_1977$DBH<-Garbett_1977$dbh.cm/2.54
Garbett_1977$HT<-Garbett_1977$tht.m*3.28084
Garbett_1977$HL_BRANCH<-NA
Garbett_1977$DW_SW<-Garbett_1977$wood.dw.kg*2.20462
Garbett_1977$DW_SB<-Garbett_1977$bark.dw.kg*2.20462
Garbett_1977$DW_STEM<-Garbett_1977$DW_SB+Garbett_1977$DW_SW
Garbett_1977$DEAD_BRANCH_DW<-Garbett_1977$dead.branch.dw.kg*2.20462
Garbett_1977$LIVE_BRANCH_DW<-Garbett_1977$live.branch.dw.kg*2.20462
Garbett_1977$DW_BRANCH<-Garbett_1977$LIVE_BRANCH_DW+Garbett_1977$DEAD_BRANCH_DW
Garbett_1977$DW_TOT<-Garbett_1977$DW_BRANCH+Garbett_1977$DW_STEM
Garbett_1977$DW_FOL<-Garbett_1977$fol.dw.kg*2.20462
Garbett_1977$BIOMASS<-Garbett_1977$DW_FOL+Garbett_1977$DW_TOT
Garbett_1977$STUMP<-NA
Garbett_1977$ABG<-Garbett_1977$BIOMASS
Garbett_1977$ORIGIN<-1
Garbett_1977$TEMP<-65
Garbett_1977$DGL<-NA
Garbett_1977$DSTEM_MIN<-3
Garbett_1977$DBLC<-NA
Garbett_1977$DW_CROWN<-Garbett_1977$DW_BRANCH+Garbett_1977$DW_FOL
Garbett_1977$ST_METH<-'1'
Garbett_1977$BR_METH<-'5'
Garbett_1977$FOL_METH<-'5'
Garbett_1977$REGION<-'SE'

Garbett_1977$Indicator <- 'unchecked'
Garbett_1977$GW_STEM<-'unchecked'
Garbett_1977$GW_BRANCH<-'unchecked'
Garbett_1977$GW_FOL<-'unchecked'
Garbett_1977$GW_CROWN<-'unchecked'
Garbett_1977$GW_BIOMASS<-'unchecked'
Garbett_1977$GW_ABG<-'unchecked'
Garbett_1977$GW_TOT<-'unchecked'
Garbett_1977$M.C.<-'unchecked'
Garbett_1977$SP.GR<-'unchecked'
Garbett_1977$Bark.Fraction<-'unchecked'
Garbett_1977$Age<-'unchecked'


Garbett_1977<-Garbett_1977[,c(11:53)]

#names(LegacyData)
LegacyData<-rbind(LegacyData,Garbett_1977)


################
# Woodard 1974 #
################
#names(Woodard_1974)
Woodard_1974<-read.csv(paste(basepath,"Woodard_1974_Data.csv",sep=""),header=T, as.is= T)
Woodard_1974$AUTHOR<-"Woodard"
Woodard_1974$LOC<-1
Woodard_1974$SPCD<-202
Woodard_1974$CCLCD<-NA
Woodard_1974$TREENO<-1:length(Woodard_1974[,1])
Woodard_1974$CW<-NA
Woodard_1974$H_STUMP<-NA
Woodard_1974$DBH<-Woodard_1974$dbh.cm/2.54
Woodard_1974$HT<-NA
Woodard_1974$HL_BRANCH<-NA
Woodard_1974$DW_SW<-NA
Woodard_1974$DW_SB<-NA
Woodard_1974$DW_STEM<-NA
Woodard_1974$DEAD_BRANCH_DW<-Woodard_1974$dead.branch.dw.g*0.00220462
Woodard_1974$LIVE_BRANCH_DW<-Woodard_1974$live.branch.dw.g*0.00220462
Woodard_1974$DW_BRANCH<-Woodard_1974$LIVE_BRANCH_DW+Woodard_1974$DEAD_BRANCH_DW
Woodard_1974$DW_TOT<-NA
Woodard_1974$DW_FOL<-Woodard_1974$fol.dw.g*0.00220462
Woodard_1974$BIOMASS<-NA
Woodard_1974$STUMP<-NA
Woodard_1974$ABG<-NA
Woodard_1974$ORIGIN<-NA
Woodard_1974$TEMP<-70
Woodard_1974$DGL<-NA
Woodard_1974$DSTEM_MIN<-NA
Woodard_1974$DBLC<-NA
Woodard_1974$DW_CROWN<-Woodard_1974$DW_BRANCH+Woodard_1974$DW_FOL
Woodard_1974$ST_METH<-'NA'
Woodard_1974$BR_METH<-'2'
Woodard_1974$FOL_METH<-'2'
Woodard_1974$REGION<-'PNW'

Woodard_1974$Indicator <- 'unchecked'
Woodard_1974$GW_STEM<-'unchecked'
Woodard_1974$GW_BRANCH<-'unchecked'
Woodard_1974$GW_FOL<-'unchecked'
Woodard_1974$GW_CROWN<-'unchecked'
Woodard_1974$GW_BIOMASS<-'unchecked'
Woodard_1974$GW_ABG<-'unchecked'
Woodard_1974$GW_TOT<-'unchecked'
Woodard_1974$M.C.<-'unchecked'
Woodard_1974$SP.GR<-'unchecked'
Woodard_1974$Bark.Fraction<-'unchecked'
Woodard_1974$Age<-'unchecked'


Woodard_1974<-Woodard_1974[,c(11:53)]
#names(Woodard_1974)
#names(LegacyData)
LegacyData<-rbind(LegacyData,Woodard_1974)

####################
# Kline et al 1973 #
####################
#names(Kline_et_al_1973)
Kline_et_al_1973<-read.csv(paste(basepath,"Kline_et_al_1973_Data.csv",sep=""),header=T, as.is= T)
Kline_et_al_1973$AUTHOR<-"Kline_et_al"
Kline_et_al_1973$LOC<-1
Kline_et_al_1973$SPCD<-202
Kline_et_al_1973$CCLCD<-NA
Kline_et_al_1973$TREENO<-Kline_et_al_1973$tree.no
Kline_et_al_1973$CW<-NA
Kline_et_al_1973$H_STUMP<-NA
Kline_et_al_1973$DBH<-Kline_et_al_1973$dbh.cm/2.54
Kline_et_al_1973$HT<-Kline_et_al_1973$tht.m*3.28084
Kline_et_al_1973$HL_BRANCH<-NA
Kline_et_al_1973$DW_SW<-NA
Kline_et_al_1973$DW_SB<-NA
Kline_et_al_1973$DW_STEM<-NA
Kline_et_al_1973$DEAD_BRANCH_DW<-NA
Kline_et_al_1973$LIVE_BRANCH_DW<-NA
Kline_et_al_1973$DW_BRANCH<-NA
Kline_et_al_1973$DW_TOT<-NA
Kline_et_al_1973$DW_FOL<-Kline_et_al_1973$fol.dw.kg*2.20462
Kline_et_al_1973$BIOMASS<-Kline_et_al_1973$tot.biomass.kg*2.20462
Kline_et_al_1973$STUMP<-NA
Kline_et_al_1973$ABG<-NA
Kline_et_al_1973$ORIGIN<-NA
Kline_et_al_1973$TEMP<-NA
Kline_et_al_1973$DGL<-NA
Kline_et_al_1973$DSTEM_MIN<-NA
Kline_et_al_1973$DBLC<-NA
Kline_et_al_1973$DW_CROWN<-NA
Kline_et_al_1973$ST_METH<-'4'
Kline_et_al_1973$BR_METH<-'4'
Kline_et_al_1973$FOL_METH<-'4'
Kline_et_al_1973$REGION<-'PNW'

Kline_et_al_1973$Indicator <- 'unchecked'
Kline_et_al_1973$GW_STEM<-'unchecked'
Kline_et_al_1973$GW_BRANCH<-'unchecked'
Kline_et_al_1973$GW_FOL<-'unchecked'
Kline_et_al_1973$GW_CROWN<-'unchecked'
Kline_et_al_1973$GW_BIOMASS<-'unchecked'
Kline_et_al_1973$GW_ABG<-'unchecked'
Kline_et_al_1973$GW_TOT<-'unchecked'
Kline_et_al_1973$M.C.<-'unchecked'
Kline_et_al_1973$SP.GR<-'unchecked'
Kline_et_al_1973$Bark.Fraction<-'unchecked'
Kline_et_al_1973$Age<-'unchecked'


Kline_et_al_1973<-Kline_et_al_1973[,c(7:49)]

#names(LegacyData)
LegacyData<-rbind(LegacyData,Kline_et_al_1973)


####################
# Marks 1971       #
####################
#Marks collected data in three different forms

Marks_1971_1<-read.csv(paste(basepath,"Marks_1971_Data_1.csv",sep=""),header=T, as.is= T)
Marks_1971_2<-read.csv(paste(basepath,"Marks_1971_Data_2.csv",sep=""),header=T, as.is= T)
Marks_1971_3<-read.csv(paste(basepath,"Marks_1971_Data_3.csv",sep=""),header=T, as.is= T)
#names(Marks_1971_1)
#names(Marks_1971_2)
#names(Marks_1971_3)
#create dbh.cm column for table 1
Marks_1971_1$dbh.cm<-NA
#remove aboveground column from table 1
Marks_1971_1$abovegroud.bio.g<-NULL
#create branch and dead branch weight columns
#Marks_1971_1$tot.dw.twigs.g<-NA
Marks_1971_1$dead.branch.dw.g<-NA
Marks_1971_2$dead.branch.dw.g<-NA
#create stem wood and stem bark columns for table 3
Marks_1971_3$stem.wood.dw.g<-NA
Marks_1971_3$stem.bark.dw.g<-NA
#remove fruit column from table 3
Marks_1971_3$fruit.dw.g<-NULL
#create dgl column for tables 2 and 3
Marks_1971_2$dgl<-NA
Marks_1971_3$dgl<-NA
#create stem total dw column for tables 1 and 2
Marks_1971_2$stem.dw.g<-NA
Marks_1971_1$stem.dw.g<-NA

#reorder columns to be the same
Marks_1971_1C<-Marks_1971_1
Marks_1971_2C<-Marks_1971_2[,c(1,2,12,4,5,6,7,8,9,10,3,11,13)]
Marks_1971_3C<-Marks_1971_3[,c(1,2,13,4,5,6,7,11,12,9,3,10,8)]
Marks_1971_1C$loc<-1
Marks_1971_2C$loc<-2
Marks_1971_3C$loc<-3
Marks_1971<-rbind(Marks_1971_1C,Marks_1971_2C,Marks_1971_3C)
#names(Marks_1971)
Marks_1971[is.na(Marks_1971)]<-0
Marks_1971$AUTHOR<-"Marks"
Marks_1971$LOC<-Marks_1971$loc
Marks_1971[Marks_1971$spp=="pin cherry","SPCD"]<-761
Marks_1971[Marks_1971$spp=="quaking aspen","SPCD"]<-746
Marks_1971$CCLCD<-NA
Marks_1971$TREENO<-1:length(Marks_1971[,1])
Marks_1971$CW<-NA
Marks_1971$H_STUMP<-0
Marks_1971$DBH<-Marks_1971$dbh.cm/2.54
Marks_1971$HT<-NA
Marks_1971$HL_BRANCH<-NA
Marks_1971$DW_SW<-Marks_1971$stem.wood.dw.g*0.00220462
Marks_1971$DW_SB<-Marks_1971$stem.bark.dw.g*0.00220462
Marks_1971$DW_STEM<-Marks_1971$DW_SW+Marks_1971$DW_SB+(Marks_1971$stem.dw.g*0.00220462)
Marks_1971$DEAD_BRANCH_DW<-Marks_1971$dead.branch.dw.g*0.00220462
Marks_1971$LIVE_BRANCH_DW<-(Marks_1971$tot.dw.twigs.g+Marks_1971$tot.dw.branch.g)*0.00220462
Marks_1971$DW_BRANCH<-Marks_1971$LIVE_BRANCH_DW+Marks_1971$DEAD_BRANCH_DW
Marks_1971$DW_TOT<-Marks_1971$DW_BRANCH+Marks_1971$DW_STEM
Marks_1971$DW_FOL<-Marks_1971$foliage.dw.g*0.00220462
Marks_1971$BIOMASS<-Marks_1971$DW_TOT+Marks_1971$DW_FOL
Marks_1971$STUMP<-NA
Marks_1971$ABG<-Marks_1971$BIOMASS
Marks_1971$ORIGIN<-1
Marks_1971$TEMP<-100
Marks_1971$DGL<-NA
Marks_1971$DSTEM_MIN<-NA
Marks_1971$DBLC<-NA
Marks_1971$DW_CROWN<-Marks_1971$DW_FOL+Marks_1971$DW_BRANCH
Marks_1971$ST_METH<-'1' # sites Whittaker in methodology
Marks_1971$BR_METH<-'3'
Marks_1971$FOL_METH<-'3'
Marks_1971$REGION<-'NE'

Marks_1971$Indicator <- 'unchecked'
Marks_1971$GW_STEM<-'unchecked'
Marks_1971$GW_BRANCH<-'unchecked'
Marks_1971$GW_FOL<-'unchecked'
Marks_1971$GW_CROWN<-'unchecked'
Marks_1971$GW_BIOMASS<-'unchecked'
Marks_1971$GW_ABG<-'unchecked'
Marks_1971$GW_TOT<-'unchecked'
Marks_1971$M.C.<-'unchecked'
Marks_1971$SP.GR<-'unchecked'
Marks_1971$Bark.Fraction<-'unchecked'
Marks_1971$Age<-'unchecked'


Marks_1971<-Marks_1971[,c(15:57)]
Marks_1971[Marks_1971==0]<-NA
#names(Marks_1971)
#names(LegacyData)
LegacyData<-rbind(LegacyData,Marks_1971)
dim(LegacyData) # 6386
####################
# Wiant            # West Virginia
####################
#names(Wiant)
Wiant<-read.csv(paste(basepath,"Wiant_Data.csv",sep=""),header=T, as.is= T)
Wiant[is.na(Wiant)]<-0
Wiant$AUTHOR<-"Wiant"
Wiant$LOC<-1
Wiant[Wiant$species=="BC","SPCD"]<-762
Wiant[Wiant$species=="BO","SPCD"]<-837
Wiant[Wiant$species=="CO","SPCD"]<-832
Wiant[Wiant$species=="HI","SPCD"]<-400
Wiant[Wiant$species=="NRO","SPCD"]<-833
Wiant[Wiant$species=="RM","SPCD"]<-316
Wiant[Wiant$species=="SO","SPCD"]<-806
Wiant[Wiant$species=="WO","SPCD"]<-802
Wiant[Wiant$species=="YP","SPCD"]<-621
Wiant$TREENO<-Wiant$treeno
Wiant$CCLCD<-NA
Wiant$CW<-NA
Wiant$H_STUMP<-0.5
Wiant$DBH<-Wiant$dbh.in
Wiant$HT<-Wiant$tht.ft
Wiant$HL_BRANCH<-NA
Wiant$DW_SW<-Wiant$merch.wood.wt
Wiant$DW_SB<-Wiant$merch.bark.wt
Wiant$DW_STEM<-Wiant$DW_SB+Wiant$DW_SW
Wiant$DEAD_BRANCH_DW<-NA
Wiant$LIVE_BRANCH_DW<-NA
Wiant$DW_BRANCH<-Wiant$dry.branch.wt
Wiant$DW_TOT<-Wiant$tot.dry.wt
Wiant$DW_FOL<-NA
Wiant$BIOMASS<-NA
Wiant$STUMP<-NA
Wiant$ABG<-NA
Wiant$ORIGIN<-NA
Wiant$TEMP<-NA
Wiant$DGL<-NA
Wiant$DSTEM_MIN<-4				
Wiant$DBLC<-NA
Wiant$DW_CROWN<-NA
Wiant$ST_METH<-'1'			# Wedge shaped, rather than disks
Wiant$BR_METH<-NA
Wiant$FOL_METH<-NA
Wiant$REGION<-'NE'

Wiant$Indicator <- 'unchecked'
Wiant$GW_STEM<-'unchecked'
Wiant$GW_BRANCH<-'unchecked'
Wiant$GW_FOL<-'unchecked'
Wiant$GW_CROWN<-'unchecked'
Wiant$GW_BIOMASS<-'unchecked'
Wiant$GW_ABG<-'unchecked'
Wiant$GW_TOT<-'unchecked'
Wiant$M.C.<-'unchecked'
Wiant$SP.GR<-'unchecked'
Wiant$Bark.Fraction<-'unchecked'
Wiant$Age<-'unchecked'


Wiant<-Wiant[,c(19:61)]
Wiant[Wiant==0]<-NA
#names(Wiant)
#names(LegacyData)
LegacyData<-rbind(LegacyData,Wiant)

#######################
# Nelson_and_Switzer  # Central Mississippi
#######################
#names(Nelson_and_Switzer)
Nelson_and_Switzer<-read.csv(paste(basepath,"Nelson_and_Switzer_1975_Data.csv",sep=""),header=T, as.is= T)
#Nelson_and_Switzer[is.na(Nelson_and_Switzer)]<-0
Nelson_and_Switzer$AUTHOR<-"Nelson_and_Switzer"
Nelson_and_Switzer$LOC<-1
Nelson_and_Switzer$SPCD<-131
Nelson_and_Switzer$TREENO<-1:length(Nelson_and_Switzer[,1])
Nelson_and_Switzer$CCLCD<-NA
Nelson_and_Switzer$CW<-NA
Nelson_and_Switzer$H_STUMP<-NA
Nelson_and_Switzer$DBH<-Nelson_and_Switzer$dbh.in
Nelson_and_Switzer$HT<-Nelson_and_Switzer$ht.ft
Nelson_and_Switzer$HL_BRANCH<-NA
Nelson_and_Switzer$DW_SW<-NA
Nelson_and_Switzer$DW_SB<-NA
Nelson_and_Switzer$DW_STEM<-NA
Nelson_and_Switzer$DEAD_BRANCH_DW<-NA
Nelson_and_Switzer$LIVE_BRANCH_DW<-NA
Nelson_and_Switzer$DW_BRANCH<-NA
Nelson_and_Switzer$DW_TOT<-NA
Nelson_and_Switzer$DW_FOL<-NA
Nelson_and_Switzer$BIOMASS<-Nelson_and_Switzer$total.weight.dry.lb
Nelson_and_Switzer$STUMP<-NA
Nelson_and_Switzer$ABG<-NA
Nelson_and_Switzer$ORIGIN<-1
Nelson_and_Switzer$TEMP<-75
Nelson_and_Switzer$DGL<-NA
Nelson_and_Switzer$DSTEM_MIN<-NA
Nelson_and_Switzer$DBLC<-NA
Nelson_and_Switzer$DW_CROWN<-NA
Nelson_and_Switzer$ST_METH<-'1'
Nelson_and_Switzer$BR_METH<-'5'
Nelson_and_Switzer$FOL_METH<-'5'
Nelson_and_Switzer$REGION<-'SE'
Nelson_and_Switzer$Indicator <- 'unchecked'
Nelson_and_Switzer$GW_STEM<-'unchecked'
Nelson_and_Switzer$GW_BRANCH<-'unchecked'
Nelson_and_Switzer$GW_FOL<-'unchecked'
Nelson_and_Switzer$GW_CROWN<-'unchecked'
Nelson_and_Switzer$GW_BIOMASS<-'unchecked'
Nelson_and_Switzer$GW_ABG<-'unchecked'
Nelson_and_Switzer$GW_TOT<-'unchecked'
Nelson_and_Switzer$M.C.<-'unchecked'
Nelson_and_Switzer$SP.GR<-'unchecked'
Nelson_and_Switzer$Bark.Fraction<-'unchecked'
Nelson_and_Switzer$Age<-'unchecked'


Nelson_and_Switzer<-Nelson_and_Switzer[,c(11:53)]

#names(LegacyData)
LegacyData<-rbind(LegacyData,Nelson_and_Switzer)

#######################
# Marshall and Wang   # # British Columbia
#######################
#names(Marshall_and_Wang_1995)
Marshall_and_Wang_1995<-read.csv(paste(basepath,"Marshall_and_Wang_1995_Data.csv",sep=""),header=T, as.is= T)
#Marshall_and_Wang_1995[is.na(Marshall_and_Wang_1995)]<-0
Marshall_and_Wang_1995$AUTHOR<-"Marshall_and_Wang"
Marshall_and_Wang_1995$LOC<-1
Marshall_and_Wang_1995$SPCD<-202
Marshall_and_Wang_1995$TREENO<-1:length(Marshall_and_Wang_1995[,1])
Marshall_and_Wang_1995$CCLCD<-Marshall_and_Wang_1995$crown.class
Marshall_and_Wang_1995$CW<-Marshall_and_Wang_1995$avg.crown.width.m*3.28084
Marshall_and_Wang_1995$H_STUMP<-NA
Marshall_and_Wang_1995$DBH<-Marshall_and_Wang_1995$dbh.cm/2.54
Marshall_and_Wang_1995$HT<-Marshall_and_Wang_1995$tht.m*3.28084
Marshall_and_Wang_1995$HL_BRANCH<-(Marshall_and_Wang_1995$tht.m-Marshall_and_Wang_1995$crown.length.m)*3.28084
Marshall_and_Wang_1995$DW_SW<-Marshall_and_Wang_1995$stemwood.kg*2.20462
Marshall_and_Wang_1995$DW_SB<-Marshall_and_Wang_1995$stembark.kg*2.20462
Marshall_and_Wang_1995$DW_STEM<-Marshall_and_Wang_1995$DW_SB+Marshall_and_Wang_1995$DW_SW
Marshall_and_Wang_1995$DEAD_BRANCH_DW<-NA
Marshall_and_Wang_1995$LIVE_BRANCH_DW<-Marshall_and_Wang_1995$live.branch.kg*2.20462
Marshall_and_Wang_1995$DW_BRANCH<-NA
Marshall_and_Wang_1995$DW_TOT<-Marshall_and_Wang_1995$LIVE_BRANCH_DW+Marshall_and_Wang_1995$DW_STEM
Marshall_and_Wang_1995$DW_FOL<-Marshall_and_Wang_1995$live.needles.kg*2.20462
Marshall_and_Wang_1995$BIOMASS<-Marshall_and_Wang_1995$DW_TOT+Marshall_and_Wang_1995$DW_FOL
Marshall_and_Wang_1995$STUMP<-(Marshall_and_Wang_1995$stumpwood.kg+Marshall_and_Wang_1995$stumpbark.kg)*2.20462
Marshall_and_Wang_1995$ABG<-Marshall_and_Wang_1995$BIOMASS+Marshall_and_Wang_1995$STUMP
Marshall_and_Wang_1995$ORIGIN<-NA
Marshall_and_Wang_1995$TEMP<-NA
Marshall_and_Wang_1995$DGL<-NA
Marshall_and_Wang_1995$DSTEM_MIN<-NA
Marshall_and_Wang_1995$DBLC<-NA
Marshall_and_Wang_1995$DW_CROWN<-Marshall_and_Wang_1995$DW_FOL+Marshall_and_Wang_1995$LIVE_BRANCH_DW
Marshall_and_Wang_1995$ST_METH<-'1'
Marshall_and_Wang_1995$BR_METH<-'4'
Marshall_and_Wang_1995$FOL_METH<-'4'
Marshall_and_Wang_1995$REGION<-NA
Marshall_and_Wang_1995$Indicator <- 'unchecked'
Marshall_and_Wang_1995$GW_STEM<-'unchecked'
Marshall_and_Wang_1995$GW_BRANCH<-'unchecked'
Marshall_and_Wang_1995$GW_FOL<-'unchecked'
Marshall_and_Wang_1995$GW_CROWN<-'unchecked'
Marshall_and_Wang_1995$GW_BIOMASS<-'unchecked'
Marshall_and_Wang_1995$GW_ABG<-'unchecked'
Marshall_and_Wang_1995$GW_TOT<-'unchecked'
Marshall_and_Wang_1995$M.C.<-'unchecked'
Marshall_and_Wang_1995$SP.GR<-'unchecked'
Marshall_and_Wang_1995$Bark.Fraction<-'unchecked'
Marshall_and_Wang_1995$Age<-'unchecked'

Marshall_and_Wang_1995<-Marshall_and_Wang_1995[,c(20:62)]

#names(LegacyData)
LegacyData<-rbind(LegacyData,Marshall_and_Wang_1995)

#######################
# Johnston and Bartos #
#######################
#names(Johnston_and_Bartos_1977)
Johnston_and_Bartos_1977<-read.csv(paste(basepath,"Johnston_and_Bartos_1977_Data.csv",sep=""),header=T, as.is= T)
#Johnston_and_Bartos_1977[is.na(Johnston_and_Bartos_1977)]<-0
Johnston_and_Bartos_1977$AUTHOR<-"Johnston_and_Bartos"
Johnston_and_Bartos_1977$LOC<-Johnston_and_Bartos_1977$site
Johnston_and_Bartos_1977$SPCD<-746
Johnston_and_Bartos_1977$TREENO<-1:length(Johnston_and_Bartos_1977[,1])
Johnston_and_Bartos_1977$CCLCD<-NA
Johnston_and_Bartos_1977$CW<-NA
Johnston_and_Bartos_1977$H_STUMP<-NA
Johnston_and_Bartos_1977$DBH<-Johnston_and_Bartos_1977$dbh.cm/2.54
Johnston_and_Bartos_1977$HT<-Johnston_and_Bartos_1977$tht.m*3.28084
Johnston_and_Bartos_1977$HL_BRANCH<-NA
Johnston_and_Bartos_1977$DW_SW<-Johnston_and_Bartos_1977$bole.kg*2.20462
Johnston_and_Bartos_1977$DW_SB<-NA
Johnston_and_Bartos_1977$DW_STEM<-NA
Johnston_and_Bartos_1977$DEAD_BRANCH_DW<-Johnston_and_Bartos_1977$dead.branch.kg*2.20462
Johnston_and_Bartos_1977[is.na(Johnston_and_Bartos_1977)]<-0
Johnston_and_Bartos_1977$LIVE_BRANCH_DW<-(Johnston_and_Bartos_1977$c.twig.wt.kg+Johnston_and_Bartos_1977$o.twig.wt.kg+Johnston_and_Bartos_1977$branch.wt.kg)*2.20462
Johnston_and_Bartos_1977[Johnston_and_Bartos_1977==0]<-NA
Johnston_and_Bartos_1977$DW_BRANCH<-Johnston_and_Bartos_1977$LIVE_BRANCH_DW+Johnston_and_Bartos_1977$DEAD_BRANCH_DW
Johnston_and_Bartos_1977$DW_TOT<-Johnston_and_Bartos_1977$DW_BRANCH+Johnston_and_Bartos_1977$DW_STEM
Johnston_and_Bartos_1977$DW_FOL<-Johnston_and_Bartos_1977$foliage.wt.kg*2.20462
Johnston_and_Bartos_1977$BIOMASS<-Johnston_and_Bartos_1977$DW_TOT+Johnston_and_Bartos_1977$DW_FOL
Johnston_and_Bartos_1977$STUMP<-NA
Johnston_and_Bartos_1977$ABG<-NA
Johnston_and_Bartos_1977$ORIGIN<-2
Johnston_and_Bartos_1977$TEMP<-70
Johnston_and_Bartos_1977$DGL<-NA
Johnston_and_Bartos_1977$DSTEM_MIN<-NA
Johnston_and_Bartos_1977$DBLC<-NA
Johnston_and_Bartos_1977$DW_CROWN<-Johnston_and_Bartos_1977$DW_FOL+Johnston_and_Bartos_1977$DW_BRANCH
Johnston_and_Bartos_1977$ST_METH <-'1'
Johnston_and_Bartos_1977$BR_METH <-'5'
Johnston_and_Bartos_1977$FOL_METH<-'5'
Johnston_and_Bartos_1977$REGION  <-'IM'
Johnston_and_Bartos_1977$Indicator <- 'unchecked'
Johnston_and_Bartos_1977$GW_STEM<-'unchecked'
Johnston_and_Bartos_1977$GW_BRANCH<-'unchecked'
Johnston_and_Bartos_1977$GW_FOL<-'unchecked'
Johnston_and_Bartos_1977$GW_CROWN<-'unchecked'
Johnston_and_Bartos_1977$GW_BIOMASS<-'unchecked'
Johnston_and_Bartos_1977$GW_ABG<-'unchecked'
Johnston_and_Bartos_1977$GW_TOT<-'unchecked'
Johnston_and_Bartos_1977$M.C.<-'unchecked'
Johnston_and_Bartos_1977$SP.GR<-'unchecked'
Johnston_and_Bartos_1977$Bark.Fraction<-'unchecked'
Johnston_and_Bartos_1977$Age<-'unchecked'


Johnston_and_Bartos_1977<-Johnston_and_Bartos_1977[,c(15:57)]

#names(LegacyData)
LegacyData<-rbind(LegacyData,Johnston_and_Bartos_1977)

#######################
# Devine et al 2013   #
#######################
#names(Devine_et_al_2013)
Devine_et_al_2013<-read.csv(paste(basepath,"Devine_et_al_2013_Data.csv",sep=""),header=T, as.is= T)
#Devine_et_al_2013[is.na(Devine_et_al_2013)]<-0
Devine_et_al_2013$AUTHOR<-"Devine_et_al"
Devine_et_al_2013$LOC<-1
Devine_et_al_2013$SPCD<-202
Devine_et_al_2013$TREENO<-Devine_et_al_2013$tree.no
Devine_et_al_2013$CCLCD<-NA
Devine_et_al_2013$CW<-NA
Devine_et_al_2013$H_STUMP<-NA
Devine_et_al_2013$DBH<-Devine_et_al_2013$dbh.cm/2.54
Devine_et_al_2013$HT<-Devine_et_al_2013$ht.m*3.28084
Devine_et_al_2013$HL_BRANCH<-NA
Devine_et_al_2013$DW_SW<-NA
Devine_et_al_2013$DW_SB<-NA
Devine_et_al_2013$DW_STEM<-Devine_et_al_2013$stem.wt.kg*2.20462
Devine_et_al_2013$DEAD_BRANCH_DW<-NA
Devine_et_al_2013$LIVE_BRANCH_DW<-NA
Devine_et_al_2013$DW_BRANCH<-Devine_et_al_2013$branch.wt.kg*2.20462
Devine_et_al_2013$DW_TOT<-Devine_et_al_2013$DW_BRANCH+Devine_et_al_2013$DW_STEM
Devine_et_al_2013$DW_FOL<-Devine_et_al_2013$foliage.wt.kg*2.20462
Devine_et_al_2013$BIOMASS<-Devine_et_al_2013$DW_TOT+Devine_et_al_2013$DW_FOL
Devine_et_al_2013$STUMP<-NA
Devine_et_al_2013$ABG<-NA
Devine_et_al_2013$ORIGIN<-2
Devine_et_al_2013$TEMP<-70
Devine_et_al_2013$DGL<-NA
Devine_et_al_2013$DSTEM_MIN<-NA
Devine_et_al_2013$DBLC<-NA
Devine_et_al_2013$DW_CROWN<-Devine_et_al_2013$DW_FOL+Devine_et_al_2013$DW_BRANCH
Devine_et_al_2013$ST_METH <-'1' # Methods follow Peterson et al 2008
Devine_et_al_2013$BR_METH <-'4' # not entirely clear in Devine, perhaps better explained in Peterson
Devine_et_al_2013$FOL_METH<-'4'
Devine_et_al_2013$REGION  <-'PNW'
Devine_et_al_2013$Indicator <- 'unchecked'
Devine_et_al_2013$GW_STEM<-'unchecked'
Devine_et_al_2013$GW_BRANCH<-'unchecked'
Devine_et_al_2013$GW_FOL<-'unchecked'
Devine_et_al_2013$GW_CROWN<-'unchecked'
Devine_et_al_2013$GW_BIOMASS<-'unchecked'
Devine_et_al_2013$GW_ABG<-'unchecked'
Devine_et_al_2013$GW_TOT<-'unchecked'
Devine_et_al_2013$M.C.<-'unchecked'
Devine_et_al_2013$SP.GR<-'unchecked'
Devine_et_al_2013$Bark.Fraction<-'unchecked'
Devine_et_al_2013$Age<-'unchecked'


Devine_et_al_2013<-Devine_et_al_2013[,c(9:51)]

#names(LegacyData)
LegacyData<-rbind(LegacyData,Devine_et_al_2013)

#######################
#    Van Gurp 1984    # #Norway Spruce in NY
#######################
#names(Van_Gurp_1984)
Van_Gurp_1984<-read.csv(paste(basepath,"Van_Gurp_1984_Data.csv",sep=""),header=T, as.is= T)
#Van_Gurp_1984[is.na(Van_Gurp_1984)]<-0
Van_Gurp_1984$AUTHOR<-"Van_Gurp"
Van_Gurp_1984$LOC<-1
Van_Gurp_1984$SPCD<-91
Van_Gurp_1984$TREENO<-Van_Gurp_1984$tree.no
Van_Gurp_1984$CCLCD<-NA
Van_Gurp_1984$CW<-NA
Van_Gurp_1984$H_STUMP<-0
Van_Gurp_1984$DBH<-Van_Gurp_1984$dbh.cm/2.54
Van_Gurp_1984$HT<-Van_Gurp_1984$th.m*3.28084
Van_Gurp_1984$HL_BRANCH<-NA
Van_Gurp_1984$DW_SW<-Van_Gurp_1984$stem.wood.kg*2.20462
Van_Gurp_1984$DW_SB<-Van_Gurp_1984$stem.bark.kg*2.20462
Van_Gurp_1984$DW_STEM<-Van_Gurp_1984$DW_SW+Van_Gurp_1984$DW_SB
Van_Gurp_1984$DEAD_BRANCH_DW<-Van_Gurp_1984$dead.branches.kg*2.20462
Van_Gurp_1984$LIVE_BRANCH_DW<-Van_Gurp_1984$live.branches.kg*2.20462
Van_Gurp_1984$DW_BRANCH<-Van_Gurp_1984$LIVE_BRANCH_DW+Van_Gurp_1984$DEAD_BRANCH_DW
Van_Gurp_1984$DW_TOT<-Van_Gurp_1984$DW_BRANCH+Van_Gurp_1984$DW_STEM
Van_Gurp_1984$DW_FOL<-Van_Gurp_1984$foliage.weight.kg*2.20462
Van_Gurp_1984$BIOMASS<-Van_Gurp_1984$DW_TOT+Van_Gurp_1984$DW_FOL
Van_Gurp_1984$STUMP<-0
Van_Gurp_1984$ABG<-Van_Gurp_1984$BIOMASS
Van_Gurp_1984$ORIGIN<-2
Van_Gurp_1984$TEMP<-65
Van_Gurp_1984$DGL<-NA
Van_Gurp_1984$DSTEM_MIN<-NA
Van_Gurp_1984$DBLC<-NA
Van_Gurp_1984$DW_CROWN<-Van_Gurp_1984$DW_FOL+Van_Gurp_1984$DW_BRANCH
Van_Gurp_1984$ST_METH <-'1'
Van_Gurp_1984$BR_METH <-'2'
Van_Gurp_1984$FOL_METH<-'2'
Van_Gurp_1984$REGION  <-'NE'				# NY
Van_Gurp_1984$Indicator <- 'unchecked'
Van_Gurp_1984$GW_STEM<-'unchecked'
Van_Gurp_1984$GW_BRANCH<-'unchecked'
Van_Gurp_1984$GW_FOL<-'unchecked'
Van_Gurp_1984$GW_CROWN<-'unchecked'
Van_Gurp_1984$GW_BIOMASS<-'unchecked'
Van_Gurp_1984$GW_ABG<-'unchecked'
Van_Gurp_1984$GW_TOT<-'unchecked'
Van_Gurp_1984$M.C.<-'unchecked'
Van_Gurp_1984$SP.GR<-'unchecked'
Van_Gurp_1984$Bark.Fraction<-'unchecked'
Van_Gurp_1984$Age<-'unchecked'
Van_Gurp_1984<-Van_Gurp_1984[,c(12:54)]

LegacyData<-rbind(LegacyData,Van_Gurp_1984)
dim(LegacyData)# 6745
#######################
# Bickelhaupt 1979    #
#######################

Bickelhaupt_1979<-read.csv(paste(basepath,"Bickelhaupt_1979_Data.csv",sep=""),header=T, as.is= T)
#head(Bickelhaupt_1979)
#Bickelhaupt_1979[is.na(Bickelhaupt_1979)]<-0
Bickelhaupt_1979$AUTHOR<-"Bickelhaupt"
Bickelhaupt_1979$LOC<-1
Bickelhaupt_1979$SPCD<-318			# Sugar Maple
Bickelhaupt_1979$TREENO<-1:nrow(Bickelhaupt_1979)
Bickelhaupt_1979$CCLCD<-NA
Bickelhaupt_1979$CW<-NA
Bickelhaupt_1979$H_STUMP<-0
Bickelhaupt_1979$DBH<-Bickelhaupt_1979$dbh.cm/2.54
Bickelhaupt_1979$HT<-Bickelhaupt_1979$th.m*3.28084
Bickelhaupt_1979$HL_BRANCH<-NA
Bickelhaupt_1979$DW_SW<-(Bickelhaupt_1979$stem.wood.1+Bickelhaupt_1979$stem.wood.2)/1000*2.20462     # 2 wood below crown; 1 wood above crown
Bickelhaupt_1979$DW_SB<-(Bickelhaupt_1979$stem.bark.1+Bickelhaupt_1979$stem.bark.2)/1000*2.20462						# 2 wood below crown; 1 wood above crown
Bickelhaupt_1979$DW_STEM<-Bickelhaupt_1979$DW_SW+Bickelhaupt_1979$DW_SB
Bickelhaupt_1979$DEAD_BRANCH_DW<-NA
Bickelhaupt_1979$LIVE_BRANCH_DW<-NA
Bickelhaupt_1979$DW_BRANCH<-(Bickelhaupt_1979$Branch.1 + Bickelhaupt_1979$Branch.2 + Bickelhaupt_1979$branch.bark.3 + Bickelhaupt_1979$branch.wood.3 + 
					Bickelhaupt_1979$branch.bark.4 + Bickelhaupt_1979$branch.wood.4 + Bickelhaupt_1979$branch.bark.5 + Bickelhaupt_1979$branch.wood.5 +
					 Bickelhaupt_1979$branch.bark.6 + Bickelhaupt_1979$branch.wood.6)/1000*2.20462
Bickelhaupt_1979$DW_TOT<-Bickelhaupt_1979$DW_BRANCH+Bickelhaupt_1979$DW_STEM
Bickelhaupt_1979$DW_FOL<-(Bickelhaupt_1979$upper.fol.g + Bickelhaupt_1979$middle.fol.g + Bickelhaupt_1979$lower.fol.g +  Bickelhaupt_1979$colored.fol.g) /1000*2.20462
Bickelhaupt_1979$BIOMASS<-Bickelhaupt_1979$DW_TOT+Bickelhaupt_1979$DW_FOL
Bickelhaupt_1979$STUMP<-0													
Bickelhaupt_1979$ABG<-Bickelhaupt_1979$BIOMASS	
Bickelhaupt_1979$ORIGIN<-1
Bickelhaupt_1979$TEMP<-65
Bickelhaupt_1979$DGL<-NA
Bickelhaupt_1979$DSTEM_MIN<-NA
Bickelhaupt_1979$DBLC<-NA
Bickelhaupt_1979$DW_CROWN<-Bickelhaupt_1979$DW_FOL+Bickelhaupt_1979$DW_BRANCH
Bickelhaupt_1979$ST_METH <-'5'
Bickelhaupt_1979$BR_METH <-'1'
Bickelhaupt_1979$FOL_METH<-'1'
Bickelhaupt_1979$REGION  <-'NE'
Bickelhaupt_1979$Indicator <- 'unchecked'
Bickelhaupt_1979$GW_STEM<-'unchecked'
Bickelhaupt_1979$GW_BRANCH<-'unchecked'
Bickelhaupt_1979$GW_FOL<-'unchecked'
Bickelhaupt_1979$GW_CROWN<-'unchecked'
Bickelhaupt_1979$GW_BIOMASS<-'unchecked'
Bickelhaupt_1979$GW_ABG<-'unchecked'
Bickelhaupt_1979$GW_TOT<-'unchecked'
Bickelhaupt_1979$M.C.<-'unchecked'
Bickelhaupt_1979$SP.GR<-'unchecked'
Bickelhaupt_1979$Bark.Fraction<-'unchecked'
Bickelhaupt_1979$Age<-'unchecked'


Bickelhaupt_1979<-Bickelhaupt_1979[,c(23:65)]


LegacyData<-rbind(LegacyData,Bickelhaupt_1979)


#######################
# Neisch	1980      #
#######################

Neisch_1980<-read.csv(paste(basepath,"Neisch_1980_Data.csv",sep=""),header=T, as.is= T)				
#Neisch_1980[is.na(Neisch_1980)]<-0
Neisch_1980$AUTHOR<-"Neisch"
Neisch_1980$LOC<-1			#	East Texas; no lat long given in study; specific locations not described for individual trees										
Neisch_1980[Neisch_1980$species=="shortleaf pine","SPCD"]<-110
Neisch_1980[Neisch_1980$species=="loblolly pine","SPCD"]<-131
Neisch_1980$TREENO<-1:nrow(Neisch_1980)
Neisch_1980$CCLCD<-NA
Neisch_1980$CW<-NA
Neisch_1980$H_STUMP<-NA						# variable; p. 8; not included in height										
Neisch_1980$DBH<-Neisch_1980$dbh.in
Neisch_1980$HT<-Neisch_1980$th.ft
Neisch_1980$HL_BRANCH<-NA
Neisch_1980$DW_SW<-Neisch_1980$stem.wood.dw     
Neisch_1980$DW_SB<-Neisch_1980$stem.bark.dw						
Neisch_1980$DW_STEM<-Neisch_1980$stem.dw.lb
Neisch_1980$DEAD_BRANCH_DW<-NA
Neisch_1980$LIVE_BRANCH_DW<-NA
Neisch_1980$DW_BRANCH<-(Neisch_1980$branch.dw.lb)					# branch portion includes the top (p. 12 ..."thus the total crown consisted of all branches plus the portion of the stem above the last cut point 
Neisch_1980$DW_TOT<-Neisch_1980$DW_BRANCH+Neisch_1980$DW_STEM
Neisch_1980$DW_FOL<-(Neisch_1980$foliage.dw.lb)
Neisch_1980$BIOMASS<-Neisch_1980$DW_TOT+Neisch_1980$DW_FOL
Neisch_1980$STUMP<-NA													
Neisch_1980$ABG<-NA
Neisch_1980$ORIGIN<-1
Neisch_1980$TEMP<-70
Neisch_1980$DGL<-NA
Neisch_1980$DSTEM_MIN<-NA
Neisch_1980$DBLC<-NA
Neisch_1980$DW_CROWN<-Neisch_1980$DW_FOL+Neisch_1980$DW_BRANCH
Neisch_1980$ST_METH <-'1'
Neisch_1980$BR_METH <-'2'
Neisch_1980$FOL_METH<-'2'
Neisch_1980$REGION  <-'SE'
Neisch_1980$Indicator <- 'unchecked'
Neisch_1980$GW_STEM<-'unchecked'
Neisch_1980$GW_BRANCH<-'unchecked'
Neisch_1980$GW_FOL<-'unchecked'
Neisch_1980$GW_CROWN<-'unchecked'
Neisch_1980$GW_BIOMASS<-'unchecked'
Neisch_1980$GW_ABG<-'unchecked'
Neisch_1980$GW_TOT<-'unchecked'
Neisch_1980$M.C.<-'unchecked'
Neisch_1980$SP.GR<-'unchecked'
Neisch_1980$Bark.Fraction<-'unchecked'
Neisch_1980$Age<-'unchecked'

Neisch_1980<-Neisch_1980[,c(21:63)]
#plot(Neisch_1980$dbh.in,Neisch_1980$stem.gw.lb)

LegacyData<-rbind(LegacyData,Neisch_1980)

#######################
# Schmitt 1979        #
#######################

Schmitt_1979<-read.csv(paste(basepath,"Schmitt_1979_data.csv",sep=""),header=T, as.is= T)				
#Schmitt_1979[is.na(Schmitt_1979)]<-0
Schmitt_1979$AUTHOR<-"Schmitt"
Schmitt_1979$LOC<-1				# Eastern NY; appear to be located in close proximity; no coords given										
Schmitt_1979[Schmitt_1979$species=="white birch","SPCD"]<-375
Schmitt_1979[Schmitt_1979$species=="bigtooth aspen","SPCD"]<-743
#unique(Schmitt_1979$species)
Schmitt_1979$TREENO<- 1:nrow(Schmitt_1979)
Schmitt_1979$CCLCD<-NA
Schmitt_1979$CW<-NA
Schmitt_1979$H_STUMP<-0               #Felled at groundline						
Schmitt_1979$DBH<-(Schmitt_1979$dbh.cm)/2.54
Schmitt_1979$HT<-(Schmitt_1979$th.m)*3.28084
Schmitt_1979$HL_BRANCH<-Schmitt_1979$HT - (Schmitt_1979$lcr.percent/100 * Schmitt_1979$HT)
Schmitt_1979$DW_SW<-Schmitt_1979$stem.dw.kg*2.20462
Schmitt_1979$DW_SB<-Schmitt_1979$inside.bark.dw.kg + Schmitt_1979$outside.bark.dw.kg*2.20462
Schmitt_1979$DW_STEM<-Schmitt_1979$DW_SW + Schmitt_1979$DW_SB
Schmitt_1979$DEAD_BRANCH_DW<-Schmitt_1979$dead.branch.dw.kg*2.20462
Schmitt_1979$LIVE_BRANCH_DW<-Schmitt_1979$live.branch.dw.kg*2.20462
Schmitt_1979$DW_BRANCH<-Schmitt_1979$DEAD_BRANCH_DW + Schmitt_1979$LIVE_BRANCH_DW
Schmitt_1979$DW_TOT<-Schmitt_1979$DW_BRANCH+Schmitt_1979$DW_STEM
Schmitt_1979$DW_FOL<-(Schmitt_1979$foliage.dw.kg)*2.20462
Schmitt_1979$BIOMASS<-Schmitt_1979$DW_TOT+Schmitt_1979$DW_FOL
Schmitt_1979$STUMP<-0
Schmitt_1979$ABG<-Schmitt_1979$BIOMASS										
Schmitt_1979$ORIGIN<-1
Schmitt_1979$TEMP<-60
Schmitt_1979$DGL<-Schmitt_1979$base.diameter.cm/2.54
Schmitt_1979$DSTEM_MIN<-NA
Schmitt_1979$DBLC<-NA
Schmitt_1979$DW_CROWN<-Schmitt_1979$DW_FOL+Schmitt_1979$DW_BRANCH
Schmitt_1979$ST_METH <-'1'
Schmitt_1979$BR_METH <-NA	# Not entirely clear from reading; probably 1 or 5
Schmitt_1979$FOL_METH<-NA	# Not entirely clear from reading; probably 1 or 5
Schmitt_1979$REGION  <-'NE'
Schmitt_1979$Indicator <- 'unchecked'
Schmitt_1979$GW_STEM<-'unchecked'
Schmitt_1979$GW_BRANCH<-'unchecked'
Schmitt_1979$GW_FOL<-'unchecked'
Schmitt_1979$GW_CROWN<-'unchecked'
Schmitt_1979$GW_BIOMASS<-'unchecked'
Schmitt_1979$GW_ABG<-'unchecked'
Schmitt_1979$GW_TOT<-'unchecked'
Schmitt_1979$M.C.<-'unchecked'
Schmitt_1979$SP.GR<-'unchecked'
Schmitt_1979$Bark.Fraction<-'unchecked'
Schmitt_1979$Age<-'unchecked'


Schmitt_1979<-Schmitt_1979[,c(15:57)]

LegacyData<-rbind(LegacyData,Schmitt_1979)


############################
# Miller_et_al_1981        # # southwestern species
############################

Miller_et_al_1981<-read.csv(paste(basepath,"Miller_et_al_1981_Data.csv",sep=""),header=T, as.is= T)  			
Miller_et_al_1981$AUTHOR<-"Miller_et_al"
Miller_et_al_1981$LOC<-Miller_et_al_1981$location
Miller_et_al_1981[Miller_et_al_1981$spp=="singleleaf pinyon","SPCD"]<-133
Miller_et_al_1981[Miller_et_al_1981$spp=="Utah juniper","SPCD"]<-65
#unique(Miller_et_al_1981$species)
Miller_et_al_1981$TREENO<-Miller_et_al_1981$tree.no
Miller_et_al_1981$CCLCD<-Miller_et_al_1981$crown.class
Miller_et_al_1981$CW<-Miller_et_al_1981$crown.diam.avg*3.28084
Miller_et_al_1981$H_STUMP<-0               #Felled at groundline						
Miller_et_al_1981$DBH<-(Miller_et_al_1981$dbh.cm)/2.54
Miller_et_al_1981$HT<-(Miller_et_al_1981$tree.ht)*3.28084
Miller_et_al_1981$HL_BRANCH<-NA
Miller_et_al_1981$DW_SW<-NA
Miller_et_al_1981$DW_SB<-NA
Miller_et_al_1981$DW_STEM<-NA
Miller_et_al_1981$DEAD_BRANCH_DW<-Miller_et_al_1981$dead.branch.dw.kg*2.20462
Miller_et_al_1981$LIVE_BRANCH_DW<-NA
Miller_et_al_1981$DW_BRANCH<-NA
Miller_et_al_1981$DW_TOT<-(Miller_et_al_1981$tot.dw.kg - Miller_et_al_1981$fol.dw.kg) * 2.20462
Miller_et_al_1981$DW_FOL<-(Miller_et_al_1981$fol.dw.kg)*2.20462
Miller_et_al_1981$BIOMASS<-Miller_et_al_1981$DW_TOT+Miller_et_al_1981$DW_FOL
Miller_et_al_1981$STUMP<-NA
Miller_et_al_1981$ABG<-Miller_et_al_1981$BIOMASS										
Miller_et_al_1981$ORIGIN<-1
Miller_et_al_1981$TEMP<-95
Miller_et_al_1981$DGL<-Miller_et_al_1981$diam.gk.cm/2.54
Miller_et_al_1981$DSTEM_MIN<-NA
Miller_et_al_1981$DBLC<-NA
Miller_et_al_1981$DW_CROWN<-NA
Miller_et_al_1981$ST_METH <-'1'
Miller_et_al_1981$BR_METH <-'2'
Miller_et_al_1981$FOL_METH<-'2'
Miller_et_al_1981$REGION  <-'IMW'
Miller_et_al_1981$Indicator <- 'unchecked'
Miller_et_al_1981$GW_STEM<-'unchecked'
Miller_et_al_1981$GW_BRANCH<-'unchecked'
Miller_et_al_1981$GW_FOL<-'unchecked'
Miller_et_al_1981$GW_CROWN<-'unchecked'
Miller_et_al_1981$GW_BIOMASS<-'unchecked'
Miller_et_al_1981$GW_ABG<-'unchecked'
Miller_et_al_1981$GW_TOT<-'unchecked'
Miller_et_al_1981$M.C.<-'unchecked'
Miller_et_al_1981$SP.GR<-'unchecked'
Miller_et_al_1981$Bark.Fraction<-'unchecked'
Miller_et_al_1981$Age<-'unchecked'


Miller_et_al_1981<-Miller_et_al_1981[,c(30:72)]
#plot(Miller_et_al_1981$dbh.in,Miller_et_al_1981$stem.gw.lb)
LegacyData<-rbind(LegacyData,Miller_et_al_1981)

################################
# Snell_and_Little_1983        # 
################################

Snell_and_Little_1983<-read.csv(paste(basepath,"Snell_and_Little_1983_Data.csv",sep=""),header=T, as.is= T)    		
#names(Snell_and_Little_1983)
#Snell_and_Little_1983
#dim(Snell_and_Little_1983)
#Snell_and_Little_1983[is.na(Snell_and_Little_1983)]<-0
Snell_and_Little_1983$AUTHOR<-"Snell_and_Little"
Snell_and_Little_1983[Snell_and_Little_1983$species=="red alder","LOC"]<-1
Snell_and_Little_1983[Snell_and_Little_1983$species=="giant chinkapin","LOC"]<-2
Snell_and_Little_1983[Snell_and_Little_1983$species=="bigleaf maple","LOC"]<-3
Snell_and_Little_1983[Snell_and_Little_1983$species=="Pacific madrone","LOC"]<-2
Snell_and_Little_1983[Snell_and_Little_1983$species=="tanoak","LOC"]<-2
Snell_and_Little_1983[Snell_and_Little_1983$species=="red alder","SPCD"]<-351
Snell_and_Little_1983[Snell_and_Little_1983$species=="giant chinkapin","SPCD"]<-431
Snell_and_Little_1983[Snell_and_Little_1983$species=="bigleaf maple","SPCD"]<-312
Snell_and_Little_1983[Snell_and_Little_1983$species=="Pacific madrone","SPCD"]<-361
Snell_and_Little_1983[Snell_and_Little_1983$species=="tanoak","SPCD"]<-631
Snell_and_Little_1983$TREENO<-1:nrow(Snell_and_Little_1983)
Snell_and_Little_1983$CCLCD<-NA
Snell_and_Little_1983$CW<-Snell_and_Little_1983$crown.width.ft
Snell_and_Little_1983$H_STUMP<-NA						
Snell_and_Little_1983$DBH<-(Snell_and_Little_1983$dbh.in)
Snell_and_Little_1983$HT<-Snell_and_Little_1983$ht.ft
Snell_and_Little_1983$HL_BRANCH<-Snell_and_Little_1983$ht.ft - Snell_and_Little_1983$crown.length.ft
Snell_and_Little_1983$DW_SW<-NA
Snell_and_Little_1983$DW_SB<-NA
Snell_and_Little_1983$DW_STEM<-NA
Snell_and_Little_1983$DEAD_BRANCH_DW<-Snell_and_Little_1983$total.dead.branch.lb
Snell_and_Little_1983$LIVE_BRANCH_DW<-NA
Snell_and_Little_1983$DW_BRANCH<-NA
Snell_and_Little_1983$DW_TOT<-NA
Snell_and_Little_1983$DW_FOL<-NA
Snell_and_Little_1983$BIOMASS<-NA
Snell_and_Little_1983$STUMP<-NA
Snell_and_Little_1983$ABG<-NA
Snell_and_Little_1983$ORIGIN<-1
Snell_and_Little_1983$TEMP<-102
Snell_and_Little_1983$DGL<-NA
Snell_and_Little_1983$DSTEM_MIN<-NA
Snell_and_Little_1983$DBLC<-Snell_and_Little_1983$dblc.in
Snell_and_Little_1983$DW_CROWN<-Snell_and_Little_1983$total.dead.branch.lb + Snell_and_Little_1983$total.live.crown.lb
Snell_and_Little_1983$ST_METH <-'NA'
Snell_and_Little_1983$BR_METH <-'NA'
Snell_and_Little_1983$FOL_METH<-'NA'
Snell_and_Little_1983$REGION  <-'PNW'
Snell_and_Little_1983$Indicator <- 'unchecked'
Snell_and_Little_1983$GW_STEM<-'unchecked'
Snell_and_Little_1983$GW_BRANCH<-'unchecked'
Snell_and_Little_1983$GW_FOL<-'unchecked'
Snell_and_Little_1983$GW_CROWN<-'unchecked'
Snell_and_Little_1983$GW_BIOMASS<-'unchecked'
Snell_and_Little_1983$GW_ABG<-'unchecked'
Snell_and_Little_1983$GW_TOT<-'unchecked'
Snell_and_Little_1983$M.C.<-'unchecked'
Snell_and_Little_1983$SP.GR<-'unchecked'
Snell_and_Little_1983$Bark.Fraction<-'unchecked'
Snell_and_Little_1983$Age<-'unchecked'


Snell_and_Little_1983<-Snell_and_Little_1983[,c(10:52)]
#plot(Snell_and_Little_1983$dbh.in,Snell_and_Little_1983$stem.gw.lb)
#names(Snell_and_Little_1983)
#names(LegacyData)
LegacyData<-rbind(LegacyData,Snell_and_Little_1983)

########################
# Blackmon_1963        # 
########################

Blackmon_1963<-read.csv(paste(basepath,"Blackmon_1963_Data.csv",sep=""),header=T, as.is= T)      	
#names(Blackmon_1963)
#Blackmon_1963
#Blackmon_1963[is.na(Blackmon_1963)]<-0
Blackmon_1963$AUTHOR<-"Blackmon"
Blackmon_1963$LOC<-1
Blackmon_1963[Blackmon_1963$species=="white oak","SPCD"]<-802
Blackmon_1963[Blackmon_1963$species=="red oak","SPCD"]<-800       # Various red oaks
Blackmon_1963[Blackmon_1963$species=="hickory","SPCD"]<-400
Blackmon_1963$TREENO<-1:nrow(Blackmon_1963)
Blackmon_1963$CCLCD<-NA
Blackmon_1963$CW<-NA
Blackmon_1963$H_STUMP<-NA						
Blackmon_1963$DBH<-Blackmon_1963$dbh.in
Blackmon_1963$HT<-Blackmon_1963$th.ft
Blackmon_1963$HL_BRANCH<-NA
Blackmon_1963$DW_SW<-NA
Blackmon_1963$DW_SB<-NA
Blackmon_1963$DW_STEM<-Blackmon_1963$stem.dw.lb
Blackmon_1963$DEAD_BRANCH_DW<-NA
Blackmon_1963$LIVE_BRANCH_DW<-NA
Blackmon_1963$DW_BRANCH<-Blackmon_1963$branch.dw.lb
Blackmon_1963$DW_TOT<-Blackmon_1963$DW_STEM + Blackmon_1963$DW_BRANCH
Blackmon_1963$DW_FOL<-Blackmon_1963$foliage.dw.lb
Blackmon_1963$BIOMASS<-Blackmon_1963$DW_TOT + Blackmon_1963$DW_FOL
Blackmon_1963$STUMP<-NA
Blackmon_1963$ABG<-NA
Blackmon_1963$ORIGIN<-1
Blackmon_1963$TEMP<-70
Blackmon_1963$DGL<-NA
Blackmon_1963$DSTEM_MIN<-NA
Blackmon_1963$DBLC<-NA
Blackmon_1963$DW_CROWN<-Blackmon_1963$DW_BRANCH + Blackmon_1963$DW_FOL
Blackmon_1963$ST_METH <-'1'            # p. 8
Blackmon_1963$BR_METH <-'1'		# no mention of subsampling; branches dried in kiln to 70, then foliage picked; fol and wb weighed
Blackmon_1963$FOL_METH<-'1'
Blackmon_1963$REGION  <-'SE'
Blackmon_1963$Indicator <- 'unchecked'
Blackmon_1963$GW_STEM<-'unchecked'
Blackmon_1963$GW_BRANCH<-'unchecked'
Blackmon_1963$GW_FOL<-'unchecked'
Blackmon_1963$GW_CROWN<-'unchecked'
Blackmon_1963$GW_BIOMASS<-'unchecked'
Blackmon_1963$GW_ABG<-'unchecked'
Blackmon_1963$GW_TOT<-'unchecked'
Blackmon_1963$M.C.<-'unchecked'
Blackmon_1963$SP.GR<-'unchecked'
Blackmon_1963$Bark.Fraction<-'unchecked'
Blackmon_1963$Age<-'unchecked'



Blackmon_1963<-Blackmon_1963[,c(8:50)]
#plot(Blackmon_1963$dbh.in,Blackmon_1963$stem.gw.lb)
#names(Blackmon_1963)
#names(LegacyData)
LegacyData<-rbind(LegacyData,Blackmon_1963)

########################
# Houser_1980          # Texas; loblolly pine
########################

Houser_1980<-read.csv(paste(basepath,"Houser_1980_Data.csv",sep=""),header=T, as.is= T)        
#names(Houser_1980)
#Houser_1980$tree.no
#Houser_1980[is.na(Houser_1980)]<-0
Houser_1980$AUTHOR<-"Houser"
Houser_1980$LOC<-1
Houser_1980$SPCD<-131
Houser_1980$TREENO<-Houser_1980$tree.no
Houser_1980$CCLCD<-NA
Houser_1980$CW<-NA
Houser_1980$H_STUMP<-NA						
Houser_1980$DBH<-Houser_1980$dbh.cm/2.54
Houser_1980$HT<-Houser_1980$th.m*3.28084
Houser_1980$HL_BRANCH<-(Houser_1980$th.m - Houser_1980$crown.length.m)*3.28084
Houser_1980$DW_SW<-Houser_1980$wood.dw.lb
Houser_1980$DW_SB<-Houser_1980$bark.dw.lb
Houser_1980$DW_STEM<-Houser_1980$DW_SW + Houser_1980$DW_SB
Houser_1980$DEAD_BRANCH_DW<-Houser_1980$dead.branch.dw.kg*2.20462
Houser_1980$LIVE_BRANCH_DW<-Houser_1980$live.branch.dw.kg*2.20462
Houser_1980$DW_BRANCH<-Houser_1980$LIVE_BRANCH_DW + Houser_1980$DEAD_BRANCH_DW
Houser_1980$DW_TOT<-Houser_1980$DW_STEM + Houser_1980$DW_BRANCH
Houser_1980$DW_FOL<-Houser_1980$foliage.dw.kg*2.20462
Houser_1980$BIOMASS<-Houser_1980$DW_TOT + Houser_1980$DW_FOL
Houser_1980$STUMP<-NA
Houser_1980$ABG<-NA
Houser_1980$ORIGIN<-2
Houser_1980$TEMP<-75
Houser_1980$DGL<-NA
Houser_1980$DSTEM_MIN<-NA
Houser_1980$DBLC<-NA
Houser_1980$DW_CROWN<-Houser_1980$DW_BRANCH + Houser_1980$DW_FOL
Houser_1980$ST_METH <-'1'
Houser_1980$BR_METH <-'5'
Houser_1980$FOL_METH<-'5'
Houser_1980$REGION  <-'SE'
Houser_1980$Indicator <- 'unchecked'
Houser_1980$GW_STEM<-'unchecked'
Houser_1980$GW_BRANCH<-'unchecked'
Houser_1980$GW_FOL<-'unchecked'
Houser_1980$GW_CROWN<-'unchecked'
Houser_1980$GW_BIOMASS<-'unchecked'
Houser_1980$GW_ABG<-'unchecked'
Houser_1980$GW_TOT<-'unchecked'
Houser_1980$M.C.<-'unchecked'
Houser_1980$SP.GR<-'unchecked'
Houser_1980$Bark.Fraction<-'unchecked'
Houser_1980$Age<-'unchecked'


Houser_1980<-Houser_1980[,c(23:65)]
#plot(Houser_1980$dbh.in,Houser_1980$stem.gw.lb)
#names(Houser_1980)
#names(LegacyData)
LegacyData<-rbind(LegacyData,Houser_1980)

########################
# Zimmerman_1979       # Western species
########################

Zimmerman_1979<-read.csv(paste(basepath,"Zimmerman_1979_Data.csv",sep=""),header=T, as.is= T)        
#names(Zimmerman_1979)
#Zimmerman_1979$tree.no
#Zimmerman_1979[is.na(Zimmerman_1979)]<-0
Zimmerman_1979$AUTHOR<-"Zimmerman"
Zimmerman_1979$LOC<-1
Zimmerman_1979[Zimmerman_1979$species=="Engelmann spruce","SPCD"]<-93
Zimmerman_1979[Zimmerman_1979$species=="subalpine fir","SPCD"]<-19
Zimmerman_1979[Zimmerman_1979$species=="quaking aspen","SPCD"]<-746
Zimmerman_1979$TREENO<-1:nrow(Zimmerman_1979)
Zimmerman_1979$CCLCD<-NA
Zimmerman_1979$CW<-NA
Zimmerman_1979$H_STUMP<-1
Zimmerman_1979$DBH<-Zimmerman_1979$dbh.cm/2.54
Zimmerman_1979$HT<-NA
Zimmerman_1979$HL_BRANCH<-NA
Zimmerman_1979$DW_SW<-Zimmerman_1979$bole.wood.dw.kg*2.20462
Zimmerman_1979$DW_SB<-Zimmerman_1979$bole.bark.dw.kg*2.20462
Zimmerman_1979$DW_STEM<-Zimmerman_1979$DW_SW + Zimmerman_1979$DW_SB
Zimmerman_1979$DEAD_BRANCH_DW<-NA
Zimmerman_1979$LIVE_BRANCH_DW<-NA
Zimmerman_1979$DW_BRANCH<-(Zimmerman_1979$branch.wood.dw.kg + Zimmerman_1979$branch.bark.dw.kg*2.20462)
Zimmerman_1979$DW_TOT<-Zimmerman_1979$DW_STEM + Zimmerman_1979$DW_BRANCH
Zimmerman_1979$DW_FOL<-NA
Zimmerman_1979$BIOMASS<-NA
Zimmerman_1979$STUMP<-NA
Zimmerman_1979$ABG<-NA
Zimmerman_1979$ORIGIN<-1
Zimmerman_1979$TEMP<-100
Zimmerman_1979$DGL<-NA
Zimmerman_1979$DSTEM_MIN<-NA
Zimmerman_1979$DBLC<-NA
Zimmerman_1979$DW_CROWN<-NA
Zimmerman_1979$ST_METH <-'3' # p. 17
Zimmerman_1979$BR_METH <-'UnChecked'
Zimmerman_1979$FOL_METH<-'UnChecked'
Zimmerman_1979$REGION  <-'IMW'
Zimmerman_1979$Indicator <- 'unchecked'
Zimmerman_1979$GW_STEM<-'unchecked'
Zimmerman_1979$GW_BRANCH<-'unchecked'
Zimmerman_1979$GW_FOL<-'unchecked'
Zimmerman_1979$GW_CROWN<-'unchecked'
Zimmerman_1979$GW_BIOMASS<-'unchecked'
Zimmerman_1979$GW_ABG<-'unchecked'
Zimmerman_1979$GW_TOT<-'unchecked'
Zimmerman_1979$M.C.<-'unchecked'
Zimmerman_1979$SP.GR<-'unchecked'
Zimmerman_1979$Bark.Fraction<-'unchecked'
Zimmerman_1979$Age<-'unchecked'


Zimmerman_1979<-Zimmerman_1979[,c(7:49)]
#plot(Zimmerman_1979$dbh.in,Zimmerman_1979$stem.gw.lb)
#names(Zimmerman_1979)
#names(LegacyData)
LegacyData<-rbind(LegacyData,Zimmerman_1979)

########################
# Manis_1977          # 
########################

Manis_1977<-read.csv(paste(basepath,"Manis_1977_Data.csv",sep=""),header=T, as.is= T)        

#Manis_1977$tree.no
#Manis_1977[is.na(Manis_1977)]<-0
Manis_1977$AUTHOR<-"Manis"
Manis_1977$LOC<-1
Manis_1977$SPCD<-111
Manis_1977$TREENO<-1:nrow(Manis_1977)
Manis_1977$CCLCD<-NA
Manis_1977$CW<-NA
Manis_1977$H_STUMP<-NA
Manis_1977$DBH<-NA
Manis_1977$HT<-NA
Manis_1977$HL_BRANCH<-NA
Manis_1977$DW_SW<-NA
Manis_1977$DW_SB<-NA
Manis_1977$DW_STEM<-Manis_1977$stem.dw.g/2204.62
Manis_1977$DEAD_BRANCH_DW<-NA
Manis_1977$LIVE_BRANCH_DW<-NA
Manis_1977$DW_BRANCH<-Manis_1977$branch.dw.g/2204.62
Manis_1977$DW_TOT<-Manis_1977$DW_STEM + Manis_1977$DW_BRANCH
Manis_1977$DW_FOL<-Manis_1977$foliage.dw.g/2204.62
Manis_1977$BIOMASS<-Manis_1977$DW_TOT + Manis_1977$DW_FOL
Manis_1977$STUMP<-NA
Manis_1977$ABG<-NA
Manis_1977$ORIGIN<-2
Manis_1977$TEMP<-80
Manis_1977$DGL<-Manis_1977$basal.diam.cm/2.54
Manis_1977$DSTEM_MIN<-NA
Manis_1977$DBLC<-NA
Manis_1977$DW_CROWN<-Manis_1977$DW_BRANCH + Manis_1977$DW_FOL
Manis_1977$ST_METH <-'1'
Manis_1977$BR_METH <-'5'
Manis_1977$FOL_METH<-'5'
Manis_1977$REGION  <-'SE'
Manis_1977$Indicator <- 'unchecked'
Manis_1977$GW_STEM<-'unchecked'
Manis_1977$GW_BRANCH<-'unchecked'
Manis_1977$GW_FOL<-'unchecked'
Manis_1977$GW_CROWN<-'unchecked'
Manis_1977$GW_BIOMASS<-'unchecked'
Manis_1977$GW_ABG<-'unchecked'
Manis_1977$GW_TOT<-'unchecked'
Manis_1977$M.C.<-'unchecked'
Manis_1977$SP.GR<-'unchecked'
Manis_1977$Bark.Fraction<-'unchecked'
Manis_1977$Age<-'unchecked'



Manis_1977<-Manis_1977[,c(8:50)]
#plot(Manis_1977$dbh.in,Manis_1977$stem.gw.lb)
#names(Manis_1977)
#names(LegacyData)
LegacyData<-rbind(LegacyData,Manis_1977)

########################
# Archibald_1983       #
########################

Archibald_1983<-read.csv(paste(basepath,"Archibald_1983_Data_All.csv",sep=""),header=T, as.is= T)        
#names(Archibald_1983)
#Archibald_1983$tree.no
#Archibald_1983[is.na(Archibald_1983)]<-0
Archibald_1983$AUTHOR<-"Archibald"
Archibald_1983$LOC<-1
Archibald_1983$SPCD<-202
Archibald_1983$TREENO<-Archibald_1983$tree.no
Archibald_1983$CCLCD<-NA
Archibald_1983$CW<-NA
Archibald_1983$H_STUMP<-NA
Archibald_1983$DBH<-Archibald_1983$dbh.cm/2.54
Archibald_1983$HT<-Archibald_1983$ht.m*3.28084
Archibald_1983$HL_BRANCH<-(Archibald_1983$ht.m - Archibald_1983$live.crown.length.m)*3.28084
Archibald_1983$DW_SW<-Archibald_1983$total.stem.wood.kg*2.20462
Archibald_1983$DW_SB<-Archibald_1983$total.stem.bark.kg*2.20462
Archibald_1983$DW_STEM<-Archibald_1983$DW_SW + Archibald_1983$DW_SB
Archibald_1983$DEAD_BRANCH_DW<-(Archibald_1983$dead.br.1 + Archibald_1983$dead.br.1)*2.20462
Archibald_1983$LIVE_BRANCH_DW<-(Archibald_1983$total.live.branch.kg + Archibald_1983$tot.twig.1.kg + Archibald_1983$tot.twig.2.kg)*2.20462
Archibald_1983$DW_BRANCH<-Archibald_1983$DEAD_BRANCH_DW + Archibald_1983$LIVE_BRANCH_DW
Archibald_1983$DW_TOT<-Archibald_1983$DW_STEM + Archibald_1983$DW_BRANCH
Archibald_1983$DW_FOL<-(Archibald_1983$tot.fol.1.kg + Archibald_1983$tot.fol.2.kg)*2.20462
Archibald_1983$BIOMASS<-Archibald_1983$DW_TOT + Archibald_1983$DW_FOL
Archibald_1983$STUMP<-NA
Archibald_1983$ABG<-NA
Archibald_1983$ORIGIN<-1
Archibald_1983$TEMP<-70
Archibald_1983$DGL<-NA
Archibald_1983$DSTEM_MIN<-NA
Archibald_1983$DBLC<-NA
Archibald_1983$DW_CROWN<-Archibald_1983$DW_BRANCH + Archibald_1983$DW_FOL
Archibald_1983$ST_METH <-3
Archibald_1983$BR_METH <-'6'
Archibald_1983$FOL_METH<-'6'
Archibald_1983$REGION  <-''

Archibald_1983$Indicator <- 'unchecked'
Archibald_1983$GW_STEM<-'unchecked'
Archibald_1983$GW_BRANCH<-'unchecked'
Archibald_1983$GW_FOL<-'unchecked'
Archibald_1983$GW_CROWN<-'unchecked'
Archibald_1983$GW_BIOMASS<-'unchecked'
Archibald_1983$GW_ABG<-'unchecked'
Archibald_1983$GW_TOT<-'unchecked'
Archibald_1983$M.C.<-'unchecked'
Archibald_1983$SP.GR<-'unchecked'
Archibald_1983$Bark.Fraction<-'unchecked'
Archibald_1983$Age<-'unchecked'


Archibald_1983<-Archibald_1983[,c(34:76)]
#plot(Archibald_1983$dbh.in,Archibald_1983$stem.gw.lb)
#names(Archibald_1983)
#names(LegacyData)
LegacyData<-rbind(LegacyData,Archibald_1983)

########################
# Gyawali_2008          # Oklahoma
########################

Gyawali_2008<-read.csv(paste(basepath,"Gyawali_2008_Data.csv",sep=""),header=T, as.is= T)        
#names(Gyawali_2008)
#Gyawali_2008$tree.no
#Gyawali_2008[is.na(Gyawali_2008)]<-0
Gyawali_2008$AUTHOR<-"Gyawali"
Gyawali_2008$LOC<-1
Gyawali_2008$SPCD<-110
Gyawali_2008$TREENO<-1:nrow(Gyawali_2008)
Gyawali_2008$CCLCD<-NA
Gyawali_2008$CW<-Gyawali_2008$CW..m*3.28084
Gyawali_2008$H_STUMP<-0.14*3.28084
Gyawali_2008$DBH<-Gyawali_2008$DBH..cm/2.54
Gyawali_2008$HT<-Gyawali_2008$HT..m*3.28084
Gyawali_2008$HL_BRANCH<-(Gyawali_2008$HT..m - Gyawali_2008$CL..m)*3.28084
Gyawali_2008$DW_SW<-as.numeric(Gyawali_2008$Bole.Wood..kg)*2.20462
Gyawali_2008$DW_SB<-Gyawali_2008$Bole.Bark..kg*2.20462
Gyawali_2008$DW_STEM<-Gyawali_2008$DW_SW + Gyawali_2008$DW_SB
Gyawali_2008$DEAD_BRANCH_DW<-NA
Gyawali_2008$LIVE_BRANCH_DW<-NA
Gyawali_2008$DW_BRANCH<-Gyawali_2008$Branch..kg*2.20462
Gyawali_2008$DW_TOT<-Gyawali_2008$DW_STEM + Gyawali_2008$DW_BRANCH
Gyawali_2008$DW_FOL<-Gyawali_2008$Foliage..kg*2.20462
Gyawali_2008$BIOMASS<-Gyawali_2008$DW_TOT + Gyawali_2008$DW_FOL
Gyawali_2008$STUMP<-NA
Gyawali_2008$ABG<-NA
Gyawali_2008$ORIGIN<-1
Gyawali_2008$TEMP<-60
Gyawali_2008$DGL<-NA
Gyawali_2008$DSTEM_MIN<-1/2.54
Gyawali_2008$DBLC<-NA
Gyawali_2008$DW_CROWN<-Gyawali_2008$DW_BRANCH + Gyawali_2008$DW_FOL
Gyawali_2008$ST_METH <-'1'
Gyawali_2008$BR_METH <-'3'
Gyawali_2008$FOL_METH<-'3'
Gyawali_2008$REGION  <-'SE'

Gyawali_2008$Indicator <- 'unchecked'
Gyawali_2008$GW_STEM<-'unchecked'
Gyawali_2008$GW_BRANCH<-'unchecked'
Gyawali_2008$GW_FOL<-'unchecked'
Gyawali_2008$GW_CROWN<-'unchecked'
Gyawali_2008$GW_BIOMASS<-'unchecked'
Gyawali_2008$GW_ABG<-'unchecked'
Gyawali_2008$GW_TOT<-'unchecked'
Gyawali_2008$M.C.<-'unchecked'
Gyawali_2008$SP.GR<-'unchecked'
Gyawali_2008$Bark.Fraction<-'unchecked'
Gyawali_2008$Age<-'unchecked'


Gyawali_2008<-Gyawali_2008[,c(16:58)]
#plot(Gyawali_2008$dbh.in,Gyawali_2008$stem.gw.lb)
#names(Gyawali_2008)
#names(LegacyData)
LegacyData<-rbind(LegacyData,Gyawali_2008)

########################
# Mueller_1976         # 
########################

Mueller_1976<-read.csv(paste(basepath,"Mueller_1976_Data.csv",sep=""),header=T, as.is= T)        
#Mueller_1976$tree.no
#Mueller_1976[is.na(Mueller_1976)]<-0
Mueller_1976$AUTHOR<-"Mueller"
Mueller_1976$LOC<-1
Mueller_1976$SPCD<-742
Mueller_1976$TREENO<-1:nrow(Mueller_1976)
Mueller_1976$CCLCD<-NA
Mueller_1976$CW<-Mueller_1976$crown.diam.m*3.28084
Mueller_1976$H_STUMP<-0
Mueller_1976$DBH<-Mueller_1976$dbh.cm/2.54
Mueller_1976$HT<-Mueller_1976$tht.m*3.28084
Mueller_1976$HL_BRANCH<-(Mueller_1976$tht.m - Mueller_1976$live.crown.length.m)*3.28084
Mueller_1976$DW_SW<-NA
Mueller_1976$DW_SB<-NA
Mueller_1976$DW_STEM<-Mueller_1976$stem.dw.kg*2.20462
Mueller_1976$DEAD_BRANCH_DW<-Mueller_1976$dead.branch.dw.kg*2.20462
Mueller_1976$LIVE_BRANCH_DW<-(Mueller_1976$current.branch.dw.kg + 
                              Mueller_1976$older.foliage.bearing.branches.dw.kg + 
                              Mueller_1976$older.nonfoliage.bearing.branches.dw.kg) * 2.20462
Mueller_1976$DW_BRANCH<-Mueller_1976$LIVE_BRANCH_DW + Mueller_1976$DEAD_BRANCH_DW
Mueller_1976$DW_TOT<-Mueller_1976$DW_STEM + Mueller_1976$DW_BRANCH
Mueller_1976$DW_FOL<-Mueller_1976$foliage.dw.kg*2.20462
Mueller_1976$BIOMASS<-Mueller_1976$DW_TOT + Mueller_1976$DW_FOL
Mueller_1976$STUMP<-0
Mueller_1976$ABG<-Mueller_1976$BIOMASS
Mueller_1976$ORIGIN<-2
Mueller_1976$TEMP<-70
Mueller_1976$DGL<-NA
Mueller_1976$DSTEM_MIN<-0
Mueller_1976$DBLC<-NA
Mueller_1976$DW_CROWN<-Mueller_1976$DW_BRANCH + Mueller_1976$DW_FOL
Mueller_1976$ST_METH <-'1'
Mueller_1976$BR_METH <-'2'
Mueller_1976$FOL_METH<-'2'
Mueller_1976$REGION  <-'SE'
Mueller_1976$Indicator <- 'unchecked'
Mueller_1976$GW_STEM<-'unchecked'
Mueller_1976$GW_BRANCH<-'unchecked'
Mueller_1976$GW_FOL<-'unchecked'
Mueller_1976$GW_CROWN<-'unchecked'
Mueller_1976$GW_BIOMASS<-'unchecked'
Mueller_1976$GW_ABG<-'unchecked'
Mueller_1976$GW_TOT<-'unchecked'
Mueller_1976$M.C.<-'unchecked'
Mueller_1976$SP.GR<-'unchecked'
Mueller_1976$Bark.Fraction<-'unchecked'
Mueller_1976$Age<-'unchecked'


Mueller_1976<-Mueller_1976[,c(15:57)]
#plot(Mueller_1976$dbh.in,Mueller_1976$stem.gw.lb)
#names(Mueller_1976)
#names(LegacyData)
LegacyData<-rbind(LegacyData,Mueller_1976)

########################
# Tossey_1982          #
########################

Tossey_1982<-read.csv(paste(basepath,"Tossey_1982_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
#names(Tossey_1982)
#Tossey_1982$species
#Tossey_1982[is.na(Tossey_1982)]<-0
Tossey_1982$AUTHOR<-"Tossey"
Tossey_1982$LOC<-1
Tossey_1982[Tossey_1982$species=="ponderosa pine","SPCD"]<-122
Tossey_1982[Tossey_1982$species=="lodgepole pine","SPCD"]<-108
Tossey_1982[Tossey_1982$species=="Engelmann spruce","SPCD"]<-93
Tossey_1982[Tossey_1982$species=="subalpine fir","SPCD"]<-19
Tossey_1982$TREENO<-1:nrow(Tossey_1982)
Tossey_1982$CCLCD<-NA
Tossey_1982$CW<-NA
Tossey_1982[Tossey_1982$size.class=="seedling","H_STUMP"]<-0
Tossey_1982[Tossey_1982$size.class=="sapling","H_STUMP"]<-0.25
Tossey_1982$DBH<-Tossey_1982$dbh.cm/2.54
Tossey_1982$HT<-Tossey_1982$tht.m*3.28084
Tossey_1982$HL_BRANCH<-NA
Tossey_1982$DW_SW<-Tossey_1982$bole.wt.g*(2.20462/1000) # kg to g
Tossey_1982$DW_SB<-Tossey_1982$bark.wt.g*(2.20462/1000) # kg to g
Tossey_1982$DW_STEM<-Tossey_1982$DW_SB + Tossey_1982$DW_SW
Tossey_1982$DEAD_BRANCH_DW<-NA
Tossey_1982$LIVE_BRANCH_DW<-NA
Tossey_1982$DW_BRANCH<-Tossey_1982$branch.wt.g * (2.20462/1000)	# not valid for BF
Tossey_1982$DW_TOT<-Tossey_1982$DW_STEM + Tossey_1982$DW_BRANCH
Tossey_1982$DW_FOL<-Tossey_1982$fol.wt.g*(2.20462/1000)
Tossey_1982$BIOMASS<-Tossey_1982$DW_TOT + Tossey_1982$DW_FOL
Tossey_1982$STUMP<-NA
Tossey_1982[Tossey_1982$H_STUMP==0,"ABG"]<-Tossey_1982[Tossey_1982$H_STUMP==0,"BIOMASS"]
Tossey_1982$ORIGIN<-1
Tossey_1982$TEMP<-105					# 70 C for foliage
Tossey_1982$DGL<-Tossey_1982$dgl.cm/2.54
Tossey_1982$DSTEM_MIN<-NA
Tossey_1982$DBLC<-Tossey_1982$dblc.cm/2.54
Tossey_1982$DW_CROWN<-Tossey_1982$crown.wt.g*(2.20462/1000)
Tossey_1982$ST_METH <-'1'	# some small saplings entire tree dried
Tossey_1982$BR_METH <-'1'
Tossey_1982$FOL_METH<-'1'
Tossey_1982$REGION  <-'IMW'
Tossey_1982$Indicator <- 'unchecked'
Tossey_1982$GW_STEM<-'unchecked'
Tossey_1982$GW_BRANCH<-'unchecked'
Tossey_1982$GW_FOL<-'unchecked'
Tossey_1982$GW_CROWN<-'unchecked'
Tossey_1982$GW_BIOMASS<-'unchecked'
Tossey_1982$GW_ABG<-'unchecked'
Tossey_1982$GW_TOT<-'unchecked'
Tossey_1982$M.C.<-'unchecked'
Tossey_1982$SP.GR<-'unchecked'
Tossey_1982$Bark.Fraction<-'unchecked'
Tossey_1982$Age<-'unchecked'


Tossey_1982<-Tossey_1982[,c(15:57)]
LegacyData<-rbind(LegacyData,Tossey_1982)

########################
# Gower_1987           # 
########################

Gower_1987<-read.csv(paste(basepath,"Gower_1987_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
#names(Gower_1987)
#Gower_1987$code
#Gower_1987[is.na(Gower_1987)]<-0
Gower_1987$AUTHOR<-"Gower"
Gower_1987$LOC<-1
Gower_1987$SPCD<-Gower_1987$code
Gower_1987$TREENO<-1:nrow(Gower_1987)
Gower_1987$CCLCD<-NA
Gower_1987$CW<-NA
Gower_1987$H_STUMP<-0
Gower_1987$DBH<-Gower_1987$dbh.cm/2.54
Gower_1987$HT<-NA
Gower_1987$HL_BRANCH<-NA
Gower_1987$DW_SW<-Gower_1987$stem.wood.kg*2.20462
Gower_1987$DW_SB<-Gower_1987$stem.bark.kg*2.20462
Gower_1987$DW_STEM<-Gower_1987$DW_SB + Gower_1987$DW_SW
Gower_1987$DEAD_BRANCH_DW<-Gower_1987$dead.branch.kg/2.20462
Gower_1987$LIVE_BRANCH_DW<-(Gower_1987$live.branch.kg + Gower_1987$cur.twig.kg)
Gower_1987$DW_BRANCH<-Gower_1987$LIVE_BRANCH_DW + Gower_1987$DEAD_BRANCH_DW
Gower_1987$DW_TOT<-Gower_1987$DW_STEM + Gower_1987$DW_BRANCH
Gower_1987$DW_FOL<-Gower_1987$tot.fol.kg*2.20462
Gower_1987$BIOMASS<-Gower_1987$DW_TOT + Gower_1987$DW_FOL
Gower_1987$STUMP<-0
Gower_1987$ABG<-Gower_1987$BIOMASS
Gower_1987$ORIGIN<-1
Gower_1987$TEMP<-70
Gower_1987$DGL<-NA
Gower_1987$DSTEM_MIN<-NA
Gower_1987$DBLC<-NA
Gower_1987$DW_CROWN<-Gower_1987$DW_BRANCH + Gower_1987$DW_FOL
Gower_1987$ST_METH <-'1'
Gower_1987$BR_METH <-'2'
Gower_1987$FOL_METH<-'2'
Gower_1987$REGION  <-'PNW'
Gower_1987$Indicator <- 'unchecked'
Gower_1987$GW_STEM<-'unchecked'
Gower_1987$GW_BRANCH<-'unchecked'
Gower_1987$GW_FOL<-'unchecked'
Gower_1987$GW_CROWN<-'unchecked'
Gower_1987$GW_BIOMASS<-'unchecked'
Gower_1987$GW_ABG<-'unchecked'
Gower_1987$GW_TOT<-'unchecked'
Gower_1987$M.C.<-'unchecked'
Gower_1987$SP.GR<-'unchecked'
Gower_1987$Bark.Fraction<-'unchecked'
Gower_1987$Age<-'unchecked'


Gower_1987<-Gower_1987[,c(15:57)]

#plot(Gower_1987$DBH,Gower_1987$BIOMASS)
#plot(Gower_1987$DBH,Gower_1987$DW_BRANCH)
#plot(Gower_1987$DBH,Gower_1987$DW_FOL)
#names(Gower_1987)
#names(LegacyData)
LegacyData<-rbind(LegacyData,Gower_1987)

########################
# Gower_1999          # 
########################
Gower_1999<-read.csv(paste(basepath,"Gower_1999.csv",sep=""),header=T,as.is=T,na.strings="NA")
names(Gower_1999)
unique(Gower_1999$SPECIES)
Gower_1999$TOTAL_BRANCH_MASS
Gower_1999$LIVE_STEM_MASS
names(Gower_1999)
head(Gower_1999)
Gower_1999<-Gower_1999[complete.cases(Gower_1999[16]),]
summary(Gower_1999)
Gower_1999$AUTHOR<-"Gower"
Gower_1999$LOC<-Gower_1999$SITE_NAME
Gower_1999$LOC <- gsub(pattern= "\\'", replacement= "", Gower_1999$LOC)
Gower_1999[grepl("Picea mariana", Gower_1999$SPECIES), "SPCD"] <- 95
Gower_1999[grepl("Pinus banksiana", Gower_1999$SPECIES), "SPCD"] <- 105
Gower_1999[grepl("Populus tremuloides", Gower_1999$SPECIES), "SPCD"] <- 746
Gower_1999[grepl("Picea glauca", Gower_1999$SPECIES), "SPCD"] <- 94
dim(Gower_1999)
Gower_1999$TREENO<-1:nrow(Gower_1999)
Gower_1999$CCLCD<-NA
Gower_1999$CW<-NA
Gower_1999$H_STUMP<-0
Gower_1999$DBH<-Gower_1999$TREE_DIAMETER_BREAST_HT/2.54
Gower_1999$HT<-Gower_1999$TREE_HEIGHT*3.28084
Gower_1999$HL_BRANCH<-Gower_1999$HEIGHT_TO_CROWN_BASE*3.28084
Gower_1999$DW_SW<-NA
Gower_1999$DW_SB<-NA
Gower_1999$DW_STEM<-round((Gower_1999$LIVE_STEM_MASS*2.20462),0)
Gower_1999[Gower_1999$DW_STEM <= 0, "DW_STEM"]<-NA
Gower_1999$DEAD_BRANCH_DW<-NA
Gower_1999$LIVE_BRANCH_DW<-NA
Gower_1999$DW_BRANCH<-round((Gower_1999$TOTAL_BRANCH_MASS*2.20462),0) # precision appears to be neared kilogram leading to some 0 values?
Gower_1999[Gower_1999$DW_BRANCH <= 0, "DW_BRANCH"]<-NA
Gower_1999$DW_TOT<-Gower_1999$DW_STEM + Gower_1999$DW_BRANCH
Gower_1999$DW_FOL<-Gower_1999$TOTAL_FOLIAGE_MASS*2.20462
Gower_1999[Gower_1999$DW_FOL <= 0,"DW_FOL"]<-NA
Gower_1999$BIOMASS<-Gower_1999$DW_TOT + Gower_1999$DW_FOL
Gower_1999$STUMP<-0
Gower_1999$ABG<-Gower_1999$BIOMASS+Gower_1999$STUMP
Gower_1999$ORIGIN<-1
Gower_1999$TEMP<-70
Gower_1999$DGL<-Gower_1999$TREE_DIAMETER_BASE*2.20462
Gower_1999$DSTEM_MIN<-NA
Gower_1999$DBLC<-NA
Gower_1999$DW_CROWN<-Gower_1999$DW_BRANCH + Gower_1999$DW_FOL
Gower_1999$ST_METH <-'1'
Gower_1999$BR_METH <-'5'
Gower_1999$FOL_METH<-'5'	# some subsampling of twigs with foliage
Gower_1999$REGION  <-'NORCEN'
Gower_1999$Indicator <- 'unchecked'
Gower_1999$GW_STEM<-'unchecked'
Gower_1999$GW_BRANCH<-'unchecked'
Gower_1999$GW_FOL<-'unchecked'
Gower_1999$GW_CROWN<-'unchecked'
Gower_1999$GW_BIOMASS<-'unchecked'
Gower_1999$GW_ABG<-'unchecked'
Gower_1999$GW_TOT<-'unchecked'
Gower_1999$M.C.<-'unchecked'
Gower_1999$SP.GR<-'unchecked'
Gower_1999$Bark.Fraction<-'unchecked'
Gower_1999$Age<-'unchecked'
Gower_1999[is.na(Gower_1999)]<-0


Gower_1999<-subset(Gower_1999, subset = DW_STEM != 0 | DW_BRANCH != 0 | DW_FOL != 0 | BIOMASS != 0 | ABG != 0 | DW_CROWN != 0)
Gower_1999[Gower_1999 <= 0]<-NA

plot(Gower_1999$DBH,Gower_1999$DW_FOL/Gower_1999$ABG)
plot(Gower_1999$DBH,Gower_1999$DW_FOL)
plot(Gower_1999$DBH,Gower_1999$DW_BRANCH)
plot(Gower_1999$DBH,Gower_1999$DW_STEM)
plot(Gower_1999$DBH,Gower_1999$BIOMASS)
plot(Gower_1999$DBH,Gower_1999$ABG)

Gower_1999<-Gower_1999[,c(37:79)]
dim(Gower_1999)

LegacyData<-rbind(LegacyData,Gower_1999)

########################
# Box_1967             # Not on scholar 1/22/2014
########################

Box_1967<-read.csv(paste(basepath,"Box_1967_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
Box_1967$AUTHOR<-"Box"
Box_1967$LOC<-1
Box_1967$SPCD<-131
Box_1967$TREENO<-1:nrow(Box_1967)
Box_1967$CCLCD<-NA
Box_1967$CW<-NA
Box_1967$H_STUMP<-0
Box_1967$DBH<-Box_1967$dbh.in
Box_1967$HT<-Box_1967$tht.ft
Box_1967$HL_BRANCH<-NA
Box_1967$DW_SW<-NA
Box_1967$DW_SB<-NA
Box_1967$DW_STEM<-NA
Box_1967$DEAD_BRANCH_DW<-NA
Box_1967$LIVE_BRANCH_DW<-NA
Box_1967$DW_BRANCH<-NA
Box_1967$DW_TOT<-NA
Box_1967$DW_FOL<-NA
Box_1967$BIOMASS<-Box_1967$tot.dw.lb
Box_1967$STUMP<-0
Box_1967$ABG<-Box_1967$BIOMASS
Box_1967$ORIGIN<-2
Box_1967$TEMP<-82
Box_1967$DGL<-NA
Box_1967$DSTEM_MIN<-NA
Box_1967$DBLC<-NA
Box_1967$DW_CROWN<-NA
Box_1967$ST_METH <-'4'
Box_1967$BR_METH <-'4'
Box_1967$FOL_METH<-'4'
Box_1967$REGION  <-'SE'
Box_1967$Indicator <- 'unchecked'
Box_1967$GW_STEM<-'unchecked'
Box_1967$GW_BRANCH<-'unchecked'
Box_1967$GW_FOL<-'unchecked'
Box_1967$GW_CROWN<-'unchecked'
Box_1967$GW_BIOMASS<-'unchecked'
Box_1967$GW_ABG<-'unchecked'
Box_1967$GW_TOT<-'unchecked'
Box_1967$M.C.<-'unchecked'
Box_1967$SP.GR<-'unchecked'
Box_1967$Bark.Fraction<-'unchecked'
Box_1967$Age<-'unchecked'


Box_1967<-Box_1967[,c(9:51)]

LegacyData<-rbind(LegacyData,Box_1967)

########################
# Nygaard_1980         # not posted 1/22
########################

Nygaard_1980<-read.csv(paste(basepath,"Nygaard_1980_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
Nygaard_1980$AUTHOR<-"Nygaard"
Nygaard_1980$LOC<-1
Nygaard_1980$SPCD<-746
Nygaard_1980$TREENO<-1:nrow(Nygaard_1980)
Nygaard_1980$CCLCD<-NA
Nygaard_1980$CW<-Nygaard_1980$crown.width.m*3.20084
Nygaard_1980$H_STUMP<-NA
Nygaard_1980$DBH<-Nygaard_1980$dbh.cm/2.54
Nygaard_1980$HT<-Nygaard_1980$tht.m*3.28084
Nygaard_1980$HL_BRANCH<-Nygaard_1980$ht.live.crown.m*3.28084
Nygaard_1980$DW_SW<-NA
Nygaard_1980$DW_SB<-NA
Nygaard_1980$DW_STEM<-NA
Nygaard_1980$DEAD_BRANCH_DW<-NA
Nygaard_1980$LIVE_BRANCH_DW<-NA
Nygaard_1980$DW_BRANCH<-Nygaard_1980$branches.kg*2.20462
Nygaard_1980$DW_TOT<-NA
Nygaard_1980$DW_FOL<-Nygaard_1980$foliage.kg*2.20462
Nygaard_1980$BIOMASS<-NA
Nygaard_1980$STUMP<-NA
Nygaard_1980$ABG<-NA
Nygaard_1980$ORIGIN<-1
Nygaard_1980$TEMP<-65
Nygaard_1980$DGL<-NA
Nygaard_1980$DSTEM_MIN<-NA
Nygaard_1980$DBLC<-NA
Nygaard_1980$DW_CROWN<-Nygaard_1980$DW_BRANCH + Nygaard_1980$DW_FOL
Nygaard_1980$ST_METH <-'NA'
Nygaard_1980$BR_METH <-'2'
Nygaard_1980$FOL_METH<-'2'
Nygaard_1980$REGION  <-'NORCEN'
Nygaard_1980$Indicator <- 'unchecked'
Nygaard_1980$GW_STEM<-'unchecked'
Nygaard_1980$GW_BRANCH<-'unchecked'
Nygaard_1980$GW_FOL<-'unchecked'
Nygaard_1980$GW_CROWN<-'unchecked'
Nygaard_1980$GW_BIOMASS<-'unchecked'
Nygaard_1980$GW_ABG<-'unchecked'
Nygaard_1980$GW_TOT<-'unchecked'
Nygaard_1980$M.C.<-'unchecked'
Nygaard_1980$SP.GR<-'unchecked'
Nygaard_1980$Bark.Fraction<-'unchecked'
Nygaard_1980$Age<-'unchecked'


Nygaard_1980<-Nygaard_1980[,c(9:51)]

LegacyData<-rbind(LegacyData,Nygaard_1980)

########################
# Shannon_1976         # not posted 1/22/13
########################

Shannon_1976<-read.csv(paste(basepath,"Shannon_1976_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
Shannon_1976$AUTHOR<-"Shannon"
Shannon_1976$LOC<-1
Shannon_1976$SPCD<-12
Shannon_1976$TREENO<-1:nrow(Shannon_1976)
Shannon_1976$CCLCD<-NA
Shannon_1976$CW<-NA
Shannon_1976$H_STUMP<-0
Shannon_1976$DBH<-Shannon_1976$dbh.in
Shannon_1976$HT<-Shannon_1976$tht.ft
Shannon_1976$HL_BRANCH<-Shannon_1976$ht.live.crown.ft
Shannon_1976$DW_SW<-Shannon_1976$bolewood.lb
Shannon_1976$DW_SB<-Shannon_1976$bolebark.lb
Shannon_1976$DW_STEM<-Shannon_1976$DW_SB + Shannon_1976$DW_SW
Shannon_1976$DEAD_BRANCH_DW<-Shannon_1976$dead.branch.lb
Shannon_1976$LIVE_BRANCH_DW<-Shannon_1976$live.branch.lb
Shannon_1976$DW_BRANCH<-Shannon_1976$DEAD_BRANCH_DW + Shannon_1976$LIVE_BRANCH_DW
Shannon_1976$DW_TOT<-Shannon_1976$DW_BRANCH + Shannon_1976$DW_STEM
Shannon_1976$DW_FOL<-Shannon_1976$foliage.lb
Shannon_1976$BIOMASS<-Shannon_1976$DW_FOL + Shannon_1976$DW_TOT
Shannon_1976$STUMP<-0
Shannon_1976$ABG<-Shannon_1976$BIOMASS
Shannon_1976$ORIGIN<-1
Shannon_1976$TEMP<-70
Shannon_1976$DGL<-NA
Shannon_1976$DSTEM_MIN<-NA
Shannon_1976$DBLC<-NA
Shannon_1976$DW_CROWN<-Shannon_1976$DW_BRANCH + Shannon_1976$DW_FOL
Shannon_1976$ST_METH <-'1'
Shannon_1976$BR_METH <-'2'
Shannon_1976$FOL_METH<-'2'
Shannon_1976$REGION  <-'NORCEN'
Shannon_1976$Indicator <- 'unchecked'
Shannon_1976$GW_STEM<-'unchecked'
Shannon_1976$GW_BRANCH<-'unchecked'
Shannon_1976$GW_FOL<-'unchecked'
Shannon_1976$GW_CROWN<-'unchecked'
Shannon_1976$GW_BIOMASS<-'unchecked'
Shannon_1976$GW_ABG<-'unchecked'
Shannon_1976$GW_TOT<-'unchecked'
Shannon_1976$M.C.<-'unchecked'
Shannon_1976$SP.GR<-'unchecked'
Shannon_1976$Bark.Fraction<-'unchecked'
Shannon_1976$Age<-'unchecked'


Shannon_1976<-Shannon_1976[,c(12:54)]
LegacyData<-rbind(LegacyData,Shannon_1976)


#########################################################
# Whittaker & Niering 1974                              #
#########################################################

Whittaker_Niering_1974<-read.csv(paste(basepath,"Whittaker_Niering_1975_data.csv",sep=""),header=T,as.is=T,na.strings="NA")

Whittaker_Niering_1974$check<-Whittaker_Niering_1974$STEM_DW_g
Whittaker_Niering_1974$AUTHOR<-"Whittaker_Niering"
Whittaker_Niering_1974$LOC<-1
Whittaker_Niering_1974$SPCD<-ifelse(Whittaker_Niering_1974$SPP == 'QUHY',843,140)
Whittaker_Niering_1974$TREENO<-Whittaker_Niering_1974$PLANT_NO
Whittaker_Niering_1974$CCLCD<-NA
Whittaker_Niering_1974$CW<-NA
Whittaker_Niering_1974$H_STUMP<-0					# Brookhaven system applied 				
Whittaker_Niering_1974$DBH<-Whittaker_Niering_1974$DBH_mm/10/2.54			# dbh in cm
Whittaker_Niering_1974$HT<-Whittaker_Niering_1974$HT_mm/1000*3.28084			# height in cm
Whittaker_Niering_1974$HL_BRANCH<-NA
Whittaker_Niering_1974$DW_SW<-Whittaker_Niering_1974$STEM_WOOD_DW_g/1000*2.20462
Whittaker_Niering_1974$DW_SB<-Whittaker_Niering_1974$STEM_BARK_DW_g/1000*2.20462
Whittaker_Niering_1974$DW_STEM<-Whittaker_Niering_1974$DW_SW+Whittaker_Niering_1974$DW_SB
# Whittaker_Niering_1974[Whittaker_Niering_1974$DEADWD < 0,"DEADWD"]<- NA
Whittaker_Niering_1974$DEAD_BRANCH_DW<-NA
Whittaker_Niering_1974$LIVE_BRANCH_DW<-NA
Whittaker_Niering_1974$DW_BRANCH<- NA        # Whittaker_Niering_1974$BRANCH_DW_g/1000*2.20462		# includes dead wood 
Whittaker_Niering_1974$DW_TOT<- NA           # Whittaker_Niering_1974$DW_STEM+Whittaker_Niering_1974$DW_BRANCH
Whittaker_Niering_1974$DW_FOL<- NA		   # foliage + twigs
Whittaker_Niering_1974$BIOMASS<-NA	
Whittaker_Niering_1974$STUMP<-NA
names(Whittaker_Niering_1974)
Whittaker_Niering_1974$ABG<-       (Whittaker_Niering_1974$DW_STEM + 
                                    Whittaker_Niering_1974$BRANCH_DW_g/1000*2.20462 + 
				            Whittaker_Niering_1974$TwigLf_DW/1000*2.20462 +
					      Whittaker_Niering_1974$Old_Lf_DW/1000*2.20462)
Whittaker_Niering_1974$ABG
Whittaker_Niering_1974$ORIGIN<-1					# assumed 
Whittaker_Niering_1974$TEMP<-105					# p.234
Whittaker_Niering_1974$DGL<-NA
Whittaker_Niering_1974$DSTEM_MIN<-0
Whittaker_Niering_1974$DBLC<-NA
Whittaker_Niering_1974$DW_CROWN<- Whittaker_Niering_1974$BRANCH_DW_g/1000*2.20462 + 
				            Whittaker_Niering_1974$TwigLf_DW/1000*2.20462 +
					      Whittaker_Niering_1974$Old_Lf_DW/1000*2.20462
 	
Whittaker_Niering_1974$ST_METH<-'1'
Whittaker_Niering_1974$BR_METH<-'3'
Whittaker_Niering_1974$FOL_METH<-'3'
Whittaker_Niering_1974$REGION<-'SW'
Whittaker_Niering_1974$Indicator <- 'unchecked'
Whittaker_Niering_1974$GW_STEM<-'unchecked'
Whittaker_Niering_1974$GW_BRANCH<-'unchecked'
Whittaker_Niering_1974$GW_FOL<-'unchecked'
Whittaker_Niering_1974$GW_CROWN<-'unchecked'
Whittaker_Niering_1974$GW_BIOMASS<-'unchecked'
Whittaker_Niering_1974$GW_ABG<-'unchecked'
Whittaker_Niering_1974$GW_TOT<-'unchecked'
Whittaker_Niering_1974$M.C.<-'unchecked'
Whittaker_Niering_1974$SP.GR<-'unchecked'
Whittaker_Niering_1974$Bark.Fraction<-'unchecked'
Whittaker_Niering_1974$Age<-'unchecked'
Whittaker_Niering_1974<-Whittaker_Niering_1974[,c(17:59)]
plot(Legacy$DBH,Legacy$DW_STEM)
points(Whittaker_Niering_1974$DBH,Whittaker_Niering_1974$DW_STEM,col='red')


plot(Legacy$DBH,Legacy$ABG)
points(Whittaker_Niering_1974$DBH,Whittaker_Niering_1974$ABG,col='red')


LegacyData<-rbind(LegacyData,Whittaker_Niering_1974)

#######################################
# Westmann Whittaker Mendicino County # added 2/10/2014; coefficient check did not agree, but are more trees than were published.
#######################################

Westmann_Whittaker_1975<-read.csv(paste(basepath,"Westmann_Whittaker_1975.csv",sep=""),header=T,as.is=T,na.strings="NA")
#Westmann_Whittaker_1975[is.na(Westmann_Whittaker_1975)]<-0
Westmann_Whittaker_1975$AUTHOR<-"Westmann_Whittaker"
Westmann_Whittaker_1975$LOC<-1
Westmann_Whittaker_1975$SPCD<-108
 Westmann_Whittaker_1975$TREENO<-Westmann_Whittaker_1975$PLANT_NO
 Westmann_Whittaker_1975$CCLCD<-NA
 Westmann_Whittaker_1975$CW<-NA
 Westmann_Whittaker_1975$H_STUMP<-0			#		 Brookhaven system applied 				
 Westmann_Whittaker_1975$DBH<-Westmann_Whittaker_1975$DBH_mm/10/2.54			 #dbh in cm
 Westmann_Whittaker_1975$HT<-Westmann_Whittaker_1975$HT_mm/1000*3.28084			 #height in cm
 Westmann_Whittaker_1975$HL_BRANCH<-NA
 Westmann_Whittaker_1975$DW_SW<-Westmann_Whittaker_1975$STEM_WOOD_DW_g/1000*2.20462
 Westmann_Whittaker_1975$DW_SB<-Westmann_Whittaker_1975$STEM_BARK_DW_g/1000*2.20462
 Westmann_Whittaker_1975$DW_STEM<-Westmann_Whittaker_1975$DW_SW+Westmann_Whittaker_1975$DW_SB
 Westmann_Whittaker_1975$DEAD_BRANCH_DW<-NA
 Westmann_Whittaker_1975$LIVE_BRANCH_DW<-NA
 Westmann_Whittaker_1975$DW_BRANCH<- NA;               # Westmann_Whittaker_1975$BRANCH_DW_g/100*2.20462		 includes dead wood 
 Westmann_Whittaker_1975$DW_TOT<- NA                   # Westmann_Whittaker_1975$DW_STEM+Westmann_Whittaker_1975$DW_BRANCH
 Westmann_Whittaker_1975$DW_FOL<- NA					# problems getting foliage biomass to match equations
 Westmann_Whittaker_1975$BIOMASS<-(Westmann_Whittaker_1975$STEM_DW_g   + 
					     Westmann_Whittaker_1975$BRANCH_DW_g + 
					     Westmann_Whittaker_1975$TwigLf_DW   + 
					     Westmann_Whittaker_1975$Old_Lf_DW)/1000*2.20462
 Westmann_Whittaker_1975$STUMP<-NA
 Westmann_Whittaker_1975$ABG<-     (Westmann_Whittaker_1975$STEM_DW_g   + 
					     Westmann_Whittaker_1975$BRANCH_DW_g + 
					     Westmann_Whittaker_1975$TwigLf_DW   + 
					     Westmann_Whittaker_1975$Old_Lf_DW)/1000*2.20462

 Westmann_Whittaker_1975$ORIGIN<-'N'					# assumed 
 Westmann_Whittaker_1975$TEMP<-105					# p.234
 Westmann_Whittaker_1975$DGL<-Westmann_Whittaker_1975$DBH_mm/10/2.54
 Westmann_Whittaker_1975$DSTEM_MIN<-0
 Westmann_Whittaker_1975$DBLC<-NA
 Westmann_Whittaker_1975$DW_CROWN<-(Westmann_Whittaker_1975$BRANCH_DW_g + Westmann_Whittaker_1975$TwigLf_DW + Westmann_Whittaker_1975$ Old_Lf_DW)/1000*2.20462
 Westmann_Whittaker_1975$ST_METH<-'1'
 Westmann_Whittaker_1975$BR_METH<-'3'
 Westmann_Whittaker_1975$FOL_METH<-'3'
 Westmann_Whittaker_1975$REGION<-'SW'
Westmann_Whittaker_1975$Indicator <- 'unchecked'
Westmann_Whittaker_1975$GW_STEM<-'unchecked'
Westmann_Whittaker_1975$GW_BRANCH<-'unchecked'
Westmann_Whittaker_1975$GW_FOL<-'unchecked'
Westmann_Whittaker_1975$GW_CROWN<-'unchecked'
Westmann_Whittaker_1975$GW_BIOMASS<-'unchecked'
Westmann_Whittaker_1975$GW_ABG<-'unchecked'
Westmann_Whittaker_1975$GW_TOT<-'unchecked'
Westmann_Whittaker_1975$M.C.<-'unchecked'
Westmann_Whittaker_1975$SP.GR<-'unchecked'
Westmann_Whittaker_1975$Bark.Fraction<-'unchecked'
Westmann_Whittaker_1975$Age<-'unchecked'
names(Westmann_Whittaker_1975)
Westmann_Whittaker_1975<-Westmann_Whittaker_1975[,c(16:58)]
names(Westmann_Whittaker_1975);names(LegacyData)
LegacyData<-rbind(LegacyData,Westmann_Whittaker_1975)
dim(LegacyData) # 7758
#write.csv(LegacyData,"C:\\FIA\\LegacyDataBase\\Legacy_Data_Dec22.csv",row.names=F)
##################################
# Whittaker SJSP no article yet  # # includes roots too
##################################
Whittaker_SJSP<-read.csv(paste(basepath,"Whittaker_SJSP_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
names(Whittaker_SJSP)
Whittaker_SJSP$AUTHOR<-"Whittaker"
Whittaker_SJSP$LOC<-1
Whittaker_SJSP$SPCD<-108
Whittaker_SJSP$TREENO<-Whittaker_SJSP$PLANT_NO
Whittaker_SJSP$CCLCD<-NA
Whittaker_SJSP$CW<-NA
Whittaker_SJSP$H_STUMP<-0					# Brookhaven system applied 				
Whittaker_SJSP$DBH<-Whittaker_SJSP$DBH_mm * 0.0393701			# dbh in cm
Whittaker_SJSP$HT<-Whittaker_SJSP$HT_mm * 0.0328084
Whittaker_SJSP$HL_BRANCH<-NA
Whittaker_SJSP$DW_SW<-Whittaker_SJSP$STEM_WOOD_DW_g/1000*2.20462
Whittaker_SJSP$DW_SB<-Whittaker_SJSP$STEM_BARK_DW_g/1000*2.20462
Whittaker_SJSP$DW_STEM<-Whittaker_SJSP$DW_SW+Whittaker_SJSP$DW_SB
# Whittaker_SJSP[Whittaker_SJSP$DEADWD < 0,"DEADWD"]<- NA
Whittaker_SJSP$DEAD_BRANCH_DW<-NA
Whittaker_SJSP$LIVE_BRANCH_DW<-NA
Whittaker_SJSP$DW_BRANCH<-NA    # Whittaker_SJSP$BRANCH_DW_g/1000*2.20462		# problem of twigs with leaves
Whittaker_SJSP$DW_TOT<-   NA    # Whittaker_SJSP$DW_STEM+Whittaker_SJSP$DW_BRANCH
Whittaker_SJSP$DW_FOL<- NA	  
Whittaker_SJSP$BIOMASS<-Whittaker_SJSP$DW_STEM + Whittaker_SJSP$BRANCH_DW_g/1000*2.20462 + Whittaker_SJSP$TwigLf_DW/1000*2.2 + Whittaker_SJSP$Old_Lf_DW/1000*2.2
Whittaker_SJSP$STUMP<-NA
Whittaker_SJSP$ABG<-   Whittaker_SJSP$DW_STEM + Whittaker_SJSP$BRANCH_DW_g/1000*2.20462 + Whittaker_SJSP$TwigLf_DW/1000*2.2 + Whittaker_SJSP$Old_Lf_DW/1000*2.2
Whittaker_SJSP$ORIGIN<-'N'					# assumed 
Whittaker_SJSP$TEMP<-105					# p.234
Whittaker_SJSP$DGL<-NA
Whittaker_SJSP$DSTEM_MIN<-0
Whittaker_SJSP$DBLC<-NA
Whittaker_SJSP$DW_CROWN<- Whittaker_SJSP$BRANCH_DW_g/1000*2.20462 + Whittaker_SJSP$TwigLf_DW/1000*2.20462 + Whittaker_SJSP$Old_Lf_DW/1000*2.20462
Whittaker_SJSP$ST_METH<-'1'
Whittaker_SJSP$BR_METH<-'3'
Whittaker_SJSP$FOL_METH<-'3'
Whittaker_SJSP$REGION<-'SW'
Whittaker_SJSP$Indicator <- 'unchecked'
Whittaker_SJSP$GW_STEM<-'unchecked'
Whittaker_SJSP$GW_BRANCH<-'unchecked'
Whittaker_SJSP$GW_FOL<-'unchecked'
Whittaker_SJSP$GW_CROWN<-'unchecked'
Whittaker_SJSP$GW_BIOMASS<-'unchecked'
Whittaker_SJSP$GW_ABG<-'unchecked'
Whittaker_SJSP$GW_TOT<-'unchecked'
Whittaker_SJSP$M.C.<-'unchecked'
Whittaker_SJSP$SP.GR<-'unchecked'
Whittaker_SJSP$Bark.Fraction<-'unchecked'
Whittaker_SJSP$Age<-'unchecked'


Whittaker_SJSP<-Whittaker_SJSP[,c(27:69)]
Whittaker_SJSP$ABG
Whittaker_SJSP
plot(Whittaker_SJSP$DBH,Whittaker_SJSP$DW_STEM)

plot(Legacy$DBH,Legacy$ABG)
points(Whittaker_SJSP$DBH,Whittaker_SJSP$ABG,col='red')
plot(Legacy$DBH,Legacy$DW_CROWN)
points(Whittaker_SJSP$DBH,Whittaker_SJSP$DW_CROWN,col='red')

LegacyData<-rbind(LegacyData,Whittaker_SJSP)
dim(Whittaker_SJSP)
#############
# Dice 1970 #
#############
Dice_1970<-read.csv(paste(basepath,"Dice_1970_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
Dice_1970$AUTHOR<-"Dice"
Dice_1970$LOC<-1
Dice_1970$SPCD<-202
Dice_1970$TREENO<-Dice_1970$tree.no
Dice_1970$CCLCD<-NA
Dice_1970$CW<-NA
Dice_1970$H_STUMP<-NA
Dice_1970$DBH<-Dice_1970$dbh.cm/2.54			# dbh in cm
Dice_1970$HT<-NA
Dice_1970$HL_BRANCH<-NA
Dice_1970$DW_SW<-Dice_1970$stem.wood.g/1000*2.20462
Dice_1970$DW_SB<-Dice_1970$stem.bark.g/1000*2.20462
Dice_1970$DW_STEM<-Dice_1970$DW_SW+Dice_1970$DW_SB
Dice_1970$DEAD_BRANCH_DW<-Dice_1970$dead.branch.g * 0.00220462
Dice_1970$LIVE_BRANCH_DW<-Dice_1970$live.branch.g * 0.00220462
Dice_1970$DW_BRANCH<-Dice_1970$LIVE_BRANCH_DW + Dice_1970$DEAD_BRANCH_DW
Dice_1970$DW_TOT<- Dice_1970$DW_STEM+Dice_1970$DW_BRANCH
Dice_1970$DW_FOL<- Dice_1970$foliage.g * 0.00220462
Dice_1970$BIOMASS<- Dice_1970$DW_FOL + Dice_1970$DW_TOT
Dice_1970$STUMP<-NA
Dice_1970$ABG<-NA
Dice_1970$ORIGIN<-1
Dice_1970$TEMP<-70
Dice_1970$DGL<-NA
Dice_1970$DSTEM_MIN<-NA
Dice_1970$DBLC<-NA
Dice_1970$DW_CROWN<- Dice_1970$DW_FOL + Dice_1970$DW_BRANCH
Dice_1970$ST_METH<-'2'				# almost 1 = 3m
Dice_1970$BR_METH<-'2'
Dice_1970$FOL_METH<-'2'
Dice_1970$REGION<-'PNW'
Dice_1970$Indicator <- 'unchecked'
Dice_1970$GW_STEM<-'unchecked'
Dice_1970$GW_BRANCH<-'unchecked'
Dice_1970$GW_FOL<-'unchecked'
Dice_1970$GW_CROWN<-'unchecked'
Dice_1970$GW_BIOMASS<-'unchecked'
Dice_1970$GW_ABG<-'unchecked'
Dice_1970$GW_TOT<-'unchecked'
Dice_1970$M.C.<-'unchecked'
Dice_1970$SP.GR<-'unchecked'
Dice_1970$Bark.Fraction<-'unchecked'
Dice_1970$Age<-'unchecked'


Dice_1970 <- Dice_1970[,c(16:58)]

LegacyData <- rbind(LegacyData,Dice_1970)

#############
# Reinhardt #
#############
Reinhardt<-read.csv(paste(basepath,"Reinhardt_Data_All.csv",sep=""),header=T,as.is=T,na.strings="NA")
Reinhardt$AUTHOR<-"Reinhardt"
Reinhardt[Reinhardt$PlotName == "Ninemile", "LOC"] <- 1
Reinhardt[Reinhardt$PlotName == "Salmon", "LOC"] <- 2
Reinhardt[Reinhardt$PlotName == "Flagstaff", "LOC"] <- 3
Reinhardt[Reinhardt$PlotName == "Blodgett", "LOC"] <- 4
Reinhardt[Reinhardt$PlotName == "Tenderfoot", "LOC"] <- 5
Reinhardt[Reinhardt$SppID == "PP", "SPCD"] <- 122
Reinhardt[Reinhardt$SppID == "DF", "SPCD"] <- 202
Reinhardt[Reinhardt$SppID == "LP", "SPCD"] <- 108
Reinhardt[Reinhardt$SppID == "IC", "SPCD"] <- 81
Reinhardt[Reinhardt$SppID == "WF", "SPCD"] <- 15
Reinhardt$TREENO<-Reinhardt$TreeID
Reinhardt[Reinhardt$Cclass == "C", "CCLCD"] <- 3
Reinhardt[Reinhardt$Cclass == "I", "CCLCD"] <- 4
Reinhardt[Reinhardt$Cclass == "S", "CCLCD"] <- 5
Reinhardt[Reinhardt$Cclass == "D", "CCLCD"] <- 2
Reinhardt$CW<-NA
Reinhardt$H_STUMP<-NA
Reinhardt$DBH<-Reinhardt$DBH_cm.x/2.54  		# dbh in cm
Reinhardt$HT<-Reinhardt$TreeHeight_m.x * 3.28084
Reinhardt$HL_BRANCH<- Reinhardt$LCBH_m.x * 3.28084
Reinhardt$DW_SW<-NA
Reinhardt$DW_SB<-NA
Reinhardt$DW_STEM<-NA
Reinhardt$DEAD_BRANCH_DW<-Reinhardt$DEAD_BRANCH_DW_g * 0.00220462
Reinhardt$LIVE_BRANCH_DW<-Reinhardt$LIVE_BRANCH_DW_g * 0.00220462
Reinhardt$DW_BRANCH<-Reinhardt$LIVE_BRANCH_DW + Reinhardt$DEAD_BRANCH_DW
Reinhardt$DW_TOT<- NA
Reinhardt$DW_FOL<- Reinhardt$DW_FOL_g * 0.00220462
Reinhardt$BIOMASS<- NA
Reinhardt$STUMP<-NA
Reinhardt$ABG<-NA
Reinhardt$ORIGIN<-1
Reinhardt$TEMP<-NA
Reinhardt$DGL<-NA
Reinhardt$DSTEM_MIN<-NA
Reinhardt$DBLC<-NA
Reinhardt$DW_CROWN<- Reinhardt$DW_FOL + Reinhardt$DW_BRANCH
Reinhardt$ST_METH<-NA
Reinhardt$BR_METH<-'2'
Reinhardt$FOL_METH<-'2'
Reinhardt$REGION<-'IM'
Reinhardt$Indicator <- 'unchecked'
Reinhardt$GW_STEM<-'unchecked'
Reinhardt$GW_BRANCH<-'unchecked'
Reinhardt$GW_FOL<-'unchecked'
Reinhardt$GW_CROWN<-'unchecked'
Reinhardt$GW_BIOMASS<-'unchecked'
Reinhardt$GW_ABG<-'unchecked'
Reinhardt$GW_TOT<-'unchecked'
Reinhardt$M.C.<-'unchecked'
Reinhardt$SP.GR<-'unchecked'
Reinhardt$Bark.Fraction<-'unchecked'
Reinhardt$Age<-'unchecked'


Reinhardt <- Reinhardt[,c(19:61)]

LegacyData <- rbind(LegacyData,Reinhardt)

##############
# Rencz 1976 #
##############
Rencz_1976<-read.csv(paste(basepath,"Rencz_1976_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
Rencz_1976$AUTHOR<-"Rencz"
Rencz_1976$LOC <- 1
Rencz_1976$SPCD <- 95
Rencz_1976$TREENO<-Rencz_1976$tree.no
Rencz_1976$CCLCD <- NA
Rencz_1976$CW<-NA
Rencz_1976$H_STUMP<-NA
Rencz_1976$DBH<-Rencz_1976$dbh.cm / 2.54    	# dbh in cm
Rencz_1976$HT<-Rencz_1976$ht.m * 3.28084
Rencz_1976$HL_BRANCH<- NA
Rencz_1976$DW_SW<- NA
Rencz_1976$DW_SB<-NA
Rencz_1976$DW_STEM<- Rencz_1976$bole.g * 0.00220462
Rencz_1976$DEAD_BRANCH_DW<- NA
Rencz_1976$LIVE_BRANCH_DW<- NA
Rencz_1976$DW_BRANCH<-Rencz_1976$branch.g * 0.00220462
Rencz_1976$DW_TOT<- Rencz_1976$DW_BRANCH + Rencz_1976$DW_STEM
Rencz_1976$DW_FOL<- Rencz_1976$needle.g * 0.00220462
Rencz_1976$BIOMASS<- Rencz_1976$DW_FOL + Rencz_1976$DW_TOT
Rencz_1976$STUMP<-NA
Rencz_1976$ABG<-NA
Rencz_1976$ORIGIN<-1
Rencz_1976$TEMP<-70
Rencz_1976$DGL<-NA
Rencz_1976$DSTEM_MIN<-NA
Rencz_1976$DBLC<-NA
Rencz_1976$DW_CROWN<- Rencz_1976$DW_FOL + Rencz_1976$DW_BRANCH
Rencz_1976$ST_METH<- '2'
Rencz_1976$BR_METH<- '1'
Rencz_1976$FOL_METH<- '1'
Rencz_1976$REGION<-'Canada'
Rencz_1976$Indicator <- 'unchecked'
Rencz_1976$GW_STEM<-'unchecked'
Rencz_1976$GW_BRANCH<-'unchecked'
Rencz_1976$GW_FOL<-'unchecked'
Rencz_1976$GW_CROWN<-'unchecked'
Rencz_1976$GW_BIOMASS<-'unchecked'
Rencz_1976$GW_ABG<-'unchecked'
Rencz_1976$GW_TOT<-'unchecked'
Rencz_1976$M.C.<-'unchecked'
Rencz_1976$SP.GR<-'unchecked'
Rencz_1976$Bark.Fraction<-'unchecked'
Rencz_1976$Age<-'unchecked'


Rencz_1976 <- Rencz_1976[,c(13:55)]

LegacyData <- rbind(LegacyData,Rencz_1976)
dim(LegacyData) # 7999
#################
# Kloeppel 1998 #
#################
Kloeppel_1998<-read.csv(paste(basepath,"Kloeppel_1998_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
Kloeppel_1998$AUTHOR<-"Kloeppel"
Kloeppel_1998[Kloeppel_1998$site == "Bonner", "LOC"] <- 1
Kloeppel_1998[Kloeppel_1998$site == "Lost Creek", "LOC"] <- 2
Kloeppel_1998[Kloeppel_1998$site == "Savage Lake", "LOC"] <- 3
Kloeppel_1998[Kloeppel_1998$species == "western larch", "SPCD"] <- 73
Kloeppel_1998[Kloeppel_1998$species == "Douglas-fir", "SPCD"] <- 202
Kloeppel_1998[Kloeppel_1998$species == "lodgepole pine", "SPCD"] <- 108
Kloeppel_1998$TREENO<- 1:nrow(Kloeppel_1998)
Kloeppel_1998$CCLCD <- NA
Kloeppel_1998$CW<-NA
Kloeppel_1998$H_STUMP<-NA
Kloeppel_1998$DBH<-Kloeppel_1998$dbh.cm / 2.54      # dbh in cm
Kloeppel_1998$HT<- NA
Kloeppel_1998$HL_BRANCH<- NA
Kloeppel_1998$DW_SW<- NA
Kloeppel_1998$DW_SB<-NA
Kloeppel_1998$DW_STEM<- Kloeppel_1998$stem.tot.g * 0.00220462
Kloeppel_1998$DEAD_BRANCH_DW<- NA
Kloeppel_1998$LIVE_BRANCH_DW<- NA
Kloeppel_1998$DW_BRANCH<- (Kloeppel_1998$branch.tot.g + Kloeppel_1998$new.twig.g) * 0.00220462
Kloeppel_1998$DW_TOT<- Kloeppel_1998$DW_BRANCH + Kloeppel_1998$DW_STEM
Kloeppel_1998$DW_FOL<- Kloeppel_1998$foliage.tot.g * 0.00220462
Kloeppel_1998$BIOMASS<- Kloeppel_1998$DW_FOL + Kloeppel_1998$DW_TOT
Kloeppel_1998$STUMP<-NA
Kloeppel_1998$ABG<-NA
Kloeppel_1998$ORIGIN<-1
Kloeppel_1998$TEMP<- 65
Kloeppel_1998$DGL<-NA
Kloeppel_1998$DSTEM_MIN<-NA
Kloeppel_1998$DBLC<-NA
Kloeppel_1998$DW_CROWN<- Kloeppel_1998$DW_FOL + Kloeppel_1998$DW_BRANCH
Kloeppel_1998$ST_METH<- '1'
Kloeppel_1998$BR_METH<- '2'
Kloeppel_1998$FOL_METH<- '2'
Kloeppel_1998$REGION<-'IM'
Kloeppel_1998$Indicator <- 'unchecked'
Kloeppel_1998$GW_STEM<-'unchecked'
Kloeppel_1998$GW_BRANCH<-'unchecked'
Kloeppel_1998$GW_FOL<-'unchecked'
Kloeppel_1998$GW_CROWN<-'unchecked'
Kloeppel_1998$GW_BIOMASS<-'unchecked'
Kloeppel_1998$GW_ABG<-'unchecked'
Kloeppel_1998$GW_TOT<-'unchecked'
Kloeppel_1998$M.C.<-'unchecked'
Kloeppel_1998$SP.GR<-'unchecked'
Kloeppel_1998$Bark.Fraction<-'unchecked'
Kloeppel_1998$Age<-'unchecked'


Kloeppel_1998 <- Kloeppel_1998[,c(12:54)]

LegacyData <- rbind(LegacyData,Kloeppel_1998)

###############
# Comeau 1986 #
###############
Comeau_1986<-read.csv(paste(basepath,"Comeau_1986_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
Comeau_1986$AUTHOR<-"Comeau"
Comeau_1986$LOC <- 1
Comeau_1986$SPCD <- 108
Comeau_1986$TREENO<- Comeau_1986$tree.no
Comeau_1986$CCLCD <- NA
Comeau_1986$CW<-NA
Comeau_1986$H_STUMP<-NA
Comeau_1986$DBH<-Comeau_1986$dob.cm / 2.54      # dbh in cm
Comeau_1986$HT<- Comeau_1986$ht.m * 3.28084
Comeau_1986$HL_BRANCH<- Comeau_1986$hbc.m * 3.28084
Comeau_1986$DW_SW<- NA
Comeau_1986$DW_SB<-NA
Comeau_1986$DW_STEM<- Comeau_1986$stem.kg * 2.20462
Comeau_1986$DEAD_BRANCH_DW<- NA
Comeau_1986$LIVE_BRANCH_DW<- NA
Comeau_1986$DW_BRANCH<- Comeau_1986$branch.kg * 2.20462
Comeau_1986$DW_TOT<- Comeau_1986$DW_BRANCH + Comeau_1986$DW_STEM
Comeau_1986$DW_FOL<- Comeau_1986$foliage.kg * 2.20462
Comeau_1986$BIOMASS<- Comeau_1986$DW_FOL + Comeau_1986$DW_TOT
Comeau_1986$STUMP<-NA
Comeau_1986$ABG<-NA
Comeau_1986$ORIGIN<-1
Comeau_1986$TEMP<- 70
Comeau_1986$DGL<-NA
Comeau_1986$DSTEM_MIN<-NA
Comeau_1986$DBLC<-NA
Comeau_1986$DW_CROWN<- Comeau_1986$DW_FOL + Comeau_1986$DW_BRANCH
Comeau_1986$ST_METH<- '1'
Comeau_1986$BR_METH<- '2'
Comeau_1986$FOL_METH<- '2'
Comeau_1986$REGION<-'Canada'
Comeau_1986$Indicator <- 'unchecked'
Comeau_1986$GW_STEM<-'unchecked'
Comeau_1986$GW_BRANCH<-'unchecked'
Comeau_1986$GW_FOL<-'unchecked'
Comeau_1986$GW_CROWN<-'unchecked'
Comeau_1986$GW_BIOMASS<-'unchecked'
Comeau_1986$GW_ABG<-'unchecked'
Comeau_1986$GW_TOT<-'unchecked'
Comeau_1986$M.C.<-'unchecked'
Comeau_1986$SP.GR<-'unchecked'
Comeau_1986$Bark.Fraction<-'unchecked'
Comeau_1986$Age<-'unchecked'


Comeau_1986 <- Comeau_1986[,c(12:54)]

LegacyData <- rbind(LegacyData,Comeau_1986)


#############
# Crow 1977 #
#############
Crow_1977<-read.csv(paste(basepath,"Crow_1977_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
Crow_1977$AUTHOR<-"Crow"
Crow_1977$LOC <- 1
Crow_1977[Crow_1977$species == "red maple", "SPCD"] <- 316
Crow_1977[Crow_1977$species == "sugar maple", "SPCD"] <- 318
Crow_1977[Crow_1977$species == "paper birch", "SPCD"] <- 375
Crow_1977$TREENO<-  1:nrow(Crow_1977)
Crow_1977$CCLCD <- NA
Crow_1977$CW<-NA
Crow_1977$H_STUMP<-NA
Crow_1977$DBH<-Crow_1977$dbh.cm / 2.54      # dbh in cm
Crow_1977$HT<- Crow_1977$tht.m * 3.28084
Crow_1977$HL_BRANCH<- NA
Crow_1977$DW_SW<- Crow_1977$bole.wood.kg * 2.20462				# JF swithced 2/13
Crow_1977$DW_SB<- Crow_1977$bole.bark.kg * 2.20462
Crow_1977$DW_STEM<- Crow_1977$DW_SB + Crow_1977$DW_SW
Crow_1977$DEAD_BRANCH_DW<- NA
Crow_1977$LIVE_BRANCH_DW<- NA
Crow_1977$DW_BRANCH<- NA
Crow_1977$DW_TOT<- NA
Crow_1977$DW_FOL<- NA
Crow_1977$BIOMASS<- Crow_1977$total.abg.kg * 2.20462
Crow_1977$STUMP<-NA
Crow_1977$ABG<-NA
Crow_1977$ORIGIN<-1
Crow_1977$TEMP<- NA
Crow_1977$DGL<-NA
Crow_1977$DSTEM_MIN<-NA
Crow_1977$DBLC<-NA
Crow_1977$DW_CROWN<- (Crow_1977$twig.and.leaf.kg + Crow_1977$branch.kg) * 2.20462
Crow_1977$ST_METH<- NA
Crow_1977$BR_METH<- NA
Crow_1977$FOL_METH<- NA
Crow_1977$REGION<-'NORCEN'			# Change 6/3/2014
Crow_1977$Indicator <- 'unchecked'
Crow_1977$GW_STEM<-'unchecked'
Crow_1977$GW_BRANCH<-'unchecked'
Crow_1977$GW_FOL<-'unchecked'
Crow_1977$GW_CROWN<-'unchecked'
Crow_1977$GW_BIOMASS<-'unchecked'
Crow_1977$GW_ABG<-'unchecked'
Crow_1977$GW_TOT<-'unchecked'
Crow_1977$M.C.<-'unchecked'
Crow_1977$SP.GR<-'unchecked'
Crow_1977$Bark.Fraction<-'unchecked'
Crow_1977$Age<-'unchecked'


Crow_1977 <- Crow_1977[,c(14:56)]

LegacyData <- rbind(LegacyData,Crow_1977)

####################
# Woods et al 1991 #
####################
Woods_1991<-read.table(paste(basepath,"Woods_biomass.dat",sep=""),header=T,as.is=T,na.strings="NA")
Woods_1991$AUTHOR<-"Woods"
Woods_1991$LOC <- 1
Woods_1991[Woods_1991$species == "Aspen", "SPCD"] <- 746
Woods_1991[Woods_1991$species == "Spruce", "SPCD"] <- 95
Woods_1991$TREENO<-  1:nrow(Woods_1991)
Woods_1991$CCLCD <- NA
Woods_1991$CW<-NA
Woods_1991$H_STUMP<-NA
Woods_1991$DBH<-Woods_1991$dbh / 2.54      # dbh in cm
Woods_1991$HT<- Woods_1991$tree_ht * 3.28084
Woods_1991$HL_BRANCH<- (Woods_1991$tree_ht - Woods_1991$doc) * 3.28084
Woods_1991$DW_SW<- NA
Woods_1991$DW_SB<- NA
Woods_1991$DW_STEM<- NA
Woods_1991$DEAD_BRANCH_DW<- NA
Woods_1991$LIVE_BRANCH_DW<- NA
Woods_1991$DW_BRANCH<- NA
Woods_1991$DW_TOT<- NA
Woods_1991$DW_FOL<- NA
Woods_1991$BIOMASS<- Woods_1991$biomass * 0.00220462
Woods_1991$STUMP<-NA
Woods_1991$ABG<-NA
Woods_1991$ORIGIN<-1
Woods_1991$TEMP<- 105
Woods_1991$DGL<-NA
Woods_1991$DSTEM_MIN<-NA
Woods_1991$DBLC<-NA
Woods_1991$DW_CROWN<- NA
Woods_1991$ST_METH<- '2'
Woods_1991$BR_METH<- '2'
Woods_1991$FOL_METH<- '2'
Woods_1991$REGION<- 'NORCEN'
Woods_1991$Indicator <- 'unchecked'
Woods_1991$GW_STEM<-'unchecked'
Woods_1991$GW_BRANCH<-'unchecked'
Woods_1991$GW_FOL<-'unchecked'
Woods_1991$GW_CROWN<-'unchecked'
Woods_1991$GW_BIOMASS<-'unchecked'
Woods_1991$GW_ABG<-'unchecked'
Woods_1991$GW_TOT<-'unchecked'
Woods_1991$M.C.<-'unchecked'
Woods_1991$SP.GR<-'unchecked'
Woods_1991$Bark.Fraction<-'unchecked'
Woods_1991$Age<-'unchecked'


Woods_1991 <- Woods_1991[,c(9:51)]

LegacyData <- rbind(LegacyData,Woods_1991)

#########
# Gholz #
#########
Gholz<-read.csv(paste(basepath,"Gholz_Checked.csv",sep=""),header=T,as.is=T,na.strings="NA")
Gholz$AUTHOR<-"Gholz"
Gholz$LOC <- Gholz$LOCATION
Gholz[Gholz$SPEC == "PSME", "SPCD"] <- 202
Gholz[Gholz$SPEC == "TSHE", "SPCD"] <- 263
Gholz$TREENO <- 1:nrow(Gholz)
Gholz$CCLCD <- NA
Gholz$CW<-NA
Gholz$H_STUMP<-NA
Gholz$DBH<-Gholz$DBH / 2.54      # dbh in cm
Gholz$HT<- Gholz$HEIGHT * 3.28084 # height in meters
Gholz$HL_BRANCH<- (Gholz$HEIGHT - Gholz$CROWNLN) * 3.28084
Gholz$DW_SW<- Gholz$WOOD * 2.20462
Gholz$DW_SB<- Gholz$BARK * 2.20462
Gholz$DW_STEM<- Gholz$DW_SB + Gholz$DW_SW
Gholz$DEAD_BRANCH_DW<- Gholz$DEADBR * 2.20462
Gholz$LIVE_BRANCH_DW<- Gholz$LIVEBRNC * 2.20462
Gholz$DW_BRANCH<- Gholz$LIVE_BRANCH_DW + Gholz$DEAD_BRANCH_DW
Gholz$DW_TOT<- Gholz$DW_BRANCH + Gholz$DW_STEM
Gholz$DW_FOL<- Gholz$FOLBIOM * 2.20462
Gholz$BIOMASS<- Gholz$DW_TOT + Gholz$DW_FOL				# Corrected 2/13ToLe
Gholz$STUMP<-NA
Gholz$ABG<-NA
Gholz$ORIGIN<-1
Gholz$TEMP<- NA
Gholz$DGL<-NA
Gholz$DSTEM_MIN<-NA
Gholz$DBLC<-NA
Gholz$DW_CROWN<- Gholz$DW_BRANCH + Gholz$DW_FOL
Gholz$ST_METH<- NA
Gholz$BR_METH<- NA
Gholz$FOL_METH<- NA
Gholz$REGION<- 'PNW'
Gholz$Indicator <- 'unchecked'
Gholz$GW_STEM<-'unchecked'
Gholz$GW_BRANCH<-'unchecked'
Gholz$GW_FOL<-'unchecked'
Gholz$GW_CROWN<-'unchecked'
Gholz$GW_BIOMASS<-'unchecked'
Gholz$GW_ABG<-'unchecked'
Gholz$GW_TOT<-'unchecked'
Gholz$M.C.<-'unchecked'
Gholz$SP.GR<-'unchecked'
Gholz$Bark.Fraction<-'unchecked'
Gholz$Age<-'unchecked'


Gholz <- Gholz[,c(4,7,20:60)]

LegacyData <- rbind(LegacyData,Gholz)

#########
# Ruark #
#########
Ruark<-read.csv(paste(basepath,"Ruark_et_al_1987_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
Ruark$AUTHOR<-"Ruark"
Ruark$LOC <- 1
Ruark$SPCD <- 746
Ruark$TREENO <- 1:nrow(Ruark)
Ruark[Ruark$CROWN.CLASS == 1, "CCLCD"] <- 2 #Dominant
Ruark[Ruark$CROWN.CLASS == 2, "CCLCD"] <- 3 #Codominant
Ruark[Ruark$CROWN.CLASS == 3, "CCLCD"] <- 4 #Intermediate
Ruark$CW<-Ruark$CROWN.WIDTH * 3.28084 #I believe its in meters
Ruark$H_STUMP<-NA
Ruark$DBH<-Ruark$DBH.CM / 2.54      # dbh in cm
Ruark$HT<- Ruark$HEIGHT.M * 3.28084 # height in meters
Ruark$HL_BRANCH<- Ruark$HT.LIVE.CROWN * 3.28084
Ruark$DW_SW<- Ruark$BOLEWOOD.KG * 2.20462
Ruark$DW_SB<- Ruark$BOLEBARK.KG * 2.20462
Ruark$DW_STEM<- Ruark$DW_SB + Ruark$DW_SW
Ruark$DEAD_BRANCH_DW<- Ruark$DEAD.BRANCH.KG * 2.20462
Ruark$LIVE_BRANCH_DW<- (Ruark$TOTAL.TWIG.KG + Ruark$TOTAL.BRANCH.KG) * 2.20462
Ruark$DW_BRANCH<- Ruark$LIVE_BRANCH_DW + Ruark$DEAD_BRANCH_DW
Ruark$DW_TOT<- Ruark$DW_BRANCH + Ruark$DW_STEM
Ruark$DW_FOL<- Ruark$TOTALFOL.KG * 2.20462
Ruark$BIOMASS<- Ruark$DW_TOT + Ruark$DW_TOT
Ruark$STUMP<-NA
Ruark$ABG<-NA
Ruark$ORIGIN<-1
Ruark$TEMP<- 60
Ruark$DGL<-NA
Ruark$DSTEM_MIN<-NA
Ruark$DBLC<-NA
Ruark$DW_CROWN<- Ruark$DW_BRANCH + Ruark$DW_FOL
Ruark$ST_METH<- 3
Ruark$BR_METH<- 1
Ruark$FOL_METH<- 1
Ruark$REGION<- 'NORCEN'						# Changed 6/3/2014
Ruark$Indicator <- 'unchecked'
Ruark$GW_STEM<-'unchecked'
Ruark$GW_BRANCH<-'unchecked'
Ruark$GW_FOL<-'unchecked'
Ruark$GW_CROWN<-'unchecked'
Ruark$GW_BIOMASS<-'unchecked'
Ruark$GW_ABG<-'unchecked'
Ruark$GW_TOT<-'unchecked'
Ruark$M.C.<-'unchecked'
Ruark$SP.GR<-'unchecked'
Ruark$Bark.Fraction<-'unchecked'
Ruark$Age<-'unchecked'


Ruark <- Ruark[,c(36:78)]
head(Ruark)
LegacyData <- rbind(LegacyData,Ruark)
dim(LegacyData) # 8349


#############################
# Bridge 1979; Rhode Island #
#############################

Bridge_1979<-read.csv(paste(basepath,"Bridge_1979_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
unique(Bridge_1979$Species)
names(Bridge_1979)
Bridge_1979
Bridge_1979$AUTHOR<-"Bridge"
Bridge_1979$LOC <- Bridge_1979$LOCATION
Bridge_1979[Bridge_1979$Species == 'RM', "SPCD"] <- 316
Bridge_1979[Bridge_1979$Species == 'WO', "SPCD"] <- 802
Bridge_1979[Bridge_1979$Species == 'BO', "SPCD"] <- 837
Bridge_1979[Bridge_1979$Species == 'SO', "SPCD"] <- 806
Bridge_1979[Bridge_1979$Species == 'RO', "SPCD"] <- 833
Bridge_1979[Bridge_1979$Species == 'S', "SPCD"] <- 931
Bridge_1979[Bridge_1979$Species == 'T', "SPCD"] <- 693
Bridge_1979$TREENO <- 1:nrow(Bridge_1979)
Bridge_1979[Bridge_1979$CrownClass == 'D', "CCLCD"] <- 2 #Dominant
Bridge_1979[Bridge_1979$CrownClass == 'CD', "CCLCD"] <- 3 #Codominant
Bridge_1979[Bridge_1979$CrownClass == 'INT', "CCLCD"] <- 4 #Intermediate
Bridge_1979[Bridge_1979$CrownClass == 'OT', "CCLCD"] <- 5 #Overtopped/Suppresed
Bridge_1979$CW<-NA
Bridge_1979$H_STUMP<-NA											# height not given in thesis; 6" in Jenkins, but they did not have the actual thesis
Bridge_1979$DBH<-Bridge_1979$DBH_cm / 2.54      # dbh in cm
Bridge_1979$HT<- Bridge_1979$Height_m * 3.28084 # height in meters
Bridge_1979$HL_BRANCH<- NA
Bridge_1979$DW_SW<- NA
Bridge_1979$DW_SB<- NA
Bridge_1979$DW_STEM<- NA
Bridge_1979$DEAD_BRANCH_DW<- NA
Bridge_1979$LIVE_BRANCH_DW<- NA
Bridge_1979$DW_BRANCH<- NA
Bridge_1979$DW_TOT<- Bridge_1979$OvenDryWeight_kg*2.20462					# appendix not clear, but assume it is entire tree excluding twigs < 2.5 cm; "impractical for fuelwood"
Bridge_1979$DW_FOL<- NA
Bridge_1979$BIOMASS<- NA
Bridge_1979$STUMP<-NA
Bridge_1979$ABG<-NA
Bridge_1979$ORIGIN<-1
Bridge_1979$TEMP<- 60
Bridge_1979$DGL<-NA
Bridge_1979$DSTEM_MIN<-1
Bridge_1979$DBLC<-NA
Bridge_1979$DW_CROWN<- NA
Bridge_1979$ST_METH<- 1
Bridge_1979$BR_METH<- 5
Bridge_1979$FOL_METH<- NA
Bridge_1979$REGION<- 'NE'
Bridge_1979$Indicator <- '1'						# Indicator 1 = branches less than 1" not included
Bridge_1979$GW_STEM<-NA
Bridge_1979$GW_BRANCH<-NA
Bridge_1979$GW_FOL<-NA
Bridge_1979$GW_CROWN<-NA
Bridge_1979$GW_BIOMASS<-NA
Bridge_1979$GW_ABG<-'unchecked'
Bridge_1979$GW_TOT<-Bridge_1979$FreshWeight_kg*2.20462
Bridge_1979$M.C.<-Bridge_1979$MC					# whole-tree moisture content
Bridge_1979$SP.GR<-Bridge_1979$SpecificGravity_g_cc
Bridge_1979$Bark.Fraction<-'unchecked'
Bridge_1979$Age<-Bridge_1979$Age_yrs

dim(Bridge_1979)
dim(LegacyData)
Bridge_1979 <- Bridge_1979[,c(13:55)]



LegacyData <- rbind(LegacyData,Bridge_1979)

#############################
# Young & Chase 1965 Maine  # Green Weights only
#############################

Young_Chase_1965<-read.csv(paste(basepath,"Young_Chase_1965_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
names(Young_Chase_1965)
Young_Chase_1965
Young_Chase_1965$AUTHOR<-"Young_Chase"
Young_Chase_1965$LOC <- 1
Young_Chase_1965[Young_Chase_1965$SPP == 'RM', "SPCD"] <- 316
Young_Chase_1965[Young_Chase_1965$SPP == 'BF', "SPCD"] <- 12
Young_Chase_1965$TREENO <- 1:nrow(Young_Chase_1965)
Young_Chase_1965$CCLCD <- NA
Young_Chase_1965$CW<-NA
Young_Chase_1965$H_STUMP<-NA											# height not given in thesis; 6" in Jenkins, but they did not have the actual thesis
Young_Chase_1965$DBH<-Young_Chase_1965$DBH_in
Young_Chase_1965$HT<- NA
Young_Chase_1965$HL_BRANCH<- NA
Young_Chase_1965$DW_SW<- NA
Young_Chase_1965$DW_SB<- NA
Young_Chase_1965$DW_STEM<- 
Young_Chase_1965$DEAD_BRANCH_DW<- NA
Young_Chase_1965$LIVE_BRANCH_DW<- NA
Young_Chase_1965$DW_BRANCH<- NA 
Young_Chase_1965$DW_TOT<- NA					
Young_Chase_1965$DW_FOL<- NA
Young_Chase_1965$BIOMASS<- NA
Young_Chase_1965$STUMP<-NA
Young_Chase_1965$ABG<-NA
Young_Chase_1965$ORIGIN<-1
Young_Chase_1965$TEMP<- 60
Young_Chase_1965$DGL<-NA
Young_Chase_1965$DSTEM_MIN<-4
Young_Chase_1965$DBLC<-NA
Young_Chase_1965$DW_CROWN<- NA
Young_Chase_1965$ST_METH<- 'unchecked'
Young_Chase_1965$BR_METH<- 'unchecked'
Young_Chase_1965$FOL_METH<- 'unchecked'
Young_Chase_1965$REGION<- 'NE'
Young_Chase_1965$Indicator <- NA				# Indicator 1 = branches less than 1" not included 
Young_Chase_1965$GW_STEM<-Young_Chase_1965$merch_bole_lbs + Young_Chase_1965$unmerch_top_lbs
Young_Chase_1965$GW_BRANCH<-NA
Young_Chase_1965$GW_FOL<-NA
Young_Chase_1965$GW_CROWN<-Young_Chase_1965$Br_sm_lbs + Young_Chase_1965$Br_lg_lbs
Young_Chase_1965$GW_BIOMASS<- Young_Chase_1965$GW_CROWN+Young_Chase_1965$GW_STEM

Young_Chase_1965$tot_tree_gw_lbs-Young_Chase_1965$GW_BIOMASS- # total tree does not always equal the sum of parts assuming addition error?
					   Young_Chase_1965$Stump_lbs-
					   Young_Chase_1965$Roots_sm_lbs-
					   Young_Chase_1965$Roots_1_4_lbs-
					   Young_Chase_1965$Roots_4_up_lbs

Young_Chase_1965$GW_ABG<- NA
Young_Chase_1965$GW_TOT<-NA
Young_Chase_1965$M.C.<-NA					# averaged by species
Young_Chase_1965$SP.GR<-NA					# averaged given by species
Young_Chase_1965$Bark.Fraction<-NA				# averaged by species
Young_Chase_1965$Age<-NA
names(Young_Chase_1965)
names(LegacyData)
Young_Chase_1965 <- Young_Chase_1965[,c(15:57)]

LegacyData <- rbind(LegacyData,Young_Chase_1965)
dim(LegacyData) # 8485

#############################
# Young 1981         Maine  # 
#############################

Young_1981<-read.csv(paste(basepath,"Young_1981_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
names(Young_1981)
Young_1981
Young_1981$AUTHOR<-"Young"
Young_1981$LOC <- 1
Young_1981[Young_1981$SPP == 'RS', "SPCD"] <- 97
Young_1981[Young_1981$SPP == 'WS', "SPCD"] <- 94
Young_1981[Young_1981$SPP == 'BF', "SPCD"] <- 12
Young_1981[Young_1981$SPP == 'EH', "SPCD"] <- 261
Young_1981$TREENO <- 1:nrow(Young_1981)
Young_1981$CCLCD <- NA
Young_1981$CW<-NA
Young_1981$H_STUMP<-NA											# height not given in thesis; 6" in Jenkins, but they did not have the actual thesis
Young_1981$DBH<-Young_1981$DBH_in
Young_1981$HT<- NA
Young_1981$HL_BRANCH<- NA
Young_1981$DW_SW<- NA
Young_1981$DW_SB<- NA
Young_1981$DW_STEM<- 
Young_1981$DEAD_BRANCH_DW<- NA
Young_1981$LIVE_BRANCH_DW<- NA
Young_1981$DW_BRANCH<- NA 
Young_1981$DW_TOT<- NA					
Young_1981$DW_FOL<- Young_1981$Tot_Fol_g/1000*2.20462
Young_1981$BIOMASS<- NA
Young_1981$STUMP<-NA
Young_1981$ABG<-NA
Young_1981$ORIGIN<-1
Young_1981$TEMP<- 60
Young_1981$DGL<-NA
Young_1981$DSTEM_MIN<-4
Young_1981$DBLC<-NA
Young_1981$DW_CROWN<- NA
Young_1981$ST_METH<- 'unchecked'
Young_1981$BR_METH<- 'unchecked'
Young_1981$FOL_METH<- 'unchecked'
Young_1981$REGION<- 'NE'

Young_1981$Indicator <- NA				# Indicator 1 = branches less than 1" not included 
Young_1981$GW_STEM<-NA
Young_1981$GW_BRANCH<-NA
Young_1981$GW_FOL<-NA
Young_1981$GW_CROWN<-NA
Young_1981$GW_BIOMASS<- NA
Young_1981$GW_ABG<- NA
Young_1981$GW_TOT<-NA
Young_1981$M.C.<-NA					# averaged by species
Young_1981$SP.GR<-NA					# averaged given by species
Young_1981$Bark.Fraction<-NA				# averaged by species
Young_1981$Age<-NA
Young_1981 <- Young_1981[,c(7:49)]

LegacyData <- rbind(LegacyData,Young_1981)
dim(LegacyData) # 8571

#write.csv(Gower.sum,"C:\\FIA\\LegacyDataBase\\Gowersum.csv")

#############################
# Lykins 1995     OK        # 
#############################

Lykins_1995<-read.csv(paste(basepath,"Lykins_1995_Data.csv",sep=""),header=T,as.is=T,na.strings="NA")
names(Lykins_1995)

Lykins_1995$AUTHOR<-"Lykins"
Lykins_1995$LOC <- 1				        # no location field in data, JF 3/3/2014 although sampled at 4 sites
Lykins_1995$SPCD <- 68
Lykins_1995$TREENO <- Lykins_1995$tree.no
Lykins_1995$CCLCD <- NA
Lykins_1995$CW<-Lykins_1995$crown.dia.m*3.28084
Lykins_1995$H_STUMP<-0										# ground level, p. 9
Lykins_1995$DBH<-Lykins_1995$dbh.cm/2.54
Lykins_1995$HT<- Lykins_1995$tot.h.m*3.28084
Lykins_1995$HL_BRANCH<- Lykins_1995$HT-Lykins_1995$crown.len.m*3.28084
Lykins_1995$DW_SW<- (Lykins_1995$heartwood.dw.kg+Lykins_1995$sapwood.dw.kg)*2.20462
summary(Lykins_1995)
Lykins_1995$DW_SB<- Lykins_1995$bark.dw.kg*2.20462
Lykins_1995$DW_STEM<- Lykins_1995$bolewood.dw.kg*2.20462
Lykins_1995$DEAD_BRANCH_DW<- Lykins_1995$dead.br.dw.kg*2.20462
Lykins_1995$LIVE_BRANCH_DW<- NA					# twigs considered 1/4 inch consolidated with leaves
Lykins_1995$DW_BRANCH<- NA 
Lykins_1995$DW_TOT<- NA					
Lykins_1995$DW_FOL<- NA
Lykins_1995$BIOMASS<- (Lykins_1995$tree.dw.kg)*2.20462
Lykins_1995$STUMP<-NA
Lykins_1995$ABG<- (Lykins_1995$tree.dw.kg)*2.20462
Lykins_1995$ORIGIN<-NA
Lykins_1995$TEMP<- 67
Lykins_1995$DGL<-NA
Lykins_1995$DSTEM_MIN<-NA
Lykins_1995$DBLC<-NA
Lykins_1995$DW_CROWN<- Lykins_1995$crown.dw.kg*2.20462
Lykins_1995$ST_METH<- '1'
Lykins_1995$BR_METH<- NA
Lykins_1995$FOL_METH<- NA
Lykins_1995$REGION<- 'SE'

Lykins_1995$Indicator <- NA				# Leaves and twigs < 1/4" lumped together 
Lykins_1995$GW_STEM<-Lykins_1995$bolewood.gw.kg*2.20462
Lykins_1995$GW_BRANCH<-NA
Lykins_1995$GW_FOL<-NA
Lykins_1995$GW_CROWN<-Lykins_1995$crown.gw.kg*2.20462
Lykins_1995$GW_BIOMASS<- Lykins_1995$tree.gw.kg*2.20462
Lykins_1995$GW_ABG<- Lykins_1995$tree.gw.kg*2.20462
Lykins_1995$GW_TOT<-'unchecked'
Lykins_1995$M.C.<-'unchecked'			
Lykins_1995$SP.GR<-'unchecked'			
Lykins_1995$Bark.Fraction<-'unchecked'			
Lykins_1995$Age<-Lykins_1995$age
names(Lykins_1995)
Lykins_1995 <- Lykins_1995[,c(31:73)]

LegacyData <- rbind(LegacyData,Lykins_1995)
dim(LegacyData) # 8585

#write.csv(Gower.sum,"C:\\FIA\\LegacyDataBase\\Gowersum.csv")
todate <- paste(substr(date(),5,7), 
                ifelse(substr(substr(date(),9,10),1,1) == " ", substr(substr(date(),9,10),2,2), substr(date(),9,10)), 
                substr(date(),21,24), sep= "_")
paste("Legacy_Data_",todate, ".csv", sep= "")

write.csv(LegacyData,paste(basepath, "Legacy_Data_1.csv", sep= ""), row.names= F)

