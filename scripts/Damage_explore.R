####################### Damage data ###########################
library(tidyverse)
ev1d <- read_csv("data/EV1 CHRONOSEQUENCE -HURRICANE ASSESSMENT.csv")
sb1d <- read_csv("data/SB1 CHRONOSEQUENCE-HURRICANE ASSESSMENT.csv")
sb3d <- read_csv("data/SB3 CHRONOSEQUENCE-HURRICANE ASSESSMENT.csv")

#HOW TO CONTROL FOR PLOT SIZE DIFFS ????? %s

# WHICH SIZED STEMS WERE SURVEYED? DIFF FOR EV1 ????

stms_ev1d <- length(ev1d$Ind.) #1751 individuals
stms_sb1d <- length(sb1d$Ind.) #645 individuals
stms_sb3d <- length(sb3d$Ind.) #600 individuals
#compare with # individuals with DBH > 10

stms10_ev1d <- length(ev1d$DBH[ev1d$DBH>=10]) #1218 individuals
stms10_sb1d <- length(sb1d$DBH[sb1d$DBH>=10]) #643 individuals
stms10_sb3d <- length(sb3d$DBH[sb3d$DBH>=10]) #301 individuals

stms5_ev1d <- length(ev1d$DBH[ev1d$DBH>=5]) #1751 individuals
stms5_sb1d <- length(sb1d$DBH[sb1d$DBH>=5]) #323 individuals ????
stms5_sb3d <- length(sb3d$DBH[sb3d$DBH>=5]) #600 individuals

#compared. next see how many were actually msd?
stms_ev1d #1751 stems in datasheet, 1218 > 10com (but all >5)
stms_sb1d #645 stems in datasheet, 643 >10cm (whats up with those 2?)
stms_sb3d #600 stems in datasheet, 301 >10cm 
#compare with # individuals with DBH > 10



ev1d_10 <- ev1d$DBH[ev1d$DBH>=10]
length(ev1d_10) #1218
ev1d_5 <- ev1d$DBH[ev1d$DBH>=5]
length(ev1d_5) #1751    looks like stems over 5cm were msd ev1 ??

sb1d_10 <- sb1d$DBH[sb1d$DBH>=10]
length(sb1d_10) #643
sb1d_5 <- sb1d$DBH[sb1d$DBH>=5]
length(sb1d_5) #323   check if these have msmts???

sb3d_10 <- sb3d$DBH[sb3d$DBH>=10]
length(sb3d_10) #301
sb3d_5 <- sb3d$DBH[sb3d$DBH>=5]
length(sb3d_5) #600

#all ev1 trees over 10cm*
sp_ev1d <- data.frame(unique(ev1d$sp))
SR_ev1d <- length(sp_ev1d[,1]) #53 species
stms_ev1d <- length(ev1d$Ind.) #1751 individuals
range(ev1d$DBH) #5- 105 cm DBH

#sb1 and sb2 trees over 10cm*
stms_sb1d <- length(ev1d$Ind.)
#*or is this counting all trees in general?
stms_ev1d <- length(ev1d$Ind.) #1751 individuals



#most abundant trees over 10cm
abund<- ev1d %>% group_by(sp) %>% summarize(n()) #how to arrange?
abund_10 <- abund %>% top_n(10)
colnames(abund_10) <- c("ev1d_spp","count")
abund_10 <- abund_10 %>% arrange(desc(count))

abund <- abund[order(abund$`n()`),] #rutuja

View(abund)#how many broken or uprooted stems
ev1d %>% select(sp,B...U)
B.U <- ev1d %>% group_by(sp,B...U) %>% summarize(n())

dmg <- ev1d %>% group_by(B...U) %>% summarize(n())
colnames(dmg) <- c("B/U","count")

no_dmg <- sum(dmg$count[1:2]) #1300 stems not broken/uprooted     (how to collapse rows??)
no_dmg/stms_ev1d # or 74.2%
yes_dmg <- sum(dmg$count[3:5]) #451 stems broken or uprooted
yes_dmg/stms_ev1d # or 25.8% 

dmg_noNA <-dmg[3:5,]
BU <- dmg_noNA
BU$prop <- BU[,2]/stms_ev1d
BU$prop <- round((BU$prop*100),3)
colnames(BU)= c("B/U","count","%")  # how to rename just one column? and remove .count?


#### SB1

#sb1 trees >10 cm
sp_sb1d <- data.frame(unique(sb1d$sp))
SR_sb1d <- length(sp_sb1d[,1]) #36 species
stms_sb1d <- length(sb1d$Ind.) #645 individuals
range(sb1d$DBH) #doesnt work??

#most abundant sp 
abund2<- sb1d %>% group_by(sp) %>% summarize(n()) #how to arrange?
abund2_10 <- abund2 %>% top_n(10) 
colnames(abund2_10) <- c("sb1d_spp","count")
abund2_10 <- abund2_10 %>% arrange(desc(count))

#how many broken or uprooted stems

dmg2 <- sb1d %>% group_by(B...U) %>% summarize(n())
colnames(dmg2) <- c("B/U","count")
dmg2[1,1]="none" #how to change value to None? says can only be NA...
str(dmg2)
factor()
library(plyr)
dmg$`B/U` <- mapvalues(dmg$`B/U`, from = c("", " ", "B", "B/U", "U"), to = c("none", "none", "B", "B/U", "U"))
str(dmg)

#how many broken/uprooted? 
sum(dmg2$count[2:4]) # 205 stems
sum(dmg2$count[2:4])/stms_sb1d *100  #31.8%
#how many broken? 
dmg2$count==  .......??###### stopped here

View(dmg)

#### SB3

#sb3 trees >10 cm
sp_sb3d <- data.frame(unique(sb3d$sp))
SR_sb3d <- length(sp_sb3d[,1]) #44  species
stms_sb3d <- length(sb3d$Ind.) #600 individuals
range(sb3d$DBH) #5 - 99 DBH

#most abundant sp 
abund3<- sb3d %>% group_by(sp) %>% summarize(n()) #how to arrange?
abund3_10 <- abund3 %>% top_n(10) 
colnames(abund3_10) <- c("sb3d_spp","count")
abund3_10 <- abund3_10 %>% arrange(desc(count))

#how many broken or uprooted stems

dmg3 <- sb1d %>% group_by(B...U) %>% summarize(n())
colnames(dmg3) <- c("B/U","count")
dmg3[1,1]="none" #how to change value to None? says can only be NA...

#how many broken/uprooted? 
sum(dmg3$count[2:4]) # 205 stems
sum(dmg3$count[2:4])/stms_sb1d *100  #31.8%



#how many broken? 
dmg2$count==  .......??###### stopped here








no_dmg <- sum(dmg$count[1:2]) #1300 stems not broken/uprooted     (how to collapse rows??)
no_dmg/stms_ev1d # or 74.2%
yes_dmg <- sum(dmg$count[3:5]) #451 stems broken or uprooted
yes_dmg/stms_ev1d # or 25.8% 


# size classes ev1
sm_stms <- ev1d %>% 
  select(sp,DBH) %>% 
  filter(DBH<10)
sm_stms
length(sm_stms$sp) #533 stems 5-10

md_stms <- ev1d %>% 
  select(sp,DBH) %>% 
  filter(DBH>=10 & DBH <20)
md_stms
length(md_stms$sp) #969 stems 10-20

lrg_stms <- ev1d %>% 
  select(sp,DBH) %>% 
  filter(DBH>=20)
lrg_stms
length(lrg_stms$sp) #249 stems >=20

largest=top_n(lrg_stms,10)
largest_sp <- ev1d %>% unique(sp) %>% group_by(sp,DBH) %>% top_n(10) ###no

length(ev1d$DBH[ev1d$DBH<10])
length(ev1d$DBH[ev1d$DBH>=20])

