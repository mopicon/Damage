####################### Comparing Damage & Mortality ###########################
library(tidyverse)

#import data from "data" folder in R project (hurricane_damage)
ev1d <- read_csv("data/EV1 CHRONOSEQUENCE -HURRICANE ASSESSMENT.csv")
sb1d <- read_csv("data/SB1 CHRONOSEQUENCE-HURRICANE ASSESSMENT.csv")
sb3d <- read_csv("data/SB3 CHRONOSEQUENCE-HURRICANE ASSESSMENT.csv")

#combine into a single data set with relevant col's; add new column for site

ev1d <- ev1d %>% 
  select(Tag, sp, DBH, `A / D  / NF`, `B / U`,`(B) Height of break`,`Crown status %`,`Comments (whenever possible measure width and height of the root bole)`) %>% 
  add_column(Site = "EV1") 
sb1d <- sb1d %>%
  select(Tag, sp, DBH,`A / D  / NF`, `B / U`,`(B) Height of break`, `Crown status %`,`Comments (whenever possible measure width and height of the root bole)`) %>% 
  add_column(Site = "SB1")
sb3d <- sb3d %>% 
  select(Tag,sp,DBH,`A / D  / NF`,`B / U`,`(B) Height of break`,`Crown status %`,`Comments (whenever possible measure width and height of the root bole)`) %>% 
  add_column(Site = "SB3")
damage <- rbind(ev1d,sb1d,sb3d)

#### should have used unique ID instead of tags, since tags are repeated

#rename columns
damage <- damage %>% rename(tag=Tag, dbh=DBH, site=Site, status=`A / D  / NF`,b_u=`B / U`, break_h=`(B) Height of break`, crown_status=`Crown status %`, comments = `Comments (whenever possible measure width and height of the root bole)`)
comment <- damage %>% select(comments) %>% unique() # what is LSP?


#change to numeric. check NA's before and after changing to numeric
dbh.original <- damage$dbh
damage$dbh <- as.numeric(damage$dbh)
dbh.original[is.na(damage$dbh)]

break_h.original <- damage$break_h
damage$break_h <- as.numeric(damage$break_h)
x <- break_h.original[is.na(damage$break_h)]
x[!is.na(x)] # no NA's introduced by transforming to numeric

crown_status.original <- damage$crown_status
damage$crown_status <- as.numeric(damage$crown_status)
x<- crown_status.original[is.na(damage$crown_status)]
x[!is.na(x)]
table(x) #NAs(nonnumbers;text) converted to NAs. OK.

#check types
str(damage)

# be aware: broken is 2 categories (B and B/U); uprooted is 2 categories (U and B/U)
table(damage$b_u)


####### check for data entry errors

#check for duplicate tags
#?duplicated; x[duplicated(x)]; x[!duplicated(x)]
damage %>% filter(duplicated(tag))
damage[duplicated(damage$tag)]
length(damage[duplicated(damage$tag)])
length(damage[!duplicated(damage$tag)])
#didnt work
t <- table(damage$tag, damage$site)
which(t > 1, arr.ind = TRUE)

#check for crown status >100%
damage %>% filter(crown_status>100)  #none
#note: not collected for palms

#check for missing info for status
no_status=damage %>% filter(status!="A" & status!="D" & status!="NF")  #one entry, Premon in EV1
no_status
#remove this row
damage1 <- damage %>% filter(tag!=4474)
length(damage$status); length(damage1$status)

#any more missing?
unique(damage1$status)
damage %>% filter(is.na(status)) #still many NAs but some have dbh < 10. check again after filtering those.


####  select only stems with >10cm dbh

#first examine dbh
summary(damage1$dbh)
hist(damage1$dbh) #many stems <10
damage1 %>% filter(is.na(dbh)) #see 2 incomplete rows in SB1

# remove rows with missing values 
length(damage1$dbh)
damage2 <- damage1 %>% filter(!is.na(dbh)) 
length(damage2$dbh)

#select stems > 10
damage10c <-  damage2 %>% filter(dbh>=10)    
summary(damage10c$dbh)
hist(damage10c$dbh)


############## COMPARE STATUS

status_tbl<- damage10c %>% 
  group_by(site,status) %>% 
  count() %>% 
  spread(key=site,value=n)

#still have NA's
unique(damage$status)
damage10c %>% filter(is.na(status)) # 7 entries with no status. remove these.
damage10 <- damage10c %>% filter(!is.na(status)) 
length(damage10c$status); length(damage10$status)


#******** use damage10 as the new df *******
table(damage10$status)
table(damage10$b_u)
length(damage10$status) #1,842 stems surveyed (assuming we dont delete NFs)


#what to do with the 24 NF's? could remove or could call them dead. see comments, site, size
NF <- damage10 %>% filter(status=="NF")
 
#change to dead
length(damage10$status[damage10$status=="D"]) #255 dead

damage10=damage10 %>% 
  mutate(status=replace(status,status=="NF","D")) #%>% as.data.frame()  ..why?

length(damage10$status[damage10$status=="D"]) #279 dead

#redo status table
status_tbl2 <- damage10 %>% 
  group_by(site,status) %>% 
  count()%>% 
  spread(key=site,value=n)
#should find how to convert status to rownames instead of a column

x <- as.matrix(status_tbl2)
rownames(x) <- x[, "status"]
x <- x[,c("EV1", "SB1", "SB3")]
class(x) <- "numeric"
x
status_tbl2 <- x # now its is numeric, with status as rownames


#% mort = proportion of total stems surveyed at each site
total=colSums(status_tbl2) 
total=rbind(total,total) #must divide by a matrix with same dims as status tbl
prop_mort=round(status_tbl2/total,2)*100

#also need raw mort, for later barplot?
mort <- status_tbl2

#mortality figure (dont need this - incorporate into damage below)
?barplot
barplot(prop_mort[2,])



############## COMPARE BROKEN & UPROOTED
str(damage10)
summary(damage10$b_u)
unique(damage10$b_u)

#examine NA's
bu_dmg_NA <- damage10 %>% filter(is.na(b_u)) #filter out trees alive and well for a sec
bu_dmg_NA <- damage10 %>% filter(is.na(b_u),status=="D") 
#94 trees dead but not B or U. minus 24 NF = 70 trees called dead but not B or U. a few have partial dmg.
#how does that compare to background mortality rate?? how likely are they hurricane morts? dep on size?

#remove all NA's (right?)
damage10_BU <- damage10 %>% filter(!is.na(b_u)) #641 stems B, U, or both

#examine b_u by site
bysite_bu <- damage10_BU %>% 
  group_by(site,b_u) %>% 
  count() %>% 
  spread(key=site,value=n)


##### column to row .....
?tibble: column_to_rownames()
column_to_rownames(as.data.frame(damage10_BU,var="b_u"))

#can make a function for this
# useful fucntions
colSums
map_if # then say if numeric
mutate_if # if numeric 
?map
map_if(mtcars,is.numeric,sum) #sum only the numeric cols
map(select(df,-col),sum)

#what i actually used
bysite_bu2 <- as.data.frame(bysite_bu)
rownames(bysite_bu2) <- bysite_bu2$b_u
bysite_bu2$b_u <- NULL
###################################



####### examine b_u by site AND STATUS

table_bu_AD <- damage10_bu %>% 
  group_by(site,status,b_u) %>% 
  count() %>% 
  spread(key=site,value=n)
## need to convert to proportions!!!
total <- colSums(table_bu_AD[,3:5])
total <- rbind(total,total,total,total,total,total)
table_bu_AD[,3:5]/total
  
  
### plot mortality and damage by site 

#mortality only
# 1) all stems >10
ggplot(damage10, aes(x = site)) +
  geom_bar(aes(fill = status), position = "dodge")

# 2) only stems >10 that were broken or uprooted
ggplot(damage10_BU, aes(x = site)) +
  geom_bar(aes(fill = status), position = "dodge")

#damage only
ggplot(damage10_BU, aes(site, n, fill = b_u)) + 
  geom_bar(stat="identity", position="dodge")




df=data.frame(
  year=rep(c("2010","2011"),each=4),
  treatment=rep(c("Impact","Control")),
  type=rep(c("Phylum1","Phylum2"),each=2),
  total=sample(1:100,8)) %>% 
  mutate(x_label = factor(str_replace(interaction(year, treatment), '\\.', ' / '), ordered=TRUE))







#***********************************************************************************
#shouldn't have removed NA's, because then I don't represent live trees not broken or uprooted
#in any graphs



#damage BU with 4 categories:

damage10_tbl <- damage10 %>% group_by(site,status,b_u) %>% count()
#change NA to "neither" or leave as is (?)
#change to just two categories, B and U ? (will sum to more than the # of trees B or U, 615)


#mortality only (this one was OK)    - but convert to proportions (speciefy y...propotions...??)
# all stems >10
ggplot(damage10, aes(x = site)) +
  geom_bar(aes(fill = status), position = "dodge")


#mortality and damage


ggplot(damage10,aes(x = site)) +
  geom_bar(aes(fill = b_u),stat="identity",position="dodge")


ggplot(damage10, aes(site, n, fill = b_u)) + 
  geom_bar(stat="identity", position="dodge")







######################



#how many broken vs uprooted?
broken <- dmg_bu %>% filter(b_u =="B" | b_u=="B/U")
uprooted <- dmg_bu %>% filter(b_u=="U" | b_u=="B/U")




# MISSING DATA (BLANK)
#29/1751*100 = 1.7% EV1
#270/645*100 = 41.9% SB1
#291/600*100 = 48.5% SB3

#Data entry errors
damage %>% filter(status=="P") # SB3: 1387 MicMir. Data entry error;changed.
damage %>% filter(status=="N/A") #EV1: 4474 PreMon. Valid NA; left alone


# overall status 
status <- damage %>% count(status)   #what are NAs and P???
# number of trees at each site      
trees<-damage %>% count(site)













###################### MAURO

library(tidyverse)

#as character vectors
setwd("C:/Users/Monique/Desktop/Documents/UPR GRADSKI/A SPRING SEM/Chronosequence/DATA_RAW/Damage data")
ev1d <- read_csv("EV1 CHRONOSEQUENCE -HURRICANE ASSESSMENT.csv")
sb1d <- read_csv("SB1 CHRONOSEQUENCE-HURRICANE ASSESSMENT.csv")
sb3d <- read_csv("SB3 CHRONOSEQUENCE-HURRICANE ASSESSMENT.csv")

#reduce function to bind 3 datasets into one
.data <- reduce(list(ev1d, sb1d, sb3d), rbind)

#rename columns before anything else
.data <-  .data %>% 
  dplyr::rename(
    status = `A / D  / NF`,
    b_u = `B / U`,
    crown_dmg = `Crown status %`
  ) %>% 
  select(Tag, sp, DBH, status, b_u, crown_dmg)

#percent mortality?
mort <- .data %>% 
  select(status) %>% 
  count()


#   # select(site,status) %>% 
#   group_by(status) %>% 
#   summarize(n = n())
# mort

count(.data, status)

#alive and dead stems
alive_stems <- damage10 %>% filter(status=="A")
dead_stems <- damage10 %>% filter(status=="D")
# not needed.. want to sep by site not A/D yet


## at very beining after improting data
#(added later: check NAs before deleting comment column)
na_ev1d <- ev1d %>% filter(is.na(`A / D  / NF`)) %>% select(Tag, sp, DBH, `A / D  / NF`, `B / U`,`Crown status %`,`Comments (whenever possible measure width and height of the root bole)`)
length(na_ev1d$Tag) #4 
na_sb1d <- sb1d %>% filter(is.na(`A / D  / NF`)) %>% select(Tag, sp, DBH, `A / D  / NF`, `B / U`,`Crown status %`,`Comments (whenever possible measure width and height of the root bole)`)
length(na_sb1d$Tag) #262 
na_sb3d <- sb3d %>% filter(is.na(`A / D  / NF`)) %>% select(Tag, sp, DBH, `A / D  / NF`, `B / U`,`Crown status %`,`Comments (whenever possible measure width and height of the root bole)`)
length(na_sb3d$Tag) #288
#rm(na_ev1d,na_sb1d,na_sb3d)



#for later
damage %>% filter(status=="A") #2,088 alive
damage %>% filter(status=="D") #318 dead
damage %>% filter(status=="NF") #35 NF
#add up to # in status column?

#combine site and status for ggplot
Rutuja$site.status <- paste(Rutuja$site, Rutuja$status, sep = ".")
ggplot(Rutuja, aes(x=site.status, y= n, fill= b_u)) +
  geom_bar(stat='identity') + labs(x='Site / Status')
