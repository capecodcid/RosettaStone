###
# USER NEEDS TO SET VALUES IN LINES 
###
# LOAD LIBRARIES
library(ggplot2)
library(dplyr)

# LOAD CSV FILE and Define classes
spreadsheet = read.csv('/Users/andrewhartnett/Documents/git/RosettaStone/UsageReport-9th Grade.csv')
testClass = c("AdebayoF172","adelekeo001","adotevim150","aguilarb103","ajuhanm002","alvesa140","wardj097","waringi117","ZabB174","zuluagas102")

caughtUp = 200;
strikingDist = 150;


#
# Definitions of Functions and Styles
#
colorPalette = c('#3FDB35','#FFED1B','#FF1605')

getStatus <- function(progress, greenLine, yellowLine) {
  if (progress >= greenLine) {
    status <- "okay"
  } else if (progress >= yellowLine) {
    status <- "warning"
  } else {
    status <- "danger"
  }
  return(status)
}




# REMOVE FIRST ROW AND ALL UNNEEDED COLUMNS
# keeping username, language, % completion, hours
# set up correct names and make sure all columns are the correct class
dat = spreadsheet[2:nrow(spreadsheet),c(3,6,10,12)]
dat<-rename(dat,user=X.1)
dat<-rename(dat,lang=X.4)
dat<-rename(dat,percent=X.8)
dat$percent <- as.numeric(as.character(dat$percent))
dat<-rename(dat,hours=X.10)
dat$hours<-as.numeric(as.character(dat$hours))
rownames(dat)<-1:nrow(dat)


# Group By user and calculate total progress and time spent
byUser <- group_by(dat, user)
userTotals<-summarise(byUser,totalProgress=sum(percent), totalTime=sum(hours)) %>% rowwise() %>% mutate(status = getStatus(totalProgress,caughtUp,strikingDist))
userTotals$status=factor(userTotals$status, levels=c("okay","warning","danger"))

# seperate classes
tC1 <- userTotals %>% 
  filter(user %in% testClass) %>%
  arrange(totalProgress)


# plot
c <- ggplot(tC1, aes(x=reorder(user,totalProgress), y=totalProgress, fill=status))
c + geom_bar(stat="identity") + coord_flip() + scale_fill_manual(values=colorPalette) + ylab("Total Progress to Date") + xlab("Username")




