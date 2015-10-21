###
# USER NEEDS TO SET VALUES IN LINES 
###
# LOAD LIBRARIES
library(ggplot2)
library(dplyr)
library(data.table)

# LOAD CSV FILE and Define classes
spreadsheet1  = read.csv('/Users/andrewhartnett/Downloads/UsageReport-Class of 2022.csv')
spreadsheet2  = read.csv('/Users/andrewhartnett/Downloads/UsageReport-Class of 2023.csv')
currentWeek = 7;


grade9Goal = 250;
grade10Goal = 500;
weeksPerYear = 38;



epstein9 = c("cadenahernandezm046","brisbonl017","mageea058","dekreonz027","padillay066","laboye093","sotoo077","wojcika134","sernad086","johnsonk053","gomesc043","murphyz086")
epstein10 = c("ajuhanm002","cardenask152","chavezs134","dasilvac180","figueroae035","ortiz-muriels142","lombak156","riverae083","maybink133","hindsc050")

sinha9 = c("cheayee145", "cruzd123", "cruzy099", "echavarriav030", "geogheganc039", "gravierv133", "nunezn065", "peckhama143", "ramosa131", "silvierae076", "vazdopios192")
sinha10 = c("adebayof172", "bayronriveral012", "granilloc155", "leporea054", "marroquinl060", "moralesp168", "rogersr171", "sosar166")

olivo9 = c("antiguaa087", "batistal151", "dufaultj029", "dwyern083", "godoyy098", "mouratob071", "silva-milersona088", "trejor144", "vieirah096", "wilsonm100")
olivo10 = c("arboledaj004", "ariasl007", "bravog078", "cassianik020", "encarnaciona179", "feldb033", "gallegosgomezm139", "lopesn057", "ricaurteg115", "tebaldih170", "trejoa092", "vasquezj175")

delanos9 = c("gomezn044", "chavezk023", "moored137", "ayalab008", "riganom071", "kinnesl057", "jadachp047", "girardt043","lemuso118", "restrepoa070")
delanos10 = c("colonias109", "zabb174", "adotevim150", "zuluagas102", "velezm124", "adelekeo001", "aguilarb103", "gomese173", "tavaresa165","youngs100","avilesb009")

caraza9 = c("agudeloj001","araujog006","armans008", "beaudoinh011","doughtyl027","geraldesj040","santoss073","silvam075","thorntons114","beaubrunk010")
caraza10 = c("seebeckm128","armans008","marrerod059","marshs061","nyantig130","pinedae078","rodriguezv084","stagerj089","wardj097","waringi117")

rupani9 = c("alvarados004",  "contrerasc019", "curryd021",  "garciaa094", "lucarioj122", "marksa109",  "quianesj068", "ramosf081", "villac085", "godineze091")
rupani10 = c("alvesa140", "corream026", "pereirak135", "ramirezb143", "riganoe137", "trinidads176", "varoner094", "torresa091")

allcock9 = c("belloj136" , "daveigae023", "davisd140" , "dejesuspereza025" , "fontalvoc038" , "gamboad" , "hernandeza146" , "jovinc132" , "maganad116" , "morenoj063" , "rojasj113", "vicented120" )
allcock10 = c("ardoncantef005", "barross011", "deighanh154" , "diaze031" , "lewise055" , "mingoesc070" , "pinak160" , "tavaresn163")

gibson9 = c("delgadot026","feleyt031","gibbonss041","godineza042","alvarezj084","guzmand095","morine110","wojcikj138","lamoureuxe145")
gibson10 = c("cardonaa019","choquettet024","munera-goezm072","flanagank037","lincolns056","mckeec065","mensahb069","muneragoezm072","rourket085","valderramal093","malikt112","garcial126")

payette9 = c("dasilvaj022","donisd028","fuentesd036","hernandezc102","khanz126","mejiaa061","millettes088","onifadef142","urzuac082","phillipsd177","veigat119")
payette10 = c("castilloj021","cortesm027","costad129","fernandess136","henrriquezw049","mckeej066","melloj113","sweeneyj090","vasquezj116", "vicentem096", "wieganda099")

classes9 = list(epstein=epstein9,sinha=sinha9,olivo=olivo9,delanos=delanos9,caraza=caraza9,rupani=rupani9,allcock=allcock9,gibson=gibson9,payette=payette9)
classes10 = list(epstein=epstein10,sinha=sinha10,olivo=olivo10,delanos=delanos10,caraza=caraza10,rupani=rupani10,allcock=allcock10,gibson=gibson10,payette=payette10)


expect9 = currentWeek*(grade9Goal/weeksPerYear);
expect10 = currentWeek*((grade10Goal-200)/weeksPerYear)+200;

strikingDist = 20;

#
# Definitions of Functions and Styles
#

colorPalette = c('#0000FF','#3FDB35','#FFED1B','#FF1605')

getStatus <- function(progress, grade) {
  status <- "danger"
  if (grade==9){
    if (progress >= grade9Goal) {
      status <- "complete"
    } else if (progress >= expect9) {
      status <- "okay"
    } else if (progress >= expect9-strikingDist) {
      status <- "warning"
    } else {
      status <- "danger"
    }
  }
  else if (grade==10){
    if (progress >= grade10Goal) {
      status <- "complete"
    } else if (progress >= expect10) {
      status <- "okay"
    } else if (progress >= expect10-strikingDist) {
      status <- "warning"
    } else {
      status <- "danger"
    }
  }
  return(status)
}

displayProgress <- function(progress, grade) {
  if (grade==9){
    return(min(progress,grade9Goal))
  } else if (grade==10){
    return(min(progress,grade10Goal))
  } else {
    return(progress)
  }
}


findGrade <- function(usr) {
  usr = as.character(usr)
  usr = tolower(usr)
  for (cl in names(classes10)){
    print
    if (tolower(usr) %in% classes10[[cl]]) {
      return('10')
    }
    else if (tolower(usr) %in% classes9[[cl]]) {
      return('9')
    }
  }
  return('0')
}

findClass <- function(usr) {
  usr = as.character(usr)
  usr = tolower(usr)
  for (cl in names(classes10)){
    print
    if (tolower(usr) %in% classes10[[cl]]) {
      return(cl)
    }
    else if (tolower(usr) %in% classes9[[cl]]) {
      return(cl)
    }
  }
  return('NoClass')
}

# REMOVE FIRST ROW AND ALL UNNEEDED COLUMNS
# keeping username, language, % completion, hours
# set up correct names and make sure all columns are the correct class
dat1 = spreadsheet1[2:nrow(spreadsheet1),c(3,6,10,12)]
dat2 = spreadsheet2[2:nrow(spreadsheet2),c(3,6,10,12)]


# special cases
# "PhillipsD177"
#for (names in dat1$)

dat <- rbind(dat1, dat2)
dat<-rename(dat,user=X.1)
dat<-rename(dat,lang=X.4)
dat<-rename(dat,percent=X.8)
dat$percent <- as.numeric(as.character(dat$percent))
dat<-rename(dat,hours=X.10)
dat$hours<-as.numeric(as.character(dat$hours))
rownames(dat)<-1:nrow(dat)

for (cnt in 1:dim(dat)[1]) {
  dat$grade[cnt] <- findGrade(dat$user[cnt])
}


dat$grade<-as.factor(dat$grade)


# Group By user and calculate total progress and time spent
byUser <- group_by(dat, user, grade)
userTotals<-summarise(byUser,totalProgress=sum(percent), totalTime=sum(hours)) 

for (cnt in 1:dim(userTotals)[1]) {
  #userTotals$grade[cnt] <- findGrade(userTotals$user[cnt])
  userTotals$proctor[cnt] <- findClass(userTotals$user[cnt])
  #userTotals$status[cnt] <- getStatus(userTotals$totalProgress[cnt], userTotals$grade[cnt])
}
userTotals$proctor <- as.factor(userTotals$proctor)


userTotals = userTotals %>% rowwise() %>% mutate(status = getStatus(totalProgress,grade), progress = displayProgress(totalProgress,grade))
userTotals$status= factor(userTotals$status, levels=c("complete","okay","warning","danger"))



testPlot <- function(dfClass, expect9, expect10, grade9Goal, grade10Goal) {
  plot <- ggplot(dfClass, aes(x=reorder(user,progress), y=progress, fill=status))
  plot <- plot + ggtitle(first(dfClass$proctor)) + geom_bar(stat="identity") + coord_flip() +
    scale_fill_manual(values=colorPalette,limits = levels(dfClass$status)) + 
    ylab("Total Progress to Date") + xlab("Username")
    
  
  if (first(dfClass$grade) == 9){
    plot <- plot + ylim(0,grade9Goal) + coord_flip() + geom_hline(aes(yintercept=expect9)) 
    #+ geom_text(aes(label = totalProgress, y = 250), size = 7, hjust=1)
  }
  if (first(dfClass$grade)==10){
    plot <- plot + ylim(0,grade10Goal) + coord_flip() + geom_hline(aes(yintercept=expect10)) 
    #+ geom_text(aes(label = totalProgress, y = 500), size = 7, hjust=1)
  }
  fileName = paste0(first(dfClass$proctor),'_',first(dfClass$grade),'_week_',currentWeek,'.pdf');
  ggsave(filename = fileName, plot = plot, width = 7, height = 5)
  #print(plot)
}
group_by(userTotals, proctor, grade) %>%
  do(plot = testPlot(.,expect9, expect10, grade9Goal, grade10Goal))

