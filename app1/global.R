library(shiny)
library(ggplot2)
library(dplyr)

constants = list(strikingDist = 20,
                 grade9Goal = 250,
                 grade10Goal = 500,
                 weeksPerYear = 38)

strikingDist = 20
grade9Goal = 250
grade10Goal = 500
weeksPerYear = 38

expect9 <- NULL
expect10 <- NULL

colorPalette = c('#0000FF','#3FDB35','#FFED1B','#FF1605')

epstein9 = c("cadenahernandezm046","brisbonl017","mageea058","dekreonz027","padillay066","laboye093","sotoo077","wojcika134","sernad086","johnsonk053","gomesc043","murphyz086")
epstein10 = c("ajuhanm002","cardenask152","chavezs134","dasilvac180","figueroae035","ortiz-muriels142","lombak156","riverae083","maybink133","hindsc050")

sinha9 = c("cheayee145", "cruzd123", "cruzy099", "echavarriav030", "geogheganc039", "gravierv133", "nunezn065", "peckhama143", "ramosa131", "silvierae076", "vazdopios192")
sinha10 = c("adebayof172", "bayronriveral012", "granilloc155", "leporea054", "marroquinl060", "moralesp168", "rogersr171", "sosar166")

olivo9 = c("antiguaa087", "batistal151", "dufaultj029", "dwyern083", "godoyy098", "mouratob071", "silva-milersona088", "trejor144", "vieirah096", "wilsonm100")
olivo10 = c("arboledaj004", "ariasl007", "bravog078", "cassianik020", "encarnaciona179", "feldb033", "gallegosgomezm139", "lopesn057", "ricaurteg115", "tebaldih170", "trejoa092", "vasquezj175")

delanos9 = c("gomezn044", "chavezk023", "moored137", "ayalab008", "riganom071", "kinnesl057", "jadachp047", "girardt043","lemuso118", "restrepoa070")
delanos10 = c("colonias109", "zabb174", "adotevim150", "zuluagas102", "velezm124", "adelekeo001", "aguilarb103", "gomese173", "tavaresa165","youngs100","vazquezl108")

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



substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

getID <- function(username, class){
  nameStem <- unlist(strsplit(as.character(username),'_'))
  idNumber <- substrRight(nameStem[1],3)
  id <- paste(class,idNumber, sep = "")
  return(id)
}

getStatus <- function(progress, grade, constants) {
  status <- "danger"
  if (grade==9){
    if (progress >= constants$grade9Goal) {
      status <- "complete"
    } else if (progress >= expect9) {
      status <- "okay"
    } else if (progress >= expect9-constants$strikingDist) {
      status <- "warning"
    } else {
      status <- "danger"
    }
  }
  else if (grade==10){
    if (progress >= constants$grade10Goal) {
      status <- "complete"
    } else if (progress >= expect10) {
      status <- "okay"
    } else if (progress >= expect10-constants$strikingDist) {
      status <- "warning"
    } else {
      status <- "danger"
    }
  }
  return(status)
}

displayProgress <- function(progress, grade, constants) {
  if (grade==9){
    return(min(progress,constants$grade9Goal))
  } else if (grade==10){
    return(min(progress,constants$grade10Goal))
  } else {
    return(progress)
  }
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


loadData <- function(file1, file2){
  if(is.null(file1) || is.null(file2)){
    return(NULL)
  } else {
    spreadsheet1 = read.csv(file1$datapath)
    spreadsheet2 = read.csv(file2$datapath)
    
    dat1 = spreadsheet1[2:nrow(spreadsheet1),c(3,6,10,12)]
    dat1$class = '22'
    dat2 = spreadsheet2[2:nrow(spreadsheet2),c(3,6,10,12)]
    dat2$class = '23'
    
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
    
    return(dat)
  }
}

generatePlot2 <- function(data, currentWeek, teacher, effectiveGrade, constants){
  if(is.null(data)){
    return(NULL)
  } else {
  expect9 <<- currentWeek*(constants$grade9Goal/constants$weeksPerYear);
  expect10 <<- currentWeek*((constants$grade10Goal-200)/constants$weeksPerYear)+200;
  
  cat(currentWeek)
  cat("\n")
  cat(expect9)
  cat("\n")
  
  byUser <- dplyr::group_by(data, user, grade, class)
  userTotals<-dplyr::summarise(byUser,totalProgress=sum(percent), totalTime=sum(hours)) 
  
  for (cnt in 1:dim(userTotals)[1]) {
    userTotals$proctor[cnt] <- findClass(userTotals$user[cnt])
    userTotals$id[cnt] <- getID(userTotals$user[cnt], userTotals$class[cnt])
  }
  userTotals$proctor <- as.factor(userTotals$proctor)
  
  
  userTotals = userTotals %>% dplyr::rowwise() %>% dplyr::mutate(status = getStatus(totalProgress,grade, constants), progress = displayProgress(totalProgress,grade, constants))
  userTotals$status= factor(userTotals$status, levels=c("complete","okay","warning","danger"))
  
  tmp <- userTotals %>% 
    dplyr::filter(proctor == teacher, grade == effectiveGrade)
  
  plot <- ggplot(tmp, aes(x=reorder(user,progress), y=progress, fill=status))
  plot <- plot + ggtitle(first(tmp$proctor)) + geom_bar(stat="identity") + coord_flip() +
    scale_fill_manual(values=colorPalette,limits = levels(tmp$status)) + 
    ylab("Total Progress to Date") + xlab("Username")
  
  if (first(tmp$grade) == 9){
    plot <- plot + ylim(0, constants$grade9Goal) + coord_flip() + geom_hline(aes(yintercept=expect9)) 
  }
  if (first(tmp$grade)==10){
    plot <- plot + ylim(0,constants$grade10Goal) + coord_flip() + geom_hline(aes(yintercept=expect10)) 
  }
  return(plot)
  }
}

