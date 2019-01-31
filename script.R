#FINAL SCRIPT#
########################################
# Libraries & Dependencies & Environments
#########################################
 
  library(stringr)
  library(data.table)
  library(RSocrata)
  library(anytime)
  #library(sp)
  #library(rgdal)
  library(sf)
  library(dplyr)
  library(RCurl)
  dat1=Sys.Date()-1
  options(scipen=999)

  #mode function
  Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
  }

  #set directory
  #getwd()
  #setwd('G:/DATABASE/City_Council/Public_Advocate/evictions_carto_2/')

  #environment variable
  source("envs.R")

###########################################################
# Dataset 1: DOB ECB VIOLATIONS - Class 1 Hazardous
################################################################
 
  ## dataset was filtered for infraction code1 - Class 1 hazardous
 
  #getting infraction codes that are Class 1 in a comma separated list for api call
  codes=read.csv('infraction_codes.csv')
 
  head(codes)
 
  codes$cleanCodes=substring(codes$Infraction.Code, 2)
 
  # run code below in r script to get correct output, not working in jupyter
  paste0(codes$cleanCodes, collapse = '\',\'')
 
 
  # these are the infraction codes associated with Class 1 violations that are Immediately Hazardous
  # https://www1.nyc.gov/site/buildings/safety/ecb-violations.page
  #Codes below are being used to create api link '179','144','1B7','1G5','1G4','127','134','156','158','1C7','1C9','1C8','106','101','1C6','113','121','157','160','182','114','125','161','122','107','108','124','103','187','1F9','1G2','172','199','1E6','112','1F8','1G1','105','1E8','1E9','1E5','153','188','1A9','189','102','154','126','132','133','136','137','138','139','140','155','163','104','1E3','1EF','151','1F7','1B1','191','1G3','147','148','1G9','141','143','1C5','190','1B8','1B2','1B3','1B4','1B5','162','1C1','1C2','152','1F1','109','115','118','120','1G6','1F2','192','181','194','183','1B9','1C3','1C4','111','110','1F3','1F4','1F5','1F6','117','176','116','175','159','1E7','131','123','119','193','174','185','130','149','150','129','146','142','177','1A1','1A2','1A3','1A4','1A5','1A6','1A7','1A8','1D1','1D2','1D3','1D4','1D5','1D6','1D7','1D8','1D9','1E1','1E2','1G7','1G8','180','135','170','164','168','165','166','169','171','167','178','1B6'
 
 
  #example of socrata
  #https://data.cityofchicago.org/resource/6zsd-86xi.json?$where=primary_type in('THEFT', 'ROBBERY', 'INTIMIDATION')
 
  #link works in url
  # https://data.cityofnewyork.us/resource/6bgk-3dad.csv?$limit=900000&$where=infraction_code1 in('179','144','1B7','1G5','1G4','127','134','156','158','1C7','1C9','1C8','106','101','1C6','113','121','157','160','182','114','125','161','122','107','108','124','103','187','1F9','1G2','172','199','1E6','112','1F8','1G1','105','1E8','1E9','1E5','153','188','1A9','189','102','154','126','132','133','136','137','138','139','140','155','163','104','1E3','1EF','151','1F7','1B1','191','1G3','147','148','1G9','141','143','1C5','190','1B8','1B2','1B3','1B4','1B5','162','1C1','1C2','152','1F1','109','115','118','120','1G6','1F2','192','181','194','183','1B9','1C3','1C4','111','110','1F3','1F4','1F5','1F6','117','176','116','175','159','1E7','131','123','119','193','174','185','130','149','150','129','146','142','177','1A1','1A2','1A3','1A4','1A5','1A6','1A7','1A8','1D1','1D2','1D3','1D4','1D5','1D6','1D7','1D8','1D9','1E1','1E2','1G7','1G8','180','135','170','164','168','165','166','169','171','167','178','1B6')
 
  ##########api read in
  #modified to be read by r
  p1='https://data.cityofnewyork.us/resource/6bgk-3dad.csv?$limit=900000&$where=infraction_code1 in('
  p1.5='\''
  p2=paste0(codes$cleanCodes, collapse = '\',\'')
  p2.5='\''
  p3=')'
  paste(p1,p1.5, p2, p2.5, p3, collapse="")
 
  ###check data
  dobecb=read.socrata(paste(p1,p1.5, p2, p2.5, p3, collapse=""))
  str(dobecb)
  dim(dobecb)
  names(dobecb)
 
  #saving an original just in case
  dobecb1=dobecb
 
  ### getting bbl key column
  #leading zeros are needed
  dobecb$block=str_pad(dobecb$block, 5, pad = "0")
  dobecb$lot=str_pad(dobecb$lot, 4, pad = "0")
  dobecb$bbl=gsub(" ", "",paste(dobecb$boro, dobecb$block, dobecb$lot))
 
  ### getting issue date into date format
  dobecb$IssueDate=as.Date(anytime(as.character(dobecb$issue_date)))
 
  ### removing entries older than 2018
  ### columns in final dataset will be: 'Violations Issued Last Year' and 'Violations Issued this Year so Far'
  cy=format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y")
  py=as.character(as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))-1)
  #py=paste(py,'01','01', sep="-")
  #db=dobecb[which(dobecb$IssueDate>=py),]
 
  ## create Year column for when new violations are issued in the present year (this is for the 'Violations Issued this Year so Far' column)
  dobecb$Year=format(as.Date(dobecb$IssueDate, format="%d/%m/%Y"),"%Y")
 
  ### removing unnecessary columns, keep: $bbl, $IssueDate, $infraction_code1, $violation_type, $violation_description, $respondent_name
  db=dobecb[,c(14,30,45:49)]
  head(db,1)
 
  ## create Classification column, all are Class 1 (Immediately Hazardous)
  db$Classification=rep('Class 1', nrow(db))
 
  # remove strange values from bbl column
  db=db[-which(db$bbl=="5NANA"),]
  db=db[-which(db$bbl=="4NANA"),]
  db=db[-which(db$bbl=="3NANA"),]
  db=db[-which(db$bbl=="2NANA"),]
  db=db[-which(db$bbl=="1NANA"),]
 
  #subsetting year
  #total number of violations for a bbl
  lastyr=data.table(table(db[which(db$Year==as.character(as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))-1)),5]))
  thisyr=data.table(table(db[which(db$Year==as.character(as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y")))),5]))
  names(lastyr)[1:2]<- c('BBL','Violations_Last_Year')
  names(thisyr)[1:2]<- c('BBL','Violations_This_Year')
 
  #getting all the infraction codes, violation types, and descriptions for a unique bbl
  db=db[which(db$Year>=py),]
  db1=data.table(db)
  setkey(db1, key='bbl')
  infcode=db1[,infraction_code1, by=.(bbl)]
  viot=db1[,violation_type, by=.(bbl)]
  viod=db1[,violation_description, by=.(bbl)]
  resn=db1[,respondent_name, by=.(bbl)]
 
  #######################
  u=unique(db1$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=infcode[which(infcode$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  infcode2=data.table(cbind(u,u2))
  names(infcode2)<-c('bbl', 'Infraction_Codes')
  ###################################
  u=unique(db1$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=viot[which(viot$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  viot2=data.table(cbind(u,u2))
  names(viot2)<-c('bbl', 'Violation_Types')
  ########################
  u=unique(db1$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=viod[which(viod$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  viod2=data.table(cbind(u,u2))
  names(viod2)<-c('bbl', 'Violation_Descriptions')
  #############################
  u=unique(db1$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=resn[which(resn$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  resn2=data.table(cbind(u,u2))
  names(resn2)<-c('bbl', 'Respondent_Names')
  v=data.frame(cbind(infcode2, viot2$Violation_Types, viod2$Violation_Descriptions, resn2$Respondent_Names))
 
  #joining
  #library(dplyr)
  v2=left_join(v,lastyr,by=c("bbl"="BBL"))
  v3=left_join(v2,thisyr,by=c("bbl"="BBL"))
  f_dobecb=v3
  #filling nas with zero
  f_dobecb[which(is.na(f_dobecb$Violations_This_Year)==TRUE),7]<-0
  f_dobecb[which(is.na(f_dobecb$Violations_Last_Year)==TRUE),6]<-0
 
  #to be used for cerating geojsons

  names(f_dobecb)<-c("BBL", "Infraction_Codes", "Violation_Types", "Violation_Descriptions", "Respondent_Names", "Violations_Last_Year", "Violations_This_Year")
  #save csv copy
  write.csv(f_dobecb, 'dobecb_class1_vio.csv', row.names=FALSE)
 
###########################################################
# Dataset 2: 311 SERVICE REQUESTS HPD (heat hot water, unsanitary conditions)
################################################################
  #reading in dataset via api
  cd=format(Sys.time(), format="%Y-%m-%dT%H:%M:%S")
  py=as.character(as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))-1)
  pd=paste(py,'01','01T00:00:00', sep="-")
  #ct='HEAT/HOT WATER'
 
  #https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?$limit=9000000&$where=created_date>='2018-01-01T00:00:00' AND complaint_type in('HEAT/HOT WATER', 'UNSANITARY CONDITION')
 
  #p=paste(c('https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?$limit=9000000&$where=created_date between', cd, 'and', pd, 'and complaint_type=', ct, 'and bbl!=', '0000000000\''), collapse="'")
  p=paste(c('https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?$limit=9000000&$where=created_date>=', pd, 'and complaint_type in(', 'HEAT/HOT WATER\')' ), collapse="'")
 
  hhw=read.socrata(p)
 
 
  #saving copy of read in dataset
 
  hhw1=hhw
  ##############################################
 
  #cleaning hhw dataset
  str(hhw)
 
  max(hhw$created_date)
  min(hhw$created_date)
  min(hhw$bbl)
  max(hhw$bbl)
 
  #removing bbls with NA
  hhw=hhw[-which(is.na(hhw$bbl)==TRUE),]
  hhw=hhw[-which(hhw$bbl==0),]
 
  #adding year column
  hhw$Year=format(as.Date(hhw$created_date, format="%Y-%m-%dT%H:%M:%S"), "%Y")
  unique(hhw$Year)
 
  #total number of 311 hhw for each bbl each year
  cy=format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y")

  #making bbl character
  hhw$bbl=as.character(hhw$bbl)
 
  lastyr=data.table(table(hhw[which(hhw$Year==py),4]))
  thisyr=data.table(table(hhw[which(hhw$Year==cy),4]))
  names(lastyr)[1:2]<- c('BBL','Requests_Last_Year')
  names(thisyr)[1:2]<- c('BBL','Requests_This_Year')
 
  #getting all the descriptors and resolution_descriptions for a unique bbl
  #subsettting hhw for relevant columns
  dbly=data.table(hhw[,c(4,12,16,36,46)])
  setkey(dbly, key='bbl')
 
  comp=dbly[,complaint_type, by=.(bbl)]
  wd=dbly[,descriptor, by=.(bbl)]
  rd=dbly[,resolution_description, by=.(bbl)]
  ###################
  u=unique(dbly$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=wd[which(wd$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  wd2=data.table(cbind(u,u2))
  names(wd2)<-c('bbl', 'Descriptors')
  ################
  u=unique(dbly$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=rd[which(rd$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  rd2=data.table(cbind(u,u2))
  names(rd2)<-c('bbl', 'Resolution_Descriptions')
  ##################
  u=unique(dbly$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=comp[which(comp$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  comp2=data.table(cbind(u,u2))
  names(comp2)<-c('bbl', 'complaint_type')
  ### cbinding
  v=data.frame(cbind(wd2,rd2$Resolution_Descriptions, comp2$complaint_type))
 
  #joining
  #library(dplyr)
  v2=left_join(v,lastyr,by=c("bbl"="BBL"))
  v3=left_join(v2,thisyr,by=c("bbl"="BBL"))
 
 
  #make Na into 0
  v3[which(is.na(v3$Requests_Last_Year)==TRUE),5]<-0
  v3[which(is.na(v3$Requests_This_Year)==TRUE),6]<-0
 
  names(v3)<-c("BBL", "Descriptors", "Resolution_Descriptions", "Complaint_Type", "Requests_Last_Year", "Requests_This_Year")
 
  #to be used for creating geojsons
  f_hhw=v3
 
  #save csv copy
  write.csv(f_hhw, 'hhw.csv', row.names=FALSE)
 
###########################################################
# Dataset 3: HPD Housing Maintenance Code Violations - Class C
################################################################
  ##########api read in
  #modified to be read by r
  py=as.character(as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))-1)
  pd=paste(py,'01','01T00:00:00', sep="-")
  p=paste(c('https://data.cityofnewyork.us/resource/b2iz-pps8.csv?$limit=9000000&$where=novissueddate>=', pd, 'AND class=', 'C\'' ), collapse="'")
 
 
  hmc=read.socrata(p)
 
 
  #check dataset
  dim(hmc)
  str(hmc)
  names(hmc)
  min(hmc$novissueddate)
  max(hmc$novissueddate)
  min(hmc$bbl)
 
  ##save a copy
  hmc2=hmc
 
  ##remove NAs from bbl and 0s
  hmc=hmc[-which(is.na(hmc$bbl)==TRUE),]
  hmc=hmc[-which(hmc$bbl==0),]
 
  # adding year column
  hmc$Year=format(as.Date(hmc$novissueddate, format="%Y-%m-%dT%H:%M:%S"), "%Y")
  unique(hmc$Year)
 
  cy=format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y")
 
  lastyr=data.table(table(hmc[which(hmc$Year==py),3]))
  thisyr=data.table(table(hmc[which(hmc$Year==cy),3]))
  names(lastyr)[1:2]<- c('BBL','Violations_Last_Year')
  names(thisyr)[1:2]<- c('BBL','Violations_This_Year')
 
  #getting all the novdescriptions and novtypes for a unique bbl
  #subsettting hhw
  dbly=data.table(hmc[,c(3,26,29)])
  setkey(dbly, key='bbl')
  nd=dbly[,novdescription, by=.(bbl)]
  nt=dbly[,novtype, by=.(bbl)]
 
  #######loop
  u=unique(dbly$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=nd[which(nd$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  nd2=data.table(cbind(u,u2))
  names(nd2)<-c('BBL', 'Descriptions')
  #########loop
  u=unique(dbly$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=nt[which(nt$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  nt2=data.table(cbind(u,u2))
  names(nt2)<-c('BBL', 'Violation_Types')
  ########cbind
  v=data.frame(cbind(nd2,nt2$Violation_Types))
 
  #joining year counts to column counts
  library(dplyr)
  v2=left_join(v,lastyr,by=c("BBL"="BBL"))
  v3=left_join(v2,thisyr,by=c("BBL"="BBL"))
 
  ##make nas into 0
  v3[which(is.na(v3$Violations_This_Year)==TRUE),5]<-0
  v3[which(is.na(v3$Violations_Last_Year)==TRUE),4]<-0
  ##
 
  #to be used to create geojsons
  f_hmc=v3
  names(f_hmc)<- c("BBL","Descriptions","Violation_Types","Violations_Last_Year","Violations_This_Year")
 
  #save csv copy
  write.csv(f_hmc, 'hmc.csv', row.names=FALSE)
 
###########################################################
# Dataset 4: Certificate of No Harrassment Building List
################################################################
 
  #api read in
  p='https://data.cityofnewyork.us/resource/fajq-hihd.csv?$limit=900000'
 
 
  conh=read.socrata(p)
 
  #check data
  #save a copy
  conh2=conh
 
  #remove NAs in bbl column
  conh=conh[-which(is.na(conh$bbl)==TRUE),]
  conh$bbl=as.character(as.numeric(conh$bbl))
 
  #doesnt need year column yet
 
  #total number of bbls
  t=data.table(table(conh$bbl))
  names(t)<-c('BBL','Number_of_Buildings')
  #t$BBL=as.character(as.numeric(t$BBL))
 
  #getting all the bins for a unique bbl
  #subsettting conh for relevant columns
  dbly=data.table(conh[,c(1:2)])
  setkey(dbly, key='bbl')
  bn=dbly[,bin, by=.(bbl)]
 
  ###############loop
  u=unique(dbly$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=bn[which(bn$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  bn2=data.table(cbind(u,u2))
  names(bn2)<-c('BBL', 'Bins')
 
  ###join
  f_conh=data.table(cbind(t,bn2$Bins))
  names(f_conh)<- c("BBL", "Number_of_Buildings", "Bins")
 
  #save file
  write.csv(f_conh, 'conh.csv', row.names=FALSE)
 
###########################################################
# Dataset 5: Subsidized Housing
################################################################
 
  #dataset has no api
  #update this csv when dataset is updated on furman center
  #sub=read.csv('SubsidizedHousingFurman.csv')
 
  #dim(sub)
  #str(sub)
  #names(sub)
  #min(sub$bbl)
 
  #save a copy
  #sub2=sub
 
  #checking dataset
  #unique(sub$agency_name)
  #unique(sub$regulatory_tool)
  #unique(sub$program_name)
  #length(unique(sub$project_name))
 
  #change bbl class to character
  #sub$bbl=as.character(as.numeric(sub$bbl))
 
  #getting expiration period 0-5, 5-10, 10-15, 15+
  #library(anytime)
  #end=anytime(as.character(sub$end_date))
  #endyr=format(as.Date(end, format="%d/%m/%Y"),"%Y")
  #currentyr=format(as.Date(Sys.time(), format="%d/%m/%Y"),"%Y")
  #yrsexpire=as.numeric(endyr)-as.numeric(currentyr)
  #yrsexpirecat=yrsexpire
  #summary(yrsexpire)
  #summary(endyr)
  #class(yrsexpirecat)
 
  #w1=which(yrsexpire>14)
  #w2=which(yrsexpire<0)
  #w3=which(yrsexpire>-1 & yrsexpire<5)
  #w4=which(yrsexpire>4 & yrsexpire<10)
  #w5=which(yrsexpire>9 & yrsexpire<15)
  #w6=which(is.na(yrsexpire)==TRUE)
 
  #yrsexpirecat[1:5]
  #yrsexpire[1:5]
 
  #yrsexpirecat[w1]<-"15 or more years"
  #yrsexpirecat[w2]<-"Already Expired"
  #yrsexpirecat[w3]<-"Less than 5 years"
  #yrsexpirecat[w4]<-"Between 5-9 years"
  #yrsexpirecat[w5]<-"Between 10-14 years"
  #yrsexpirecat[w6]<-"Permanentaly Affordable"
 
  #yrsexpire[1:5]
  #yrsexpirecat[1:5]
  #unique(yrsexpire)
  #unique(yrsexpirecat)
 
  #sub$yrsexpire=yrsexpire
  #sub$yrsexpirecat=yrsexpirecat
 
  #t=data.table(table(sub$bbl))
  #names(t)<-c('BBL','Number_of_Subsidies')
  #t$BBL=as.character(as.numeric(t$BBL))
 
  #getting all the agency names, regulatory tools, program names, and project names for a unique bbl
  #subsettting conh
  #dbly=data.table(sub[,c(1,5:8,33)])
  #setkey(dbly, key='bbl')
  #agn=dbly[,agency_name, by=.(bbl)]
  #rgt=dbly[,regulatory_tool, by=.(bbl)]
  #pgn=dbly[,program_name, by=.(bbl)]
  #pjn=dbly[,project_name, by=.(bbl)]
  #yec=dbly[,yrsexpirecat, by=.(bbl)]
 
  ####loop
  #u=unique(dbly$bbl)
  #r=list()
  #for (i in 1:length(u)){
  #r[[i]]=agn[which(agn$bbl==u[i]),2]
  #}
  #r[1:5]
 
  #u2=c()
  #for (i in 1:length(r))
  #{
  #u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  #}
  #u2[3]
 
  #agn2=data.table(cbind(u,u2))
  #names(agn2)<-c('BBL', 'agency_name')
  ###############
  #u=unique(dbly$bbl)
  #r=list()
  #for (i in 1:length(u)){
  #r[[i]]=rgt[which(rgt$bbl==u[i]),2]
  #}
  #r[1:5]
 
  #u2=c()
  #for (i in 1:length(r))
  #{
  #u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  #}
  #u2[3]
 
  #rgt2=data.table(cbind(u,u2))
  #names(rgt2)<-c('BBL', 'regulatory_tool')
 
  #########
  #u=unique(dbly$bbl)
  #r=list()
  #for (i in 1:length(u)){
  #r[[i]]=pgn[which(pgn$bbl==u[i]),2]
  #}
  #r[1:5]
 
  #u2=c()
  #for (i in 1:length(r))
  #{
  #u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  #}
  #u2[3]
 
  #pgn2=data.table(cbind(u,u2))
  #names(pgn2)<-c('BBL', 'program_name')
 
  ##########
  #u=unique(dbly$bbl)
  #r=list()
  #for (i in 1:length(u)){
  #r[[i]]=pjn[which(pjn$bbl==u[i]),2]
  #}
  #r[1:5]
 
  #u2=c()
  #for (i in 1:length(r))
  #{
  #u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  #}
  #u2[3]
 
  #pjn2=data.table(cbind(u,u2))
  #names(pjn2)<-c('BBL', 'project_name')
  ##########
  #u=unique(dbly$bbl)
  #r=list()
  #for (i in 1:length(u)){
  #r[[i]]=yec[which(yec$bbl==u[i]),2]
  #}
  #r[1:5]
 
  #u2=c()
  #for (i in 1:length(r))
  #{
  #u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  #}
  #u2[3]
 
  #yec2=data.table(cbind(u,u2))
  #names(yec2)<-c('BBL', 'yrsexpirecat')
 
 
  ####join
  #library(dplyr)
 
  #v=data.frame(cbind(agn2,rgt2$regulatory_tool, pgn2$program_name, pjn2$project_name, yec2$yrsexpirecat))
 
  #joining
  #library(dplyr)
  #v2=left_join(v,t,by=c("BBL"="BBL"))
 
 
  #f_sub=v2
  #names(f_sub)<- c('BBL','Agency_Names', 'Regulatory_Tools', 'Program_Names', 'Project_Names', 'Expiration_Category', 'Number_of_Subsidies')
 
  #make Na into none
 
  #f_sub[which(f_sub$Project_Names==""),5]<-"Not Provided"
  #f_sub[which(f_sub$Number_of_Subsidies==""),7]<-0
 
  #save file
  #write.csv(f_sub, 'sub.csv', row.names=FALSE)
 
###################################################
# Dataset 6: Speculation Watchlist
#################################################
  #api read in
  p='https://data.cityofnewyork.us/resource/dge8-thm3.csv?$limit=900000'
  spw=read.socrata(p)
 
  #check data
  head(spw)
  names(spw)
  dim(spw)
  length(unique(spw$bbl))
  class(spw$bbl)
  unique(spw$yearqtr)
 
  #save a copy
  spw2=spw
 
  #remove NAs in bbl column
  spw=spw[which(is.na(spw$bbl)==FALSE),]
  spw$bbl=as.character(as.numeric(spw$bbl))
 
  #make columns into character for join to work
  #spw$crfn=as.character(as.numeric(spw$crfn))
 
  #needs a yr column
  spw$Year=substr(spw$yearqtr,1,4)

  #create netoperating income column
  spw$netoperincome=spw$cap_rate*spw$price
  #boro cap_rate as percentage
  spw$borough_cap_rate=spw$borough_cap_rate*100
  #set cap_rate as percentage
  spw$cap_rate=spw$cap_rate*100
  
 
  #total number of bbls
  t=data.table(table(spw$bbl))
  names(t)<-c('BBL','Speculation_Potential')
  #t$BBL=as.character(as.numeric(t$BBL))
 
  #getting all the crfn, boro cap rates, cap rates, buyer/sellers names, NTAs, zips, prices, yearqtrs, and deed dates for a unique bbl
  #in this case they are all unique, but in case in future they aren't
  #subsettting spw for relevant columns
  dbly=data.table(spw[,c(1,6:7, 11:12, 18:19, 25:27, 29:30)])
 
  cy=format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y")
 
  lastyr=data.table(table(spw[which(spw$Year==py),1]))
  thisyr=data.table(table(spw[which(spw$Year==cy),1]))
  names(lastyr)[1:2]<- c('BBL','Speculations_Last_Year')
  names(thisyr)[1:2]<- c('BBL','Speculations_This_Year')
 
  setkey(dbly, key='bbl')
  bcr=dbly[,borough_cap_rate, by=.(bbl)]
  cr=dbly[,cap_rate, by=.(bbl)]
  #crfn=dbly[,crfn, by=.(bbl)]
  #nta=dbly[,nta, by=.(bbl)]
  #deed=dbly[,deed, by=.(bbl)]
  pr=dbly[,price, by=.(bbl)]
  yqtr=dbly[,yearqtr, by=.(bbl)]
  slr= dbly[,grantee, by=.(bbl)]
  byr= dbly[,grantor, by=.(bbl)]
  ################
  u=unique(dbly$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=bcr[which(bcr$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  bcr2=data.table(cbind(u,u2))
  names(bcr2)<-c('BBL', 'borough_cap_rate')
  ################
  ################
  u=unique(dbly$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=cr[which(cr$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  cr2=data.table(cbind(u,u2))
  names(cr2)<-c('BBL', 'cap_rate')
  ################
  
  ################
  u=unique(dbly$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=pr[which(pr$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  pr2=data.table(cbind(u,u2))
  names(pr2)<-c('BBL', 'price')
 
  ################
  u=unique(dbly$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=yqtr[which(yqtr$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  yqtr2=data.table(cbind(u,u2))
  names(yqtr2)<-c('BBL', 'qtryr')
  ################
  u=unique(dbly$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=slr[which(slr$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  slr2=data.table(cbind(u,u2))
  names(slr2)<-c('BBL', 'Sellers')
 
  ################
  u=unique(dbly$bbl)
  r=list()
  for (i in 1:length(u)){
  r[[i]]=byr[which(byr$bbl==u[i]),2]
  }
  r[1:5]
 
  u2=c()
  for (i in 1:length(r))
  {
  u2[i]=paste(sort(unique(unlist(r[[i]]))),  collapse=", ")
  }
  u2[3]
 
  byr2=data.table(cbind(u,u2))
  names(byr2)<-c('BBL', 'Buyers')
 
 
  v=data.frame(cbind(yqtr2,bcr2$borough_cap_rate,cr2$cap_rate, pr2$price,slr2$Sellers,byr2$Buyers))
 
  #joining
  library(dplyr)
  v2=left_join(v,lastyr,by=c("BBL"="BBL"))
  #none so far
  #v3=left_join(v2,thisyr,by=c("BBL"="BBL"))
 
  #f_spw=v3
  f_spw=v2
  names(f_spw)<- c("BBL","Quarter","bcaprate","cap_rate", "Price", "Sellers", "Buyers", 'Spec_LY')
 
 
  write.csv(f_spw, 'spw.csv', row.names=FALSE)
 
################################################
# Dataset 7: Rent Stabilized Buildings list
#############################################
  #scraped script to be inserted here
 
  #dataset has no api
  #update this csv when new years list is posted
  #stab=read.csv('stabilizedbldgs.csv')
 
  #dim(stab)
  #str(stab)
  #names(stab)
  #checking dataset
  #unique(stab$Brough)
  #class(stab$Brough)
 
  #save a copy
  #stab2=stab
 
  #checking dataset
  #unique(stab$Brough)
  #as.character(stab$Lot)
  #as.character(stab$Block)
 
 
  #make borough column into character
  #boro=as.character(stab$Brough)
 
  #boro[which(boro=='BRONX')]<-'2'
  #boro[which(boro=='RONX')]<-'2'
  #boro[which(boro=='BROOKLYN')]<-'3'
  #boro[which(boro=='ROOKLYN')]<-'3'
  #boro[which(boro=='NEW YORK')]<-'1'
  #boro[which(boro=='EW YORK')]<-'1'
  #boro[which(boro=='STATEN ISLAND')]<-'5'
  #boro[which(nchar(boro)>1)]<-'4'
 
  #stab$boro=boro
 
  #stab$Block=str_pad(stab$Block, 5, pad = "0")
  #stab$Lot=str_pad(stab$Lot, 4, pad = "0")
  #stab$BBL=gsub(" ", "",paste(stab$boro, stab$Block, stab$Lot))
 
  #t=data.table(table(stab$BBL))
  #names(t)<-c('BBL','Rent_Stabilized_Bldgs')
  #t$BBL=as.character(as.numeric(t$BBL))
 
  #join with subsidy file
 
  ####join
  #full_subrent=full_join(f_sub,t,by=c("BBL"="BBL"))
 
  #make Na into none
 
  #full_subrent[which(is.na(full_subrent$Rent_Stabilized_Bldgs)==TRUE),8]<-0
 
  #full_subrent[which(is.na(full_subrent$Number_of_Subsidies)==TRUE),7]<-0
 
  #save file
  #write.csv(full_subrent, 'sub_rent.csv', row.names=FALSE)
 
#####################################
# Read in need shapefiles 
#########################################
 
  # read in pluto shapefiles
  ###sf
  bx <-st_read('Bronx/Bronx_Pluto_18.shp',layer="Bronx_Pluto_18")
  st_transform(bx, "+proj=longlat +datum=WGS84")
  bk<-st_read('Brooklyn/Brooklyn_Pluto_18.shp',layer="Brooklyn_Pluto_18")
  st_transform(bk, "+proj=longlat +datum=WGS84")
  mn<- st_read('Manhattan/Manhattan_Pluto_18.shp',layer="Manhattan_Pluto_18")
  st_transform(mn, "+proj=longlat +datum=WGS84")
  qn<-st_read('Queens/Queens_Pluto_18.shp',layer="Queens_Pluto_18")
  st_transform(qn, "+proj=longlat +datum=WGS84")
  si<-st_read('StatenIsland/StatenIsland_Pluto_18.shp',layer="StatenIsland_Pluto_18")
  st_transform(si, "+proj=longlat +datum=WGS84")
 
  #join boroughs in nyc
  nyc=rbind(mn,bx,bk,qn,si)
 
  #change class to character
  nyc$BBL=as.character(as.numeric(nyc$BBL))
  nyc$LandUse=as.character(as.numeric(nyc$LandUse))
  nyc$OwnerName=as.character(nyc$OwnerName)
 
  #subset nyc
  nyc=nyc[,c(1,4,8:9,17,30,33,45:48,62,72)]
 
  #land use conversions
  nyc[which(nyc$LandUse=="1"),6]<-"1-2 Family"
  nyc[which(nyc$LandUse=="2"),6]<-"Multifamily Walkup"
  nyc[which(nyc$LandUse=="3"),6]<-"Multifamily Elevator"
  nyc[which(nyc$LandUse=="4"),6]<-"Mixed Residential/Commercial"
  nyc[which(nyc$LandUse=="5"),6]<-"Commercial/Office"
  nyc[which(nyc$LandUse=="6"),6]<-"Industrial"
  nyc[which(nyc$LandUse=="7"),6]<-"Transportation/Utility"
  nyc[which(nyc$LandUse=="8"),6]<-"Public Facilities"
  nyc[which(nyc$LandUse=="9"),6]<-"Open Space"
  nyc[which(nyc$LandUse=="10"),6]<-"Parking"
  nyc[which(nyc$LandUse=="11"),6]<-"Vacant"
  nyc[which(is.na(nyc$LandUse)==TRUE),6]<-"Not Provided"

  #data.table
  #nyc=as.data.table(nyc)

  #getting nyc as a data.table only
  df_nyc <- nyc
  st_geometry(df_nyc) <- NULL
 
  #read in hexbin
  #sf
  hex <- st_read('hex_small/Small_1500.shp')
  hex=st_transform(hex,"+proj=longlat +datum=WGS84")

  #read in zipcode
  zip_sf <- st_read('ZIP_CODE/ZIP_CODE_040114.shp')%>%
  st_transform("+proj=longlat +datum=WGS84")
  zip_sf$ZIPCODE=as.numeric(as.character(zip_sf$ZIPCODE))

  #read in community board
  cd_sf <- st_read('Community_Districts/geo_export_a3cf7f07-83ef-460c-bcd8-bee5ef2f297d.shp')%>%
  st_transform("+proj=longlat +datum=WGS84")

  #read in council district
  cc_sf <- st_read('City_Council_Districts/geo_export_2cee7667-624f-4e9d-a7bf-06e9901cf2ee.shp') %>%
  st_transform("+proj=longlat +datum=WGS84")

  #read in senate district
  senate <- st_read('State_Senate_Districts/geo_export_7b5d585c-56e1-46ec-b1c2-ac8f3398a70e.shp') %>%
  st_transform("+proj=longlat +datum=WGS84")

  #read in assembly district
  assembly <-st_read('State_Assembly_Districts/geo_export_743f1700-3b1c-469b-8489-808b1c49e6d3.shp') %>%
  st_transform("+proj=longlat +datum=WGS84")

######################################
# other layers
####################################
  #cert. of no harassment
  nycc=nyc

  nycc=left_join(nycc,f_conh, by=c("BBL"="BBL"))
  nycc=nycc[which(is.na(nycc$Number_of_Buildings)==FALSE),]
  st_write(nycc,'conh_pluto.geojson', driver='GeoJSON', delete_dsn=TRUE)

  #subsidies or rent stabilization
  #nycs=nyc

  #nycs=left_join(nycs,full_subrent, by=c("BBL"="BBL"))
  #nycs=nycs[which(is.na(nycs$Number_of_Subsidies)==FALSE),]
  #st_write(nycs,'subsidies_pluto.geojson', driver='GeoJSON', delete_dsn=TRUE)

  #heat hot water
  nych=nyc

  nych=left_join(nych,f_hhw, by=c("BBL"="BBL"))
  nych=nych[which(is.na(nych$Descriptors)==FALSE),]
  st_write(nych,'hhw311_pluto.geojson', driver='GeoJSON',delete_dsn=TRUE)

  #hpd violations
  nycm=nyc

  nycm=left_join(nycm,f_hmc, by=c("BBL"="BBL"))
  nycm=nycm[which(is.na(nycm$Descriptions)==FALSE),]
  st_write(nycm,'hpdvio_pluto.geojson', driver='GeoJSON',delete_dsn=TRUE)

  #dobecb violations
  nycd=nyc

  nycd=left_join(nycd,f_dobecb, by=c("BBL"="BBL"))
  nycd=nycd[which(is.na(nycd$Infraction_Codes)==FALSE),]
  st_write(nycd,'dobecbvio_pluto.geojson', driver='GeoJSON',delete_dsn=TRUE)

  #speculation watchlist
  nycw=nyc

  nycw=left_join(nycw,f_spw, by=c("BBL"="BBL"))
  nycw=nycw[which(is.na(nycw$Price)==FALSE),]
  st_write(nycw,'speculation_pluto.geojson', driver='GeoJSON',delete_dsn=TRUE)

##########################################
# MAIN DATASET: EVICTIONS
#########################################
 
  ############################api read in
  #link=https://data.cityofnewyork.us/resource/fxkt-ewig.json?$limit=150000&$where=eviction_zip!='2700000'
  #p=paste('https://data.cityofnewyork.us/resource/fxkt-ewig.json?$limit=500000')
  dat=Sys.Date()-1
  p= paste('https://data.cityofnewyork.us/resource/fxkt-ewig.json?$limit=150000&$where=executed_date=','\'',dat,'\'',sep="")
  e=read.socrata(p)
 
  #if statement

  if(is.data.frame(e) && nrow(e)!=0) { ## saving a copy of original
    e2=e
    ##########################checking dataset
    names(e)
    str(e)
    unique(e[,1])
    #boros look good
    length(unique(e[,2]))
    length(unique(e[,3]))
    unique(e[,2])[1:10]
    #there are couple duplicate court index #s and docket #s
    length(unique(e[,4]))
    unique(e[,4])[1:10]
    #addresses need cleaning and also duplicate addresses
    unique(e[,5])[1:10]
    length(unique(e[,5]))
    #missing apartment numbers
    min(e[,6])
    length(unique(e[,6]))
    #all evictions have zipcode, 3, zipcodes have '00000', can get those zipcodes from geocoding
    unique(e[,7])[1:10]
    length(unique(e[,7]))
    min(e[,7])
    max(e[,7])
    class(e[,7])
    sort(unique(e[,7]), decreasing=TRUE)[1:5]
    #there may be seasonality, trend in the executed dates, earliest date is 2017-01-03, latest date is 2070-03-29. There is only one, remove.
    length(unique(e[,8]))
    length(unique(e[,9]))
    unique(e[,8])
    unique(e[,9])
    #cbind/paste marshall first and last name
    length(unique(e[,10]))
    unique(e[,10])
    #residential and commercial type looks good
    #only one value in scheduled status, 'scheduled', this dataset may not be the whole universe of evictions
   
    ####################cleaning dates & adding a Year column for grouping later
    e$Executed_Date=as.Date(anytime(e$executed_date))
    #e=e[-which(e$Executed_Date>Sys.Date()),]
    e$Year=format(as.Date(e$Executed_Date, format="%d/%m/%Y"),"%Y")
    ###################creating marshal's fullname column
    #e$Marshal_Name=paste(e$marshal_first_name, e$marshal_last_name, sep=" ")
   
    ######################cleaning addresses
    #aka
    newadd_1=gsub('A/K/A', 'AKA', e$eviction_address)
    #remove +
    newadd_1=gsub("\\+", '', newadd_1)
    #remove all periods
    newadd_1=gsub('\\.', '', newadd_1)
    #remove all commas
    newadd_1=gsub('\\,', '', newadd_1)
    #STREET
    newadd_1=gsub("STREE T", 'STREET', newadd_1)
    newadd_1=gsub("STR EET", 'STREET', newadd_1)
    newadd_1=gsub("ST REET", 'STREET', newadd_1)
    newadd_1=gsub("STRE ET", 'STREET', newadd_1)
    newadd_1=gsub("S TREET", 'STREET', newadd_1)
    #ROAD
    newadd_1=gsub("ROA D", 'ROAD', newadd_1)
    newadd_1=gsub("RO AD", 'ROAD', newadd_1)
    newadd_1=gsub("R OAD", 'ROAD', newadd_1)
    #AVENUE
    newadd_1=gsub("AVENU E", 'AVENUE', newadd_1)
    newadd_1=gsub("AVEN UE", 'AVENUE', newadd_1)
    newadd_1=gsub("AVE NUE", 'AVENUE', newadd_1)
    newadd_1=gsub("AV ENUE", 'AVENUE', newadd_1)
    newadd_1=gsub("A VENUE", 'AVENUE', newadd_1)
    newadd_1=gsub("AVNUE", 'AVENUE', newadd_1)
    #PARKWAY
    newadd_1=gsub("P ARKWAY", 'PARKWAY', newadd_1)
    newadd_1=gsub("PA RKWAY", 'PARKWAY', newadd_1)
    newadd_1=gsub("PAR KWAY", 'PARKWAY', newadd_1)
    newadd_1=gsub("PARK WAY", 'PARKWAY', newadd_1)
    newadd_1=gsub("PARKWA Y", 'PARKWAY', newadd_1)
    newadd_1=gsub("PARKWA",'PARKWAY', newadd_1)
    #HIGHWAY
    newadd_1=gsub("HWY", 'HIGHWAY', newadd_1)
    #NORTH
    newadd_1=gsub("N ORTH", 'NORTH', newadd_1)
    newadd_1=gsub("NOR TH", 'NORTH', newadd_1)
    #SOUTH
    newadd_1=gsub("SOUT H", 'SOUTH', newadd_1)
    newadd_1=gsub("S OUTH", 'SOUTH', newadd_1)
    newadd_1=gsub("SOU TH", 'SOUTH', newadd_1)
    #BOULEVARD
    newadd_1=gsub("BOULEVAR D", 'BOULEVARD', newadd_1)
    newadd_1=gsub("BOULEVA RD", 'BOULEVARD', newadd_1)
    newadd_1=gsub("BOULEV ARD", 'BOULEVARD', newadd_1)
    newadd_1=gsub("BOULE VARD", 'BOULEVARD', newadd_1)
    newadd_1=gsub("BOUL EVARD", 'BOULEVARD', newadd_1)
    newadd_1=gsub("BOU LEVARD", 'BOULEVARD', newadd_1)
    newadd_1=gsub("B OULEVARD", 'BOULEVARD', newadd_1)
    #BLVD
    newadd_1=gsub("BLVD", 'BLVD', newadd_1)
    newadd_1=gsub("BLV D", 'BLVD', newadd_1)
    newadd_1=gsub("BL VD", 'BLVD', newadd_1)
    newadd_1=gsub("B LVD", 'BLVD', newadd_1)
    #remove double space
    newadd_1=gsub("  ", " ", newadd_1)
    newadd_1=gsub("   ", " ", newadd_1)

    ####
    ## ask about keeping delimiters
    # separate newadd_1 along 'apt'like words
    spadd=strsplit(newadd_1,"APT|APARTMENT|#|UNIT|STREET -|AVENUE -|STREET-|ST - |AVE -|AVE-|ST- |PORTION|OFFICE|AKA|ENTIRE|FRONT RM|FRONT ENTRANCE|CORNER STORE|STREET LEVEL|1ST FLOOR|FIRST FLOOR|BSMT|FLOOR|SUITE|ON THE", perl=TRUE)

    spadd1=c()
    for(i in 1:length(spadd))
    {
    spadd1[i]=spadd[[i]][1]
    }

    #sp2add=strsplit(spadd1," ")
   
    ########geocode run: SFS single field search
   
    Gcode2<-function(Number,Borough){
   
    require(RCurl)
   
    u1="https://api.cityofnewyork.us/geoclient/v1/search.xml?input="
    u2=paste0("&app_id=", Sys.getenv("GEOCLIENT_API_ID"), "&app_key=", Sys.getenv("GEOCLIENT_API_KEY"))
   
    p1=Number
    p1=gsub(" ","%20",p1)
    BR=Borough
    BR=gsub(" ","%20",BR)
    url=paste(c(u1,p1,BR,u2),collapse="")
    TMP=getURL(url)

   
    xc=strsplit(TMP,"longitude>|</longitude")[[1]][2]
    yc=strsplit(TMP,"latitude>|</latitude")[[1]][2]
    #xz=strsplit(TMP,"zipCode>|</zipCode" )[[1]][2]
    xbbl=strsplit(TMP, "bbl>|</bbl")[[1]][2]
   
    XY=c(xc,yc,xbbl)
    names(XY)<-c("Longitude","Latitude", "BBL")
    return(XY)
    }
   
    ##now run geocoder in loop
    #subset e to the rows that didnt geocode or geocoded incorrectly
   
    lat=c()
    lon=c()
    #zip2=c()
    bbl=c()
    for (i in 1:dim(e)[1])
    #for (i in 1:5)
    {
    G=Gcode2(spadd1[i],e$borough[i])
    lat[i]=G[2]
    lon[i]=G[1]
    #zip2[i]=G[3]
    bbl[i]=G[3]
    }
   
    ########################
    ##update and write file
   
    e$latitude=lat
    e$longitude=lon
    #e$zip=zip
    e$BBL=as.character(bbl)
    e$Eviction_Address=spadd1
   
    e2=e

    e=e[,c(1,6,10,12:17)]
   
    #unable to geocode
    length(which(is.na(e$latitude)==TRUE))
    length(which(nchar(e$latitude)>25))
    #6.5% is missing, below 10%
   
    #Remove NAs to create points shapefile
    e=e[which(is.na(e$latitude)==FALSE),]
    e=e[which(nchar(e$latitude)<25),]
   
    #Separte Res and Comm evictions, carto is being difficult with its sql query and styling, and geocoding quota limits
    #ecomm=e[which(e$residential_commercial_ind=='Commercial'),]
    eres=e[which(e$residential_commercial_ind=='Residential'),]
    eres=data.table(eres)

    #Number of evictons since 2018 at this addresses
    all=data.table(table(eres$BBL))
    names(all)<-c("BBL", "All_Evictions")
    v=left_join(eres,all,by=c("BBL"="BBL"))
    #v=left_join(v,df_nyc,by=c("BBL"="BBL"))
    eres_map=as.data.table(v)
    eres_map$latitude=as.numeric(eres_map$latitude)
    eres_map$longitude=as.numeric(eres_map$longitude)

    #read in previous days evictions and rowbind with todays
    Nofun=fread("eres_map.csv")
    Nofun$BBL=as.character(Nofun$BBL)
    Nofun$Executed_Date=as.Date(Nofun$Executed_Date)
    eres_map=rbind(Nofun,eres_map)

    write.csv(eres_map,'eres_map.csv',row.names=FALSE)
    #eres_map=read.csv('eres_map.csv', stringsAsFactor=FALSE)

    #####################################
    # Evictions Residential layer
    ####################################
      eres_maply <- st_as_sf(eres_map, coords = c(7,6), crs = "+proj=longlat +datum=WGS84") %>%
      st_transform("+proj=longlat +datum=WGS84")
      #eres_maply=eres_maply[,-c(9:20)]
      st_write(eres_maply, 'res_evictions.geojson', driver = "GeoJSON", delete_dsn=TRUE)

    ######################################
    # Evictions Pluto layer
    ####################################

      cy=format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y")
      py=as.character(as.numeric(format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"))-1)

      # total number of evictions for each bbl each year for eres
      lastyr=data.table(table(eres_map[which(eres_map$Year==py),8]))
      thisyr=data.table(table(eres_map[which(eres_map$Year==cy),8]))
      names(lastyr)[1:2]<- c('BBL','Evictions_Last_Year')
      names(thisyr)[1:2]<- c('BBL','Evictions_This_Year')

      # total Number of evictons since 2017 at this addresses
      all=data.table(table(eres_map$BBL))
      names(all)<-c("BBL", "All_Evictions")

      #eres$BBL=as.character(eres$BBL)
      v=full_join(thisyr,lastyr,by=c("BBL"="BBL"))
      v2=left_join(v,all,by=c("BBL"="BBL"))
      v2[which(is.na(v2$Evictions_Last_Year)==TRUE),3]<-0
      v2[which(is.na(v2$Evictions_This_Year)==TRUE),2]<-0

      #add rent stabilization info
      v2=left_join(v2,full_subrent,by=c("BBL"="BBL"))
      v2=v2[,-c(5:9)]
      v2[which(is.na(v2$Number_of_Subsidies)==TRUE),5]<-0
      v2[which(is.na(v2$Rent_Stabilized_Bldgs)==TRUE),6]<-0

      #add the other column info
      v2=left_join(v2,as.data.table(f_dobecb), by=c("BBL"="BBL"))
      v2=v2[,-c(7:10,12)]
      v2[which(is.na(v2$Violations_Last_Year)==TRUE),7]<-0
      names(v2)[7]<-'oath_vio_ly'

      #add the other column info
      v2=left_join(v2,as.data.table(f_hhw), by=c("BBL"="BBL"))
      v2=v2[,-c(8:10,12)]
      v2[which(is.na(v2$Requests_Last_Year)==TRUE),8]<-0
      names(v2)[8]<-'hhw311_ly'

      #add the other column info
      v2=left_join(v2,as.data.table(f_hmc), by=c("BBL"="BBL"))
      v2=v2[,-c(9:10,12)]
      v2[which(is.na(v2$Violations_Last_Year)==TRUE),9]<-0
      names(v2)[9]<-'hmc_ly'

      #add the other column info
      v2=left_join(v2,as.data.table(f_conh), by=c("BBL"="BBL"))
      v2=v2[,-c(11)]
      v2[which(is.na(v2$Number_of_Buildings)==TRUE),10]<-0
      names(v2)[10]<-'conh_bldgs'

      #add the other column info
      v2=left_join(v2,as.data.table(f_spw), by=c("BBL"="BBL"))
      v2=v2[,-c(11:16)]
      v2[which(is.na(v2$Spec_LY)==TRUE),11]<-0
      
      f_er=v2
      f_er$id=seq(1, nrow(v2), 1)
      write.csv(f_er, 'f_er.csv',row.names=FALSE)
      #er=read.csv('f_er.csv', stringsAsFactors=FALSE)

      #evictions pluto geojson
      nycer=nyc

      nycer=left_join(nycer,f_er, by=c("BBL"="BBL"))
      nycer=nycer[which(is.na(nycer$id)==FALSE),]
      st_write(nycer, "res_evictions_pluto.geojson", driver = "GeoJSON", delete_dsn=TRUE)

    ##############################################
    # Spatial joins
    ##############################################
      #spatial join with zipcode shapefile
      #columns needed, eviction sum
      zip_sj <- st_join(zip_sf, eres_maply %>% st_transform(st_crs(zip_sf)), st_intersects) %>%
        as.data.frame() %>% 
          group_by(ZIPCODE) %>%
          summarize(count_evictions = sum(!is.na(All_Evictions))) %>% 
        left_join(zip_sf, by="ZIPCODE") %>% 
        st_as_sf()
      zip_sj <- st_join(zip_sj, nyc %>% st_transform(st_crs(zip_sj)), st_intersects) %>%
        as.data.frame() %>% 
        group_by(ZIPCODE, count_evictions) %>%
        summarize(sum_resunits = sum(UnitsRes, na.rm = TRUE)) %>% 
        left_join(zip_sf, by="ZIPCODE") %>% 
        st_as_sf()

      st_write(zip_sj, "zip_res.geojson", driver = "GeoJSON", delete_dsn=TRUE)



      #hex units res totals
      #nyct=as.data.table(nyc)
        hex_sj <- st_join(hex, eres_maply %>% st_transform(st_crs(hex)), st_intersects) %>%
          as.data.frame() %>%
            group_by(ID) %>%
            summarize(count_evictions = sum(!is.na(All_Evictions))) %>%
          left_join(hex) %>%
          st_as_sf()
        hex_sj <- st_join(hex_sj, nyc %>% st_transform(st_crs(hex)), st_intersects) %>%
          group_by(ID, count_evictions) %>%
          summarize(sum_resunits = sum(UnitsRes, na.rm = TRUE), CC=Mode(Council), CD=Mode(CD), Zip=Mode(ZipCode))

        st_write(hex_sj, "hex_res.geojson", driver = "GeoJSON", delete_dsn=TRUE)



      #spatial join with cd shapefile
      #columns needed, eviction sum

      cd_sj <- st_join(cd_sf, eres_maply %>% st_transform(st_crs(cd_sf)), st_intersects) %>%
        as.data.frame() %>%
          group_by(boro_cd) %>%
          summarize(count_evictions = sum(!is.na(All_Evictions))) %>%
        left_join (cd_sf, by="boro_cd") %>%
        st_as_sf()
      cd_sj <- st_join(cd_sj, nyc %>% st_transform(st_crs(cd_sj)), st_intersects) %>%
        as.data.frame() %>% 
          group_by(boro_cd, count_evictions) %>%
          summarize(sum_resunits = sum(UnitsRes, na.rm = TRUE)) %>% 
        left_join(cd_sf, by="boro_cd") %>% 
        st_as_sf()

      st_write(cd_sj, "cd_res.geojson", driver = "GeoJSON", delete_dsn=TRUE)


      #spatial join with cc shapefile
      #columns needed, eviction sum
     
      cc_sj <- st_join(cc_sf, eres_maply %>% st_transform(st_crs(cc_sf)), st_intersects) %>%
        as.data.frame() %>% 
          group_by(coun_dist) %>%
          summarize(count_evictions = sum(!is.na(All_Evictions))) %>%
        left_join(cc_sf, by = "coun_dist") %>% 
        st_as_sf()
      cc_sj <- st_join(cc_sj, nyc %>% st_transform(st_crs(cc_sj)), st_intersects) %>%
        as.data.frame() %>% 
          group_by(coun_dist, count_evictions) %>%
          summarize(sum_resunits = sum(UnitsRes, na.rm = TRUE)) %>% 
        left_join(cc_sf, by="coun_dist") %>% 
        st_as_sf()

      st_write(cc_sj, "cc_res.geojson", driver = "GeoJSON", delete_dsn=TRUE)

      
      #spatial join senate
      sen_sj <- st_join(senate, eres_maply %>% st_transform(st_crs(senate)), st_intersects) %>%
        as.data.frame() %>% 
          group_by(st_sen_dis) %>%
          summarize(count_evictions = sum(!is.na(All_Evictions))) %>% 
        left_join(senate, by = "st_sen_dis") %>% 
        st_as_sf()
      sen_sj <- st_join(sen_sj, nyc %>% st_transform(st_crs(sen_sj)), st_intersects) %>%
        as.data.frame() %>% 
        group_by(st_sen_dis, count_evictions) %>%
        summarize(sum_resunits = sum(UnitsRes, na.rm = TRUE)) %>% 
        left_join(senate, by = "st_sen_dis") %>% 
        st_as_sf()

      st_write(sen_sj, "sen_res.geojson", driver = "GeoJSON", delete_dsn=TRUE)



      #spatial join assembly
      assem_sj <- st_join(assembly, eres_maply %>% st_transform(st_crs(assembly)), st_intersects) %>%
      as.data.frame()%>%
          group_by(assem_dist) %>%
          summarize(count_evictions = sum(!is.na(All_Evictions)))%>%
          left_join(assembly, by = "assem_dist")%>%
          st_as_sf()
      assem_sj <- st_join(assem_sj, nyc %>% st_transform(st_crs(assem_sj)), st_intersects) %>%
        as.data.frame()%>%
        group_by(assem_dist , count_evictions) %>%
        summarize(sum_resunits = sum(UnitsRes, na.rm = TRUE)) %>%
        left_join(assembly, by = "assem_dist") %>%
        st_as_sf()
      st_write(assem_sj, "assembly_res.geojson", driver = "GeoJSON", delete_dsn=TRUE)


    ##end script if e is empty
  }else{print('e is empty')}

