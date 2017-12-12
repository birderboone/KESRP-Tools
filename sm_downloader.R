# Names of drives
drives<-  c('M','F')
# Folder to download files into
folder<- 'L:/'
# Date collected (can be approximate)
collect<- '6/8/2017'
# What Round is it?
round<-3
# What Season is it
season<-2017
# Where is the log file
log.file<-'C:/Users/UMP 3/Dropbox/KESRP_data/logfiles/2017/UMP/smlog_UMP_R3.csv'
accessimport<-'C:/Users/UMP 3/Dropbox/KESRP_data/AccessImportLog/UMP_Log_2017_R3.csv'
# Does a new log file need to be created?
make_file<-T
###########################################
# start code
{#
#
#
#############################################
ptm<-proc.time()
if(make_file==T &(file.size(log.file)<1000|is.na(file.size(log.file))) ) {log<-data.frame(matrix(nrow=100, ncol=13))
colnames(log)<-c('ID1', 'SensorName', 'DownloadDate', 'CollectDate', 'Sourcefiles.A', 'Sourcefiles.B', 'Sourcefiles.C', 'Sourcefiles.D','Round','Season','NumberFiles','FirstDateonCard','LastDateonCard')
log$ID1<-1:100}

if(make_file==F | (make_file==T & !(file.size(log.file)<1000|is.na(file.size(log.file))) )){
  log<-read.csv(log.file,stringsAsFactors = F)
  log<-log[order(log$ID1),]
}
baddrives<-list()
errorlist<-list()
for(i in drives){
  #i<-drives[1]
  if(which(drives==i)==1){counter<-0}
  ##########################################
  print(paste0('Downloading drive ',i,':'))
  #########################################
 all.files<-list.files(paste0(i,':/Data')) 
    all.sm<-unique(substr(all.files,1,regexpr('_',all.files)-1))
    if(length(all.files)>0){counter<-counter+1}
    if(length(all.files)==0){baddrives[[i]]<-i}
  for(l in all.sm){
    #l<-all.sm[1]
    #does resulting entry exist in log file?
    entry<-which(paste(log$CollectDate[!is.na(log$CollectDate)],log$SensorName[!is.na(log$SensorName)])%in%paste(collect,l))
    
    #find the card number from the sensor file
    sensor_path<-unique(c(all.files[grepl(paste0('^',l,'_Sensor'),all.files)],list.files(paste0(i,':/'))[grepl(paste0('^',l,'_._Summary'),list.files(paste0(i,':/')))]))[1]
    if(grepl('Sensor',sensor_path)){sensor_pos<-regexpr('Sensor',sensor_path)[[1]][1]}
    if(grepl('Summary',sensor_path)){sensor_pos<-(regexpr('Summary',sensor_path)[[1]][1] - 9)}
   card<-substr(sensor_path,sensor_pos+7,sensor_pos+7)
  
   #collect the info on the sensors files
    d_collect<-as.POSIXlt(collect, format="%m/%d/%Y")
    dest.folder<-paste0(folder,'/',l,'_',d_collect$mday, "_",months(d_collect),d_collect$year+1900)
    source.files<-list.files(paste0(i,':/Data'))
   if(!dir.exists(dest.folder)) {dir.create(dest.folder)}
    dest.files<-list.files(dest.folder)
    sub.files<- source.files[grepl(paste0('^',l),source.files)]
    ####get times
    
    dd<-strsplit(sub.files[grepl('.wa',sub.files)],'_')
    dd1<-do.call(rbind, dd)
    file.times<-as.POSIXct(dd1[,2],format='%Y%m%d')
    first<-min(file.times)
    newfirst<-substr(first,1,10)
    last<-max(file.times)
    newlast<-substr(last,1,10)
    #start the recursive file copying
    #file.copy(paste0(i,':/Data/',sub.files),dest.folder,copy.mode=T,overwrite=F)
    system(paste0('Robocopy ',i,':/Data/ ',dest.folder,' /E'))
    #figure out where to save the info in the log file
    if(length(entry)==1) { 
      next.entry<-entry
    }
    if(length(entry)==0) {
      log<-log[order(log$ID1),]
      next.entry<-log$ID1[nchar(log$SensorName)>0]
      #next.entry<-log$ID1[apply(subset(log,select=-c(NumberFiles,ID1)),1,function(x)sum(is.na(x)))<(ncol(log)-5)]
      if(length(next.entry)==0){next.entry<-0}
      if(length(next.entry)!=0){next.entry<-max(next.entry)}
      next.entry<-next.entry+1
    }
      if(!is.na(log[next.entry,paste0('Sourcefiles.',card)])){errorlist[[paste0(i,l)]]<-paste0('duplicate entry, file already downloaded:', l,' on ',d_collect);next}
      log$CollectDate[next.entry]<-collect
      log$SensorName[next.entry]<-l
      log[next.entry,paste0('Sourcefiles.',card)]<-length(sub.files)
      log$DownloadDate[next.entry]<-paste0(as.POSIXlt(Sys.Date())$mon+1,'/',as.POSIXlt(Sys.Date())$mday,'/', as.POSIXlt(Sys.Date())$year+1900)
      log$Round[next.entry]<-round
      log$Season[next.entry]<-season
      
      ###Actively recalculate the first and last date on card
      oldfirst<-log$FirstDateonCard[next.entry]
      
      if(regexpr('-',oldfirst)>0){ oldfirst<-as.POSIXct(oldfirst)}
        if(!regexpr('-',oldfirst)>0){oldfirst<-as.POSIXct(oldfirst,format='%m/%d/%Y')}
     
      #if(is.na(oldfirst)){newfirst<-substr(first,1,10)}
      if(!is.na(oldfirst)&&oldfirst<first){newfirst<-substr(oldfirst,1,10)}
      log$FirstDateonCard[next.entry]<-newfirst
     
      oldlast<-log$LastDateonCard[next.entry]
    
      if(regexpr('-',oldlast)>0){ oldlast<-as.POSIXct(oldlast)}
      if(!regexpr('-',oldlast)>0){oldlast<-as.POSIXct(oldlast,format='%m/%d/%Y')}
      #if(is.na(oldfirst)){newfirst<-substr(first,1,10)}
      if(!is.na(oldlast)&&oldlast>last){newlast<-substr(oldlast,1,10)}
      log$LastDateonCard[next.entry]<-newlast
    if(length(entry)>1){
      stop(paste0('greater than one entry for the entry: ',l,' on ',d_collect))
    }
    
  }
    
}
proc.time() - ptm
print(paste0('System attempted to download: ',length(drives),' drives, succesfully downloaded: ',counter,'. Did not download: ',unlist(baddrives)))
errorlist
log$NumberFiles<-apply(log[,grepl('Sourcefiles',colnames(log))],1,sum,na.rm=T)
write.csv(log, log.file,row.names=F, na="")

###write the access upload file
head(log)
colnames(log)
access<-subset(log, select=c(SensorName,DownloadDate,Round, Season, NumberFiles, FirstDateonCard, LastDateonCard))
access<-access[nchar(access$SensorName)>0,]
write.csv(access, accessimport, row.names=F)
}