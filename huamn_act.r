
library(changepoint)


runResult <- function(){
Data_Path <- "/Users/xxue/Documents/proj/UCI/RawData/"
Label<-read.table(paste(Data_Path, "labels.txt", sep = ""), header = FALSE)
allErrors <- c()
for(i in 1:61){
explabel = Label[which(Label[,1] == i), ]
errors <- getResultOfExperiment(Label, i)
print(errors)
allErrors <- append(allErrors, errors)
}
return(allErrors)
}

getResultOfExperiment <- function(Label, experimentID){
explabel <- Label[which(Label[,1] == experimentID), ]
errors <- c()
for(i in 1:(nrow(explabel)-1)){
startPre <- explabel[i, 4]
endPre <- explabel[i, 5]
startCur <- explabel[i+1, 4]
endCur <- explabel[i+1, 5]
fileName <- paste("acc_exp", (if(explabel[i, 1]<10) "0" else ""), explabel[i, 1], "_user", (if(explabel[i, 2]<10) "0" else ""), explabel[i, 2], ".txt", sep = "")
path <- paste("/Users/xxue/Documents/proj/UCI/RawData/", fileName, sep = "")
actPre <- explabel[i, 3]
actCur <- explabel[i+1, 3]
if(actPre!= actCur){
error <- plotGraphByRecords(path, startPre, endPre, startCur, endCur, actPre, actCur, experimentID)
errors <- append(errors, error)
}
}
return(errors)
}


plotGraphByRecords <- function(path, startPre, endPre, startCur, endCur, actPre, actCur, experimentID){
Data1<-read.table(path, header = FALSE)
pre <- Data1[startPre:endPre,]
cur <- Data1[startCur:endCur,]
error <-plotGraph(pre, cur, actPre, actCur, experimentID)
return(error)
}



plotGraph <- function(firstRawData, scondRawData, actPre, actCur, experimentID){
l<-c()
l<-c(l,getSpeed(firstRawData))
l<-c(l,getSpeed(scondRawData))
ansmeanvar=cpt.meanvar(l)
uuid <- system("uuidgen", intern=T)
output <- paste("/Users/xxue/Documents/proj/UCI/pdf/", experimentID, "_", actPre, "_", actCur, "_", uuid, sep = "")
pdf(output,width=7,height=5)
plot(ansmeanvar,cpt.width=3)
dev.off()
error <- (abs(ansmeanvar@cpts[1] - nrow(firstRawData)))/(nrow(firstRawData) + nrow(scondRawData) - 2)
print(paste(experimentID, actPre, actCur, error, sep = " "))
return(error)
}


getSpeed <- function(rawData){
l<-c()
for(i in 1:nrow(rawData)-1){
sumq = 0
for(j in 1:3){
x = rawData[i, j] - rawData[i-1, j];
sumq = sumq + x * x
}
l<-c(l,sqrt(sumq))
}
return (l)
} 


his_plot <- function(errs){
distance = c()
for(e in errs){
if(e==0){
distance <- append(distance,0)
}else{
distance <- append(distance, as.integer(e*100) + 1)
}
}
hist(distance, xlim=c(0,100),  ylim=c(0,420),  breaks=100)
}


cal_per <- function(errs){
E_01 = 0
for(i in errs){
if(i<0.01){
E_01 = E_01+1
}
}
E_03 = 0
for(i in errs){
if(i<=0.03){
E_03 = E_03+1
}
}
E_05 = 0
for(i in errs){
if(i<=0.05){
E_05 = E_05+1
}
}
e_1_str = paste("percentage of error distance 1: ", E_01/length(errs))
e_3_str = paste("percentage of error distance 3: ", E_03/length(errs))
e_5_str = paste("percentage of error distance 5: ", E_05/length(errs))
result = paste(e_1_str, e_3_str, e_5_str, "\n", sep = "\n")
print(cat(result))
}



errs <- runResult()
his_plot(errs)
cal_per(errs)







