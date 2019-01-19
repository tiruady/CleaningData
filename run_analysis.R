clean<-function(path=""){
	#loading necessary libraries
	library(dplyr)
	library(reshape2)
	
	
	#reading files in R
	testX<-read.table(file.path(path,"test","X_test.txt"))
	testy<-read.table(file.path(path,"test","y_test.txt"))
	subjecttest<-read.table(file.path(path,"test","subject_test.txt"))
	arr=c("WALKING","WALKING UPSTAIRS","WALKING DOWNSTAIRS","SITTING","STANDING","LAYING")
	label_test<-factor(sapply(testy[,1],function(y){arr[y]}),levels=arr)
	
	#training set
	trainX<-read.table(file.path(path,"train","X_train.txt"))
	trainy<-read.table(file.path(path,"train","y_train.txt"))
	subjecttrain<-read.table(file.path(path,"train","subject_train.txt"));
	label_train<-factor(sapply(trainy[,1],function(y){arr[y]}),levels=arr)
	
	#collecting the indices of the reqiired features
	features<-read.table(file.path(path,"features.txt"))
	colIndices<-grep("-mean|-std",features[,2])
	
	#taking subsets
	ntestX<-testX[,colIndices]
	ntrainX<-trainX[,colIndices]	
	colnames(ntestX)<-colnames(ntrainX)<-features[colIndices,2]	

	#merging the datasets
	mergedX<-rbind(ntestX,ntrainX)
	mergedy<-rbind(testy,trainy)
	mergedlabel<-factor(c(label_test,label_train))
	mgl<-factor(sapply(mergedlabel,function(y){arr[y]}),levels=arr)
	mergedsubjects<-rbind(subjecttest,subjecttrain)
	mergedX<-mutate(mergedX, activity=mgl, subjectNo=mergedsubjects[,1])
	
	#grouping the dataset by activity and subject Number
      clean<-mergedX%>%group_by(activity,subjectNo)%>%summarize_all(mean)


	#writing dataset to a text file with proper labels
	colnames(clean)<-gsub("\\()","",colnames(clean))
	colnames(clean)<-sapply(colnames(clean),function(y){ if(y!="activity" & y!="subjectNo"){paste0(y,"(average value)")}else{y}})
 	str(clean)
	write.table(clean,file="mergedData.txt",row.name=FALSE)


	
      #writing the codebook
	dd<-data.frame(variableName=colnames(clean),datatype=sapply(clean,class),range = sapply(clean,function(gg){	
		if(class(gg)=="numeric"){
			paste0(min(gg)," to ", max(gg))
		}
	
		else {
			paste(levels(gg), collapse='',sep=",")
		}  
		       
 	}
))
	write.table(dd,"cod.md",row.names=F,sep='				|		')

}	