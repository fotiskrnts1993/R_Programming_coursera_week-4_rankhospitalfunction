rankhospital<-function(state, outcome, rank){
        datafile<-read.csv('outcome-of-care-measures.csv')
        
        #keep the columns that I want
        hospitals2<-datafile[,c(2,7,11,17,23)]
        
        #Giving names
        colnames(hospitals2)<-c('Hospital Name', 'State', 'Heart Attack', 'Heart Failure', 'Pneumonia')
        
        #Create a subset of the states of the dataframe
        hospitalssub2<-subset(hospitals2, State==state)
        
        #transform the columns from char to numeric
        hospitalssub2<-transform(hospitalssub2, 'Heart Attack'=as.numeric(hospitalssub2$'Heart Attack'),
                                'Heart Failure'=as.numeric(hospitalssub2$'Heart Failure'),
                                'Pneumonia'=as.numeric(hospitalssub2$Pneumonia))
        
        #create the conditions that will result to the ranked list of hospitals
        if(outcome=="Heart Attack" && rank=='best'){
                ha<-which(hospitalssub2[,3]==min(hospitalssub2[,3], na.rm = TRUE))
        }
        else if(outcome=="Heart Attack" && rank=='worst'){
                ha<-which(hospitalssub2[,3]==max(hospitalssub2[,3], na.rm = TRUE))
        }
        else if(outcome=="Heart Attack"){
                haord<-order(hospitalssub2$Heart.Attack, hospitalssub2$Hospital.Name, decreasing = FALSE, na.last = NA)
                ha<-haord[rank]
        }
        else if(outcome=="Heart Failure" && rank=='best'){
                ha<-which(hospitalssub2[,4]==min(hospitalssub2[,4], na.rm = TRUE))
        }
        else if(outcome=="Heart Failure" && rank=='best'){
                ha<-which(hospitalssub2[,4]==min(hospitalssub2[,4], na.rm = TRUE))
        }
        else if(outcome=="Heart Failure"){
                haord<-order(hospitalssub2$Heart.Failure, hospitalssub2$Hospital.Name, decreasing = FALSE, na.last = NA)
                ha<-haord[rank]
        }
        else if(outcome=="Pneumonia" && rank=='best'){
                ha<-which(hospitalssub2[,5]==min(hospitalssub2[,5], na.rm = TRUE))
        }
        else if(outcome=="Pneumonia" && rank=='best'){
                ha<-which(hospitalssub2[,5]==min(hospitalssub2[,5], na.rm = TRUE))
        }
        else if(outcome=="Pneumonia"){
                haord<-order(hospitalssub2$Pneumonia, hospitalssub2$Hospital.Name, decreasing = FALSE, na.last = NA)
                ha<-haord[rank]
        }
        
        hospitalname<-hospitalssub2[ha,1]
        hospitalname
}