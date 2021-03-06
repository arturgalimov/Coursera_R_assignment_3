best<- function(state, outcome){
    file_path<-"C:/Users/Artur/OneDrive/Dokumente/STUDIES/R/Workspace/outcome-of-care-measures.csv"
    x<-read.csv(file_path, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","", "Not Available"))
    
    # comment for gitHub
    if (outcome== "heart attack"){
        outcome_int<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome== "heart failure"){
        outcome_int<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome== "pneumonia"){
        outcome_int<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
        stop("Error in best(",state,", ",outcome,") : invalid outcome")
    }
    
    states<-x[,"State"]
    v<-(state %in% states)
    if (v!=TRUE ){
        stop("Error in best(",state,", ",outcome,") : invalid state")
    }
    
    state_right<- states==state
    x1<-split(x,state_right)["TRUE"]
    
    
    len<-length(x1[[1]][,"Hospital.Name"])
    
    data_f <-data.frame(name = 1:len, rate = 1:len, rank = 1:len)
    
    for (i in 1:len){
        data_f[i,1]<-x1[[1]][,"Hospital.Name"][i]
        data_f[i,2]<-x1[[1]][,outcome_int][i]
    }
    cc<-complete.cases(data_f[,"rate"])
    data_f_cc<-data_f[cc,]
    
    #Sorting dataframe 1) according to rate 2) according to name 
    
    ordered_data_f_cc<- data_f_cc[with(data_f_cc, order(data_f_cc[,"rate"],data_f_cc[,"name"])), ]

    respond<-ordered_data_f_cc[,"name"][1]
    
    respond
}

