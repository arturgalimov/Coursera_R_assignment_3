rankall<- function(outcome, num){
    file_path<-"C:/Users/Artur/OneDrive/Dokumente/STUDIES/R/Workspace/outcome-of-care-measures.csv"
    x<-read.csv(file_path, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","", "Not Available"))
    
    # creating final output dataframe
    output_data <- data.frame(hospital = character(), state= character(),stringsAsFactors=FALSE)
    
    colnames(output_data)<-c("Hospital","State")
    
    if (outcome== "heart attack"){
        outcome_int<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if (outcome== "heart failure"){
        outcome_int<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else if (outcome== "pneumonia"){
        outcome_int<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } else {
        stop("Error in best(",state,", ",outcome,") : invalid outcome")
    }
    

    
    #creating a list with elements corresponding to each "state"
    x1<-split(x,x[,"State"])
    
    #finding "hospital" for specified "rank" in a particular state
    for (i in 1:length(x1)){
    
        len<-length(x1[[i]][,"Hospital.Name"])
    
        data_f <-data.frame(name = 1:len, rate = 1:len)
    
        for (j in 1:len){
            data_f[j,1]<-x1[[i]][,"Hospital.Name"][j]
            data_f[j,2]<-x1[[i]][,outcome_int][j]
        }
        cc<-complete.cases(data_f[,"rate"])
        data_f_cc<-data_f[cc,]
        
        # conditions for "num" parametr
        
        if (num == "worst"){
            num_int<-nrow(data_f_cc)
        } else if (num == "best"){
            num_int<-1L
        } else if (num <= nrow(data_f_cc)){
            num_int<- num
        } else if (num > nrow(data_f_cc)){
            num_int<-nrow(data_f_cc)+1
        }
    
        #Sorting dataframe 1) according to rate 2) according to name 
    
        ordered_data_f_cc<- data_f_cc[with(data_f_cc, order(data_f_cc[,"rate"],data_f_cc[,"name"])), ]

        respond<-ordered_data_f_cc[,"name"][num_int]
        # making new row "hospital" / "state" pair tp insert into "output_data" dataframe
        newrow <- c(hospital=respond, state = names(x1)[i])
        # inserting new row into "output_data" dataframe
        output_data <- rbind(output_data, data.frame(as.list(newrow)))
    }
    output_data
}

