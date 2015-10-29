## Reading the income dataset from file
    income_df<-read.csv(file.choose())
    
    size<-dim(income_df)
    income_row<-size[1]
    income_col<-size[2]

    ## Outlier eliminations
    age_mean <- mean(income_df$age)
    age_sd <- sd(income_df$age)
    
    hpw_mean <- mean(income_df$hour_per_week)
    hpw_sd <- sd(income_df$hour_per_week)
    
    net_cap<-vector(mode ="integer",length = income_row)
    
    for(j in 1:income_row)
        net_cap[j]=income_df[["capital_gain"]][j]-income_df[["capital_loss"]][j]
    
    netCap_mean <- mean(net_cap)
    netCap_sd <- sd(net_cap)
    
    outlier_index_vec<-vector(mode ="integer",length = income_row)
    count<-1
    
    #Compute all rows that need to be removed
    for(i in 1:income_row)
    {
      age<-income_df[["age"]][i];
      hpw<-income_df[["hour_per_week"]][i]
      net<-net_cap[i]
      
      if(age > (age_mean+3*age_sd) | age < (age_mean-3*age_sd) |
         hpw > (hpw_mean+3*hpw_sd) | hpw < (hpw_mean-3*hpw_sd) |
         net > (netCap_mean+3*netCap_sd) | net < (netCap_mean-3*netCap_sd)) {
        
        outlier_index_vec[count] <- i;
        count<-count+1
      }
    }
    
    old_income_df <- income_df
    income_df <- income_df[-outlier_index_vec,]
    
    size<-dim(income_df)
    income_row<-size[1]
    income_col<-size[2]
    
    print(income_row)
    print(income_col)
    
    ##----------------------------------------------------------
    #Function for getting max occurance of a column value
    
    maxOccurance <- function(varcol)
    {
      
      items <- unique(varcol)
      freq <- vector(mode="numeric",length=length(items))
      
      for(i in 1:length(items))
      {
        freq[i] <- length(subset(varcol,varcol==items[i]))
        
      }
      
      order_vec <- order(freq,decreasing=TRUE)
      
      max_index <- order_vec[1]
      
      result <- items[max_index]
      
      if(result==" ?")
      {
        max_index <- order_vec[2]
        result <- items[max_index]
      }
      
      return (result)
    }
    
    #result <- maxOccurance(newInc_df$race)
    
    
    
    ##------------------------------------------------------------
    ## Missing value calculations
    
    find_missing<-function(){
      
      for(i in 1:length(income_df$workclass))
      {
        if(income_df[["workclass"]][i]==" ?"){
          income_df[["workclass"]][i]=maxOccurance(income_df$workclass)
        }
      }
      
      for(i in 1:length(income_df$occupation))
      {
        if(income_df[["occupation"]][i]==" ?"){
          income_df[["occupation"]][i]=maxOccurance(income_df$occupation)
        }
      }
      
      
      for(i in 1:length(income_df$native_country))
      {
        if(income_df[["native_country"]][i]==" ?"){
          income_df[["native_country"]][i]=maxOccurance(income_df$native_country)
        }
      }
      
      return(income_df)
      
    }
    
    income_df<-find_missing()
    
    #-------------------------------------------------------------
    #Creating a new income data frame to hold final data set
    temp_df <- income_df
    drops <- c("age","workclass","fnlwgt","education","education_cat","marital_status","occupation","relationship","race","gender","capital_gain","capital_loss","hour_per_week","native_country","class")
    newInc_df <- temp_df[,!(names(temp_df) %in% drops)]
    
    #--------------------------------------------------------------
    
    #discretization of  age 
    
    age_vec<-vector(mode ="integer",length = income_row)
    
    discretize_age<-function(){
      
      age_vec <- age<-cut(income_df$age,seq(0,100,10),labels=c(1:10))
      return(age_vec)
    }
    
    age_vec <- discretize_age();
    
    #Binding age to newInc
    newInc_df <- cbind(newInc_df,age_vec)
    
    #Binding workclass to newInc
    newInc_df$workclass <- income_df$workclass
    
    
    #normalize fnwgt using normalization
    nfnlwgt<-vector(mode ="integer",length = income_row)

    #Binding education_cat to newInc
    newInc_df$education_cat <- income_df$education_cat
    
    #Binding marital_status to newInc
    newInc_df$marital_status <- income_df$marital_status
    
    #Binding occupation to newInc
    newInc_df$occupation <- income_df$occupation
    
    #Binding relationship to newInc
    newInc_df$relationship <- income_df$relationship
    
    #Binding race to newInc
    newInc_df$race <- income_df$race
    
    #Binding gender to gender
    newInc_df$gender <- income_df$gender
    
    #normalize capital gain-loss using net
    
    net_capital<-vector(mode ="integer",length = income_row)
    
    find_zscore_net_capital<-function(){
      
      min_cap<-max(income_df[["capital_loss"]])
      max_cap<-max(income_df[["capital_gain"]])
      
      for(j in 1:income_row){
        net_capital[j]=income_df[["capital_gain"]][j]-income_df[["capital_loss"]][j]
        #    print(net_capital[j])
      }
      
      mean_cap<-mean(net_capital)
      sd_cap<-sd(net_capital)
      
      for(j in 1:income_row){
        net_capital[j]=(net_capital[j]-mean_cap)/sd_cap
      }
      
      return(net_capital)
    }
    
    net_capital<-find_zscore_net_capital()

    #Binding relationship to newInc
    newInc_df <- cbind(newInc_df,net_capital)
    
    
    #normalize hour_per_week based on divided factor 10 
    
    hour_per_week<-vector(mode ="integer",length = income_row)
    
    zscore_normalize_hour_per_week<-function(){
      
      mean_hour_per_week<-mean(income_df$hour_per_week)
      sd_hour_per_week<-mean(income_df$hour_per_week)
      
      for(j in 1:income_row){
        hour_per_week[j]=(income_df[["age"]][j]-mean_hour_per_week)/sd_hour_per_week
      }
      return(hour_per_week)
    }
    
    hour_per_week<-zscore_normalize_hour_per_week()
    
    #Binding hour_per_week to newInc
    newInc_df <- cbind(newInc_df,hour_per_week)
    
    #Binding native_country to newInc
    newInc_df$native_country <- income_df$native_country
    
    #----------------------------------------------------
    
   
    
    convert_df_to_matrix<-function()
    {
      
      size<-dim(newInc_df)
      income_row<-size[1]
      income_col<-size[2]
      print(income_row)
      print(income_col)
      
      data_mat<-matrix(0L,nrow=income_row,ncol=income_col+1)
      
      colnames(data_mat)<-c("transID","age","workclass","education_cat","marital_status","occupation","relationship","race","gender","net_capital","hour_per_week","native_country")
      
      y<-rownames(newInc_df)
      y<-as.numeric(y)
      data_mat[,1]<-cbind(y)
      y<-as.integer(factor(newInc_df$age))
      data_mat[,2]<-cbind(y)
      y<-as.integer(factor(newInc_df$workclass))
      data_mat[,3]<-cbind(y)
      data_mat[,4]<-cbind(newInc_df$education_cat)
      y<-as.integer(factor(newInc_df$marital_status))
      data_mat[,5]<-cbind(y)
      y<-as.integer(factor(newInc_df$occupation))
      data_mat[,6]<-cbind(y)
      y<-as.integer(factor(newInc_df$relationship))
      data_mat[,7]<-cbind(y)
      y<-as.integer(factor(newInc_df$race))
      data_mat[,8]<-cbind(y)
      y<-as.integer(factor(newInc_df$gender))
      data_mat[,9]<-cbind(y)
      data_mat[,10]<-cbind(newInc_df$net_capital)
      data_mat[,11]<-cbind(newInc_df$hour_per_week)
      y<-as.integer(factor(newInc_df$native_country))
      data_mat[,12]<-cbind(y)
      
      return(data_mat)
    }
    
    data_mat <- convert_df_to_matrix()
    
    
    
    ##------------------------------------------------------------
    
    #Distance Calculation
    
    #Euclid distance for income data
    
    eu_mat <- matrix(0L,nrow = income_row,ncol = 11,byrow = T)
    colnames(eu_mat) <- c("Transaction ID","1st","1-dist","2nd","2-dist","3rd","3-dist","4","4-dist","5","5-dist")
  
    age_levels <- nlevels(newInc_df$age)
    education_cat_levels <- max(newInc_df$education_cat)
   
    euclid_income<-function()
    {
      #First data point with all others
      #for(i in 1:income_row)
      for(i in 1:income_row)
      {	
        
        least_k_dist<-matrix(c(1,100,2,100,3,100,4,100,5,100),nrow=5,ncol=2,byrow=T)
       # least_k_dist<-round(least_k_dist,digits=3)
       
        colnames(least_k_dist) <- c("index","euclid distance")
        for(j in 1:income_row)
        {
          if(i!=j)
          {
            
            dist_income<-vector(mode ="integer",length = 12)
            
            dist_income[1] <- (((data_mat[i,2]-data_mat[j,2])/age_levels)^2)
  
            if(data_mat[i,3] != data_mat[j,3])
                dist_income[2]=1
            
            dist_income[3] <- (((data_mat[i,4] - data_mat[j,4])/education_cat_levels)^2)
            
            if(data_mat[i,5] != data_mat[j,5])
              dist_income[4]=1
            
            if(data_mat[i,6] != data_mat[j,6])
              dist_income[5]=1
            
            if(data_mat[i,7] != data_mat[j,7])
              dist_income[6]=1
            
            if(data_mat[i,8] != data_mat[j,8])
              dist_income[7]=1
            
            if(data_mat[i,9] != data_mat[j,9])
              dist_income[8]=1
            
            dist_income[9] <- ((data_mat[i,10] - data_mat[j,10])^2) 
            
            dist_income[10] <- ((data_mat[i,11] - data_mat[j,11])^2) 
            
            if(data_mat[i,12] != data_mat[j,12])
              dist_income[11]=1
            
            dist_income<-round(dist_income,digits=3)
            
            d <- sum(dist_income)^0.5
  
           # d<-round(d,digits=3)
            
            if(least_k_dist[5,2]>d)
            {
              least_k_dist[5,2]=d
              least_k_dist[5,1]=data_mat[j,1];
              least_k_dist<-least_k_dist[order(least_k_dist[,2]),]
            }
            
          }
        }
        
        ## Now to save it in a result matrix
        
        eu_mat[i,1]<- data_mat[i,1]
        eu_mat[i,2]<- least_k_dist[1,1]
        eu_mat[i,3]<- least_k_dist[1,2]
        eu_mat[i,4]<- least_k_dist[2,1]
        eu_mat[i,5]<- least_k_dist[2,2]
        eu_mat[i,6]<- least_k_dist[3,1]
        eu_mat[i,7]<- least_k_dist[3,2]
        eu_mat[i,8]<- least_k_dist[4,1]
        eu_mat[i,9]<- least_k_dist[4,2]
        eu_mat[i,10]<- least_k_dist[5,1]
        eu_mat[i,11]<- least_k_dist[5,2]
      }
      return(eu_mat)
    }
    
    eu_mat<-euclid_income()
    
    eu <- Sys.time()
  
  ##--------------------------------------------------------------------------
    #Manhattan distance for income data
    
    man_mat <- matrix(0L,nrow = income_row,ncol = 11,byrow = T)
    colnames(man_mat) <- c("Transaction ID","1st","1-dist","2nd","2-dist","3rd","3-dist","4","4-dist","5","5-dist")
    
    #transID <- rownames(newInc_df)
    
    manhattan_income<-function()
    {
      #First data point with all others
      for(i in 1:income_row)
      {	
        
        least_k_dist<-matrix(c(1,100,2,100,3,100,4,100,5,100),nrow=5,ncol=2,byrow=T)
       # least_k_dist<-round(least_k_dist,digits=3)
        
        colnames(least_k_dist) <- c("index","manhattan distance")
        for(j in 1:income_row)
        {
          if(i!=j)
          {
            
            dist_income<-vector(mode ="integer",length = 12)
            
            dist_income[1] <- abs(data_mat[i,2]-data_mat[j,2])/age_levels
            
            if(data_mat[i,3] != data_mat[j,3])
              dist_income[2]=1
            
            dist_income[3] <- abs(data_mat[i,4] - data_mat[j,4])/education_cat_levels
            

            if(data_mat[i,5] != data_mat[j,5])
              dist_income[4]=1
            
            if(data_mat[i,6] != data_mat[j,6])
              dist_income[5]=1
            
            if(data_mat[i,7] != data_mat[j,7])
              dist_income[6]=1
            
            if(data_mat[i,8] != data_mat[j,8])
              dist_income[7]=1
            
            if(data_mat[i,9] != data_mat[j,9])
              dist_income[8]=1
            
            dist_income[9] <- abs(data_mat[i,10] - data_mat[j,10])
            
            dist_income[10] <- abs(data_mat[i,11] - data_mat[j,11])
            
            if(data_mat[i,12] != data_mat[j,12])
              dist_income[11]=1
            
            dist_income<-round(dist_income,digits=3)
            
            d <- sum(dist_income)
            
            #d<-round(d,digits=3)
            
            if(least_k_dist[5,2]>d)
            {
              least_k_dist[5,2]=d
              least_k_dist[5,1]=data_mat[j,1];
              least_k_dist<-least_k_dist[order(least_k_dist[,2]),]
            }
          }
        }
        
        ## Now to save it in a result matrix
        man_mat[i,1]<- data_mat[i,1]
        man_mat[i,2]<- least_k_dist[1,1]
        man_mat[i,3]<- least_k_dist[1,2]
        man_mat[i,4]<- least_k_dist[2,1]
        man_mat[i,5]<- least_k_dist[2,2]
        man_mat[i,6]<- least_k_dist[3,1]
        man_mat[i,7]<- least_k_dist[3,2]
        man_mat[i,8]<- least_k_dist[4,1]
        man_mat[i,9]<- least_k_dist[4,2]
        man_mat[i,10]<- least_k_dist[5,1]
        man_mat[i,11]<- least_k_dist[5,2]
      }
      return(man_mat)
    }
    
    man_mat<-manhattan_income()
    
    
    #Load xls lib for exporting distances to csv files 
    library(xlsx)
    write.csv(eu_mat,file="euclid_income.csv")
    write.csv(man_mat,file="manhattan_income.csv")