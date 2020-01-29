# Kallesh_Flowcytometry

## PACKAGES (INSTALLATION AND LOADING)
install.packages ("scatterplot3d")
library ("scatterplot3d")

## DATA MANIPULATIN
### Import and view data through the environment window in RStudio
### Re-arrange, rename, & normalize (percentile transformation) patient data

P12_Pre <- P12_Pre [-1,c(7,14,15)]            #delete 1st row #select 7,14,15 columns
colnames (P12_Pre) <- c("MCL","BCL","IgG")    #rename column names
P12_Pre$Stage <- "Red"                        #new column with indicator color (pre/post)
P12_Post <- P12_Post [-1,c(7,14,15)]          #delete 1st row #select 7,14,15 columns
colnames (P12_Post) <- c("MCL","BCL","IgG")   #rename column names
P12_Post$Stage <- "Blue"                      #new column with indicator color (pre/post)
P12 <- rbind (P12_Pre, P12_Post)              #binding (merging) the pre and post data
P12$MCL <- as.numeric(as.character(P12$MCL))  #Converting MCL from factor into numerical
P12$BCL <- as.numeric(as.character(P12$BCL))  #Converting BCL from factor into numerical
P12$IgG <- as.numeric(as.character(P12$IgG))  #Converting IgG from factor into numerical
P12$MCL_p <- ecdf(P12$MCL)(P12$MCL)           #Creating a percentile variable for MCL
P12$BCL_p <- ecdf(P12$BCL)(P12$BCL)           #Creating a percentile variable for MCL
P12$IgG_p <- ecdf(P12$IgG)(P12$IgG)           #Creating a percentile variable for MCL

P21_Pre <- P21_Pre [-1,c(7,14,15)]            #Similar data manipulation for P21
colnames (P21_Pre) <- c("MCL","BCL","IgG")    
P21_Pre$Stage <- "Red"                        
P21_Post <- P21_Post [-1,c(7,14,15)]          
colnames (P21_Post) <- c("MCL","BCL","IgG")   
P21_Post$Stage <- "Blue"                      
P21 <- rbind (P21_Pre, P21_Post)              
P21$MCL <- as.numeric(as.character(P21$MCL))  
P21$BCL <- as.numeric(as.character(P21$BCL))  
P21$IgG <- as.numeric(as.character(P21$IgG))  
P21$MCL_p <- ecdf(P21$MCL)(P21$MCL)           
P21$BCL_p <- ecdf(P21$BCL)(P21$BCL)           
P21$IgG_p <- ecdf(P21$IgG)(P21$IgG)  

P29_Pre <- P29_Pre [-1,c(7,14,15)]            #Similar data manipulation for P29
colnames (P29_Pre) <- c("MCL","BCL","IgG")    
P29_Pre$Stage <- "Red"                        
P29_Post <- P29_Post [-1,c(7,14,15)]          
colnames (P29_Post) <- c("MCL","BCL","IgG")   
P29_Post$Stage <- "Blue"                      
P29 <- rbind (P29_Pre, P29_Post)              
P29$MCL <- as.numeric(as.character(P29$MCL))  
P29$BCL <- as.numeric(as.character(P29$BCL))  
P29$IgG <- as.numeric(as.character(P29$IgG))  
P29$MCL_p <- ecdf(P29$MCL)(P29$MCL)           
P29$BCL_p <- ecdf(P29$BCL)(P29$BCL)           
P29$IgG_p <- ecdf(P29$IgG)(P29$IgG)  

P31_Pre <- P31_Pre [-1,c(7,14,15)]            #Similar data manipulation for P31
colnames (P31_Pre) <- c("MCL","BCL","IgG")    
P31_Pre$Stage <- "Red"                        
P31_Post <- P31_Post [-1,c(7,14,15)]          
colnames (P31_Post) <- c("MCL","BCL","IgG")   
P31_Post$Stage <- "Blue"                      
P31 <- rbind (P31_Pre, P31_Post)              
P31$MCL <- as.numeric(as.character(P31$MCL))  
P31$BCL <- as.numeric(as.character(P31$BCL))  
P31$IgG <- as.numeric(as.character(P31$IgG))  
P31$MCL_p <- ecdf(P31$MCL)(P31$MCL)           
P31$BCL_p <- ecdf(P31$BCL)(P31$BCL)           
P31$IgG_p <- ecdf(P31$IgG)(P31$IgG)  

P71_Pre <- P71_Pre [-1,c(7,14,15)]            #Similar data manipulation for P71
colnames (P71_Pre) <- c("MCL","BCL","IgG")    
P71_Pre$Stage <- "Red"                        
P71_Post <- P71_Post [-1,c(7,14,15)]          
colnames (P71_Post) <- c("MCL","BCL","IgG")   
P71_Post$Stage <- "Blue"                      
P71 <- rbind (P71_Pre, P71_Post)              
P71$MCL <- as.numeric(as.character(P71$MCL))  
P71$BCL <- as.numeric(as.character(P71$BCL))  
P71$IgG <- as.numeric(as.character(P71$IgG))  
P71$MCL_p <- ecdf(P71$MCL)(P71$MCL)           
P71$BCL_p <- ecdf(P71$BCL)(P71$BCL)           
P71$IgG_p <- ecdf(P71$IgG)(P71$IgG)  

All <- rbind (P12,P21,P29,P31,P71)              #binding (merging) the multiple patient data
All$MCL_r <- within (All, All$MCL_r <- ifelse (MCL_p<=0.33, 'L',
                            ifelse (MCL_p>0.66, 'H',
                              ifelse ('M'))))




## 3D SCATTERPLOTS

### Individual & All Patients with non-transformed data

library (scatterplot3d)
with(P12,
   scatterplot3d(MCL,
                 BCL,
                 IgG,
                 color=Stage,
                 main="Patient12 with non-transformed data",
                 xlab="MCL",
                 ylab="BCL",
                 zlab="IgG"
                 ))
                 

library (scatterplot3d)
with(P21,
   scatterplot3d(MCL,
                 BCL,
                 IgG,
                 color=Stage,
                 main="Patient21 with non-transformed data",
                 xlab="MCL",
                 ylab="BCL",
                 zlab="IgG"
                 ))


library (scatterplot3d)
with(P29,
   scatterplot3d(MCL,
                 BCL,
                 IgG,
                 color=Stage,
                 main="Patient29 with non-transformed data",
                 xlab="MCL",
                 ylab="BCL",
                 zlab="IgG"
                 ))

library (scatterplot3d)
with(P31,
   scatterplot3d(MCL,
                 BCL,
                 IgG,
                 color=Stage,
                 main="Patient31 with non-transformed data",
                 xlab="MCL",
                 ylab="BCL",
                 zlab="IgG"
                 ))
                 
library (scatterplot3d)
with(P71,
   scatterplot3d(MCL,
                 BCL,
                 IgG,
                 color=Stage,
                 main="Patient71 with non-transformed data",
                 xlab="MCL",
                 ylab="BCL",
                 zlab="IgG"
                 ))
                 
library (scatterplot3d)
with(All,
   scatterplot3d(MCL,
                 BCL,
                 IgG,
                 color=Stage,
                 main="Patients (all) with non-transformed data",
                 xlab="MCL",
                 ylab="BCL",
                 zlab="IgG"
                 ))

### Individual & All Patients with percentile-transformed data

library (scatterplot3d)
with(P12,
   scatterplot3d(MCL_p,
                 BCL_p,
                 IgG_p,
                 color=Stage,
                 main="Patient12 with percentile-transformed data",
                 xlab="MCL_p",
                 ylab="BCL_p",
                 zlab="IgG_p"
                 ))
                 

library (scatterplot3d)
with(P21,
   scatterplot3d(MCL_p,
                 BCL_p,
                 IgG_p,
                 color=Stage,
                 main="Patient21 with percentile-transformed data",
                 xlab="MCL_p",
                 ylab="BCL_p",
                 zlab="IgG_p"
                 ))


library (scatterplot3d)
with(P29,
   scatterplot3d(MCL_p,
                 BCL_p,
                 IgG_p,
                 color=Stage,
                 main="Patient29 with percentile-transformed data",
                 xlab="MCL_p",
                 ylab="BCL_p",
                 zlab="IgG_p"
                 ))

library (scatterplot3d)
with(P31,
   scatterplot3d(MCL_p,
                 BCL_p,
                 IgG_p,
                 color=Stage,
                 main="Patient31 with percentile-transformed data",
                 xlab="MCL_p",
                 ylab="BCL_p",
                 zlab="IgG_p"
                 ))

library (scatterplot3d)
with(P71,
   scatterplot3d(MCL_p,
                 BCL_p,
                 IgG_p,
                 color=Stage,
                 main="Patient71 with percentile-transformed data",
                 xlab="MCL_p",
                 ylab="BCL_p",
                 zlab="IgG_p"
                 ))

library (scatterplot3d)
with(All,
   scatterplot3d(MCL_p,
                 BCL_p,
                 IgG_p,
                 color=Stage,
                 main="Patients (all) with percentile-transformed data",
                 xlab="MCL_p",
                 ylab="BCL_p",
                 zlab="IgG_p"
                 ))



