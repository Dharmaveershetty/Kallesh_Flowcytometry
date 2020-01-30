# Kallesh_Flowcytometry

## PACKAGES (INSTALLATION AND LOADING)
install.packages ("scatterplot3d")
library ("scatterplot3d")
install.packages ("tidyverse")
library("tidyverse")
install.packages("Rmisc")
library("Rmisc")

## DATA MANIPULATION
### Import and view data through the environment window in RStudio
### Re-arrange, rename, & normalize (percentile transformation) patient data

P12_Pre <- P12_Pre [-1,c(7,14,15)]             #delete 1st row #select 7,14,15 columns
colnames (P12_Pre) <- c("MCL","BCL","IgG")     #rename column names
P12_Pre$Stage <- "Red"                         #new column with indicator color (pre/post)
P12_Post <- P12_Post [-1,c(7,14,15)]           #delete 1st row #select 7,14,15 columns
colnames (P12_Post) <- c("MCL","BCL","IgG")    #rename column names
P12_Post$Stage <- "Blue"                       #new column with indicator color (pre/post)
P12 <- rbind (P12_Pre, P12_Post)               #binding (merging) the pre and post data
P12$Patient <- "P12"                           #Creating Patient identifier variable
P12$MCL <- as.numeric(as.character(P12$MCL))   #Converting MCL from factor into numerical
P12$BCL <- as.numeric(as.character(P12$BCL))   #Converting BCL from factor into numerical
P12$IgG <- as.numeric(as.character(P12$IgG))   #Converting IgG from factor into numerical
P12$MCL_p <- ecdf(P12$MCL)(P12$MCL)            #Creating a percentile variable for MCL
P12$BCL_p <- ecdf(P12$BCL)(P12$BCL)            #Creating a percentile variable for MCL
P12$IgG_p <- ecdf(P12$IgG)(P12$IgG)            #Creating a percentile variable for MCL
P12$MCL_r <- ifelse(P12$MCL_p <= 0.33, 'Low',  #Categorizing data
         ifelse(P12$MCL_p <= 0.66, 'Medium', 
         'High'))
P12$BCL_r <- ifelse(P12$BCL_p <= 0.33, 'Low',   
         ifelse(P12$BCL_p <= 0.66, 'Medium', 
         'High'))
P12$IgG_r <- ifelse(P12$IgG_p <= 0.33, 'Low',  
         ifelse(P12$IgG_p <= 0.66, 'Medium', 
         'High'))

P21_Pre <- P21_Pre [-1,c(7,14,15)]            #Similar data manipulation for P21
colnames (P21_Pre) <- c("MCL","BCL","IgG")    
P21_Pre$Stage <- "Red"                        
P21_Post <- P21_Post [-1,c(7,14,15)]          
colnames (P21_Post) <- c("MCL","BCL","IgG")   
P21_Post$Stage <- "Blue"                      
P21 <- rbind (P21_Pre, P21_Post) 
P21$Patient <- "P21"
P21$MCL <- as.numeric(as.character(P21$MCL))  
P21$BCL <- as.numeric(as.character(P21$BCL))  
P21$IgG <- as.numeric(as.character(P21$IgG))  
P21$MCL_p <- ecdf(P21$MCL)(P21$MCL)           
P21$BCL_p <- ecdf(P21$BCL)(P21$BCL)           
P21$IgG_p <- ecdf(P21$IgG)(P21$IgG)
P21$MCL_r <- ifelse(P21$MCL_p <= 0.33, 'Low',
         ifelse(P21$MCL_p <= 0.66, 'Medium', 
         'High'))
P21$BCL_r <- ifelse(P21$BCL_p <= 0.33, 'Low',   
         ifelse(P21$BCL_p <= 0.66, 'Medium', 
         'High'))
P21$IgG_r <- ifelse(P21$IgG_p <= 0.33, 'Low',  
         ifelse(P21$IgG_p <= 0.66, 'Medium', 
         'High'))

P29_Pre <- P29_Pre [-1,c(7,14,15)]            #Similar data manipulation for P29
colnames (P29_Pre) <- c("MCL","BCL","IgG")    
P29_Pre$Stage <- "Red"                        
P29_Post <- P29_Post [-1,c(7,14,15)]          
colnames (P29_Post) <- c("MCL","BCL","IgG")   
P29_Post$Stage <- "Blue"                      
P29 <- rbind (P29_Pre, P29_Post)
P29$Patient <- "P29"
P29$MCL <- as.numeric(as.character(P29$MCL))  
P29$BCL <- as.numeric(as.character(P29$BCL))  
P29$IgG <- as.numeric(as.character(P29$IgG))  
P29$MCL_p <- ecdf(P29$MCL)(P29$MCL)           
P29$BCL_p <- ecdf(P29$BCL)(P29$BCL)           
P29$IgG_p <- ecdf(P29$IgG)(P29$IgG)  
P29$MCL_r <- ifelse(P29$MCL_p <= 0.33, 'Low',
         ifelse(P29$MCL_p <= 0.66, 'Medium', 
         'High'))
P29$BCL_r <- ifelse(P29$BCL_p <= 0.33, 'Low',   
         ifelse(P29$BCL_p <= 0.66, 'Medium', 
         'High'))
P29$IgG_r <- ifelse(P29$IgG_p <= 0.33, 'Low',  
         ifelse(P29$IgG_p <= 0.66, 'Medium', 
         'High'))

P31_Pre <- P31_Pre [-1,c(7,14,15)]              #Similar data manipulation for P31
colnames (P31_Pre) <- c("MCL","BCL","IgG")    
P31_Pre$Stage <- "Red"                        
P31_Post <- P31_Post [-1,c(7,14,15)]          
colnames (P31_Post) <- c("MCL","BCL","IgG")   
P31_Post$Stage <- "Blue"                      
P31 <- rbind (P31_Pre, P31_Post)   
P31$Patient <- "P31"
P31$MCL <- as.numeric(as.character(P31$MCL))  
P31$BCL <- as.numeric(as.character(P31$BCL))  
P31$IgG <- as.numeric(as.character(P31$IgG))  
P31$MCL_p <- ecdf(P31$MCL)(P31$MCL)           
P31$BCL_p <- ecdf(P31$BCL)(P31$BCL)           
P31$IgG_p <- ecdf(P31$IgG)(P31$IgG)
P31$MCL_r <- ifelse(P31$MCL_p <= 0.33, 'Low',
         ifelse(P31$MCL_p <= 0.66, 'Medium', 
         'High'))
P31$BCL_r <- ifelse(P31$BCL_p <= 0.33, 'Low',   
         ifelse(P31$BCL_p <= 0.66, 'Medium', 
         'High'))
P31$IgG_r <- ifelse(P31$IgG_p <= 0.33, 'Low',  
         ifelse(P31$IgG_p <= 0.66, 'Medium', 
         'High'))

P71_Pre <- P71_Pre [-1,c(7,14,15)]              #Similar data manipulation for P71
colnames (P71_Pre) <- c("MCL","BCL","IgG")    
P71_Pre$Stage <- "Red"                        
P71_Post <- P71_Post [-1,c(7,14,15)]          
colnames (P71_Post) <- c("MCL","BCL","IgG")   
P71_Post$Stage <- "Blue"                      
P71 <- rbind (P71_Pre, P71_Post)
P71$Patient <- "P71"
P71$MCL <- as.numeric(as.character(P71$MCL))  
P71$BCL <- as.numeric(as.character(P71$BCL))  
P71$IgG <- as.numeric(as.character(P71$IgG))  
P71$MCL_p <- ecdf(P71$MCL)(P71$MCL)           
P71$BCL_p <- ecdf(P71$BCL)(P71$BCL)           
P71$IgG_p <- ecdf(P71$IgG)(P71$IgG)
P71$MCL_r <- ifelse(P71$MCL_p <= 0.33, 'Low',
         ifelse(P71$MCL_p <= 0.66, 'Medium', 
         'High'))
P71$BCL_r <- ifelse(P71$BCL_p <= 0.33, 'Low',   
         ifelse(P71$BCL_p <= 0.66, 'Medium', 
         'High'))
P71$IgG_r <- ifelse(P71$IgG_p <= 0.33, 'Low',  
         ifelse(P71$IgG_p <= 0.66, 'Medium', 
         'High'))

All <- rbind (P12,P21,P29,P31,P71)              #binding (merging) the multiple patient data


## 3D SCATTERPLOTS (CONTINUOUS DATA)

### Patients (Individual & combined) with non-transformed data

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

### Patients (Individual & combined) with percentile-transformed data

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



## STACKED COLUMN CHART (CATEGORICAL DATA)

### Patient12
#### Categorization and summation 
P12_MCL_Pre_H <- sum (P12$Stage == "Red" & P12$MCL_r == "High")
P12_MCL_Pre_M <- sum (P12$Stage == "Red" & P12$MCL_r == "Medium")
P12_MCL_Pre_L <- sum (P12$Stage=="Red" & P12$MCL_r=="Low")
P12_MCL_Post_H <- sum (P12$Stage=="Blue" & P12$MCL_r=="High")
P12_MCL_Post_M <- sum (P12$Stage=="Blue" & P12$MCL_r=="Medium")
P12_MCL_Post_L <- sum (P12$Stage=="Blue" & P12$MCL_r=="Low")
P12_BCL_Pre_H <- sum (P12$Stage == "Red" & P12$BCL_r == "High")
P12_BCL_Pre_M <- sum (P12$Stage == "Red" & P12$BCL_r == "Medium")
P12_BCL_Pre_L <- sum (P12$Stage=="Red" & P12$BCL_r=="Low")
P12_BCL_Post_H <- sum (P12$Stage=="Blue" & P12$BCL_r=="High")
P12_BCL_Post_M <- sum (P12$Stage=="Blue" & P12$BCL_r=="Medium")
P12_BCL_Post_L <- sum (P12$Stage=="Blue" & P12$BCL_r=="Low")
P12_IgG_Pre_H <- sum (P12$Stage == "Red" & P12$IgG_r == "High")
P12_IgG_Pre_M <- sum (P12$Stage == "Red" & P12$IgG_r == "Medium")
P12_IgG_Pre_L <- sum (P12$Stage=="Red" & P12$IgG_r=="Low")
P12_IgG_Post_H <- sum (P12$Stage=="Blue" & P12$IgG_r=="High")
P12_IgG_Post_M <- sum (P12$Stage=="Blue" & P12$IgG_r=="Medium")
P12_IgG_Post_L <- sum (P12$Stage=="Blue" & P12$IgG_r=="Low")
#### Creating a data.frame
P12_protein=c(rep("MCL",6), rep("BCL",6), rep("IgG",6))
P12_stage=c(rep("i.Pre",3), rep ("ii.During",3))
P12_level=rep(c("a.high", "b.medium", "c.low"),2)
P12_value=c(P12_MCL_Pre_H, P12_MCL_Pre_M, P12_MCL_Pre_L,
            P12_MCL_Post_H, P12_MCL_Post_M, P12_MCL_Post_L,
            P12_BCL_Pre_H, P12_BCL_Pre_M, P12_BCL_Pre_L,
            P12_BCL_Post_H, P12_BCL_Post_M, P12_BCL_Post_L,
            P12_IgG_Pre_H, P12_IgG_Pre_M, P12_IgG_Pre_L,
            P12_IgG_Post_H, P12_IgG_Post_M, P12_IgG_Post_L)
P12_cat=data.frame(P12_protein,P12_stage, P12_level, P12_value)
#### Creating a subset
P12_cat_MCL <- subset (P12_cat, P12_protein=="MCL",
                     select=P12_protein:P12_value)
P12_cat_BCL <- subset (P12_cat, P12_protein=="BCL",
                     select=P12_protein:P12_value)
P12_cat_IgG <- subset (P12_cat, P12_protein=="IgG",
                     select=P12_protein:P12_value)
#### Plotting multiple plots in the same page
P1a <- ggplot (P12_cat_MCL, aes(fill=P12_level, y=P12_value, x=P12_stage)) + 
    geom_bar( stat="identity", position="fill")
P1 <- P1a + labs(title = "MCL") + theme(legend.position="none") + xlab(NULL) + ylab(NULL)
P2a <- ggplot (P12_cat_BCL, aes(fill=P12_level, y=P12_value, x=P12_stage)) + 
    geom_bar( stat="identity", position="fill")
P2 <- P2a + labs(title = "BCL") + theme(legend.position="none") + xlab(NULL) + ylab(NULL)
P3a <- ggplot (P12_cat_IgG, aes(fill=P12_level, y=P12_value, x=P12_stage)) + 
    geom_bar( stat="identity", position="fill")
P3 <- P3a + labs(title = "IgG") + xlab(NULL) + ylab(NULL)
multiplot(P1, P2, P3, cols=3)

### Patient21
#### Categorization and summation 
P21_MCL_Pre_H <- sum (P21$Stage == "Red" & P21$MCL_r == "High")
P21_MCL_Pre_M <- sum (P21$Stage == "Red" & P21$MCL_r == "Medium")
P21_MCL_Pre_L <- sum (P21$Stage=="Red" & P21$MCL_r=="Low")
P21_MCL_Post_H <- sum (P21$Stage=="Blue" & P21$MCL_r=="High")
P21_MCL_Post_M <- sum (P21$Stage=="Blue" & P21$MCL_r=="Medium")
P21_MCL_Post_L <- sum (P21$Stage=="Blue" & P21$MCL_r=="Low")
P21_BCL_Pre_H <- sum (P21$Stage == "Red" & P21$BCL_r == "High")
P21_BCL_Pre_M <- sum (P21$Stage == "Red" & P21$BCL_r == "Medium")
P21_BCL_Pre_L <- sum (P21$Stage=="Red" & P21$BCL_r=="Low")
P21_BCL_Post_H <- sum (P21$Stage=="Blue" & P21$BCL_r=="High")
P21_BCL_Post_M <- sum (P21$Stage=="Blue" & P21$BCL_r=="Medium")
P21_BCL_Post_L <- sum (P21$Stage=="Blue" & P21$BCL_r=="Low")
P21_IgG_Pre_H <- sum (P21$Stage == "Red" & P21$IgG_r == "High")
P21_IgG_Pre_M <- sum (P21$Stage == "Red" & P21$IgG_r == "Medium")
P21_IgG_Pre_L <- sum (P21$Stage=="Red" & P21$IgG_r=="Low")
P21_IgG_Post_H <- sum (P21$Stage=="Blue" & P21$IgG_r=="High")
P21_IgG_Post_M <- sum (P21$Stage=="Blue" & P21$IgG_r=="Medium")
P21_IgG_Post_L <- sum (P21$Stage=="Blue" & P21$IgG_r=="Low")
#### Creating a data.frame
P21_protein=c(rep("MCL",6), rep("BCL",6), rep("IgG",6))
P21_stage=c(rep("i.Pre",3), rep ("ii.During",3))
P21_level=rep(c("a.high", "b.medium", "c.low"),2)
P21_value=c(P21_MCL_Pre_H, P21_MCL_Pre_M, P21_MCL_Pre_L,
            P21_MCL_Post_H, P21_MCL_Post_M, P21_MCL_Post_L,
            P21_BCL_Pre_H, P21_BCL_Pre_M, P21_BCL_Pre_L,
            P21_BCL_Post_H, P21_BCL_Post_M, P21_BCL_Post_L,
            P21_IgG_Pre_H, P21_IgG_Pre_M, P21_IgG_Pre_L,
            P21_IgG_Post_H, P21_IgG_Post_M, P21_IgG_Post_L)
P21_cat=data.frame(P21_protein,P21_stage, P21_level, P21_value)
#### Creating a subset
P21_cat_MCL <- subset (P21_cat, P21_protein=="MCL",
                     select=P21_protein:P21_value)
P21_cat_BCL <- subset (P21_cat, P21_protein=="BCL",
                     select=P21_protein:P21_value)
P21_cat_IgG <- subset (P21_cat, P21_protein=="IgG",
                     select=P21_protein:P21_value)
#### Plotting multiple plots in the same page
P1a <- ggplot (P21_cat_MCL, aes(fill=P21_level, y=P21_value, x=P21_stage)) + 
    geom_bar( stat="identity", position="fill")
P1 <- P1a + labs(title = "MCL") + theme(legend.position="none") + xlab(NULL) + ylab(NULL)
P2a <- ggplot (P21_cat_BCL, aes(fill=P21_level, y=P21_value, x=P21_stage)) + 
    geom_bar( stat="identity", position="fill")
P2 <- P2a + labs(title = "BCL") + theme(legend.position="none") + xlab(NULL) + ylab(NULL)
P3a <- ggplot (P21_cat_IgG, aes(fill=P21_level, y=P21_value, x=P21_stage)) + 
    geom_bar( stat="identity", position="fill")
P3 <- P3a + labs(title = "IgG") + xlab(NULL) + ylab(NULL)
multiplot(P1, P2, P3, cols=3)

### Patient31
#### Categorization and summation 
P31_MCL_Pre_H <- sum (P31$Stage == "Red" & P31$MCL_r == "High")
P31_MCL_Pre_M <- sum (P31$Stage == "Red" & P31$MCL_r == "Medium")
P31_MCL_Pre_L <- sum (P31$Stage=="Red" & P31$MCL_r=="Low")
P31_MCL_Post_H <- sum (P31$Stage=="Blue" & P31$MCL_r=="High")
P31_MCL_Post_M <- sum (P31$Stage=="Blue" & P31$MCL_r=="Medium")
P31_MCL_Post_L <- sum (P31$Stage=="Blue" & P31$MCL_r=="Low")
P31_BCL_Pre_H <- sum (P31$Stage == "Red" & P31$BCL_r == "High")
P31_BCL_Pre_M <- sum (P31$Stage == "Red" & P31$BCL_r == "Medium")
P31_BCL_Pre_L <- sum (P31$Stage=="Red" & P31$BCL_r=="Low")
P31_BCL_Post_H <- sum (P31$Stage=="Blue" & P31$BCL_r=="High")
P31_BCL_Post_M <- sum (P31$Stage=="Blue" & P31$BCL_r=="Medium")
P31_BCL_Post_L <- sum (P31$Stage=="Blue" & P31$BCL_r=="Low")
P31_IgG_Pre_H <- sum (P31$Stage == "Red" & P31$IgG_r == "High")
P31_IgG_Pre_M <- sum (P31$Stage == "Red" & P31$IgG_r == "Medium")
P31_IgG_Pre_L <- sum (P31$Stage=="Red" & P31$IgG_r=="Low")
P31_IgG_Post_H <- sum (P31$Stage=="Blue" & P31$IgG_r=="High")
P31_IgG_Post_M <- sum (P31$Stage=="Blue" & P31$IgG_r=="Medium")
P31_IgG_Post_L <- sum (P31$Stage=="Blue" & P31$IgG_r=="Low")
#### Creating a data.frame
P31_protein=c(rep("MCL",6), rep("BCL",6), rep("IgG",6))
P31_stage=c(rep("i.Pre",3), rep ("ii.During",3))
P31_level=rep(c("a.high", "b.medium", "c.low"),2)
P31_value=c(P31_MCL_Pre_H, P31_MCL_Pre_M, P31_MCL_Pre_L,
            P31_MCL_Post_H, P31_MCL_Post_M, P31_MCL_Post_L,
            P31_BCL_Pre_H, P31_BCL_Pre_M, P31_BCL_Pre_L,
            P31_BCL_Post_H, P31_BCL_Post_M, P31_BCL_Post_L,
            P31_IgG_Pre_H, P31_IgG_Pre_M, P31_IgG_Pre_L,
            P31_IgG_Post_H, P31_IgG_Post_M, P31_IgG_Post_L)
P31_cat=data.frame(P31_protein,P31_stage, P31_level, P31_value)
#### Creating a subset
P31_cat_MCL <- subset (P31_cat, P31_protein=="MCL",
                     select=P31_protein:P31_value)
P31_cat_BCL <- subset (P31_cat, P31_protein=="BCL",
                     select=P31_protein:P31_value)
P31_cat_IgG <- subset (P31_cat, P31_protein=="IgG",
                     select=P31_protein:P31_value)
#### Plotting multiple plots in the same page
P1a <- ggplot (P31_cat_MCL, aes(fill=P31_level, y=P31_value, x=P31_stage)) + 
    geom_bar( stat="identity", position="fill")
P1 <- P1a + labs(title = "MCL") + theme(legend.position="none") + xlab(NULL) + ylab(NULL)
P2a <- ggplot (P31_cat_BCL, aes(fill=P31_level, y=P31_value, x=P31_stage)) + 
    geom_bar( stat="identity", position="fill")
P2 <- P2a + labs(title = "BCL") + theme(legend.position="none") + xlab(NULL) + ylab(NULL)
P3a <- ggplot (P31_cat_IgG, aes(fill=P31_level, y=P31_value, x=P31_stage)) + 
    geom_bar( stat="identity", position="fill")
P3 <- P3a + labs(title = "IgG") + xlab(NULL) + ylab(NULL)
multiplot(P1, P2, P3, cols=3)

### Patient12
#### Categorization and summation 
P12_MCL_Pre_H <- sum (P12$Stage == "Red" & P12$MCL_r == "High")
P12_MCL_Pre_M <- sum (P12$Stage == "Red" & P12$MCL_r == "Medium")
P12_MCL_Pre_L <- sum (P12$Stage=="Red" & P12$MCL_r=="Low")
P12_MCL_Post_H <- sum (P12$Stage=="Blue" & P12$MCL_r=="High")
P12_MCL_Post_M <- sum (P12$Stage=="Blue" & P12$MCL_r=="Medium")
P12_MCL_Post_L <- sum (P12$Stage=="Blue" & P12$MCL_r=="Low")
P12_BCL_Pre_H <- sum (P12$Stage == "Red" & P12$BCL_r == "High")
P12_BCL_Pre_M <- sum (P12$Stage == "Red" & P12$BCL_r == "Medium")
P12_BCL_Pre_L <- sum (P12$Stage=="Red" & P12$BCL_r=="Low")
P12_BCL_Post_H <- sum (P12$Stage=="Blue" & P12$BCL_r=="High")
P12_BCL_Post_M <- sum (P12$Stage=="Blue" & P12$BCL_r=="Medium")
P12_BCL_Post_L <- sum (P12$Stage=="Blue" & P12$BCL_r=="Low")
P12_IgG_Pre_H <- sum (P12$Stage == "Red" & P12$IgG_r == "High")
P12_IgG_Pre_M <- sum (P12$Stage == "Red" & P12$IgG_r == "Medium")
P12_IgG_Pre_L <- sum (P12$Stage=="Red" & P12$IgG_r=="Low")
P12_IgG_Post_H <- sum (P12$Stage=="Blue" & P12$IgG_r=="High")
P12_IgG_Post_M <- sum (P12$Stage=="Blue" & P12$IgG_r=="Medium")
P12_IgG_Post_L <- sum (P12$Stage=="Blue" & P12$IgG_r=="Low")
#### Creating a data.frame
P12_protein=c(rep("MCL",6), rep("BCL",6), rep("IgG",6))
P12_stage=c(rep("i.Pre",3), rep ("ii.During",3))
P12_level=rep(c("a.high", "b.medium", "c.low"),2)
P12_value=c(P12_MCL_Pre_H, P12_MCL_Pre_M, P12_MCL_Pre_L,
            P12_MCL_Post_H, P12_MCL_Post_M, P12_MCL_Post_L,
            P12_BCL_Pre_H, P12_BCL_Pre_M, P12_BCL_Pre_L,
            P12_BCL_Post_H, P12_BCL_Post_M, P12_BCL_Post_L,
            P12_IgG_Pre_H, P12_IgG_Pre_M, P12_IgG_Pre_L,
            P12_IgG_Post_H, P12_IgG_Post_M, P12_IgG_Post_L)
P12_cat=data.frame(P12_protein,P12_stage, P12_level, P12_value)
#### Creating a subset
P12_cat_MCL <- subset (P12_cat, P12_protein=="MCL",
                     select=P12_protein:P12_value)
P12_cat_BCL <- subset (P12_cat, P12_protein=="BCL",
                     select=P12_protein:P12_value)
P12_cat_IgG <- subset (P12_cat, P12_protein=="IgG",
                     select=P12_protein:P12_value)
#### Plotting multiple plots in the same page
P1a <- ggplot (P12_cat_MCL, aes(fill=P12_level, y=P12_value, x=P12_stage)) + 
    geom_bar( stat="identity", position="fill")
P1 <- P1a + labs(title = "MCL") + theme(legend.position="none") + xlab(NULL) + ylab(NULL)
P2a <- ggplot (P12_cat_BCL, aes(fill=P12_level, y=P12_value, x=P12_stage)) + 
    geom_bar( stat="identity", position="fill")
P2 <- P2a + labs(title = "BCL") + theme(legend.position="none") + xlab(NULL) + ylab(NULL)
P3a <- ggplot (P12_cat_IgG, aes(fill=P12_level, y=P12_value, x=P12_stage)) + 
    geom_bar( stat="identity", position="fill")
P3 <- P3a + labs(title = "IgG") + xlab(NULL) + ylab(NULL)
multiplot(P1, P2, P3, cols=3)

### Patient71
#### Categorization and summation 
P71_MCL_Pre_H <- sum (P71$Stage == "Red" & P71$MCL_r == "High")
P71_MCL_Pre_M <- sum (P71$Stage == "Red" & P71$MCL_r == "Medium")
P71_MCL_Pre_L <- sum (P71$Stage=="Red" & P71$MCL_r=="Low")
P71_MCL_Post_H <- sum (P71$Stage=="Blue" & P71$MCL_r=="High")
P71_MCL_Post_M <- sum (P71$Stage=="Blue" & P71$MCL_r=="Medium")
P71_MCL_Post_L <- sum (P71$Stage=="Blue" & P71$MCL_r=="Low")
P71_BCL_Pre_H <- sum (P71$Stage == "Red" & P71$BCL_r == "High")
P71_BCL_Pre_M <- sum (P71$Stage == "Red" & P71$BCL_r == "Medium")
P71_BCL_Pre_L <- sum (P71$Stage=="Red" & P71$BCL_r=="Low")
P71_BCL_Post_H <- sum (P71$Stage=="Blue" & P71$BCL_r=="High")
P71_BCL_Post_M <- sum (P71$Stage=="Blue" & P71$BCL_r=="Medium")
P71_BCL_Post_L <- sum (P71$Stage=="Blue" & P71$BCL_r=="Low")
P71_IgG_Pre_H <- sum (P71$Stage == "Red" & P71$IgG_r == "High")
P71_IgG_Pre_M <- sum (P71$Stage == "Red" & P71$IgG_r == "Medium")
P71_IgG_Pre_L <- sum (P71$Stage=="Red" & P71$IgG_r=="Low")
P71_IgG_Post_H <- sum (P71$Stage=="Blue" & P71$IgG_r=="High")
P71_IgG_Post_M <- sum (P71$Stage=="Blue" & P71$IgG_r=="Medium")
P71_IgG_Post_L <- sum (P71$Stage=="Blue" & P71$IgG_r=="Low")
#### Creating a data.frame
P71_protein=c(rep("MCL",6), rep("BCL",6), rep("IgG",6))
P71_stage=c(rep("i.Pre",3), rep ("ii.During",3))
P71_level=rep(c("a.high", "b.medium", "c.low"),2)
P71_value=c(P71_MCL_Pre_H, P71_MCL_Pre_M, P71_MCL_Pre_L,
            P71_MCL_Post_H, P71_MCL_Post_M, P71_MCL_Post_L,
            P71_BCL_Pre_H, P71_BCL_Pre_M, P71_BCL_Pre_L,
            P71_BCL_Post_H, P71_BCL_Post_M, P71_BCL_Post_L,
            P71_IgG_Pre_H, P71_IgG_Pre_M, P71_IgG_Pre_L,
            P71_IgG_Post_H, P71_IgG_Post_M, P71_IgG_Post_L)
P71_cat=data.frame(P71_protein,P71_stage, P71_level, P71_value)
#### Creating a subset
P71_cat_MCL <- subset (P71_cat, P71_protein=="MCL",
                     select=P71_protein:P71_value)
P71_cat_BCL <- subset (P71_cat, P71_protein=="BCL",
                     select=P71_protein:P71_value)
P71_cat_IgG <- subset (P71_cat, P71_protein=="IgG",
                     select=P71_protein:P71_value)
#### Plotting multiple plots in the same page
P1a <- ggplot (P71_cat_MCL, aes(fill=P71_level, y=P71_value, x=P71_stage)) + 
    geom_bar( stat="identity", position="fill")
P1 <- P1a + labs(title = "MCL") + theme(legend.position="none") + xlab(NULL) + ylab(NULL)
P2a <- ggplot (P71_cat_BCL, aes(fill=P71_level, y=P71_value, x=P71_stage)) + 
    geom_bar( stat="identity", position="fill")
P2 <- P2a + labs(title = "BCL") + theme(legend.position="none") + xlab(NULL) + ylab(NULL)
P3a <- ggplot (P71_cat_IgG, aes(fill=P71_level, y=P71_value, x=P71_stage)) + 
    geom_bar( stat="identity", position="fill")
P3 <- P3a + labs(title = "IgG") + xlab(NULL) + ylab(NULL)
multiplot(P1, P2, P3, cols=3)

### All (Patients_combined)
#### Categorization and summation 
All_MCL_Pre_H <- sum (All$Stage == "Red" & All$MCL_r == "High")
All_MCL_Pre_M <- sum (All$Stage == "Red" & All$MCL_r == "Medium")
All_MCL_Pre_L <- sum (All$Stage=="Red" & All$MCL_r=="Low")
All_MCL_Post_H <- sum (All$Stage=="Blue" & All$MCL_r=="High")
All_MCL_Post_M <- sum (All$Stage=="Blue" & All$MCL_r=="Medium")
All_MCL_Post_L <- sum (All$Stage=="Blue" & All$MCL_r=="Low")
All_BCL_Pre_H <- sum (All$Stage == "Red" & All$BCL_r == "High")
All_BCL_Pre_M <- sum (All$Stage == "Red" & All$BCL_r == "Medium")
All_BCL_Pre_L <- sum (All$Stage=="Red" & All$BCL_r=="Low")
All_BCL_Post_H <- sum (All$Stage=="Blue" & All$BCL_r=="High")
All_BCL_Post_M <- sum (All$Stage=="Blue" & All$BCL_r=="Medium")
All_BCL_Post_L <- sum (All$Stage=="Blue" & All$BCL_r=="Low")
All_IgG_Pre_H <- sum (All$Stage == "Red" & All$IgG_r == "High")
All_IgG_Pre_M <- sum (All$Stage == "Red" & All$IgG_r == "Medium")
All_IgG_Pre_L <- sum (All$Stage=="Red" & All$IgG_r=="Low")
All_IgG_Post_H <- sum (All$Stage=="Blue" & All$IgG_r=="High")
All_IgG_Post_M <- sum (All$Stage=="Blue" & All$IgG_r=="Medium")
All_IgG_Post_L <- sum (All$Stage=="Blue" & All$IgG_r=="Low")
#### Creating a data.frame
All_protein=c(rep("MCL",6), rep("BCL",6), rep("IgG",6))
All_stage=c(rep("i.Pre",3), rep ("ii.During",3))
All_level=rep(c("a.high", "b.medium", "c.low"),2)
All_value=c(All_MCL_Pre_H, All_MCL_Pre_M, All_MCL_Pre_L,
            All_MCL_Post_H, All_MCL_Post_M, All_MCL_Post_L,
            All_BCL_Pre_H, All_BCL_Pre_M, All_BCL_Pre_L,
            All_BCL_Post_H, All_BCL_Post_M, All_BCL_Post_L,
            All_IgG_Pre_H, All_IgG_Pre_M, All_IgG_Pre_L,
            All_IgG_Post_H, All_IgG_Post_M, All_IgG_Post_L)
All_cat=data.frame(All_protein,All_stage, All_level, All_value)
#### Creating a subset
All_cat_MCL <- subset (All_cat, All_protein=="MCL",
                     select=All_protein:All_value)
All_cat_BCL <- subset (All_cat, All_protein=="BCL",
                     select=All_protein:All_value)
All_cat_IgG <- subset (All_cat, All_protein=="IgG",
                     select=All_protein:All_value)
#### Plotting multiple plots in the same page
P1a <- ggplot (All_cat_MCL, aes(fill=All_level, y=All_value, x=All_stage)) + 
    geom_bar( stat="identity", position="fill")
P1 <- P1a + labs(title = "MCL") + theme(legend.position="none") + xlab(NULL) + ylab(NULL)
P2a <- ggplot (All_cat_BCL, aes(fill=All_level, y=All_value, x=All_stage)) + 
    geom_bar( stat="identity", position="fill")
P2 <- P2a + labs(title = "BCL") + theme(legend.position="none") + xlab(NULL) + ylab(NULL)
P3a <- ggplot (All_cat_IgG, aes(fill=All_level, y=All_value, x=All_stage)) + 
    geom_bar( stat="identity", position="fill")
P3 <- P3a + labs(title = "IgG") + xlab(NULL) + ylab(NULL)
multiplot(P1, P2, P3, cols=3)