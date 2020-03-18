# Kallesh_Flowcytometry

## STACKED BARCHART

### Delete all data from the environment 
### Import and view data through the environment window in RStudio
### Re-arrange, rename, percentile transform, and categorize patient data

library("dplyr")
P12b1_pre <- P12a1_pre [-1,c(7,8,14,15)]             #delete 1st row #select 7,8,14,15 columns
P12b2_pre <- P12a2_pre [-1,c(15,17)]               #delete 1st row #select 15th column and dummy 17th column (to avoid converting to value)
P12b3_post <- P12a3_post [-1,c(7,8,14,15)]           #delete 1st row #select 7,8,14,15 columns
P12b4_post <- P12a4_post [-1,c(15,17)]             #delete 1st row #select 15th column and dummy 17th column (to avoid converting to value)
colnames (P12b1_pre) <- c("MCL","BIM","BCL2","BCLXL")    #rename column names
colnames (P12b2_pre) <- c("BFI", "Dummy")          
colnames (P12b3_post) <- c("MCL","BIM","BCL2","BCLXL")   
colnames (P12b4_post) <- c("BFI", "Dummy")         
P12b1_pre$Stage <- "Pre"                           #creating stage identifier variable (pre/post)
P12b2_pre$Stage <- "Pre"                           
P12b3_post$Stage <- "Post"                         
P12b4_post$Stage <- "Post"                        
P12c1 <- rbind (P12b1_pre, P12b3_post)             #binding (merging) the pre and post data
P12c2 <- rbind (P12b2_pre, P12b4_post)            
P12c1$Patient <- "P12"                              #Creating Patient identifier variable
P12c2$Patient <- "P12"                              
P12c1$MCL <- as.numeric(as.character(P12c1$MCL))        #Converting MCL from factor into numerical
P12c1$BIM <- as.numeric(as.character(P12c1$BIM))        #Converting BIM from factor into numerical
P12c1$BCL2 <- as.numeric(as.character(P12c1$BCL2))      #Converting BCL2 from factor into numerical
P12c1$BCLXL <- as.numeric(as.character(P12c1$BCLXL))    #Converting BCLXL from factor into numerical
P12c2$BFI <- as.numeric(as.character(P12c2$BFI))        #Converting BFI from factor into numerical
P12c1$MCL_p <- ecdf(P12c1$MCL)(P12c1$MCL)               #Creating a percentile variable for MCL
P12c1$BIM_p <- ecdf(P12c1$BIM)(P12c1$BIM)               #Creating a percentile variable for BIM
P12c1$BCL2_p <- ecdf(P12c1$BCL2)(P12c1$BCL2)            #Creating a percentile variable for BCL2
P12c1$BCLXL_p <- ecdf(P12c1$BCLXL)(P12c1$BCLXL)         #Creating a percentile variable for BCLXL
P12c2$BFI_p <- ecdf(P12c2$BFI)(P12c2$BFI)               #Creating a percentile variable for BFI
P12c1$MCL_r <- ifelse(P12c1$MCL_p <= 0.33, 'Low',
         ifelse(P12c1$MCL_p <= 0.66, 'Medium', 
         'High'))                                       #Creating ordinal percentile variables
P12c1$BIM_r <- ifelse(P12c1$BIM_p <= 0.33, 'Low',
         ifelse(P12c1$BIM_p <= 0.66, 'Medium', 
         'High'))                                       
P12c1$BCL2_r <- ifelse(P12c1$BCL2_p <= 0.33, 'Low',   
         ifelse(P12c1$BCL2_p <= 0.66, 'Medium', 
         'High'))
P12c1$BCLXL_r <- ifelse(P12c1$BCLXL_p <= 0.33, 'Low',  
         ifelse(P12c1$BCLXL_p <= 0.66, 'Medium', 
         'High'))
P12c2$BFI_r <- ifelse(P12c2$BFI_p <= 0.33, 'Low',  
         ifelse(P12c2$BFI_p <= 0.66, 'Medium', 
         'High'))
P12d1_pre <- subset (P12c1, P12c1$Stage=="Pre",        #Creating 'pre' and 'post' subsets
                    select = c(11:14))
P12d2_pre <- subset (P12c2, P12c2$Stage=="Pre", 
                    select = c(6))
P12d3_post <- subset (P12c1, P12c1$Stage=="Post", 
                    select = c(11:14))
P12d4_post <- subset (P12c2, P12c2$Stage=="Post", 
                    select = c(6))
P12e1_pre_MCL <- P12d1_pre %>% dplyr::count (MCL_r) %>%               #Creating tables
                dplyr::rename (Level=MCL_r) %>%
                dplyr::mutate (Protein = "MCL", Stage = "i.Pre") %>%
                dplyr::select (Protein, Stage, Level, n)
P12e2_pre_BIM <- P12d1_pre %>% dplyr::count (BIM_r) %>%               #Creating tables
                dplyr::rename (Level=BIM_r) %>%
                dplyr::mutate (Protein = "BIM", Stage = "i.Pre") %>%
                dplyr::select (Protein, Stage, Level, n)
P12e3_pre_BCL2 <- P12d1_pre %>% dplyr::count (BCL2_r) %>%               
               dplyr::rename (Level=BCL2_r) %>%
               dplyr::mutate (Protein = "BCL2", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P12e4_pre_BCLXL <- P12d1_pre %>% dplyr::count (BCLXL_r) %>%               
               dplyr::rename (Level=BCLXL_r) %>%
               dplyr::mutate (Protein = "BCLXL", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P12e5_pre_BFI <- P12d2_pre %>% dplyr::count (BFI_r) %>%               
               dplyr::rename (Level=BFI_r) %>%
               dplyr::mutate (Protein = "BFI", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P12e6_post_MCL <- P12d3_post %>% dplyr::count (MCL_r) %>%               
               dplyr::rename (Level=MCL_r) %>%
               dplyr::mutate (Protein = "MCL", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P12e7_post_BIM <- P12d3_post %>% dplyr::count (BIM_r) %>%               
               dplyr::rename (Level=BIM_r) %>%
               dplyr::mutate (Protein = "BIM", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)          
P12e8_post_BCL2 <- P12d3_post %>% dplyr::count (BCL2_r) %>%               
               dplyr::rename (Level=BCL2_r) %>%
               dplyr::mutate (Protein = "BCL2", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P12e9_post_BCLXL <- P12d3_post %>% dplyr::count (BCLXL_r) %>%               
               dplyr::rename (Level=BCLXL_r) %>%
               dplyr::mutate (Protein = "BCLXL", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P12e10_post_BFI <- P12d4_post %>% dplyr::count (BFI_r) %>%               
               dplyr::rename (Level=BFI_r) %>%
               dplyr::mutate (Protein = "BFI", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P12f1_MCL <- rbind (P12e1_pre_MCL,P12e6_post_MCL)               #binding (merging) the pre and post data for each protein
P12f2_BIM <- rbind (P12e2_pre_BIM,P12e7_post_BIM)
P12f3_BCL2 <- rbind (P12e3_pre_BCL2,P12e8_post_BCL2)
P12f4_BCLXL <- rbind (P12e4_pre_BCLXL,P12e9_post_BCLXL)
P12f5_BFI <- rbind (P12e5_pre_BFI,P12e10_post_BFI)

### Plotting

library (ggplot2)
P12g1 <- ggplot (P12f1_MCL, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P12g2 <- ggplot (P12f3_BCL2, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P12g3 <- ggplot (P12f4_BCLXL, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")               
P12g4 <- ggplot (P12f5_BFI, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P12g5 <- ggplot (P12f2_BIM, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P12g6 <- P12g1 + labs (title = "MCL") + theme (legend.position="none") + xlab(NULL) + ylab(NULL) 
P12g7 <- P12g2 + labs (title = "BCL2") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
P12g8 <- P12g3 + labs (title = "BCLXL") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
P12g9 <- P12g4 + labs (title = "BFI") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
P12g10 <- P12g5 + labs (title = "BIM") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
multiplot (P12g6, P12g7, P12g8, P12g9, p12g10, cols=5)


### Repeating the code for other patients 

library("dplyr")
P21b1_pre <- P21a1_pre [-1,c(7,14,15)]             #delete 1st row #select 7,14,15 columns
P21b2_pre <- P21a2_pre [-1,c(15,17)]               #delete 1st row #select 15th column and dummy 17th column (to avoid converting to value)
P21b3_post <- P21a3_post [-1,c(7,14,15)]           #delete 1st row #select 7,14,15 columns
P21b4_post <- P21a4_post [-1,c(15,17)]             #delete 1st row #select 15th column and dummy 17th column (to avoid converting to value)
colnames (P21b1_pre) <- c("MCL","BCL2","BCLXL")    #rename column names
colnames (P21b2_pre) <- c("BFI", "Dummy")          
colnames (P21b3_post) <- c("MCL","BCL2","BCLXL")   
colnames (P21b4_post) <- c("BFI", "Dummy")         
P21b1_pre$Stage <- "Pre"                           #creating stage identifier variable (pre/post)
P21b2_pre$Stage <- "Pre"                           
P21b3_post$Stage <- "Post"                         
P21b4_post$Stage <- "Post"                        
P21c1 <- rbind (P21b1_pre, P21b3_post)             #binding (merging) the pre and post data
P21c2 <- rbind (P21b2_pre, P21b4_post)            
P21c1$Patient <- "P21"                              #Creating Patient identifier variable
P21c2$Patient <- "P21"                              
P21c1$MCL <- as.numeric(as.character(P21c1$MCL))        #Converting MCL from factor into numerical
P21c1$BCL2 <- as.numeric(as.character(P21c1$BCL2))      #Converting BCL2 from factor into numerical
P21c1$BCLXL <- as.numeric(as.character(P21c1$BCLXL))    #Converting BCLXL from factor into numerical
P21c2$BFI <- as.numeric(as.character(P21c2$BFI))        #Converting BFI from factor into numerical
P21c1$MCL_p <- ecdf(P21c1$MCL)(P21c1$MCL)               #Creating a percentile variable for MCL
P21c1$BCL2_p <- ecdf(P21c1$BCL2)(P21c1$BCL2)            #Creating a percentile variable for BCL2
P21c1$BCLXL_p <- ecdf(P21c1$BCLXL)(P21c1$BCLXL)         #Creating a percentile variable for BCLXL
P21c2$BFI_p <- ecdf(P21c2$BFI)(P21c2$BFI)               #Creating a percentile variable for BFI
P21c1$MCL_r <- ifelse(P21c1$MCL_p <= 0.33, 'Low',
         ifelse(P21c1$MCL_p <= 0.66, 'Medium', 
         'High'))                                       #Creating ordinal percentile variables
P21c1$BCL2_r <- ifelse(P21c1$BCL2_p <= 0.33, 'Low',   
         ifelse(P21c1$BCL2_p <= 0.66, 'Medium', 
         'High'))
P21c1$BCLXL_r <- ifelse(P21c1$BCLXL_p <= 0.33, 'Low',  
         ifelse(P21c1$BCLXL_p <= 0.66, 'Medium', 
         'High'))
P21c2$BFI_r <- ifelse(P21c2$BFI_p <= 0.33, 'Low',  
         ifelse(P21c2$BFI_p <= 0.66, 'Medium', 
         'High'))
P21d1_pre <- subset (P21c1, P21c1$Stage=="Pre",        #Creating 'pre' and 'post' subsets
                    select = c(9:11))
P21d2_pre <- subset (P21c2, P21c2$Stage=="Pre", 
                    select = c(6))
P21d3_post <- subset (P21c1, P21c1$Stage=="Post", 
                    select = c(9:11))
P21d4_post <- subset (P21c2, P21c2$Stage=="Post", 
                    select = c(6))
P21e1_pre_MCL <- P21d1_pre %>% dplyr::count (MCL_r) %>%               #Creating tables
                dplyr::rename (Level=MCL_r) %>%
                dplyr::mutate (Protein = "MCL", Stage = "i.Pre") %>%
                dplyr::select (Protein, Stage, Level, n)
P21e2_pre_BCL2 <- P21d1_pre %>% dplyr::count (BCL2_r) %>%               
               dplyr::rename (Level=BCL2_r) %>%
               dplyr::mutate (Protein = "BCL2", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P21e3_pre_BCLXL <- P21d1_pre %>% dplyr::count (BCLXL_r) %>%               
               dplyr::rename (Level=BCLXL_r) %>%
               dplyr::mutate (Protein = "BCLXL", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P21e4_pre_BFI <- P21d2_pre %>% dplyr::count (BFI_r) %>%               
               dplyr::rename (Level=BFI_r) %>%
               dplyr::mutate (Protein = "BFI", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P21e5_post_MCL <- P21d3_post %>% dplyr::count (MCL_r) %>%               
               dplyr::rename (Level=MCL_r) %>%
               dplyr::mutate (Protein = "MCL", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P21e6_post_BCL2 <- P21d3_post %>% dplyr::count (BCL2_r) %>%               
               dplyr::rename (Level=BCL2_r) %>%
               dplyr::mutate (Protein = "BCL2", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P21e7_post_BCLXL <- P21d3_post %>% dplyr::count (BCLXL_r) %>%               
               dplyr::rename (Level=BCLXL_r) %>%
               dplyr::mutate (Protein = "BCLXL", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P21e8_post_BFI <- P21d4_post %>% dplyr::count (BFI_r) %>%               
               dplyr::rename (Level=BFI_r) %>%
               dplyr::mutate (Protein = "BFI", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)  
P21f1_MCL <- rbind (P21e1_pre_MCL,P21e5_post_MCL)               #binding (merging) the pre and post data for each protein
P21f2_BCL2 <- rbind (P21e2_pre_BCL2,P21e6_post_BCL2)
P21f3_BCLXL <- rbind (P21e3_pre_BCLXL,P21e7_post_BCLXL)
P21f4_BFI <- rbind (P21e4_pre_BFI,P21e8_post_BFI)
library (ggplot2)
P21g1 <- ggplot (P21f1_MCL, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P21g2 <- ggplot (P21f2_BCL2, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P21g3 <- ggplot (P21f3_BCLXL, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")               
P21g4 <- ggplot (P21f4_BFI, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P21g5 <- P21g1 + labs (title = "MCL") + theme (legend.position="none") + xlab(NULL) + ylab(NULL) 
P21g6 <- P21g2 + labs (title = "BCL2") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
P21g7 <- P21g3 + labs (title = "BCLXL") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
P21g8 <- P21g4 + labs (title = "BFI") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
multiplot(P21g5, P21g6, P21g7, P21g8, cols=4)


library("dplyr")
P29b1_pre <- P29a1_pre [-1,c(7,14,15)]             #delete 1st row #select 7,14,15 columns
P29b2_pre <- P29a2_pre [-1,c(15,17)]               #delete 1st row #select 15th column and dummy 17th column (to avoid converting to value)
P29b3_post <- P29a3_post [-1,c(7,14,15)]           #delete 1st row #select 7,14,15 columns
P29b4_post <- P29a4_post [-1,c(15,17)]             #delete 1st row #select 15th column and dummy 17th column (to avoid converting to value)
colnames (P29b1_pre) <- c("MCL","BCL2","BCLXL")    #rename column names
colnames (P29b2_pre) <- c("BFI", "Dummy")          
colnames (P29b3_post) <- c("MCL","BCL2","BCLXL")   
colnames (P29b4_post) <- c("BFI", "Dummy")         
P29b1_pre$Stage <- "Pre"                           #creating stage identifier variable (pre/post)
P29b2_pre$Stage <- "Pre"                           
P29b3_post$Stage <- "Post"                         
P29b4_post$Stage <- "Post"                        
P29c1 <- rbind (P29b1_pre, P29b3_post)             #binding (merging) the pre and post data
P29c2 <- rbind (P29b2_pre, P29b4_post)            
P29c1$Patient <- "P29"                              #Creating Patient identifier variable
P29c2$Patient <- "P29"                              
P29c1$MCL <- as.numeric(as.character(P29c1$MCL))        #Converting MCL from factor into numerical
P29c1$BCL2 <- as.numeric(as.character(P29c1$BCL2))      #Converting BCL2 from factor into numerical
P29c1$BCLXL <- as.numeric(as.character(P29c1$BCLXL))    #Converting BCLXL from factor into numerical
P29c2$BFI <- as.numeric(as.character(P29c2$BFI))        #Converting BFI from factor into numerical
P29c1$MCL_p <- ecdf(P29c1$MCL)(P29c1$MCL)               #Creating a percentile variable for MCL
P29c1$BCL2_p <- ecdf(P29c1$BCL2)(P29c1$BCL2)            #Creating a percentile variable for BCL2
P29c1$BCLXL_p <- ecdf(P29c1$BCLXL)(P29c1$BCLXL)         #Creating a percentile variable for BCLXL
P29c2$BFI_p <- ecdf(P29c2$BFI)(P29c2$BFI)               #Creating a percentile variable for BFI
P29c1$MCL_r <- ifelse(P29c1$MCL_p <= 0.33, 'Low',
         ifelse(P29c1$MCL_p <= 0.66, 'Medium', 
         'High'))                                       #Creating ordinal percentile variables
P29c1$BCL2_r <- ifelse(P29c1$BCL2_p <= 0.33, 'Low',   
         ifelse(P29c1$BCL2_p <= 0.66, 'Medium', 
         'High'))
P29c1$BCLXL_r <- ifelse(P29c1$BCLXL_p <= 0.33, 'Low',  
         ifelse(P29c1$BCLXL_p <= 0.66, 'Medium', 
         'High'))
P29c2$BFI_r <- ifelse(P29c2$BFI_p <= 0.33, 'Low',  
         ifelse(P29c2$BFI_p <= 0.66, 'Medium', 
         'High'))
P29d1_pre <- subset (P29c1, P29c1$Stage=="Pre",        #Creating 'pre' and 'post' subsets
                    select = c(9:11))
P29d2_pre <- subset (P29c2, P29c2$Stage=="Pre", 
                    select = c(6))
P29d3_post <- subset (P29c1, P29c1$Stage=="Post", 
                    select = c(9:11))
P29d4_post <- subset (P29c2, P29c2$Stage=="Post", 
                    select = c(6))
P29e1_pre_MCL <- P29d1_pre %>% dplyr::count (MCL_r) %>%               #Creating tables
                dplyr::rename (Level=MCL_r) %>%
                dplyr::mutate (Protein = "MCL", Stage = "i.Pre") %>%
                dplyr::select (Protein, Stage, Level, n)
P29e2_pre_BCL2 <- P29d1_pre %>% dplyr::count (BCL2_r) %>%               
               dplyr::rename (Level=BCL2_r) %>%
               dplyr::mutate (Protein = "BCL2", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P29e3_pre_BCLXL <- P29d1_pre %>% dplyr::count (BCLXL_r) %>%               
               dplyr::rename (Level=BCLXL_r) %>%
               dplyr::mutate (Protein = "BCLXL", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P29e4_pre_BFI <- P29d2_pre %>% dplyr::count (BFI_r) %>%               
               dplyr::rename (Level=BFI_r) %>%
               dplyr::mutate (Protein = "BFI", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P29e5_post_MCL <- P29d3_post %>% dplyr::count (MCL_r) %>%               
               dplyr::rename (Level=MCL_r) %>%
               dplyr::mutate (Protein = "MCL", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P29e6_post_BCL2 <- P29d3_post %>% dplyr::count (BCL2_r) %>%               
               dplyr::rename (Level=BCL2_r) %>%
               dplyr::mutate (Protein = "BCL2", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P29e7_post_BCLXL <- P29d3_post %>% dplyr::count (BCLXL_r) %>%               
               dplyr::rename (Level=BCLXL_r) %>%
               dplyr::mutate (Protein = "BCLXL", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P29e8_post_BFI <- P29d4_post %>% dplyr::count (BFI_r) %>%               
               dplyr::rename (Level=BFI_r) %>%
               dplyr::mutate (Protein = "BFI", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)  
P29f1_MCL <- rbind (P29e1_pre_MCL,P29e5_post_MCL)               #binding (merging) the pre and post data for each protein
P29f2_BCL2 <- rbind (P29e2_pre_BCL2,P29e6_post_BCL2)
P29f3_BCLXL <- rbind (P29e3_pre_BCLXL,P29e7_post_BCLXL)
P29f4_BFI <- rbind (P29e4_pre_BFI,P29e8_post_BFI)
library (ggplot2)
P29g1 <- ggplot (P29f1_MCL, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P29g2 <- ggplot (P29f2_BCL2, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P29g3 <- ggplot (P29f3_BCLXL, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")               
P29g4 <- ggplot (P29f4_BFI, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P29g5 <- P29g1 + labs (title = "MCL") + theme (legend.position="none") + xlab(NULL) + ylab(NULL) 
P29g6 <- P29g2 + labs (title = "BCL2") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
P29g7 <- P29g3 + labs (title = "BCLXL") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
P29g8 <- P29g4 + labs (title = "BFI") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
multiplot(P29g5, P29g6, P29g7, P29g8, cols=4)


library("dplyr")
P31b1_pre <- P31a1_pre [-1,c(7,14,15)]             #delete 1st row #select 7,14,15 columns
P31b2_pre <- P31a2_pre [-1,c(15,17)]               #delete 1st row #select 15th column and dummy 17th column (to avoid converting to value)
P31b3_post <- P31a3_post [-1,c(7,14,15)]           #delete 1st row #select 7,14,15 columns
P31b4_post <- P31a4_post [-1,c(15,17)]             #delete 1st row #select 15th column and dummy 17th column (to avoid converting to value)
colnames (P31b1_pre) <- c("MCL","BCL2","BCLXL")    #rename column names
colnames (P31b2_pre) <- c("BFI", "Dummy")          
colnames (P31b3_post) <- c("MCL","BCL2","BCLXL")   
colnames (P31b4_post) <- c("BFI", "Dummy")         
P31b1_pre$Stage <- "Pre"                           #creating stage identifier variable (pre/post)
P31b2_pre$Stage <- "Pre"                           
P31b3_post$Stage <- "Post"                         
P31b4_post$Stage <- "Post"                        
P31c1 <- rbind (P31b1_pre, P31b3_post)             #binding (merging) the pre and post data
P31c2 <- rbind (P31b2_pre, P31b4_post)            
P31c1$Patient <- "P31"                              #Creating Patient identifier variable
P31c2$Patient <- "P31"                              
P31c1$MCL <- as.numeric(as.character(P31c1$MCL))        #Converting MCL from factor into numerical
P31c1$BCL2 <- as.numeric(as.character(P31c1$BCL2))      #Converting BCL2 from factor into numerical
P31c1$BCLXL <- as.numeric(as.character(P31c1$BCLXL))    #Converting BCLXL from factor into numerical
P31c2$BFI <- as.numeric(as.character(P31c2$BFI))        #Converting BFI from factor into numerical
P31c1$MCL_p <- ecdf(P31c1$MCL)(P31c1$MCL)               #Creating a percentile variable for MCL
P31c1$BCL2_p <- ecdf(P31c1$BCL2)(P31c1$BCL2)            #Creating a percentile variable for BCL2
P31c1$BCLXL_p <- ecdf(P31c1$BCLXL)(P31c1$BCLXL)         #Creating a percentile variable for BCLXL
P31c2$BFI_p <- ecdf(P31c2$BFI)(P31c2$BFI)               #Creating a percentile variable for BFI
P31c1$MCL_r <- ifelse(P31c1$MCL_p <= 0.33, 'Low',
         ifelse(P31c1$MCL_p <= 0.66, 'Medium', 
         'High'))                                       #Creating ordinal percentile variables
P31c1$BCL2_r <- ifelse(P31c1$BCL2_p <= 0.33, 'Low',   
         ifelse(P31c1$BCL2_p <= 0.66, 'Medium', 
         'High'))
P31c1$BCLXL_r <- ifelse(P31c1$BCLXL_p <= 0.33, 'Low',  
         ifelse(P31c1$BCLXL_p <= 0.66, 'Medium', 
         'High'))
P31c2$BFI_r <- ifelse(P31c2$BFI_p <= 0.33, 'Low',  
         ifelse(P31c2$BFI_p <= 0.66, 'Medium', 
         'High'))
P31d1_pre <- subset (P31c1, P31c1$Stage=="Pre",        #Creating 'pre' and 'post' subsets
                    select = c(9:11))
P31d2_pre <- subset (P31c2, P31c2$Stage=="Pre", 
                    select = c(6))
P31d3_post <- subset (P31c1, P31c1$Stage=="Post", 
                    select = c(9:11))
P31d4_post <- subset (P31c2, P31c2$Stage=="Post", 
                    select = c(6))
P31e1_pre_MCL <- P31d1_pre %>% dplyr::count (MCL_r) %>%               #Creating tables
                dplyr::rename (Level=MCL_r) %>%
                dplyr::mutate (Protein = "MCL", Stage = "i.Pre") %>%
                dplyr::select (Protein, Stage, Level, n)
P31e2_pre_BCL2 <- P31d1_pre %>% dplyr::count (BCL2_r) %>%               
               dplyr::rename (Level=BCL2_r) %>%
               dplyr::mutate (Protein = "BCL2", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P31e3_pre_BCLXL <- P31d1_pre %>% dplyr::count (BCLXL_r) %>%               
               dplyr::rename (Level=BCLXL_r) %>%
               dplyr::mutate (Protein = "BCLXL", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P31e4_pre_BFI <- P31d2_pre %>% dplyr::count (BFI_r) %>%               
               dplyr::rename (Level=BFI_r) %>%
               dplyr::mutate (Protein = "BFI", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P31e5_post_MCL <- P31d3_post %>% dplyr::count (MCL_r) %>%               
               dplyr::rename (Level=MCL_r) %>%
               dplyr::mutate (Protein = "MCL", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P31e6_post_BCL2 <- P31d3_post %>% dplyr::count (BCL2_r) %>%               
               dplyr::rename (Level=BCL2_r) %>%
               dplyr::mutate (Protein = "BCL2", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P31e7_post_BCLXL <- P31d3_post %>% dplyr::count (BCLXL_r) %>%               
               dplyr::rename (Level=BCLXL_r) %>%
               dplyr::mutate (Protein = "BCLXL", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P31e8_post_BFI <- P31d4_post %>% dplyr::count (BFI_r) %>%               
               dplyr::rename (Level=BFI_r) %>%
               dplyr::mutate (Protein = "BFI", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)  
P31f1_MCL <- rbind (P31e1_pre_MCL,P31e5_post_MCL)               #binding (merging) the pre and post data for each protein
P31f2_BCL2 <- rbind (P31e2_pre_BCL2,P31e6_post_BCL2)
P31f3_BCLXL <- rbind (P31e3_pre_BCLXL,P31e7_post_BCLXL)
P31f4_BFI <- rbind (P31e4_pre_BFI,P31e8_post_BFI)
library (ggplot2)
P31g1 <- ggplot (P31f1_MCL, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P31g2 <- ggplot (P31f2_BCL2, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P31g3 <- ggplot (P31f3_BCLXL, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")               
P31g4 <- ggplot (P31f4_BFI, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P31g5 <- P31g1 + labs (title = "MCL") + theme (legend.position="none") + xlab(NULL) + ylab(NULL) 
P31g6 <- P31g2 + labs (title = "BCL2") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
P31g7 <- P31g3 + labs (title = "BCLXL") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
P31g8 <- P31g4 + labs (title = "BFI") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
multiplot(P31g5, P31g6, P31g7, P31g8, cols=4)


library("dplyr")
P71b1_pre <- P71a1_pre [-1,c(7,14,15)]             #delete 1st row #select 7,14,15 columns
P71b2_pre <- P71a2_pre [-1,c(15,17)]               #delete 1st row #select 15th column and dummy 17th column (to avoid converting to value)
P71b3_post <- P71a3_post [-1,c(7,14,15)]           #delete 1st row #select 7,14,15 columns
P71b4_post <- P71a4_post [-1,c(15,17)]             #delete 1st row #select 15th column and dummy 17th column (to avoid converting to value)
colnames (P71b1_pre) <- c("MCL","BCL2","BCLXL")    #rename column names
colnames (P71b2_pre) <- c("BFI", "Dummy")          
colnames (P71b3_post) <- c("MCL","BCL2","BCLXL")   
colnames (P71b4_post) <- c("BFI", "Dummy")         
P71b1_pre$Stage <- "Pre"                           #creating stage identifier variable (pre/post)
P71b2_pre$Stage <- "Pre"                           
P71b3_post$Stage <- "Post"                         
P71b4_post$Stage <- "Post"                        
P71c1 <- rbind (P71b1_pre, P71b3_post)             #binding (merging) the pre and post data
P71c2 <- rbind (P71b2_pre, P71b4_post)            
P71c1$Patient <- "P71"                              #Creating Patient identifier variable
P71c2$Patient <- "P71"                              
P71c1$MCL <- as.numeric(as.character(P71c1$MCL))        #Converting MCL from factor into numerical
P71c1$BCL2 <- as.numeric(as.character(P71c1$BCL2))      #Converting BCL2 from factor into numerical
P71c1$BCLXL <- as.numeric(as.character(P71c1$BCLXL))    #Converting BCLXL from factor into numerical
P71c2$BFI <- as.numeric(as.character(P71c2$BFI))        #Converting BFI from factor into numerical
P71c1$MCL_p <- ecdf(P71c1$MCL)(P71c1$MCL)               #Creating a percentile variable for MCL
P71c1$BCL2_p <- ecdf(P71c1$BCL2)(P71c1$BCL2)            #Creating a percentile variable for BCL2
P71c1$BCLXL_p <- ecdf(P71c1$BCLXL)(P71c1$BCLXL)         #Creating a percentile variable for BCLXL
P71c2$BFI_p <- ecdf(P71c2$BFI)(P71c2$BFI)               #Creating a percentile variable for BFI
P71c1$MCL_r <- ifelse(P71c1$MCL_p <= 0.33, 'Low',
         ifelse(P71c1$MCL_p <= 0.66, 'Medium', 
         'High'))                                       #Creating ordinal percentile variables
P71c1$BCL2_r <- ifelse(P71c1$BCL2_p <= 0.33, 'Low',   
         ifelse(P71c1$BCL2_p <= 0.66, 'Medium', 
         'High'))
P71c1$BCLXL_r <- ifelse(P71c1$BCLXL_p <= 0.33, 'Low',  
         ifelse(P71c1$BCLXL_p <= 0.66, 'Medium', 
         'High'))
P71c2$BFI_r <- ifelse(P71c2$BFI_p <= 0.33, 'Low',  
         ifelse(P71c2$BFI_p <= 0.66, 'Medium', 
         'High'))
P71d1_pre <- subset (P71c1, P71c1$Stage=="Pre",        #Creating 'pre' and 'post' subsets
                    select = c(9:11))
P71d2_pre <- subset (P71c2, P71c2$Stage=="Pre", 
                    select = c(6))
P71d3_post <- subset (P71c1, P71c1$Stage=="Post", 
                    select = c(9:11))
P71d4_post <- subset (P71c2, P71c2$Stage=="Post", 
                    select = c(6))
P71e1_pre_MCL <- P71d1_pre %>% dplyr::count (MCL_r) %>%               #Creating tables
                dplyr::rename (Level=MCL_r) %>%
                dplyr::mutate (Protein = "MCL", Stage = "i.Pre") %>%
                dplyr::select (Protein, Stage, Level, n)
P71e2_pre_BCL2 <- P71d1_pre %>% dplyr::count (BCL2_r) %>%               
               dplyr::rename (Level=BCL2_r) %>%
               dplyr::mutate (Protein = "BCL2", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P71e3_pre_BCLXL <- P71d1_pre %>% dplyr::count (BCLXL_r) %>%               
               dplyr::rename (Level=BCLXL_r) %>%
               dplyr::mutate (Protein = "BCLXL", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P71e4_pre_BFI <- P71d2_pre %>% dplyr::count (BFI_r) %>%               
               dplyr::rename (Level=BFI_r) %>%
               dplyr::mutate (Protein = "BFI", Stage = "i.Pre") %>%
               dplyr::select (Protein, Stage, Level, n)
P71e5_post_MCL <- P71d3_post %>% dplyr::count (MCL_r) %>%               
               dplyr::rename (Level=MCL_r) %>%
               dplyr::mutate (Protein = "MCL", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P71e6_post_BCL2 <- P71d3_post %>% dplyr::count (BCL2_r) %>%               
               dplyr::rename (Level=BCL2_r) %>%
               dplyr::mutate (Protein = "BCL2", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P71e7_post_BCLXL <- P71d3_post %>% dplyr::count (BCLXL_r) %>%               
               dplyr::rename (Level=BCLXL_r) %>%
               dplyr::mutate (Protein = "BCLXL", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)
P71e8_post_BFI <- P71d4_post %>% dplyr::count (BFI_r) %>%               
               dplyr::rename (Level=BFI_r) %>%
               dplyr::mutate (Protein = "BFI", Stage = "ii.Post") %>%
               dplyr::select (Protein, Stage, Level, n)  
P71f1_MCL <- rbind (P71e1_pre_MCL,P71e5_post_MCL)               #binding (merging) the pre and post data for each protein
P71f2_BCL2 <- rbind (P71e2_pre_BCL2,P71e6_post_BCL2)
P71f3_BCLXL <- rbind (P71e3_pre_BCLXL,P71e7_post_BCLXL)
P71f4_BFI <- rbind (P71e4_pre_BFI,P71e8_post_BFI)
library (ggplot2)
P71g1 <- ggplot (P71f1_MCL, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P71g2 <- ggplot (P71f2_BCL2, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P71g3 <- ggplot (P71f3_BCLXL, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")               
P71g4 <- ggplot (P71f4_BFI, aes (fill=Level, y=n, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
P71g5 <- P71g1 + labs (title = "MCL") + theme (legend.position="none") + xlab(NULL) + ylab(NULL) 
P71g6 <- P71g2 + labs (title = "BCL2") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
P71g7 <- P71g3 + labs (title = "BCLXL") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
P71g8 <- P71g4 + labs (title = "BFI") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
multiplot(P71g5, P71g6, P71g7, P71g8, cols=4)


### Re-arrange and categorize data for all (merged data from all patients) 

P12e1_pre_MCL$p <- P12e1_pre_MCL$n/sum(P12e1_pre_MCL$n)            #Finding the relative proportion (new variable)
P21e1_pre_MCL$p <- P21e1_pre_MCL$n/sum(P21e1_pre_MCL$n)
P29e1_pre_MCL$p <- P29e1_pre_MCL$n/sum(P29e1_pre_MCL$n)
P31e1_pre_MCL$p <- P31e1_pre_MCL$n/sum(P31e1_pre_MCL$n)
P71e1_pre_MCL$p <- P71e1_pre_MCL$n/sum(P71e1_pre_MCL$n)
P12e2_pre_BCL2$p <- P12e2_pre_BCL2$n/sum(P12e2_pre_BCL2$n)            
P21e2_pre_BCL2$p <- P21e2_pre_BCL2$n/sum(P21e2_pre_BCL2$n)
P29e2_pre_BCL2$p <- P29e2_pre_BCL2$n/sum(P29e2_pre_BCL2$n)
P31e2_pre_BCL2$p <- P31e2_pre_BCL2$n/sum(P31e2_pre_BCL2$n)
P71e2_pre_BCL2$p <- P71e2_pre_BCL2$n/sum(P71e2_pre_BCL2$n)
P12e3_pre_BCLXL$p <- P12e3_pre_BCLXL$n/sum(P12e3_pre_BCLXL$n)            
P21e3_pre_BCLXL$p <- P21e3_pre_BCLXL$n/sum(P21e3_pre_BCLXL$n)
P29e3_pre_BCLXL$p <- P29e3_pre_BCLXL$n/sum(P29e3_pre_BCLXL$n)
P31e3_pre_BCLXL$p <- P31e3_pre_BCLXL$n/sum(P31e3_pre_BCLXL$n)
P71e3_pre_BCLXL$p <- P71e3_pre_BCLXL$n/sum(P71e3_pre_BCLXL$n)
P12e4_pre_BFI$p <- P12e4_pre_BFI$n/sum(P12e4_pre_BFI$n)            
P21e4_pre_BFI$p <- P21e4_pre_BFI$n/sum(P21e4_pre_BFI$n)
P29e4_pre_BFI$p <- P29e4_pre_BFI$n/sum(P29e4_pre_BFI$n)
P31e4_pre_BFI$p <- P31e4_pre_BFI$n/sum(P31e4_pre_BFI$n)
P71e4_pre_BFI$p <- P71e4_pre_BFI$n/sum(P71e4_pre_BFI$n)

P12e5_post_MCL$p <- P12e5_post_MCL$n/sum(P12e5_post_MCL$n)            
P21e5_post_MCL$p <- P21e5_post_MCL$n/sum(P21e5_post_MCL$n)
P29e5_post_MCL$p <- P29e5_post_MCL$n/sum(P29e5_post_MCL$n)
P31e5_post_MCL$p <- P31e5_post_MCL$n/sum(P31e5_post_MCL$n)
P71e5_post_MCL$p <- P71e5_post_MCL$n/sum(P71e5_post_MCL$n)
P12e6_post_BCL2$p <- P12e6_post_BCL2$n/sum(P12e6_post_BCL2$n)            
P21e6_post_BCL2$p <- P21e6_post_BCL2$n/sum(P21e6_post_BCL2$n)
P29e6_post_BCL2$p <- P29e6_post_BCL2$n/sum(P29e6_post_BCL2$n)
P31e6_post_BCL2$p <- P31e6_post_BCL2$n/sum(P31e6_post_BCL2$n)
P71e6_post_BCL2$p <- P71e6_post_BCL2$n/sum(P71e6_post_BCL2$n)
P12e7_post_BCLXL$p <- P12e7_post_BCLXL$n/sum(P12e7_post_BCLXL$n)            
P21e7_post_BCLXL$p <- P21e7_post_BCLXL$n/sum(P21e7_post_BCLXL$n)
P29e7_post_BCLXL$p <- P29e7_post_BCLXL$n/sum(P29e7_post_BCLXL$n)
P31e7_post_BCLXL$p <- P31e7_post_BCLXL$n/sum(P31e7_post_BCLXL$n)
P71e7_post_BCLXL$p <- P71e7_post_BCLXL$n/sum(P71e7_post_BCLXL$n)
P12e8_post_BFI$p <- P12e8_post_BFI$n/sum(P12e8_post_BFI$n)            
P21e8_post_BFI$p <- P21e8_post_BFI$n/sum(P21e8_post_BFI$n)
P29e8_post_BFI$p <- P29e8_post_BFI$n/sum(P29e8_post_BFI$n)
P31e8_post_BFI$p <- P31e8_post_BFI$n/sum(P31e8_post_BFI$n)
P71e8_post_BFI$p <- P71e8_post_BFI$n/sum(P71e8_post_BFI$n)

Alle1_pre_MCL <- cbind (P12e1_pre_MCL, P21e1_pre_MCL, P29e1_pre_MCL, P31e1_pre_MCL, P71e1_pre_MCL)
Alle1_pre_MCL <- Alle1_pre_MCL [,-c(4,6:9,11:14,16:19,21:24)]
Alle1_pre_MCL$p_av <- (Alle1_pre_MCL$p + Alle1_pre_MCL$p.1 + Alle1_pre_MCL$p.2 + Alle1_pre_MCL$p.3 + Alle1_pre_MCL$p.4)/5
Alle2_pre_BCL2 <- cbind (P12e2_pre_BCL2, P21e2_pre_BCL2, P29e2_pre_BCL2, P31e2_pre_BCL2, P71e2_pre_BCL2)
Alle2_pre_BCL2 <- Alle2_pre_BCL2 [,-c(4,6:9,11:14,16:19,21:24)]
Alle2_pre_BCL2$p_av <- (Alle2_pre_BCL2$p + Alle2_pre_BCL2$p.1 + Alle2_pre_BCL2$p.2 + Alle2_pre_BCL2$p.3 + Alle2_pre_BCL2$p.4)/5
Alle3_pre_BCLXL <- cbind (P12e3_pre_BCLXL, P21e3_pre_BCLXL, P29e3_pre_BCLXL, P31e3_pre_BCLXL, P71e3_pre_BCLXL)
Alle3_pre_BCLXL <- Alle3_pre_BCLXL [,-c(4,6:9,11:14,16:19,21:24)]
Alle3_pre_BCLXL$p_av <- (Alle3_pre_BCLXL$p + Alle3_pre_BCLXL$p.1 + Alle3_pre_BCLXL$p.2 + Alle3_pre_BCLXL$p.3 + Alle3_pre_BCLXL$p.4)/5
Alle4_pre_BFI <- cbind (P12e4_pre_BFI, P21e4_pre_BFI, P29e4_pre_BFI, P31e4_pre_BFI, P71e4_pre_BFI)
Alle4_pre_BFI <- Alle4_pre_BFI [,-c(4,6:9,11:14,16:19,21:24)]
Alle4_pre_BFI$p_av <- (Alle4_pre_BFI$p + Alle4_pre_BFI$p.1 + Alle4_pre_BFI$p.2 + Alle4_pre_BFI$p.3 + Alle4_pre_BFI$p.4)/5

Alle5_post_MCL <- cbind (P12e5_post_MCL, P21e5_post_MCL, P29e5_post_MCL, P31e5_post_MCL, P71e5_post_MCL)
Alle5_post_MCL <- Alle5_post_MCL [,-c(4,6:9,11:14,16:19,21:24)]
Alle5_post_MCL$p_av <- (Alle5_post_MCL$p + Alle5_post_MCL$p.1 + Alle5_post_MCL$p.2 + Alle5_post_MCL$p.3 + Alle5_post_MCL$p.4)/5
Alle6_post_BCL2 <- cbind (P12e6_post_BCL2, P21e6_post_BCL2, P29e6_post_BCL2, P31e6_post_BCL2, P71e6_post_BCL2)
Alle6_post_BCL2 <- Alle6_post_BCL2 [,-c(4,6:9,11:14,16:19,21:24)]
Alle6_post_BCL2$p_av <- (Alle6_post_BCL2$p + Alle6_post_BCL2$p.1 + Alle6_post_BCL2$p.2 + Alle6_post_BCL2$p.3 + Alle6_post_BCL2$p.4)/5
Alle7_post_BCLXL <- cbind (P12e7_post_BCLXL, P21e7_post_BCLXL, P29e7_post_BCLXL, P31e7_post_BCLXL, P71e7_post_BCLXL)
Alle7_post_BCLXL <- Alle7_post_BCLXL [,-c(4,6:9,11:14,16:19,21:24)]
Alle7_post_BCLXL$p_av <- (Alle7_post_BCLXL$p + Alle7_post_BCLXL$p.1 + Alle7_post_BCLXL$p.2 + Alle7_post_BCLXL$p.3 + Alle7_post_BCLXL$p.4)/5
Alle8_post_BFI <- cbind (P12e8_post_BFI, P21e8_post_BFI, P29e8_post_BFI, P31e8_post_BFI, P71e8_post_BFI)
Alle8_post_BFI <- Alle8_post_BFI [,-c(4,6:9,11:14,16:19,21:24)]
Alle8_post_BFI$p_av <- (Alle8_post_BFI$p + Alle8_post_BFI$p.1 + Alle8_post_BFI$p.2 + Alle8_post_BFI$p.3 + Alle8_post_BFI$p.4)/5

Allf1_MCL <- rbind (Alle1_pre_MCL, Alle5_post_MCL)
Allf2_BCL2 <- rbind (Alle2_pre_BCL2, Alle6_post_BCL2)
Allf3_BCLXL <- rbind (Alle3_pre_BCLXL, Alle7_post_BCLXL)
Allf4_BFI <- rbind (Alle4_pre_BFI, Alle8_post_BFI)

### Plotting

library (ggplot2)
Allg1 <- ggplot (Allf1_MCL, aes (fill=Level, y=p_av, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
Allg2 <- ggplot (Allf2_BCL2, aes (fill=Level, y=p_av, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
Allg3 <- ggplot (Allf3_BCLXL, aes (fill=Level, y=p_av, x=Stage)) + 
    geom_bar( stat="identity", position="fill")               
Allg4 <- ggplot (Allf4_BFI, aes (fill=Level, y=p_av, x=Stage)) + 
    geom_bar( stat="identity", position="fill")
Allg5 <- Allg1 + labs (title = "MCL") + theme (legend.position="none") + xlab(NULL) + ylab(NULL) 
Allg6 <- Allg2 + labs (title = "BCL2") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
Allg7 <- Allg3 + labs (title = "BCLXL") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
Allg8 <- Allg4 + labs (title = "BFI") + theme (legend.position="none") + xlab(NULL) + ylab(NULL)
multiplot(Allg5, Allg6, Allg7, Allg8, cols=4)


## SUPPLEMENTAL PLOTS: 3D BUBBLE PLOTS (CATEGORICAL DATA)

### Import and view data through the environment window in RStudio
### Re-arrange, rename, & normalize (percentile transformation) patient data

library("dplyr")
P12_Pre <- P12_Pre [-1,c(7,14,15)]                         #delete 1st row #select 7,14,15 columns
colnames (P12_Pre) <- c("MCL","BCL","IgG")                 #rename column names
P12_Pre$Stage <- "Pre"                                     #creating stage identifier variable (pre/post)
P12_Pre$MCL <- as.numeric(as.character(P12_Pre$MCL))       #Converting MCL from factor into numerical
P12_Pre$BCL <- as.numeric(as.character(P12_Pre$BCL))       #Converting BCL from factor into numerical
P12_Pre$IgG <- as.numeric(as.character(P12_Pre$IgG))       #Converting IgG from factor into numerical
P12_Pre$MCL_p <- ecdf(P12_Pre$MCL)(P12_Pre$MCL)            #Creating a percentile variable for MCL
P12_Pre$BCL_p <- ecdf(P12_Pre$BCL)(P12_Pre$BCL)            #Creating a percentile variable for BCL
P12_Pre$IgG_p <- ecdf(P12_Pre$IgG)(P12_Pre$IgG)            #Creating a percentile variable for IgG
P12_Pre$MCL_r <- ifelse(P12_Pre$MCL_p <= 0.33, '1',      
         ifelse(P12_Pre$MCL_p <= 0.66, '2', 
         '3'))
P12_Pre$BCL_r <- ifelse(P12_Pre$BCL_p <= 0.33, '1',   
         ifelse(P12_Pre$BCL_p <= 0.66, '2', 
         '3'))
P12_Pre$IgG_r <- ifelse(P12_Pre$IgG_p <= 0.33, '1',  
         ifelse(P12_Pre$IgG_p <= 0.66, '2', 
         '3'))
P12_Pre_levels <- P12_Pre [,c(8:10)]
P12_Pre_cat <- P12_Pre_levels %>% count (MCL_r, BCL_r, IgG_r)
P12_Pre_cat$n <- ecdf(P12_Pre_cat$n)(P12_Pre_cat$n)

P12_Post <- P12_Post [-1,c(7,14,15)]                      #Performing the same tasks for post-treatment data    
colnames (P12_Post) <- c("MCL","BCL","IgG")            
P12_Post$Stage <- "Post"                                    
P12_Post$MCL <- as.numeric(as.character(P12_Post$MCL))           
P12_Post$BCL <- as.numeric(as.character(P12_Post$BCL))           
P12_Post$IgG <- as.numeric(as.character(P12_Post$IgG))          
P12_Post$MCL_p <- ecdf(P12_Post$MCL)(P12_Post$MCL)          
P12_Post$BCL_p <- ecdf(P12_Post$BCL)(P12_Post$BCL)            
P12_Post$IgG_p <- ecdf(P12_Post$IgG)(P12_Post$IgG)            
P12_Post$MCL_r <- ifelse(P12_Post$MCL_p <= 0.33, '1',      
         ifelse(P12_Post$MCL_p <= 0.66, '2', 
         '3'))
P12_Post$BCL_r <- ifelse(P12_Post$BCL_p <= 0.33, '1',   
         ifelse(P12_Post$BCL_p <= 0.66, '2', 
         '3'))
P12_Post$IgG_r <- ifelse(P12_Post$IgG_p <= 0.33, '1',  
         ifelse(P12_Post$IgG_p <= 0.66, '2', 
         '3'))
P12_Post_levels <- P12_Post [,c(8:10)]
P12_Post_cat <- P12_Post_levels %>% count (MCL_r, BCL_r, IgG_r)
P12_Post_cat$n <- ecdf(P12_Post_cat$n)(P12_Post_cat$n)

P12_Difference <- full_join(P12_Pre_cat, P12_Post_cat,                       #Creating a post-pre treatment data
                   by = c("MCL_r", "BCL_r", "IgG_r")) %>%
                   mutate_if(is.numeric,coalesce,0)
P12_Difference$n <- P12_Difference$n.y - P12_Difference$n.x
P12_Difference$Diff <- ifelse(P12_Difference$n == 0, "Null", 
                    ifelse (P12_Difference$n > 0, "Increasing", 
                    "Decreasing"))
P12_Difference$n <- abs (P12_Difference$n)

### Constructing 3D bubble plots 

library("plotly")
p12_pre_plot <- plot_ly(P12_Pre_cat, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, size = ~n, 
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for p12, Pre-treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL_xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

p12_pre_plot


p12_post_plot <- plot_ly(P12_Post_cat, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, size = ~n,
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for p12, Post-treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL_xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

p12_post_plot


p12_diff_plot <- plot_ly(P12_Difference, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, color = ~Diff, size = ~n, 
                        colors = c('#FF7070', '#4AC6B7', '#1972A4'),
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for p12, (Post-Pre) treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL-xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

p12_diff_plot


### Processing for Patient 21 in a similar fashion 

library("dplyr")
P21_Pre <- P21_Pre [-1,c(7,14,15)]                         #delete 1st row #select 7,14,15 columns
colnames (P21_Pre) <- c("MCL","BCL","IgG")                 #rename column names
P21_Pre$Stage <- "Pre"                                     #creating stage identifier variable (pre/post)
P21_Pre$MCL <- as.numeric(as.character(P21_Pre$MCL))       #Converting MCL from factor into numerical
P21_Pre$BCL <- as.numeric(as.character(P21_Pre$BCL))       #Converting BCL from factor into numerical
P21_Pre$IgG <- as.numeric(as.character(P21_Pre$IgG))       #Converting IgG from factor into numerical
P21_Pre$MCL_p <- ecdf(P21_Pre$MCL)(P21_Pre$MCL)            #Creating a percentile variable for MCL
P21_Pre$BCL_p <- ecdf(P21_Pre$BCL)(P21_Pre$BCL)            #Creating a percentile variable for BCL
P21_Pre$IgG_p <- ecdf(P21_Pre$IgG)(P21_Pre$IgG)            #Creating a percentile variable for IgG
P21_Pre$MCL_r <- ifelse(P21_Pre$MCL_p <= 0.33, '1',      
         ifelse(P21_Pre$MCL_p <= 0.66, '2', 
         '3'))
P21_Pre$BCL_r <- ifelse(P21_Pre$BCL_p <= 0.33, '1',   
         ifelse(P21_Pre$BCL_p <= 0.66, '2', 
         '3'))
P21_Pre$IgG_r <- ifelse(P21_Pre$IgG_p <= 0.33, '1',  
         ifelse(P21_Pre$IgG_p <= 0.66, '2', 
         '3'))
P21_Pre_levels <- P21_Pre [,c(8:10)]
P21_Pre_cat <- P21_Pre_levels %>% count (MCL_r, BCL_r, IgG_r)
P21_Pre_cat$n <- ecdf(P21_Pre_cat$n)(P21_Pre_cat$n)

P21_Post <- P21_Post [-1,c(7,14,15)]                      #Performing the same tasks for post-treatment data    
colnames (P21_Post) <- c("MCL","BCL","IgG")            
P21_Post$Stage <- "Post"                                    
P21_Post$MCL <- as.numeric(as.character(P21_Post$MCL))           
P21_Post$BCL <- as.numeric(as.character(P21_Post$BCL))           
P21_Post$IgG <- as.numeric(as.character(P21_Post$IgG))          
P21_Post$MCL_p <- ecdf(P21_Post$MCL)(P21_Post$MCL)          
P21_Post$BCL_p <- ecdf(P21_Post$BCL)(P21_Post$BCL)            
P21_Post$IgG_p <- ecdf(P21_Post$IgG)(P21_Post$IgG)            
P21_Post$MCL_r <- ifelse(P21_Post$MCL_p <= 0.33, '1',      
         ifelse(P21_Post$MCL_p <= 0.66, '2', 
         '3'))
P21_Post$BCL_r <- ifelse(P21_Post$BCL_p <= 0.33, '1',   
         ifelse(P21_Post$BCL_p <= 0.66, '2', 
         '3'))
P21_Post$IgG_r <- ifelse(P21_Post$IgG_p <= 0.33, '1',  
         ifelse(P21_Post$IgG_p <= 0.66, '2', 
         '3'))
P21_Post_levels <- P21_Post [,c(8:10)]
P21_Post_cat <- P21_Post_levels %>% count (MCL_r, BCL_r, IgG_r)
P21_Post_cat$n <- ecdf(P21_Post_cat$n)(P21_Post_cat$n)

P21_Difference <- full_join(P21_Pre_cat, P21_Post_cat,                #Creating a post-pre treatment data
                   by = c("MCL_r", "BCL_r", "IgG_r")) %>%
                   mutate_if(is.numeric,coalesce,0)
P21_Difference$n <- P21_Difference$n.y - P21_Difference$n.x
P21_Difference$Diff <- ifelse(P21_Difference$n == 0, "Null", 
                    ifelse (P21_Difference$n > 0, "Increasing", 
                    "Decreasing"))
P21_Difference$n <- abs (P21_Difference$n)


library("plotly")
P21_pre_plot <- plot_ly(P21_Pre_cat, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, size = ~n, 
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for P21, Pre-treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL_xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

P21_pre_plot


P21_post_plot <- plot_ly(P21_Post_cat, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, size = ~n,
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for P21, Post-treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL_xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

P21_post_plot


P21_diff_plot <- plot_ly(P21_Difference, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, color = ~Diff, size = ~n, 
                        colors = c('#FF7070', '#4AC6B7', '#1972A4'),
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for P21, (Post-Pre) treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL-xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

P21_diff_plot


### Processing for Patient 29 in a similar fashion 

library("dplyr")
P29_Pre <- P29_Pre [-1,c(7,14,15)]                         #delete 1st row #select 7,14,15 columns
colnames (P29_Pre) <- c("MCL","BCL","IgG")                 #rename column names
P29_Pre$Stage <- "Pre"                                     #creating stage identifier variable (pre/post)
P29_Pre$MCL <- as.numeric(as.character(P29_Pre$MCL))       #Converting MCL from factor into numerical
P29_Pre$BCL <- as.numeric(as.character(P29_Pre$BCL))       #Converting BCL from factor into numerical
P29_Pre$IgG <- as.numeric(as.character(P29_Pre$IgG))       #Converting IgG from factor into numerical
P29_Pre$MCL_p <- ecdf(P29_Pre$MCL)(P29_Pre$MCL)            #Creating a percentile variable for MCL
P29_Pre$BCL_p <- ecdf(P29_Pre$BCL)(P29_Pre$BCL)            #Creating a percentile variable for BCL
P29_Pre$IgG_p <- ecdf(P29_Pre$IgG)(P29_Pre$IgG)            #Creating a percentile variable for IgG
P29_Pre$MCL_r <- ifelse(P29_Pre$MCL_p <= 0.33, '1',      
         ifelse(P29_Pre$MCL_p <= 0.66, '2', 
         '3'))
P29_Pre$BCL_r <- ifelse(P29_Pre$BCL_p <= 0.33, '1',   
         ifelse(P29_Pre$BCL_p <= 0.66, '2', 
         '3'))
P29_Pre$IgG_r <- ifelse(P29_Pre$IgG_p <= 0.33, '1',  
         ifelse(P29_Pre$IgG_p <= 0.66, '2', 
         '3'))
P29_Pre_levels <- P29_Pre [,c(8:10)]
P29_Pre_cat <- P29_Pre_levels %>% count (MCL_r, BCL_r, IgG_r)
P29_Pre_cat$n <- ecdf(P29_Pre_cat$n)(P29_Pre_cat$n)

P29_Post <- P29_Post [-1,c(7,14,15)]                      #Performing the same tasks for post-treatment data    
colnames (P29_Post) <- c("MCL","BCL","IgG")            
P29_Post$Stage <- "Post"                                    
P29_Post$MCL <- as.numeric(as.character(P29_Post$MCL))           
P29_Post$BCL <- as.numeric(as.character(P29_Post$BCL))           
P29_Post$IgG <- as.numeric(as.character(P29_Post$IgG))          
P29_Post$MCL_p <- ecdf(P29_Post$MCL)(P29_Post$MCL)          
P29_Post$BCL_p <- ecdf(P29_Post$BCL)(P29_Post$BCL)            
P29_Post$IgG_p <- ecdf(P29_Post$IgG)(P29_Post$IgG)            
P29_Post$MCL_r <- ifelse(P29_Post$MCL_p <= 0.33, '1',      
         ifelse(P29_Post$MCL_p <= 0.66, '2', 
         '3'))
P29_Post$BCL_r <- ifelse(P29_Post$BCL_p <= 0.33, '1',   
         ifelse(P29_Post$BCL_p <= 0.66, '2', 
         '3'))
P29_Post$IgG_r <- ifelse(P29_Post$IgG_p <= 0.33, '1',  
         ifelse(P29_Post$IgG_p <= 0.66, '2', 
         '3'))
P29_Post_levels <- P29_Post [,c(8:10)]
P29_Post_cat <- P29_Post_levels %>% count (MCL_r, BCL_r, IgG_r)
P29_Post_cat$n <- ecdf(P29_Post_cat$n)(P29_Post_cat$n)

P29_Difference <- full_join(P29_Pre_cat, P29_Post_cat, by = c("MCL_r", "BCL_r", "IgG_r")) %>%
                            mutate_if(is.numeric,coalesce,0)

P29_Difference <- full_join(P29_Pre_cat, P29_Post_cat,                       #Creating a post-pre treatment data
                   by = c("MCL_r", "BCL_r", "IgG_r")) %>%
                   mutate_if(is.numeric,coalesce,0)
P29_Difference$n <- P29_Difference$n.y - P29_Difference$n.x
P29_Difference$Diff <- ifelse(P29_Difference$n == 0, "Null", 
                    ifelse (P29_Difference$n > 0, "Increasing", 
                    "Decreasing"))
P29_Difference$n <- abs (P29_Difference$n)

library("plotly")
P29_pre_plot <- plot_ly(P29_Pre_cat, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, size = ~n, 
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for P29, Pre-treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL_xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

P29_pre_plot


P29_post_plot <- plot_ly(P29_Post_cat, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, size = ~n,
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for P29, Post-treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL_xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

P29_post_plot


P29_diff_plot <- plot_ly(P29_Difference, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, color = ~Diff, size = ~n, 
                        colors = c('#FF7070', '#4AC6B7', '#1972A4'),
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for P29, (Post-Pre) treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL-xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

P29_diff_plot


### Processing for Patient 31 in a similar fashion 

library("dplyr")
P31_Pre <- P31_Pre [-1,c(7,14,15)]                         #delete 1st row #select 7,14,15 columns
colnames (P31_Pre) <- c("MCL","BCL","IgG")                 #rename column names
P31_Pre$Stage <- "Pre"                                     #creating stage identifier variable (pre/post)
P31_Pre$MCL <- as.numeric(as.character(P31_Pre$MCL))       #Converting MCL from factor into numerical
P31_Pre$BCL <- as.numeric(as.character(P31_Pre$BCL))       #Converting BCL from factor into numerical
P31_Pre$IgG <- as.numeric(as.character(P31_Pre$IgG))       #Converting IgG from factor into numerical
P31_Pre$MCL_p <- ecdf(P31_Pre$MCL)(P31_Pre$MCL)            #Creating a percentile variable for MCL
P31_Pre$BCL_p <- ecdf(P31_Pre$BCL)(P31_Pre$BCL)            #Creating a percentile variable for BCL
P31_Pre$IgG_p <- ecdf(P31_Pre$IgG)(P31_Pre$IgG)            #Creating a percentile variable for IgG
P31_Pre$MCL_r <- ifelse(P31_Pre$MCL_p <= 0.33, '1',      
         ifelse(P31_Pre$MCL_p <= 0.66, '2', 
         '3'))
P31_Pre$BCL_r <- ifelse(P31_Pre$BCL_p <= 0.33, '1',   
         ifelse(P31_Pre$BCL_p <= 0.66, '2', 
         '3'))
P31_Pre$IgG_r <- ifelse(P31_Pre$IgG_p <= 0.33, '1',  
         ifelse(P31_Pre$IgG_p <= 0.66, '2', 
         '3'))
P31_Pre_levels <- P31_Pre [,c(8:10)]
P31_Pre_cat <- P31_Pre_levels %>% count (MCL_r, BCL_r, IgG_r)
P31_Pre_cat$n <- ecdf(P31_Pre_cat$n)(P31_Pre_cat$n)

P31_Post <- P31_Post [-1,c(7,14,15)]                      #Performing the same tasks for post-treatment data    
colnames (P31_Post) <- c("MCL","BCL","IgG")            
P31_Post$Stage <- "Post"                                    
P31_Post$MCL <- as.numeric(as.character(P31_Post$MCL))           
P31_Post$BCL <- as.numeric(as.character(P31_Post$BCL))           
P31_Post$IgG <- as.numeric(as.character(P31_Post$IgG))          
P31_Post$MCL_p <- ecdf(P31_Post$MCL)(P31_Post$MCL)          
P31_Post$BCL_p <- ecdf(P31_Post$BCL)(P31_Post$BCL)            
P31_Post$IgG_p <- ecdf(P31_Post$IgG)(P31_Post$IgG)            
P31_Post$MCL_r <- ifelse(P31_Post$MCL_p <= 0.33, '1',      
         ifelse(P31_Post$MCL_p <= 0.66, '2', 
         '3'))
P31_Post$BCL_r <- ifelse(P31_Post$BCL_p <= 0.33, '1',   
         ifelse(P31_Post$BCL_p <= 0.66, '2', 
         '3'))
P31_Post$IgG_r <- ifelse(P31_Post$IgG_p <= 0.33, '1',  
         ifelse(P31_Post$IgG_p <= 0.66, '2', 
         '3'))
P31_Post_levels <- P31_Post [,c(8:10)]
P31_Post_cat <- P31_Post_levels %>% count (MCL_r, BCL_r, IgG_r)
P31_Post_cat$n <- ecdf(P31_Post_cat$n)(P31_Post_cat$n)

P31_Difference <- full_join(P31_Pre_cat, P31_Post_cat, by = c("MCL_r", "BCL_r", "IgG_r")) %>%
                            mutate_if(is.numeric,coalesce,0)

P31_Difference <- full_join(P31_Pre_cat, P31_Post_cat,                       #Creating a post-pre treatment data
                   by = c("MCL_r", "BCL_r", "IgG_r")) %>%
                   mutate_if(is.numeric,coalesce,0)
P31_Difference$n <- P31_Difference$n.y - P31_Difference$n.x
P31_Difference$Diff <- ifelse(P31_Difference$n == 0, "Null", 
                    ifelse (P31_Difference$n > 0, "Increasing", 
                    "Decreasing"))
P31_Difference$n <- abs (P31_Difference$n)

library("plotly")
P31_pre_plot <- plot_ly(P31_Pre_cat, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, size = ~n, 
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for P31, Pre-treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL_xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

P31_pre_plot


P31_post_plot <- plot_ly(P31_Post_cat, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, size = ~n,
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for P31, Post-treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL_xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

P31_post_plot


P31_diff_plot <- plot_ly(P31_Difference, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, color = ~Diff, size = ~n, 
                        colors = c('#FF7070', '#4AC6B7', '#1972A4'),
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for P31, (Post-Pre) treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL-xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

P31_diff_plot


### Processing for Patient 71 in a similar fashion 

library("dplyr")
P71_Pre <- P71_Pre [-1,c(7,14,15)]                         #delete 1st row #select 7,14,15 columns
colnames (P71_Pre) <- c("MCL","BCL","IgG")                 #rename column names
P71_Pre$Stage <- "Pre"                                     #creating stage identifier variable (pre/post)
P71_Pre$MCL <- as.numeric(as.character(P71_Pre$MCL))       #Converting MCL from factor into numerical
P71_Pre$BCL <- as.numeric(as.character(P71_Pre$BCL))       #Converting BCL from factor into numerical
P71_Pre$IgG <- as.numeric(as.character(P71_Pre$IgG))       #Converting IgG from factor into numerical
P71_Pre$MCL_p <- ecdf(P71_Pre$MCL)(P71_Pre$MCL)            #Creating a percentile variable for MCL
P71_Pre$BCL_p <- ecdf(P71_Pre$BCL)(P71_Pre$BCL)            #Creating a percentile variable for BCL
P71_Pre$IgG_p <- ecdf(P71_Pre$IgG)(P71_Pre$IgG)            #Creating a percentile variable for IgG
P71_Pre$MCL_r <- ifelse(P71_Pre$MCL_p <= 0.33, '1',      
         ifelse(P71_Pre$MCL_p <= 0.66, '2', 
         '3'))
P71_Pre$BCL_r <- ifelse(P71_Pre$BCL_p <= 0.33, '1',   
         ifelse(P71_Pre$BCL_p <= 0.66, '2', 
         '3'))
P71_Pre$IgG_r <- ifelse(P71_Pre$IgG_p <= 0.33, '1',  
         ifelse(P71_Pre$IgG_p <= 0.66, '2', 
         '3'))
P71_Pre_levels <- P71_Pre [,c(8:10)]
P71_Pre_cat <- P71_Pre_levels %>% count (MCL_r, BCL_r, IgG_r)
P71_Pre_cat$n <- ecdf(P71_Pre_cat$n)(P71_Pre_cat$n)

P71_Post <- P71_Post [-1,c(7,14,15)]                      #Performing the same tasks for post-treatment data    
colnames (P71_Post) <- c("MCL","BCL","IgG")            
P71_Post$Stage <- "Post"                                    
P71_Post$MCL <- as.numeric(as.character(P71_Post$MCL))           
P71_Post$BCL <- as.numeric(as.character(P71_Post$BCL))           
P71_Post$IgG <- as.numeric(as.character(P71_Post$IgG))          
P71_Post$MCL_p <- ecdf(P71_Post$MCL)(P71_Post$MCL)          
P71_Post$BCL_p <- ecdf(P71_Post$BCL)(P71_Post$BCL)            
P71_Post$IgG_p <- ecdf(P71_Post$IgG)(P71_Post$IgG)            
P71_Post$MCL_r <- ifelse(P71_Post$MCL_p <= 0.33, '1',      
         ifelse(P71_Post$MCL_p <= 0.66, '2', 
         '3'))
P71_Post$BCL_r <- ifelse(P71_Post$BCL_p <= 0.33, '1',   
         ifelse(P71_Post$BCL_p <= 0.66, '2', 
         '3'))
P71_Post$IgG_r <- ifelse(P71_Post$IgG_p <= 0.33, '1',  
         ifelse(P71_Post$IgG_p <= 0.66, '2', 
         '3'))
P71_Post_levels <- P71_Post [,c(8:10)]
P71_Post_cat <- P71_Post_levels %>% count (MCL_r, BCL_r, IgG_r)
P71_Post_cat$n <- ecdf(P71_Post_cat$n)(P71_Post_cat$n)

P71_Difference <- full_join(P71_Pre_cat, P71_Post_cat, by = c("MCL_r", "BCL_r", "IgG_r")) %>%
                            mutate_if(is.numeric,coalesce,0)

P71_Difference <- full_join(P71_Pre_cat, P71_Post_cat,                       #Creating a post-pre treatment data
                   by = c("MCL_r", "BCL_r", "IgG_r")) %>%
                   mutate_if(is.numeric,coalesce,0)
P71_Difference$n <- P71_Difference$n.y - P71_Difference$n.x
P71_Difference$Diff <- ifelse(P71_Difference$n == 0, "Null", 
                    ifelse (P71_Difference$n > 0, "Increasing", 
                    "Decreasing"))
P71_Difference$n <- abs (P71_Difference$n)

library("plotly")
P71_pre_plot <- plot_ly(P71_Pre_cat, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, size = ~n, 
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for P71, Pre-treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL_xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

P71_pre_plot


P71_post_plot <- plot_ly(P71_Post_cat, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, size = ~n,
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for P71, Post-treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL_xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

P71_post_plot


P71_diff_plot <- plot_ly(P71_Difference, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, color = ~Diff, size = ~n, 
                        colors = c('#FF7070', '#4AC6B7', '#1972A4'),
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for P71, (Post-Pre) treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL-xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

P71_diff_plot

### Processing for all the patients (combined) in a similar fashion

P12_Diff <- P12_Difference [c(-6,-7)]
P12_Diff <- P12_Diff %>% rename(n.x.12 = n.x, n.y.12 = n.y)
P21_Diff <- P21_Difference [c(-6,-7)]
P21_Diff <- P21_Diff %>% rename(n.x.21 = n.x, n.y.21 = n.y)      
P29_Diff <- P29_Difference [c(-6,-7)]
P29_Diff <- P29_Diff %>% rename(n.x.29 = n.x, n.y.29 = n.y)
P31_Diff <- P31_Difference [c(-6,-7)]
P31_Diff <- P31_Diff %>% rename(n.x.31 = n.x, n.y.31 = n.y)
P71_Diff <- P71_Difference [c(-6,-7)]
P71_Diff <- P71_Diff %>% rename(n.x.71 = n.x, n.y.71 = n.y)
All_Diff <- cbind (P12_Diff, P21_Diff, P29_Diff, P31_Diff, P71_Diff)
All_Diff <- All_Diff [-c(6:8, 11:13, 16:18, 21:23)]
All_Diff$n.x <- rowMeans (All_Diff[c(4,6,8,10,12)])
All_Diff$n.y <- rowMeans (All_Diff[c(5,7,9,11,13)])
All_Diff$n <- All_Diff$n.y - All_Diff$n.x
All_Diff$Diff <- ifelse(All_Diff$n == 0, "Null", 
                    ifelse (All_Diff$n > 0, "Increasing", 
                    "Decreasing"))
All_Diff$n <- abs (All_Diff$n)


library("plotly")
All_pre_plot <- plot_ly(All_Diff, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, size = ~n.x, 
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for All, Pre-treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL_xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

All_pre_plot


All_post_plot <- plot_ly(All_Diff, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, size = ~n.y,
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for All, Post-treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL_xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

All_post_plot


All_diff_plot <- plot_ly(All_Diff, x = ~MCL_r, y = ~BCL_r, z = ~IgG_r, color = ~Diff, size = ~n, 
                        colors = c('#FF7070', '#4AC6B7'),
                 type = "scatter3d", mode = "markers",
                 marker = list(symbol = 'circle', sizemode ='diameter')) %>%
                 layout (title = 'Anti-apoptopic protiens levels for All, (Post-Pre) treatment',
                            scene = list(xaxis = list(title = 'McL',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwidth = 2),
                           yaxis = list(title = 'BcL-2',
                                  range = c(0, 3),
                                  zerolinewidth = 1,
                                  ticklen = 5,
                                  gridwith = 2),
                           zaxis = list(title = 'BcL-xL',
                                        range = c(0, 3),
                                        zerolinewidth = 1,
                                        ticklen = 5,
                                        gridwith = 2)))

All_diff_plot



### SUPPLEMENTAL PLOTS (SCATTER PLOTS AND BAR PLOTS)

#### Delete all data from the environment 
#### Import and view data through the environment window in RStudio
#### Re-arrange, rename, & normalize (percentile transformation) patient data

P12_Post <- P12_Post [-1,c(7,14,15)]           #delete 1st row #select 7,14,15 columns
colnames (P12_Post) <- c("MCL","BCL","IgG")    #rename column names
P12_Post$Stage <- "Post"                       #creating stage identifier variable (pre/post)
P12 <- rbind (P12_Pre, P12_Post)               #binding (merging) the pre and post data
P12$Patient <- "P12"                           #Creating Patient identifier variable
P12$MCL <- as.numeric(as.character(P12$MCL))   #Converting MCL from factor into numerical
P12$BCL <- as.numeric(as.character(P12$BCL))   #Converting BCL from factor into numerical
P12$IgG <- as.numeric(as.character(P12$IgG))   #Converting IgG from factor into numerical
P12$MCL_p <- ecdf(P12$MCL)(P12$MCL)            #Creating a percentile variable for MCL
P12$BCL_p <- ecdf(P12$BCL)(P12$BCL)            #Creating a percentile variable for BCL
P12$IgG_p <- ecdf(P12$IgG)(P12$IgG)            #Creating a percentile variable for IgG


P21_Pre <- P21_Pre [-1,c(7,14,15)]            #Similar data manipulation for P21
colnames (P21_Pre) <- c("MCL","BCL","IgG")    
P21_Pre$Stage <- "Pre"                        
P21_Post <- P21_Post [-1,c(7,14,15)]          
colnames (P21_Post) <- c("MCL","BCL","IgG")   
P21_Post$Stage <- "Post"                      
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
P29_Pre$Stage <- "Pre"                        
P29_Post <- P29_Post [-1,c(7,14,15)]          
colnames (P29_Post) <- c("MCL","BCL","IgG")   
P29_Post$Stage <- "Post"                      
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
P31_Pre$Stage <- "Pre"                        
P31_Post <- P31_Post [-1,c(7,14,15)]          
colnames (P31_Post) <- c("MCL","BCL","IgG")   
P31_Post$Stage <- "Post"                      
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
P71_Pre$Stage <- "Pre"                        
P71_Post <- P71_Post [-1,c(7,14,15)]          
colnames (P71_Post) <- c("MCL","BCL","IgG")   
P71_Post$Stage <- "Post"                      
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

#### 3D SCATTERPLOTS (CONTINUOUS DATA)

##### Patients (Individual & combined) with non-transformed data

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

##### Patients (Individual & combined) with percentile-transformed data

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


