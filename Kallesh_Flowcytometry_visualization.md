# Kallesh_Flowcytometry

## 3D BUBBLE PLOTS (CATEGORICAL DATA)

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


#### STACKED COLUMN CHART (CATEGORICAL DATA)

##### Patient12
###### Categorization and summation 
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