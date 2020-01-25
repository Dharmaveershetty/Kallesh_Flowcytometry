# Kallesh_Flowcytometry
Flowcytometry_data_visualization_Kallesh

## PACKAGES (INSTALLATION AND LOADING)
install.packages ("scatterplot3d")
library ("scatterplot3d")

## DATA (IMPORTATION)

### Import data using the import tab in RStudio
View(A1_PT21_PRE_VEN_092016_Mcl1_BCLxL_Bcl2_BIM) #View Data file after 
                                                  changing the 'dash' and 'space'
                                                  symbols to 'underscore'
                                                  and deleting the 'brackets'

## DATA REARRANGEMENT (Creating two data subsets (pre & post) with 3 selected variables each)

### Selecting the required variables
Variables <- c ("MCL,BAX AF488-Alexa Fluor™ 488-A", "Bcl-2 PE-R-PE-A", "IgG Rb AF594-AF594-A")

### Creating two data subsets (pre and post)
P12.pre <- (D1_PT12_PRE_VEN_050117_Mcl1_BCLxL_Bcl2_BIM [Variables])
P12.post <- (E1_PT12_POST_VEN_053017_Mcl1_BCLxL_Bcl2_BIM [Variables])
P21.pre <- (A1_PT21_PRE_VEN_092016_Mcl1_BCLxL_Bcl2_BIM [Variables])
P21.post <- (C1_PT21_POST_VEN_011117_Mcl1_BCLxL_Bcl2_BIM [Variables])
P29.pre <- (D1_PT29_PRE_VEN_040516_Mcl1_BCLxL_Bcl2_BIM [Variables])
P29.post <- (D2_PT29_PRE_VEN_040516_Mcl1_BCLxL_Bcl2_BIM [Variables])
P31.pre <- (A5_PT31_Pre_VEN_011718_Mcl1_Bcl2_BclxL_Bim [Variables])
P31.post <- (A6_PT31_Post_VEN_092519_Mcl1_Bcl2_BclxL_Bim [Variables])

### Renaming the variables in the data subset
P12.pre$MCL <- P12.pre$"MCL,BAX AF488-Alexa Fluor™ 488-A"
P12.post$MCL <- P12.post$"MCL,BAX AF488-Alexa Fluor™ 488-A"   
P12.pre$"MCL,BAX AF488-Alexa Fluor™ 488-A" <- NULL
P12.post$"MCL,BAX AF488-Alexa Fluor™ 488-A" <- NULL
P12.pre$BCL <- P12.pre$"Bcl-2 PE-R-PE-A"
P12.post$BCL <- P12.post$"Bcl-2 PE-R-PE-A"
P12.pre$"Bcl-2 PE-R-PE-A" <- NULL
P12.post$"Bcl-2 PE-R-PE-A" <- NULL
P12.pre$IgG <- P12.pre$"IgG Rb AF594-AF594-A"
P12.post$IgG <- P12.post$"IgG Rb AF594-AF594-A"
P12.pre$"IgG Rb AF594-AF594-A" <- NULL
P12.post$"IgG Rb AF594-AF594-A" <- NULL

P21.pre$MCL <- P21.pre$"MCL,BAX AF488-Alexa Fluor™ 488-A"
P21.post$MCL <- P21.post$"MCL,BAX AF488-Alexa Fluor™ 488-A"   
P21.pre$"MCL,BAX AF488-Alexa Fluor™ 488-A" <- NULL
P21.post$"MCL,BAX AF488-Alexa Fluor™ 488-A" <- NULL
P21.pre$BCL <- P21.pre$"Bcl-2 PE-R-PE-A"
P21.post$BCL <- P21.post$"Bcl-2 PE-R-PE-A"
P21.pre$"Bcl-2 PE-R-PE-A" <- NULL
P21.post$"Bcl-2 PE-R-PE-A" <- NULL
P21.pre$IgG <- P21.pre$"IgG Rb AF594-AF594-A"
P21.post$IgG <- P21.post$"IgG Rb AF594-AF594-A"
P21.pre$"IgG Rb AF594-AF594-A" <- NULL
P21.post$"IgG Rb AF594-AF594-A" <- NULL

P29.pre$MCL <- P29.pre$"MCL,BAX AF488-Alexa Fluor™ 488-A"
P29.post$MCL <- P29.post$"MCL,BAX AF488-Alexa Fluor™ 488-A"   
P29.pre$"MCL,BAX AF488-Alexa Fluor™ 488-A" <- NULL
P29.post$"MCL,BAX AF488-Alexa Fluor™ 488-A" <- NULL
P29.pre$BCL <- P29.pre$"Bcl-2 PE-R-PE-A"
P29.post$BCL <- P29.post$"Bcl-2 PE-R-PE-A"
P29.pre$"Bcl-2 PE-R-PE-A" <- NULL
P29.post$"Bcl-2 PE-R-PE-A" <- NULL
P29.pre$IgG <- P29.pre$"IgG Rb AF594-AF594-A"
P29.post$IgG <- P29.post$"IgG Rb AF594-AF594-A"
P29.pre$"IgG Rb AF594-AF594-A" <- NULL
P29.post$"IgG Rb AF594-AF594-A" <- NULL

P31.pre$MCL <- P31.pre$"MCL,BAX AF488-Alexa Fluor™ 488-A"
P31.post$MCL <- P31.post$"MCL,BAX AF488-Alexa Fluor™ 488-A"   
P31.pre$"MCL,BAX AF488-Alexa Fluor™ 488-A" <- NULL
P31.post$"MCL,BAX AF488-Alexa Fluor™ 488-A" <- NULL
P31.pre$BCL <- P31.pre$"Bcl-2 PE-R-PE-A"
P31.post$BCL <- P31.post$"Bcl-2 PE-R-PE-A"
P31.pre$"Bcl-2 PE-R-PE-A" <- NULL
P31.post$"Bcl-2 PE-R-PE-A" <- NULL
P31.pre$IgG <- P31.pre$"IgG Rb AF594-AF594-A"
P31.post$IgG <- P31.post$"IgG Rb AF594-AF594-A"
P31.pre$"IgG Rb AF594-AF594-A" <- NULL
P31.post$"IgG Rb AF594-AF594-A" <- NULL


### Adding a new variable into each dataset (status = pre/post)
P12.pre$Status <- rep("red",nrow(P12.pre))
P12.post$Status <- rep("blue",nrow(P12.post))

P21.pre$Status <- rep("red",nrow(P21.pre))
P21.post$Status <- rep("blue",nrow(P21.post))

P29.pre$Status <- rep("red",nrow(P29.pre))
P29.post$Status <- rep("blue",nrow(P29.post))

P31.pre$Status <- rep("red",nrow(P31.pre))
P31.post$Status <- rep("blue",nrow(P31.post))

### Merging the two datasets
P12 <- rbind (P12.pre, P12.post)
P21 <- rbind (P21.pre, P21.post)
P29 <- rbind (P29.pre, P29.post)
P31 <- rbind (P31.pre, P31.post)

### Normalizing the individual patient datasets with the patient median value

#### Median
median.MCL.P12 <- median (P12$MCL)
median.MCL.P21 <- median (P21$MCL)
median.MCL.P29 <- median (P29$MCL)
median.MCL.P31 <- median (P31$MCL)

median.BCL.P12 <- median (P12$BCL)
median.BCL.P21 <- median (P21$BCL)
median.BCL.P29 <- median (P29$BCL)
median.BCL.P31 <- median (P31$BCL)

median.IgG.P12 <- median (P12$IgG)
median.IgG.P21 <- median (P21$IgG)
median.IgG.P29 <- median (P29$IgG)
median.IgG.P31 <- median (P31$IgG)

#### Normalized Values (adding a new variable)
P12$MCL_n <- P12$MCL/median.MCL.P12
P12$BCL_n <- P12$BCL/median.BCL.P12
P12$IgG_n <- P12$IgG/median.IgG.P12

P21$MCL_n <- P21$MCL/median.MCL.P21
P21$BCL_n <- P21$BCL/median.BCL.P21
P21$IgG_n <- P21$IgG/median.IgG.P21

P29$MCL_n <- P29$MCL/median.MCL.P29
P29$BCL_n <- P29$BCL/median.BCL.P29
P29$IgG_n <- P29$IgG/median.IgG.P29

P31$MCL_n <- P31$MCL/median.MCL.P31
P31$BCL_n <- P31$BCL/median.BCL.P31
P31$IgG_n <- P31$IgG/median.IgG.P31

#### Merged Group Dataset with normalized values
P_All <- rbind (P12, P21, P29, P31)


## 3D SCATTERPLOTS

### Individual

library (scatterplot3d)
with(P12,
   scatterplot3d(MCL,
                 BCL,
                 IgG,
                 color=Status,
                 main="Patient12",
                 xlab="MCL",
                 ylab="BCL",
                 zlab="IgG"
                 ))
                 

library (scatterplot3d)
with(P21,
   scatterplot3d(MCL,
                 BCL,
                 IgG,
                 color=Status,
                 main="Patient21",
                 xlab="MCL",
                 ylab="BCL",
                 zlab="IgG"
                 ))


library (scatterplot3d)
with(P29,
   scatterplot3d(MCL,
                 BCL,
                 IgG,
                 color=Status,
                 main="Patient29",
                 xlab="MCL",
                 ylab="BCL",
                 zlab="IgG"
                 ))

library (scatterplot3d)
with(P31,
   scatterplot3d(MCL,
                 BCL,
                 IgG,
                 color=Status,
                 main="Patient31",
                 xlab="MCL",
                 ylab="BCL",
                 zlab="IgG"
                 ))

### Normalized Group 3D Scatter Plot

library (scatterplot3d)
with(P_All,
   scatterplot3d(MCL,
                 BCL,
                 IgG,
                 color=Status,
                 main="All Patients_Normalized_Values",
                 xlab="MCL_n",
                 ylab="BCL_n",
                 zlab="IgG_n"
                 ))
