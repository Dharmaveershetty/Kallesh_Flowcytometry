# Kallesh_Flowcytometry
Flowcytometry_data_visualization_Kallesh

## Installing and loading the required package
install.packages ("scatterplot3d")
library ("scatterplot3d")

## Data importation
### Import data using the import tab in RStudio
View(A1_PT21_PRE_VEN_092016_Mcl1_BCLxL_Bcl2_BIM) #View Data file 

## Creating two data subsets (pre & post) with 3 selected variables each
### Selecting the required variables
Variables <- c ("MCL,BAX AF488-Alexa Fluor™ 488-A", "Bcl-2 PE-R-PE-A", "IgG Rb AF594-AF594-A")
### Creating two data subsets (pre and post)
P21.pre <- (A1_PT21_PRE_VEN_092016_Mcl1_BCLxL_Bcl2_BIM [Variables])
P21.post <- (C1_PT21_POST_VEN_011117_Mcl1_BCLxL_Bcl2_BIM [Variables])
### Renaming the variables in the data subset
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
### Adding a new variable into each dataset (status = pre/post)
P21.pre$Status <- rep("red",nrow(P21.pre))
P21.post$Status <- rep("blue",nrow(P21.post))
### Merging the two datasets
P21 <- rbind (P21.pre, P21.post)


## 3d Scatterplot
### Creating different shapes and colors for pre and post data
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
                 








