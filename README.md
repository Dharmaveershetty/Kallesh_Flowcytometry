# Kallesh_Flowcytometry
Flowcytometry_data_visualization_Kallesh

## Installing the required package
install.packages ("scatterplot3d")

## Data importation
#Import data using the import tab in RStudio
View(A1_PT21_PRE_VEN_092016_Mcl1_BCLxL_Bcl2_BIM) #View Data file 

## Creating two data subsets (pre & post) with 3 selected variables each
# Selecting the required variables
Variables <- c ("MCL,BAX AF488-Alexa Fluor™ 488-A", "Bcl-2 PE-R-PE-A", "IgG Rb AF594-AF594-A")
# Creating two data subsets (pre and post)
P21.pre <- (A1_PT21_PRE_VEN_092016_Mcl1_BCLxL_Bcl2_BIM [Variables])
P21.post <- (C1_PT21_POST_VEN_011117_Mcl1_BCLxL_Bcl2_BIM [Variables])
# Renaming the variables in the data subset
P21.pre$MCL.pre <- P21.pre$"MCL,BAX AF488-Alexa Fluor™ 488-A"
P21.post$MCL.post <- P21.post$"MCL,BAX AF488-Alexa Fluor™ 488-A"   
P21.pre$"MCL,BAX AF488-Alexa Fluor™ 488-A" <- NULL
P21.post$"MCL,BAX AF488-Alexa Fluor™ 488-A" <- NULL
P21.pre$BCL.pre <- P21.pre$"Bcl-2 PE-R-PE-A"
P21.post$BCL.post <- P21.post$"Bcl-2 PE-R-PE-A"
P21.pre$"Bcl-2 PE-R-PE-A" <- NULL
P21.post$"Bcl-2 PE-R-PE-A" <- NULL
P21.pre$IgG.pre <- P21.pre$"IgG Rb AF594-AF594-A"
P21.post$IgG.post <- P21.post$"IgG Rb AF594-AF594-A"
P21.pre$"IgG Rb AF594-AF594-A" <- NULL
P21.post$"IgG Rb AF594-AF594-A" <- NULL






## Constructing a 3d Scatterplot
scatterplot3d(A1_PT21_PRE_VEN_092016_Mcl1_BCLxL_Bcl2_BIM [MCL,BAX AF488-Alexa Fluor™ 488-A`, Bcl-2 PE-R-PE-A, IgG Rb AF594-AF594-A])
