#########
# MDS TRIAL:
# STEP 1: Create distance and weight matrix
# STEP 2: Create coordinates and distances files
# STEP 3: Create plot
# Author: Nena Oana
#########

# You will need these packages:

library(smacof)
library(readstata13)
library(ggplot2)

# Set your working directory:

setwd("~/Desktop/MDS_R_final")

# Load the data:

GM <- read.dta13("only_germany.dta")
head(GM)
election_GM <- GM
save(election_GM, file ="election_GM.rda")
load("election_GM.rda")
head(election_GM)

####
# STEP 1: Create dist and weight matrix (equivalent of the mdsmat8 command in STATA):
####

MGM <- pd_mdsmat(data=election_GM, 
                 party_var="partys_all", 
                 issue_var="issue_agg_all",
                 proximity_var = "direction_agg_all",
                 year_var="year_election",
                 cell=TRUE, 
                 min=20, 
                 imin=3)

#=>mdsmat

# this function has the same options as the mdsmat one, 
# you can therefore changhe the following defaults:
# years= NULL - the years to be included, if more than one election (not recommended yet)
# noweight = FALSE  - If noweight is TRUE, the matrix of weights is not computed
# row = FALSE - specifies that weights are computed separately for each party (and within each year if the option year is specified)
# cell = FALSE - weights are computed separately for each election (100% =one election)
# min = 1  - min -  minimum number of observations for a party in an election
# imin = NULL - minimum % of observations for an issue to be included


####
# Note: Most of the data that you sent me for testing was already after Step 1 
# as it had distances and weights!!!
####

####
# STEP 2: Create distances and coordinates (equivalent to the SPSS step):
####

DI_CO <- pd_pscale(mdsmat = MGM,
                   ndim = 2)

#=>distmat

# MAT - the object created in step 1;
# ndim - the number of dimensions, default is 2, but can be changes for testing the stess;
# Keep in mind though that the following steps (plotting, etc.) are only for the case of ndim=2;

#######
# Step 3: Rescale coordinates:
#######

RS_COORD <- pd_rescale(mdsmat = MGM, 
                       pscalemat = DI_CO,  
                       n_issues = 11, 
                       years= 1)

#=>rescalemat

# OLD_MAT - object created in Step 1;
# NEW_MAT - onject created in Step 2;
# n_issues - number of issues left after deletion;
# years - number of years included;

#####
# Step 4: Rotate coordinates:
#####

RS_COORD_r <- pd_mdsrot(rescalemat = RS_COORD, 
                        WF = "10", 
                        EL = "21")

#=>coordmat

# RS_COORD - the rescaled coordinates produced in Step 3;
# WF - the number of the welfare issue, in this case "10"
# EL - the number of the ecolib issue, in this case "21"

#####
# Step 5: Plotting:
#####

# Get the labels and be careful to follow the order in the RS_COORD_r data:
my_labs <- pd_labelattr(data = GM,
                       party_var="partys_all", 
                       issue_var="issue_agg_all",
                       coordmat = RS_COORD_r)

# Plot
# 10 - welfare
# 21 - ecolib
# 50 - cultlib
# 80 - defense

pd_mdsplot(coordmat = RS_COORD_r, 
           labels = my_labs,
           WF = "10",
           EL = "21",
           CL = "50",
           DF = "80")

# The plot has several options that can be used,
# Here are all on them with their default values:

# RS_COORD_r - the rotated coordinates produced in Step 4;
# labels = NULL - the vector containing the labels in the right order;
# point_col = "black" - the colour of the dots;
# point_size = 1.5 - size of the dots;
# text_col = "black" - colour of labels;
# text_size = 3 - size of labels;
# line_col = "red" - colour of lines between issues;
# title = "MDS" - title for the entire plot;
# WF = "10" - the number of the welfare issue (default is "10")
# EL = "21" - the number of the ecolib issue (default is "21")
# CL = "50" - the number of the cultlib issue (default is "50")
# DF = "80" - the number of the defense issue (default is "80")






