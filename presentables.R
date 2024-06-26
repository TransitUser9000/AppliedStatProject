
################################################################################
# DATA LOADING and GENERAL PREPROCESSING

library(tidyverse)
train_df <- read_tsv("Data/ticdata2000.txt")
test_df <- read_tsv("Data/ticeval2000.txt")
test_target <- read_tsv("Data/tictgts2000.txt")
descriptive_colnames <- read_delim("Data/descriptive_colnames.csv", delim=";")
descriptive_colnames$Description[2] <- "Numberofhouses1" # change them to get rid of special characters
descriptive_colnames$Description[3] <- "Avgsizehousehold1"

L0_cat <- read_delim("Data/L0_categories.csv", delim=";", col_names = F)
L0_cat[5, 2] <- "Mixed Seniors (Cat 5)" 

L2_cat <- read_delim("Data/L2_categories.csv", delim=";", col_names = F)
L2_cat

train_df <- train_df %>% setNames(descriptive_colnames$Description)
test_df <- test_df %>% setNames(descriptive_colnames$Description)

################################################################################
# CREATE DATA SUBSETS

dom_socialclass <- 25:29
dom_car <- 32:34
dom_income <- 37:41
dom_education <- 16:18
dom_household <- 14:15
dom_religion <- 6:9
dom_job <- 19:24

dom_contrib <- 44:64
dom_nopol <- 65:86

cm_education <- train_df[, c(5, dom_education)] %>% 
  group_by(CustomermaintypeseeL2) %>% 
  summarise(across(everything(), mean))
cm_education <- cm_education[,2:ncol(cm_education)]
rownames(cm_education) <- L2_cat[, 2] %>% t()


cm_car <- train_df[, c(5, dom_car)] %>% 
  group_by(CustomermaintypeseeL2) %>% 
  summarise(across(everything(), mean)) 
cm_car <- cm_car[,2:ncol(cm_car)]
rownames(cm_car) <- L2_cat[, 2] %>% t()


cm_income <- train_df[, c(5, dom_income)] %>% 
  group_by(CustomermaintypeseeL2) %>% 
  summarise(across(everything(), mean))
cm_income <- cm_income[,2:ncol(cm_income)]
rownames(cm_income) <- L2_cat[, 2] %>% t()

cm_job <- train_df[, c(5, dom_job)] %>% 
  group_by(CustomermaintypeseeL2) %>% 
  summarise(across(everything(), mean)) 
cm_job <- cm_job[,2:ncol(cm_job)]
rownames(cm_job) <- L2_cat[, 2] %>% t()

cm_nopol <- train_df[, c(5, dom_nopol)] %>% 
  group_by(CustomermaintypeseeL2) %>% 
  summarise(across(everything(), mean)) 
cm_nopol <- cm_nopol[,2:ncol(cm_nopol)]
rownames(cm_nopol) <- L2_cat[, 2] %>% t()

cm_contrib <- train_df[, c(5, dom_contrib)] %>% 
  group_by(CustomermaintypeseeL2) %>% 
  summarise(across(everything(), mean)) 
cm_contrib <- cm_contrib[,2:ncol(cm_contrib)]
rownames(cm_contrib) <- L2_cat[, 2] %>% t()

cm_household <- train_df[, c(5, dom_household)] %>% 
  group_by(CustomermaintypeseeL2) %>% 
  summarise(across(everything(), mean)) 
cm_household <- cm_household[,2:ncol(cm_household)]
rownames(cm_household) <- L2_cat[, 2] %>% t()

cm_religion <- train_df[, c(5, dom_religion)] %>% 
  group_by(CustomermaintypeseeL2) %>% 
  summarise(across(everything(), mean)) 
cm_religion <- cm_religion[,2:ncol(cm_religion)]
rownames(cm_religion) <- L2_cat[, 2] %>% t()

cm_socialclass <- train_df[, c(5, dom_socialclass)] %>% 
  group_by(CustomermaintypeseeL2) %>% 
  summarise(across(everything(), mean)) 
cm_socialclass <- cm_socialclass[,2:ncol(cm_socialclass)]
rownames(cm_socialclass) <- L2_cat[, 2] %>% t()

cs_income <- train_df[, c(1, dom_income)] %>% 
  group_by(CustomerSubtypeseeL0) %>% 
  summarise(across(everything(), mean))
cs_income <- cs_income[1:40,2:ncol(cs_income)]
rownames(cs_income) <- L0_cat[1:40, 2] %>% t()

cs_job <- train_df[, c(1, dom_job)] %>% 
  group_by(CustomerSubtypeseeL0) %>% 
  summarise(across(everything(), mean)) 
cs_job <- cs_job[1:40,2:ncol(cs_job)]
rownames(cs_job) <- L0_cat[1:40, 2] %>% t()

################################################################################
# CA
library(FactoMineR)

# summary(CA(ca_df) , nbelements = Inf)

ca_df <- t(cm_nopol)

CA(ca_df)
rownames(cmtype)

#TODO aus Gespr. schauen wie wir Prozentzahlen implementieren
# --> farmers seem to be outlier , thus they are excluded by 1:9 above in cmtype creation

cm_job %>% t() %>% CA()
cs_job %>% t() %>% CA()
cm_socialclass %>% t() %>% CA()
cm_contrib %>% t() %>% CA()
cm_education %>% t() %>% CA()
cm_car %>% t() %>% CA()


################################################################################
# CCA

library(CCA)

ccX <- cm_education
ccY <- cm_car

colnames(cm_education)
colnames(cm_car)
cc_result <- cc(ccX, ccY)
cc_result
plotable <- cbind(cc_result$scores$xscores[,1], cc_result$scores$yscores[,1])
plotable
plot(plotable, xlab = "Dimension 1", ylab = "Dimension 2")
text(plotable, labels = rownames(plotable), cex = 0.8, pos = 4)

#-----------

ccX <- cm_income
ccY <- cm_job

cc_result <- cc(ccX, ccY)
cc_result
plotable <- cbind(cc_result$scores$xscores[,1], cc_result$scores$yscores[,1])
plotable
plot(plotable, xlab = "Dimension 1", ylab = "Dimension 2")
text(plotable, labels = rownames(plotable), cex = 0.8, pos = 4)

#----------------

ccX <- cm_religion
ccY <- cs_job

cc_result <- cc(ccX, ccY)
cc_result
plotable <- cbind(cc_result$scores$xscores[,1], cc_result$scores$yscores[,1])
plotable
plot(plotable, xlab = "Dimension 1", ylab = "Dimension 2")
text(plotable, labels = rownames(plotable), cex = 0.8, pos = 4)

#TODO find why always the same plot

################################################################################
# MDS
par(mfrow=c(1,1))
mds_input_df <- t(cm_car)

train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4)

mds_input_df <- t(cm_religion)

train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4)

mds_input_df <- t(cm_household)

train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4)
