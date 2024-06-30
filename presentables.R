
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
dom_job <- 20:24

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

cm_caravan <- train_df[, c(5, 86)] %>% 
  group_by(CustomermaintypeseeL2) %>% 
  summarise(across(everything(), mean)) 
cm_caravan <- cm_caravan[,2:ncol(cm_caravan)]
rownames(cm_caravan) <- L2_cat[, 2] %>% t()

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

cs_caravan <- train_df[, c(1, 86)] %>% 
  group_by(CustomerSubtypeseeL0) %>% 
  summarise(across(everything(), mean)) 
cs_caravan <- cs_caravan[1:40,2:ncol(cs_caravan)]
rownames(cs_caravan) <- L0_cat[1:40, 2] %>% t()

################################################################################

# finding patterns in which customers choose to use the caravan policy
caravan_customers_main <- train_df %>% group_by(CustomermaintypeseeL2, `Numberofmobilehomepolicies0-1`) %>% 
  summarise(no_caravan = n()) %>% 
  group_by(CustomermaintypeseeL2) %>% 
  mutate(share = no_caravan / sum(no_caravan))  %>% 
  filter(`Numberofmobilehomepolicies0-1` == 1) %>% 
  left_join(L2_cat, by = c(CustomermaintypeseeL2 = "X1")) %>%   
  arrange(desc(share)) %>% 
  view()

ggplot(caravan_customers_main, aes(x = reorder(X2, -share), y = share)) + 
  geom_bar(stat = "identity") + 
  labs(x = "customer main type", y = "share of no. mobile home policies = 1") +
  theme_classic()
# used in MDS section of presentation for verification


caravan_customers_sub <- train_df %>% group_by(CustomerSubtypeseeL0, `Numberofmobilehomepolicies0-1`) %>% 
  summarise(no_caravan = n()) %>% 
  group_by(CustomerSubtypeseeL0) %>% 
  mutate(share = no_caravan / sum(no_caravan))  %>% 
  filter(`Numberofmobilehomepolicies0-1` == 1) %>% 
  left_join(L0_cat, by = c(CustomerSubtypeseeL0 = "X1")) %>% 
  arrange(desc(share)) %>% 
  view()

ggplot(caravan_customers_sub[1:10,], aes(x = reorder(X2, -share), y = share)) + 
  geom_bar(stat = "identity") + 
  labs(x = "customer sub type", y = "share of no. mobile home policies = 1") + 
  theme_classic()


################################################################################
# CA
library(FactoMineR)
par(mfrow=c(2,2))

cm_nopol %>% t() %>% CA()
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
par(mfrow=c(1,2))

library(CCA)
colnames(cm_car)
ccX <- cm_car[,1:2] # only taking the positive directing columns, i.e. 1 or 2 cars
ccY <-cm_education

cc_result <- cc(ccX, ccY)
cc_result
plotable <- cbind(cc_result$scores$xscores[,1], cc_result$scores$yscores[,1])
plotable
plot(plotable, xlab = "Dimension 1", ylab = "Dimension 2", xlim=c(-1.6, 1.6),
     ylim=c(-2, 1.5))
text(plotable, labels = rownames(plotable), cex = 0.8, pos = 4)


ccX <- cm_religion[,1:3]
ccY <- cm_education

cc_result <- cc(ccX, ccY)
cc_result
plotable <- cbind(cc_result$scores$xscores[,1], cc_result$scores$yscores[,1])
plotable
plot(plotable, xlab = "Dimension 1", ylab = "Dimension 2", xlim=c(-1.6, 1.6), 
     ylim=c(-2, 1.5))
text(plotable, labels = rownames(plotable), cex = 0.8, pos = 4)


################################################################################
# MDS
par(mfrow=c(2,2))
# interesting, shows that farmers are outliers
mds_input_df <- t(cm_nopol)

train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2", 
     main = "MDS using variables regarding no. policies (customer main type)",
     xlim = c(-0.3, 0.5))
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4)

mds_input_df <- t(cm_contrib)

train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2", 
     xlim = c(-1, 2), main = "MDS using variables regarding contribution on policies (customer main type)")
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4)
# farmer also outlier regarding contribution varw; also cruising seniors and careerLoners somehat different


mds_input_df <- t(cm_caravan)

train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2", 
     main = "MDS using only no. mobile home policies (customer main type)", 
     xlim = c(-0.1, 0.07))
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4)
# driven Growers have higher probability to be customer of caravan policy than the others and we see indeed it is seperated by MDS

mds_input_df <- t(cs_caravan)

train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2",
     xlim = c(-0.1, 0.1),
     main = "MDS using only no. mobile home policies (customer sub type)")
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4)
# middle class families and affluent young families stechen heraus, passt zur Analyse mit den Anzahlen
par(mfrow=c(1,1))

