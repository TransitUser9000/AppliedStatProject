
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

dom_socialclass <- 25:29
dom_car <- 32:34
dom_income <- 37:41
dom_education <- 16:18
dom_household <- 14:15
dom_religion <- 7:10
dom_job <- 19:24

dom_contrib <- 43:64
dom_nopol <- 65:86



cstype <- train_df[, c(1, 60:86)] %>% 
  group_by(CustomerSubtypeseeL0) %>% 
  summarise(across(`Numberofprivatethirdpartyinsurance1-12`:`Numberofmobilehomepolicies0-1`, mean)) %>% 
  full_join(L0_cat, by= c("CustomerSubtypeseeL0" = "X1")) %>% 
  rename(Cust_Subtype = X2) %>% 
  select(Cust_Subtype, everything()) %>% 
  select(-CustomerSubtypeseeL0) 



future_rownames <- cstype$Cust_Subtype
cstype <- cstype %>% select(-Cust_Subtype)
cstype <- cstype[1:40,]
rownames(cstype) <- future_rownames[1:40]


#---------Creation of aggregated su b data sets of train_df


cmtype <- train_df[, c(5, 60:86)] %>% 
  group_by(CustomermaintypeseeL2) %>% 
  summarise(across(`Numberofprivatethirdpartyinsurance1-12`:`Numberofmobilehomepolicies0-1`, mean)) %>% 
  full_join(L2_cat, by= c("CustomermaintypeseeL2" = "X1")) %>% 
  rename(Cust_Maintype = X2) %>% 
  select(Cust_Maintype, everything()) %>% 
  select(-CustomermaintypeseeL2) 


future_rownames <- cmtype$Cust_Maintype
cmtype <- cmtype %>% select(-Cust_Maintype)
cmtype <- cmtype[1:9,] # removed col "Farmers", since they seem to be an outlier 
rownames(cmtype) <- future_rownames[1:9]

#-------------
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


# too much for one plot, thus splitting
colnames(train_df)

var_blocks <- c(1, 44, 65, 86)
var_block <- 1
var_blocks[var_block + 1] 

train_df[,var_blocks[var_block]:var_blocks[var_block + 1] - 1] %>% cor %>% corrplot(method="color",  
                                                                                    # addCoef.col = 'black',
                                                                                    diag=FALSE, 
                                                                                    type="lower")



################################################################################
# EDA

train_df %>% head() 
train_df %>% summary() 
train_df %>% str() 

hist(train_df$`Numberofmobilehomepolicies0-1`)

correl <- cor(train_df)

correl_new <- correl %>% as.data.frame() 
correl_new$first_var <- rownames(correl)

prepared_cors <- correl_new %>% pivot_longer(-first_var)
prepared_cors$abs_value <- prepared_cors$value %>% abs() 

prepared_cors <- prepared_cors %>% 
  arrange(desc(abs_value)) %>%
  filter(abs_value != 1)
prepared_cors %>% View()
prepared_cors %>% filter(first_var == "Numberofmobilehomepolicies0-1") %>% 
  arrange(desc(abs_value)) %>% 
  View()

train_df %>% summarise(across(everything(), ~ sum(is.na(.)))) %>% t()
# no NA in either of the data sets 

library(corrplot)

# train_df %>% cor %>% corrplot(method="color",  
                                                # addCoef.col = 'black',
                                                # diag=FALSE, 
                                                # type="lower")
chosen_cols <- cbind(train_df[,2:3], train_df[,86])

library(GGally)
ggpairs(chosen_cols)

# finding patterns in which customers choose to use the caravan policy
caravan_customers_main <- train_df %>% group_by(CustomermaintypeseeL2, `Numberofmobilehomepolicies0-1`) %>% 
  summarise(no_caravan = n()) %>% 
  group_by(CustomermaintypeseeL2) %>% 
  mutate(share = no_caravan / sum(no_caravan))  %>% 
  filter(`Numberofmobilehomepolicies0-1` == 1) %>% 
  left_join(L2_cat, by = c(CustomermaintypeseeL2 = "X1")) %>%   
  arrange(desc(share)) %>% 
  view()
train_df$CustomerSubtypeseeL0
caravan_customers_sub <- train_df %>% group_by(CustomerSubtypeseeL0, `Numberofmobilehomepolicies0-1`) %>% 
  summarise(no_caravan = n()) %>% 
  group_by(CustomerSubtypeseeL0) %>% 
  mutate(share = no_caravan / sum(no_caravan))  %>% 
  filter(`Numberofmobilehomepolicies0-1` == 1) %>% 
  left_join(L0_cat, by = c(CustomerSubtypeseeL0 = "X1")) %>% 
  arrange(desc(share)) %>% 
  view()


################################################################################
# PCA

library(factoextra)
pca_input_df <- train_df[,var_blocks[var_block]:var_blocks[var_block + 1] - 1]
# because data is of various scale we use normalized PCA
pca_result <- prcomp(pca_input_df,
                     center =TRUE, 
                     scale. = TRUE)
print("PCA Summary:")
pca_result %>% summary()
print("Standard deviations of PCs")
pca_result$sdev
print("Rotated component matrix (i.e., the principal component scores)")
pca_result$x
print("Proportion of variance explained by each PC")
pca_result$importance$percentage
print("Show loadings of the original variables on the PCs  - \n If value is high: high correlation between the PC and the variable")
pca_result$rotation
print("Plot the scree plot to visualize the eigenvalues")
fviz_eig(pca_result)
print("Biplot: Visualization of the loadings - i.e. the relationship between the PCs and the variables")
fviz_pca_biplot(pca_result)
# In this plot, the observations are represented by the points, and the variables are represented by the arrows. The first two principal components (PC1 and PC2) are shown, which explain the most variation in the data.
# 
# To interpret the plot, you can look at the position of each observation relative to the variables. For example, if an observation is located in the direction of a variable arrow, it means that the observation has a high value for that variable. Conversely, if an observation is located in the opposite direction of a variable arrow, it means that the observation has a low value for that variable.
# 
# Additionally, you can look at the length of each variable arrow to determine the importance of that variable in the PCA. Longer arrows indicate more important variables, while shorter arrows indicate less important variables.

library(ggforce)

# Create a data frame of the PC scores
pca_scores <- data.frame(PC1 = pca_result$x[,1], PC2 = pca_result$x[,2])

# Add a column for the radius of the circle
pca_scores$radius <- sqrt(pca_scores$PC1^2 + pca_scores$PC2^2)

# Create the plot
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(size = 2, shape = 21, fill = "steelblue", stroke = 0.5) +
  # geom_circle(aes(r = radius), color = "black", alpha = 0.2) +
  labs(x = "PC1", y = "PC2", title = "PCA Plot") +
  theme_bw()

# NEW PART STARTS HERE

plot(pca_result)

plot(pca_result$x[,1:2], pch=20, col="blue", xlab="PC1", ylab="PC2", main="Car market")
text(pca_result$x[,1:2], labels=rownames(pca_input_df), col="blue", cex=0.8, pos=4)
# plot in general correct, but not sensible in our case 

biplot(pca_result)

# Variance explained by PCs and cumulative proportion:
pca_result$sdev^2 /sum(pca_result$sdev^2)
cumsum(pca_result$sdev^2) /sum(pca_result$sdev^2)

#TODO find variable which can be colored in pca_input_df which is seperated relatively good by the PCs
# maybe find that out by the previous plots 
#FIXME change the labels of the legend and the variable correct to the chosen variable 
plot(pca_result$x[,1:2], pch = 20, col = c("red", "green", "blue", "black")[pca_input_df$Averageincome],
     xlab = "PC1", ylab = "PC2",
     main = "Insurance market")
text(pca_result$x[,1:2], labels = rownames(pca_input_df$Averageincome),
     col = c("red", "green", "blue", "black")[pca_input_df$Averageincome],
     cex = 0.8, pos = 4)
legend("topright", legend = levels(pca_input_df$Averageincome),
       col = c("red", "green", "blue", "black"), pch =19)

cor_pc_var <- pca_result$rotation
cor_pc_var
# par(mfrow=c(1,2))
plot(cos((0:360)/180*pi), sin((0:360)/180*pi), type = "l", lty = "dashed",
     col = "red",
     xlab = "cor with PC1", ylab = "cor with PC2",
     main = "Insurance market")
text(cor_pc_var[,1:2], labels = colnames(pca_input_df), cex = 0.8, col = "blue")
plot(cor_pc_var[,1:2], pch = 20, col = "blue",
     xlab = "PC1", ylab = "PC2",
     xlim = c(-2, 2),
     main = "Insurance market")
text(cor_pc_var[,1:2], labels = rownames(pca_input_df), col = "blue", cex = 0.8, pos = 4)



#TODO find sensible solution to avoid overcrowding of the plots 


# ----------------------PCR-----------------------------------------------------
pc_regr <- pca_result$x[,1:2]
pc_regr
colnames(train_df)[86]
# Regression with first two components
pcr <- lm(train_df$`Numberofmobilehomepolicies0-1` ~ pc_regr - 1)
summary(pcr)

# Comparing the adjR² of linear regression of ALL VARS and PC-Regr. with only the 
# first 2 PCs we achieve almost the same score
summary(pcr)$adj.r.squared # at the moment extremely bad result

# #comparison to linear model
#TODO find correct variable set (should be all vars except the target in train_df)
# lin_model <- lm(train_df$`Numberofmobilehomepolicies0-1` ~.)
# summary(lm_car)

################################################################################
# FA

fac_df <- train_df 
fac_df
fac_p <- ncol(fac_df)

calc_d <- function(k){
  0.5*(fac_p - k)^2 - 0.5*(fac_p + k)
}

choice_k <- data.frame(k = 30:90)
choice_k$d <- sapply(choice_k$k, FUN = calc_d)
fac_k <- 5
# --> max k here is 73


plot_fca_result <- function(fac_result, fac_df, str_rotation) {
  plot(cbind(cos((0:360)/180*pi),sin((0:360)/180*pi)),
       type="l",
       lty="dotted",
       xlab = "Factor 1",
       ylab = "Factor 2",
       main=str_rotation)
  abline(h = 0)
  abline(v = 0)
  text(fac_result$loadings[,1:2],
       labels=colnames(fac_df),
       col="black")
  
  
  plot(fac_result$scores, type = "n")
  text(fac_result$scores, rownames(fac_result$scores), xlim = c(-5, 5), ylim = c(-5, 5))
  
}

par(mfrow=c(1,1))

# strategy taken from lecture slide 37
fac_result_pc_no_rotation <- factanal(~., 
                    factors=fac_k,
                    method = "pc",
                    rotation="none",
                    scores="regression",
                    data=data.frame(fac_df))

plot_fca_result(fac_result = fac_result_pc_no_rotation, 
                fac_df = fac_df, 
                str_rotation = "PC-FA - no rotation")

# apply test for the number of factors

# --> we see in the end of the output: p-value is 0 < 0.05, i.e. we reject H_0 (5 factors are not sufficient)
#TODO find out why factanal()h does not accept igher values

par(mfrow=c(2,2))

# Varimax rotation
fac_result_pc_varimax <- factanal(~.,
                            factors=fac_k,
                            method = "pc",
                            sources="regression", 
                            rotation="varimax",
                            data=data.frame(fac_df))

plot_fca_result(fac_result = fac_result_pc_varimax, 
                fac_df = fac_df, 
                str_rotation = "PC-FA - varimax")

# Varimax rotation
fac_result_ml_varimax <- factanal(~.,
                                  factors=fac_k,
                                  method = "ml",
                                  sources="regression", 
                                  rotation="varimax",
                                  data=data.frame(fac_df))

plot_fca_result(fac_result = fac_result_ml_varimax, 
                fac_df = fac_df, 
                str_rotation = "ML-FA - varimax")

# comparison of both last analyses: Do the loadings group in the same manner? 
# --> They do

# repeat previous step for another number of common factors k
fac_k <- 2

# Varimax rotation
fac_result_pc_varimax <- factanal(~.,
                                  factors=fac_k,
                                  method = "pc",
                                  sources="regression", 
                                  rotation="varimax",
                                  data=data.frame(fac_df))

plot_fca_result(fac_result = fac_result_pc_varimax, 
                fac_df = fac_df, 
                str_rotation = "PC-FA - varimax")

# Varimax rotation
fac_result_ml_varimax <- factanal(~.,
                                  factors=fac_k,
                                  method = "ml",
                                  sources="regression", 
                                  rotation="varimax",
                                  data=data.frame(fac_df))

plot_fca_result(fac_result = fac_result_ml_varimax, 
                fac_df = fac_df, 
                str_rotation = "ML-FA - varimax")


# oblique rotation
fac_result_oblique <- factanal(~.,
                               factors=k,
                               sources="regression", 
                               rotation="promax",
                               data=data.frame(fac_df))


################################################################################
# CA
library(FactoMineR)

ca_df <- t(cstype)

CA(ca_df)

summary(CA(ca_df) , nbelements = Inf)

ca_df <- t(cmtype)

CA(ca_df)
rownames(cmtype)

#TODO aus Gespr. schauen wie wir Prozentzahlen implementieren
# --> farmers seem to be outlier , thus they are excluded by 1:9 above in cmtype creation

# since principle is shown now, starting now with short version
ca_df <- t(cm_education)
cm_education %>% t() %>% CA()
cm_car %>% t() %>% CA()
cm_income %>% t() %>% CA()
cm_job %>% t() %>% CA()
cs_income %>% t() %>% CA()
cs_job %>% t() %>% CA()
# OLD

#TODO Although here not suitable! -> Do Interpretation of the result , especially of all the infos in summary

# --> CA is made for datasets with small n (maybe also named), thus CA analysis 
# is always overcrowded and not suitable here 
train_df$`Numberofmobilehomepolicies0-1`
ca_df <- train_df %>% filter(`Numberofmobilehomepolicies0-1` == 1)

CA(ca_df)


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

ccX <- cs_income
ccY <- cs_job

cc_result <- cc(ccX, ccY)
cc_result
plotable <- cbind(cc_result$scores$xscores[,1], cc_result$scores$yscores[,1])
plotable
plot(plotable, xlab = "Dimension 1", ylab = "Dimension 2")
text(plotable, labels = rownames(plotable), cex = 0.8, pos = 4)


#-----------

colnames(train_df[,25:29])
ccX <- train_df[,dom_education]
ccY <- train_df[,dom_socialclass]

cc_result <- cc(ccX, ccY)
cc_result
plotable <- cbind(cc_result$scores$xscores[,1], cc_result$scores$yscores[,1])
plotable
plot(plotable, xlab = "Dimension 1", ylab = "Dimension 2")
text(plotable, labels = rownames(plotable), cex = 0.8, pos = 4)

# --> finding: does not make much sence since too large n and no meaning of each 
# n individually , maybe only if we want to know the "personality" of individuals 
# and don't look at the general

ccX <- train_df[20:100,8:9] 
ccY <- train_df[20:100,84:86]

cc_result <- cc(ccX, ccY)
plotable <- cbind(cc_result$scores$xscores[,1], cc_result$scores$yscores[,1])
plotable
plot(plotable, xlab = "Dimension 1", ylab = "Dimension 2")
text(plotable, labels = 20:100, cex = 0.8, pos = 4)


################################################################################
# MDS


mds_input_df <- t(cm_car)

train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4)



mds_input_df <- t(cstype)

train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4)

mds_result %>% summary() # not really necessary...



# OLD 
mds_input_df <- train_df 

train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4)

mds_result %>% summary() # not really necessary...

#get more closer look of variables in the centre
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2", xlim=c(-110,100),
     ylim=c(0,50))
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4)
# --> contribution... variables seem to be all at one place -> exclude them 

var_block <- 1
mds_input_df <- train_df[,var_blocks[var_block]:var_blocks[var_block + 1] - 1]
train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2", xlim=c(-300,300))
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4)

# --> tested with varblock 2 and 3, but doesnt work that well AT FIRST GLANCE
colnames(train_df)

mds_input_df <- cbind(train_df[,1:44], train_df[,65:86])
train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2", xlim=c(-300,300))
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4, col = "blue")

# ...zooming in
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2", xlim=c(-150,00),ylim=c(-50,-40))
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4, col = "blue")

colnames(train_df)

mds_input_df <- cbind(train_df[,1:44], train_df[,86])
train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2", xlim=c(-300,300))
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4, 
     col = ifelse(colnames(mds_input_df) == "Numberofmobilehomepolicies0-1", "blue", "red"))

# --> finding: our target, numberofmobilehomepolicies is related to Farmer, Income > 123k, LivingTogether, 
# Entrepreneur, ContributionprivatethirdpartyinsuranceesL4
# Further, to romancatholics and Numberofhouses1
# also interesting: Highleveleducation, SocialclassA and Highstatus are close to each other 
# and avgsiz ehousehold1 middlemanagement and household with children are close to each other 
 


mds_input_df <- cbind(train_df[,1:44], train_df[,84]) %>% scale()

train_dist <- dist(t(mds_input_df)) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)
plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2", xlim=c(-300,300))
text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4, 
     col = ifelse(colnames(mds_input_df) == "Numberofpropertyinsurancepolicies", "blue", "red"))
# --> seems that the numberof... always localize at (0,0), so interpretation above 
# is probably wrong 



#-------------------------------------------------------------------------------

# now for the observations 
# i.e. "How similar are people regarding all dimensions"
mds_input_df_with_orig_id <- train_df %>% mutate(orig_id=row_number())
# mds_input_df_with_orig_id$orig_id <- mds_input_df_with_orig_id$orig_id %>% as.data.frame.integer(column_name = "orig_id")

# my_colors <- c("steelblue", "steelblue", "steelblue", 
#                "blue", "blue", "blue", 
#                "red", "red", "red")
my_colors_Palette <- colorRampPalette(c("red", "blue"))

mds_input_df_with_orig_id <- mds_input_df_with_orig_id %>% sample_n(500) #using less rowss to save computation time
vec_orig_ids <- mds_input_df_with_orig_id[,"orig_id"]
vec_orig_ids
mds_input_df <- mds_input_df_with_orig_id %>% select(-orig_id)

train_dist <- dist(mds_input_df) # should be then always two vectors of size 1000 in distance difference
mds_result <- cmdscale(train_dist)

observed_var <- mds_input_df$Highleveleducation
no_var_levels <- length(unique(observed_var))

plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
text(mds_result, labels = rownames(mds_input_df), cex = 0.8, 
     pos = 4, col = my_colors_Palette(no_var_levels)[as.numeric(factor(observed_var))])
legend("topright", legend = sort(unique(observed_var)), col = my_colors_Palette(no_var_levels), pch=19)


# --> finding: three groups of people: two larger and one smaller between them 

observed_var[411]

mds_result <- mds_result %>% 
  as.data.frame() %>% cbind(vec_orig_ids)
left_group <- mds_result  %>% filter(V1 < -5) 
middle_group <- mds_result %>% filter(V1 < 5, V1 > -5) 
right_group <- mds_result  %>% filter(V1 > 5) 

left_group_attr <- mds_input_df_with_orig_id %>% filter(orig_id %in% left_group$orig_id) 

middle_group_attr <- mds_input_df_with_orig_id %>% filter(orig_id %in% middle_group$orig_id) 

right_group_attr <- mds_input_df_with_orig_id %>% filter(orig_id %in% right_group$orig_id) 

colMeans(left_group_attr) 
rbind(colMedian(left_group_attr), colMeans(middle_group_attr), colMeans(right_group_attr)) %>% View()

rbind(
  apply(left_group_attr, 2, median),
  apply(middle_group_attr, 2, median),
  apply(right_group_attr, 2, median)
  ) %>% View()

unique(mds_input_df$Lowerleveleducation)


#TODO HIER WEITER find coninous discrete coloring for numerical data
# also: find solution why no single '1' observation of caravan insurance is visible 
par(mfrow=c(2,2))
for (rc in 1:2){
  for (i in 1:4){
    if (i == 4) {
      mds_input_df <- train_df %>% sample_n(500) }
    else {
      mds_input_df <- train_df[,var_blocks[i]:var_blocks[i + 1] - 1] %>% sample_n(500) #using less rowss to save computation time
     }
    if (rc == 1){
      train_dist <- dist(mds_input_df) # should be then always two vectors of size 1000 in distance difference    
    }
    else {
      train_dist <- dist(t(mds_input_df))
    }
  
    mds_result <- cmdscale(train_dist)
    plot(mds_result, type = "n", xlab = "Dimension 1", ylab = "Dimension 2", 
         main = as.character(i))
    if (rc == 1){
      text(mds_result, labels = rownames(mds_input_df), cex = 0.8, pos = 4)
    }
    else {
      text(mds_result, labels = colnames(mds_input_df), cex = 0.8, pos = 4)
    }
  }
}

################################################################################
# DA
# 
library(MASS)
train_df$`Numberofmobilehomepolicies0-1`
lda_result <- lda(`Numberofmobilehomepolicies0-1` ~ . , data =train_df)
lda_result

qda_result <- qda(`Numberofmobilehomepolicies0-1` ~ . , data =train_df)


?lda()
################################################################################
# Regression

library("leaps")

# first, without any preprocessing all the data plugged into the model
leaps(x = train_df[,63:82], y = train_df$`Numberofmobilehomepolicies0-1`, method="adjr2", nbest=1)

regr_plot <- regsubsets(`Numberofmobilehomepolicies0-1`~., data = train_df, 
                        method="backward", nbest=1)
plot(regr_plot, scale = "adjr2")

# --> only R² of 0.052  -> very poor result

# PCR 
#TODO look up if categorical vars destroy PCA and general robustness of PCA
pca_model <- prcomp(train_df[,1:85], scale. = TRUE)
train_pcr <- cbind(pca_model$x, train_df$`Numberofmobilehomepolicies0-1`) %>% 
  as_data_frame() 
train_pcr <-  train_pcr %>%  rename(TARGET = last(colnames(train_pcr)))  
pcr_model <- lm(TARGET ~ ., data = train_pcr)
summary(pcr_model)$r.squared

regr_plot <- regsubsets(TARGET~., data = train_pcr, 
                        method="backward", nbest=1)
plot(regr_plot, scale = "adjr2")

#TODO include DUMMY variables for customer types

################################################################################
# Logistic Regression
library(caret)

# first try without any adjustments 
lreg_model <- glm(`Numberofmobilehomepolicies0-1` ~ ., data = train_df, 
                   family = binomial(logit))
summary(lreg_model)

preds <- predict(lreg_model, newdata = test_df, type = "response") 

conf_mat <- confusionMatrix(data = factor(ifelse(preds > 0.5, 1, 0)), reference = factor(test_target$`0`))
conf_mat # shows also specifi. accuracy and precision 

library(pROC) 

# Calculate the ROC curve
roc_obj <- roc(response = test_target$`0`, predictor = preds)

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve for Ligistic Regr. Model",
     col = "blue", lwd = 2,
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)")
#TODO find out why x-axis is in wrong order , doesn't make sense this way

# PCLR 

library(factoextra)
pca_model <- prcomp(train_df[,1:85], scale. = TRUE)

fviz_eig(pca_model) # plot the scree plot for determination of number of PCs to take

# biplot(pca_model) overcrowded and takes a while to compute

train_pcr <- cbind(pca_model$x[,1:3], train_df$`Numberofmobilehomepolicies0-1`) %>% 
  as_data_frame() 
train_pcr <-  train_pcr %>%  rename(TARGET = last(colnames(train_pcr)))  



pclr_model <- glm(TARGET ~ ., data = train_pcr, 
                   family = binomial(logit))
summary(pclr_model)

preds <- predict(pclr_model, newdata = test_df, type = "response") 

conf_mat <- confusionMatrix(data = factor(ifelse(preds > 0.5, 1, 0)), reference = factor(test_target$`0`))
conf_mat # shows also specifi. accuracy and precision 

#TODO hier weiter: find solution for problem that we need the PCws also for the 
# test set, how to calculate them ?


# schauen wwelche Vars normalverteilt sind
# bei PCqa alle ohne Main und Subtype