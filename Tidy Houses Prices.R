# House Prices


# library ----

library(tidyverse)
library(tidymodels)
library(magrittr)
library(viridis)
library(ggthemes)
library(scales)
library(ggcorrplot)
library(plyr)

library(tidyr)
library(ggplot2)
library(stringr)
library(scales)
library(tibble)
library(corrplot)
library(plotly)
library(simputation)
library(psych)

library(mice)
library(randomForest)
library(lme4)
library(caret)


tidymodels_packages()
# Import data ----

train = read.csv("C:/Users/ivan/Google Drive/Machine learning/Kaggle/House Prices/train.csv", sep = "," , header = T,
                 stringsAsFactors = F)  %>% mutate(data = "train") # # Change factors to characters to more easily tranforms data in integrers

test = read.csv("C:/Users/ivan/Google Drive/Machine learning/Kaggle/House Prices/test.csv", sep = "," , header = T,
                stringsAsFactors = F) %>% mutate(data = "test")


full = bind_rows(train, test)

# Data exploration ----

head(train)
glimpse(train)
levels(train)

# Plots

# PoolArea
train %>% ggplot(aes(SalePrice, fill = "red")) + geom_histogram(bins = 50) + scale_x_continuous(labels = comma, n.breaks = 20)

# See most important numeric predictors (Correlations with SalePrice)

numericVars = select_if(train, is.numeric)
cor_numVar = cor(numericVars, use="pairwise.complete.obs")
cor_numVar %>% as.data.frame() %>% rownames_to_column(var = "Cor") %>%
  filter(SalePrice > 0.5) %>% mutate_at(c("Cor"), as.factor) %>%
  select_if(names(.) %in% (.$Cor) | names(.) == "Cor") %>% column_to_rownames(var = "Cor") %>% #arrange(desc(SalePrice)) %>%
  as.matrix() %>%
  ggcorrplot(hc.order = TRUE,type = "lower", lab = TRUE)
# We can see that multicollinearity is an issue (>0.5)

# Plot the two predictors with the highest correlation with SalePrice
# Overall quality
train %>% ggplot(aes(as.factor(OverallQual), SalePrice, fill = as.factor(OverallQual))) +
  geom_boxplot() +
  scale_y_continuous(labels = comma, n.breaks = 20)

# GrLivArea
p = train %>% ggplot(aes(SalePrice, GrLivArea, #color = Neighborhood
                     )) + geom_point() + geom_smooth(method="lm", formula= y~x) +
  scale_x_continuous(labels = comma, n.breaks = 20)
ggplotly(p)

# Year / Month
glimpse(full)
full %>% filter(data == "train") %>% ggplot(aes(MoSold, SalePrice, fill = as.factor(MoSold))) +
  stat_summary(geom = "bar", fun = "median", alpha = 0.5) +
  scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  geom_hline(yintercept = median(full$SalePrice, na.rm = T), linetype = "dashed", color = "red")


# Pre-processing ----


# Utilities useless
train %>% select(Utilities) %>% na.omit() %>% group_by(Utilities) %>% dplyr::summarise(Count = n())
full %<>% select(-Utilities)

# Editing data
colSums(is.na(full)) %>% as.data.frame() %>% rownames_to_column(var = "variables") %>% arrange(desc(.))
Qualities = c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)
glimpse(full)


# PoolQC

full %<>% mutate(PoolQC = ifelse(is.na(PoolQC), "None", as.character(PoolQC))) %>%
  mutate(PoolQC = as.integer(revalue(PoolQC, Qualities)))
table(full$PoolQC)

# PoolArea

n = full %>% select(Id, PoolArea, PoolQC, OverallQual) %>% filter(PoolQC == "0" & !PoolArea == "0")
rm(n)

full %<>%
  mutate(PoolQC = replace(PoolQC, Id == 2421, 2)) %>%
  mutate(PoolQC = replace(PoolQC, Id == 2504, 3)) %>%
  mutate(PoolQC = replace(PoolQC, Id == 2600, 2))

# MiscFeature / Alley / Fence / FireplaceQu / ExterQual / ExterCond

full %>% ggplot(aes(MiscFeature, SalePrice)) + geom_bar(stat = "summary", fill = "blue") +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

full %<>%
  mutate(MiscFeature =  ifelse(is.na(MiscFeature), "None", MiscFeature)) %>%
  mutate(Alley =  ifelse(is.na(Alley), "None", Alley)) %>%
  mutate(Fence =  ifelse(is.na(Fence), "None", Fence)) %>%
  mutate(FireplaceQu =  ifelse(is.na(FireplaceQu), "None", FireplaceQu)) %>%
  mutate(FireplaceQu = as.integer(revalue(FireplaceQu,Qualities))) %>%
  mutate(LandSlope = as.integer(revalue(LandSlope, c("Sev"= 0, "Mod"= 1, "Gtl"= 2)))) %>%
  mutate(Street = as.integer(revalue(Street, c("Grvl"= 0, "Pave"= 1)))) %>%
  mutate(PavedDrive = as.integer(revalue(PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))) %>%
  mutate_at(vars(ExterCond,ExterQual,HeatingQC), list(~ as.integer(revalue(., Qualities))))


# Garage variables

full %<>% mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt), YearBuilt, GarageYrBlt)) %>%
  mutate_at(vars(GarageCond, GarageFinish, GarageQual, GarageType), ~ifelse(is.na(.), "None", .)) %>%
  mutate_at(vars(GarageQual,GarageCond), list(~ as.integer(revalue(., Qualities)))) %>%
  mutate(GarageFinish = as.integer(revalue(GarageFinish, c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3))))

n =full %>% filter_at(vars(GarageCond, GarageFinish, GarageQual, GarageType), all_vars(is.na(.))) # I'm missing 2 Na

# Basement variables
full %<>% mutate_at(vars(BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2), ~ifelse(is.na(.), "None", .)) %>%
  mutate_at(vars(BsmtQual,BsmtCond), list(~as.integer(revalue(., Qualities)))) %>%
  mutate(BsmtExposure = revalue(BsmtExposure, c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4))) %>%
  mutate(BsmtFinType1 = revalue(BsmtFinType1, c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6))) %>%
  mutate(BsmtFinType2 = revalue(BsmtFinType2, c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6))) %>%
  mutate_at(vars(BsmtExposure, BsmtFinType1, BsmtFinType2), as.integer) %>% glimpse()


n = full %>% filter_at(vars(BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2), all_vars(is.na(.)))
full$BsmtQual
train$BsmtQual

# Masonry variables
glimpse(full)
full %>% filter(!is.na(MasVnrType)) %>% filter(!is.na(SalePrice)) %>%
  ggplot(aes(reorder(MasVnrType, SalePrice), SalePrice, fill = MasVnrType)) +
  stat_summary(geom = "bar", fun = "median", alpha = 0.3) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

# After seen the plot we can change de varaible MasVnrType as numeric
full %<>% mutate(MasVnrType = ifelse(is.na(MasVnrType), "None", MasVnrType)) %>%
  mutate(MasVnrType = as.integer(revalue(MasVnrType, c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)))) %>%
  mutate(MasVnrArea = ifelse(is.na(MasVnrArea), 0, MasVnrArea))


# KitchenQual
full %<>% mutate(KitchenQual = as.integer(revalue(KitchenQual, Qualities)))

# LotShape

full %<>%
  mutate(LotShape = as.integer(revalue (LotShape, c('IR3' = 0, 'IR2' = 1, 'IR1' = 2, 'Reg' = 3))))

# LotFrontage (use median by neigborhood with package "recepies")

full %>% ggplot(aes(Neighborhood, LotFrontage, fill = Neighborhood)) +
  stat_summary(geom = "bar", fun = "median", alpha = 0.3) +
  geom_hline(yintercept = median(full$LotFrontage, na.rm = T), color = "red", linetype = "dashed")

train = full %>% filter(data == "train") %>% impute_median(LotFrontage ~ Neighborhood)
test = full %>% filter(data == "test") %>% impute_median(LotFrontage ~ Neighborhood)

trainmedia = train %>% impute_median(LotFrontage ~ Neighborhood)
n1 = trainmedia %>% select(LotFrontage)

LotFrontage_rec = recipe(SalePrice ~ ., train) %>%
  step_bagimpute(LotFrontage, impute_with = "Neighborhood")

LotFrontage_prep = prep(LotFrontage_rec)

LotFrontage_juice = juice(LotFrontage_prep)
n2 = LotFrontage_juice %>% select(LotFrontage)
Lot = bind_cols(n1,n2) # very similar

# Still missing some Na to fix! but very few

glimpse(full)

# characters to numeric (done already when revalue)

# characters to factors ----

glimpse(full)
full %<>% mutate_if(~ is.character(.), as.factor) #%>% glimpse()



# numeric to factors ----
full %<>% mutate_at(c("MoSold", "YrSold", "MSSubClass"), as.factor) %>%
  mutate(MSSubClass = revalue(MSSubClass, c("20"="1 story 1946+", "30"="1 story 1945-", "40"="1 story unf attic", "45"="1,5 story unf", "50"="1,5 story fin", "60"="2 story 1946+", "70"="2 story 1945-", "75"="2,5 story all ages", "80"="split/multi level", "85"="split foyer", "90"="duplex all style/age", "120"="1 story PUD 1946+", "150"="1,5 story PUD all", "160"="2 story PUD 1946+", "180"="PUD multilevel", "190"="2 family conversion"))) %>% glimpse()


# Check variables class ----

glimpse(full)
full %>% select_if(~is.numeric(.)) %>% length()
full %>% select_if(~is.factor(.)) %>% length()

# Check again correlations ----

full %>% select_if(~is.numeric(.)) %>% cor(., use="pairwise.complete.obs") %>% as.data.frame() %>% rownames_to_column() %>%
  filter(SalePrice > 0.5) %>%
  select_if(names(.) %in% .$rowname | names(.) %in%  c("rowname")) %>% column_to_rownames(var = "rowname") %>%
  ggcorrplot(hc.order = TRUE,type = "lower", lab = TRUE)


# Finding variable importance with a quick Random Forest ----
set.seed(2018)
Quick_RF = full %>% filter(data == "train") %>% na.omit() %>% randomForest(SalePrice ~ ., ntree = 100, importance = T, data = .)
importance(Quick_RF) %>%
  as.data.frame() %>% rownames_to_column(var = "Variables") %>%
  rename(MSE = `%IncMSE`) %>% arrange(desc(MSE)) %>% filter(MSE > 5) %>%
  ggplot(aes(reorder(Variables,MSE), MSE, Variables, fill = Variables)) + geom_bar(stat = "identity") + guides(fill = F) +
  coord_flip() + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted')



# Add a new variables combining old variables ----
glimpse(full)
full %<>% mutate(TotBathrooms = FullBath + HalfBath*0.5 + BsmtFullBath + BsmtHalfBath*0.5) %>%
  mutate(Remod = ifelse(YearBuilt == YearRemodAdd, 0, 1)) %>%
  mutate(Age = as.numeric(as.character(YrSold)) - YearRemodAdd) %>%
  mutate(IsNew = ifelse(as.numeric(as.character(YrSold)) == YearBuilt, 1, 0)) %>%
  glimpse()

table(full$IsNew)

# Binning Neighborhood

full %<>% mutate(NeighRich = 1) %>%
  dplyr::mutate(NeighRich = case_when(
    Neighborhood %in% c("StoneBr", "NridgHt", "NoRidge") ~ 2,
    Neighborhood %in% c("MeadowV", "IDOTRR", "BrDale") ~ 0,
                TRUE ~ NeighRich)) %>%
  glimpse()
full %>% filter(!is.na(SalePrice))%>%
  ggplot(aes(x = reorder(Neighborhood, SalePrice), y = SalePrice, fill = NeighRich)) +
  stat_summary(geom = "bar", fun = "mean", alpha = 0.5) +
  guides(fill = F) + scale_y_continuous(labels = comma, n.breaks = 20) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  geom_hline(yintercept = mean(full$SalePrice, na.rm = T), color = "red", linetype = "dashed")
# Total Square Feet and Consolidating Porch variables
full %<>% mutate(TotalSqFeet = GrLivArea + TotalBsmtSF) %>%
  mutate(TotalPorchSF = OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch)


# Preparing data for modeling ----

# Dropping highly correlated variables / I am dropping the variable with the lowest correlation with SalePrice
# Dropping outliers
glimpse(n)
qqnorm(n$SalePrice)
qqline(n$SalePrice)
full %<>% select(-YearRemodAdd, -GarageYrBlt, -GarageArea, -GarageCond, -TotalBsmtSF, -TotRmsAbvGrd, -BsmtFinSF1) %>%
  mutate_at(vars(MoSold, YrSold, SalePrice, OverallQual, OverallCond,Age,
                 TotalPorchSF, TotBathrooms, TotalSqFeet), as.numeric) %>%
  # Skewness
  mutate_at(vars(MoSold, YrSold, SalePrice, OverallQual, OverallCond,
                 Age, TotalPorchSF, TotBathrooms, TotalSqFeet),
            ~(ifelse(rep(abs(skew(., na.rm = T)) > 0.8, n()), log(.+1), .)))
  # mutate_at(vars(MoSold, YrSold, SalePrice, OverallQual, OverallCond,
  #                Age, TotalPorchSF, TotBathrooms, TotalSqFeet),
  #           ~if(abs(skew(., na.rm = T)) > 0.8) log(.+1) else .)

  # Normalizing the data




# Removing levels with few or no observations in train or test?
full %>%  nearZeroVar(freqCut = 95/5, uniqueCut = 10, saveMetrics = T,
                      names = T)

# center and scale the 'true numeric' predictors (so not variables that have been label encoded)
full %>% mutate_at(vars(MoSold, YrSold, SalePrice, OverallQual, OverallCond,
                           Age, TotalPorchSF, TotBathrooms, TotalSqFeet), as.numeric) %>% select(MoSold, YrSold, SalePrice, OverallQual, OverallCond) %>%
  mutate_at(vars(MoSold, YrSold, SalePrice, OverallQual, OverallCond),
            ~ if(mean(., na.rm = T) > 4, log(.) else .)) %>%
  head()

full %>% mutate_at(vars(MoSold, YrSold, SalePrice, OverallQual, OverallCond,
                           Age, TotalPorchSF, TotBathrooms, TotalSqFeet), as.numeric) %>%
  select(MoSold, YrSold, SalePrice, OverallQual, OverallCond) %>%
  mutate(across(c("MoSold", "YrSold"),~ if(mean(., na.rm = TRUE) > 4) log(.) else .)) %>%
  head()
log(full$TotalPorchSF)

full %>%  mutate_at(vars(MoSold, YrSold, SalePrice, OverallQual, OverallCond,
                         Age, TotalPorchSF, TotBathrooms, TotalSqFeet), as.numeric) %>%
  select(MoSold, YrSold, SalePrice, OverallQual, OverallCond) %>%
  mutate_at(vars(MoSold, YrSold, SalePrice, OverallQual, OverallCond),
            ~(ifelse(rep(abs(skew(., na.rm = T)) > 0.8, n()), log(.+1), .))) %>% head()

full %>% mutate_at(vars(MoSold, YrSold, SalePrice, OverallQual, OverallCond,
                        Age, TotalPorchSF, TotBathrooms, TotalSqFeet), as.numeric) %>%
  select(MoSold, YrSold, SalePrice, OverallQual, OverallCond,
         Age, TotalPorchSF, TotBathrooms, TotalSqFeet) %>% summarise(swek = abs(skew(.)))

# Analisis lm ----

mod = lm(SalePrice ~ . - Fence - MiscFeature - PoolQC - FireplaceQu - Street, data = train) +
  geom_label(stat = "count", aes(y = ..count..)) +

summary(mod)



# RandomForest ----

forest = randomForest(SalePrice ~ ., data = train)

