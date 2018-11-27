import::here(grwth_county, .from="data_cleaning.R")

train_data = grwth_county %>% select(sex_ratio:pct_dep_class)

train_data = na.omit(train_data)
fit1 = lm(income ~ ., data=train_data)
summary(fit1)

# Alias analysis to remove linearly dependent variables
ld_vars <- attributes(alias(fit1)$Complete)$dimnames[[1]]

train_data = train_data %>% select(-pct_occ05, -pct_cow04)

fit2 = lm(income ~ ., data=train_data)
summary(fit2)

# Vif Analysis

vif(fit2)

# removing pct_empd (1225.380839) with highest vif
train_data = train_data %>% select(-pct_empd)
fit2 = lm(income ~ ., data=train_data)
summary(fit2)
vif(fit2)

# removing pct_occ01 (11.641654) with value greater than 10
train_data = train_data %>% select(-pct_occ01)
fit3 = lm(income ~ ., data=train_data)
summary(fit3)
