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

########################################################################################################
# Residual Diagnostics

#saved as Residual_1
p1 <- ggplot(mapping=aes(fit3$fitted.values,fit3$residuals)) + geom_point() + geom_smooth()
plot(p1)

#saved as Residual_2
p1 <- ggplot(mapping=aes(train_data$sex_ratio,fit3$residuals)) + geom_point() + geom_smooth()
p2 <- ggplot(mapping=aes(train_data$pct_bpl,fit3$residuals)) + geom_point() + geom_smooth()
layout <- matrix(c(1,2),2,2,byrow = TRUE)
multiplot(p1,p2,layout = layout)

##saved as Residual_3
p1 <- ggplot(mapping=aes(train_data$pct_occ02,fit3$residuals)) + geom_point() + geom_smooth()
p2 <- ggplot(mapping=aes(train_data$pct_occ03,fit3$residuals)) + geom_point() + geom_smooth()
p3 <- ggplot(mapping=aes(train_data$pct_occ04,fit3$residuals)) + geom_point() + geom_smooth()
layout <- matrix(c(1,2,3,3),2,2,byrow = TRUE)
multiplot(p1,p2,p3,layout = layout)

#saved as Residual_4
p1 <- ggplot(mapping=aes(train_data$pct_lessthan_highgrad,fit3$residuals)) + geom_point() + geom_smooth()
p2 <- ggplot(mapping=aes(train_data$pct_highschool_grad,fit3$residuals)) + geom_point() + geom_smooth()
p3 <- ggplot(mapping=aes(train_data$pct_bachdeg_or_highr,fit3$residuals)) + geom_point() + geom_smooth()
layout <- matrix(c(1,2,3,3),2,2,byrow = TRUE)
multiplot(p1,p2,p3,layout = layout)

#saved as Residual_5
p1 <- ggplot(mapping=aes(train_data$pct_cow01,fit3$residuals)) + geom_point() + geom_smooth()
p2 <- ggplot(mapping=aes(train_data$pct_cow02,fit3$residuals)) + geom_point() + geom_smooth()
p3 <- ggplot(mapping=aes(train_data$pct_cow03,fit3$residuals)) + geom_point() + geom_smooth()
layout <- matrix(c(1,2,3,3),2,2,byrow = TRUE)
multiplot(p1,p2,p3,layout = layout)



#saved as Residual_6
p1 <- ggplot(mapping=aes(train_data$tax_in,fit3$residuals)) + geom_point() + geom_smooth()
p2 <- ggplot(mapping=aes(train_data$tax_sr,fit3$residuals)) + geom_point() + geom_smooth()
layout <- matrix(c(1,2),2,2,byrow = TRUE)
multiplot(p1,p2,layout = layout)

#saved as Residual_7
p1 <- ggplot(mapping=aes(train_data$pct_wrk_class,fit3$residuals)) + geom_point() + geom_smooth()
p2 <- ggplot(mapping=aes(train_data$pct_dep_class,fit3$residuals)) + geom_point() + geom_smooth()
layout <- matrix(c(1,2),2,2,byrow = TRUE)
multiplot(p1,p2,layout = layout)
