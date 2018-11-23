library(ggplot2)
library(data.table)
library(dplyr)
library(leaps)
library(geosphere)
#install.packages("car")
library(corrplot)
library(leaps)
library(car)
library(MASS)

Incomed <- read.csv('C:\\Users\\Sanchayni\\Desktop\\IIT\\Fall18\\Applied_Stats\\Project\\Database1114(e).csv')
colnames(Incomed)[1] <- 'ID'
sum(is.na(Incomed))

colnames(Incomed)

Incomed <- Incomed[,-29]
Incomed$Working_class <- (Incomed$Age_group_18to34 + Incomed$Age_group_35to49)

Incomed$Dependency_ratio <- (Incomed$Age_group_75andover + +Incomed$Age_group_50to74+
                                 Incomed$Age_group_5to17)/Incomed$Working_class

Incomed.numeric <- Incomed[c(6:ncol(Incomed))]
M <- cor(Incomed.numeric)
corrplot(M, method="color", type ='lower', tl.col="black")

ggplot(Incomed, aes(Median_Household_Income )) +
  geom_histogram()
############# Model01 - date: 1911 ############

Incomed.model1 <- Incomed[c('Sex_ratio','pct_OCC01','pct_OCC02','pct_OCC03',
                            'pct_OCC04','pct_OCC05','pct_COW01','pct_COW02',
                            'pct_COW03','Working_class',
                            'Dependency_ratio','EDU01','EDU02',
                            'EDU03')]

Incomed.model1$TAX_INC01 <- scale(Incomed$TAX_INC)
Incomed.model1$TAX_SR01 <- scale(Incomed$TAX_SR)
Incomed.model1$Median_Household_Income <- scale(Incomed$Median_Household_Income)

Incomed.model1 <- as.data.frame(lapply(Incomed.model1, as.numeric))

##################### EDA ##################################


#ggplot(Incomed, aes(y =Median_Household_Income, x= ID )) +
#geom_point()
#identify(row.names(Incomed), tolerance = 4,n=5)
################# Data imputation for TAX variables ################
#install.packages('Hmisc')

#KNN imputation

#Check the distribution of Tax variables
library(Hmisc)

#Imputing with a median would be a better call than mean because of the outliers
Incomed.model1$TAX_INC01 <- impute(Incomed.model1$TAX_INC01, median)

######### Dealing with outliers #########################

####### Basic model ###################
model01 <- lm(Median_Household_Income~.,data = Incomed.model1 )
summary(model01)
res <- model01$residuals

M01 <- cor(Incomed.model1)
corrplot(M01, method="color", type ='lower', tl.col="black")

# Outlying X values
M <- model.matrix(model01)
H <- M%*%solve(t(M)%*%M)%*%t(M)
leverage<-as.data.frame(diag(H))
leverage$ID <- Incomed$Id2
colnames(leverage) <- c('leverage','ID2')

leverage <- leverage[order(-leverage$leverage),]
ggplot(leverage, aes(y =leverage, x= ID2 )) +
  geom_point() + geom_text(aes(label=ID2),hjust=0, vjust=0)
#identify(row.names(Incomed), tolerance = 10,n=5)

######Threshold should be 2p/n
n <-  nrow(Incomed.model1)
p <- (ncol(Incomed.model1)-1)
2*p/n
#==0.0101846
########### A lot of them ############
count(leverage[leverage$leverage>= (2*p/n),])

# Outlying Y values
# Studentized Deleted Residuals
t_value<-as.data.frame(res*sqrt((n-p-1)/(sum(res^2)*(1-leverage$leverage)-res^2)))
t_value$ID2 <- Incomed$Id2
colnames(t_value) <- c('t_vals','ID2')
qt(1-(0.05/(2*n)),n-p-1)

t_value[t_value$t_vals>qt(1-(0.05/(2*n)),n-p-1),]

#Outliers wrt Y
#t_vals   ID2
#4.457662  2060
#4.772829 24009
#4.805234 24017
#4.423782 36059
#4.191479 38105
#4.610567 49009
#4.643473 51059
#6.408388 51107
#4.947210 51153
#4.648849 51179


### Influencial points
## Outlying wrt X as well as Y

#DFFITS
DFFITS <- as.data.frame(t_value$t_vals*sqrt(leverage$leverage/(1-leverage$leverage)))
DFFITS$ID2 <- Incomed$Id2
colnames(DFFITS) <- c('DFFITS','ID2')
DFFITS <- DFFITS[order(-DFFITS$DFFITS),]
count(DFFITS[DFFITS$DFFITS >= 2*sqrt(p/n),])
#Lot of outliers exists under this threshold

#Cooks dist
cooks_data <- influence.measures(model01)
cooks_data <- as.data.frame(cooks_data$infmat)
cooks_data$ID2 <- Incomed$Id2

ggplot(cooks_data, aes(y =cook.d, x= ID2 )) +
  geom_point() + geom_text(aes(label=ID2),hjust=0, vjust=0)
#identify(cooks_data$ID2, tolerance = 10,n=3)

#Threshold around 0.04
#30069
#15005
#30051
#48269
#12119


####### Removing Outliers ###############
Incomed.model1 <- Incomed.model1[-c(72,1197,1201,1858,2782,2849,
                                    2873,2893,2906),]


################## Model Diagnostics #########################
influence <- as.data.frame(influence(model01))
plot(model01)
anova(model01)

################### Removing multicollinear variables #####
vif(model01)
ncol(Incomed.model1)
step(model01)

Incomed.model1 <- subset(Incomed.model1, select=-c(pct_COW01,Working_class,pct_OCC01,EDU03))
colnames(Incomed.model1)
qqnorm(Incomed.model1$Median_Household_Income)
qqline(Incomed.model1$Median_Household_Income) 

################# 

# Basic Scatterplot Matrix
pairs(~Median_Household_Income+Sex_ratio+pct_OCC02+pct_OCC03+
      pct_OCC04,data=Incomed.model1, 
      main="Simple Scatterplot Matrix")

Incomed.model1$ID2 <- Incomed$Id2
ggplot(Incomed.model1, aes(Median_Household_Income )) +
  geom_histogram()
# approx normal, some outliers

model02 <- lm(Median_Household_Income~.,data = Incomed.model1)
vif(model02)
mean(vif(model02))
M01 <- cor(Incomed.model1)
corrplot(M01, method="color", type ='lower', tl.col="black")
summary(model02)
########## Reduction of Explainatory variables ############

step <- stepAIC(model02, direction="both")
summary(step) 

best1 <- regsubsets(x=Incomed.model1[,-13], y=Incomed.model1$Median_Household_Income,nbest=1,
                    nvmax=ncol(Incomed.model1)-1,method="exhaustive")
summary(best1)

best5 <- regsubsets(x=Incomed.model1[,-13], y=Incomed.model1$Median_Household_Income,nbest=5,
           nvmax=12,method="exhaustive")
summary(best5)
plot(model02)
