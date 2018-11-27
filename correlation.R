############################################################################################################################
# scatter plot and correlation matrix

grwth_county_numeric<- grwth_county[c(3:ncol(grwth_county))]
p1 <- cor(grwth_county_numeric)
corrplot(p1 , method="color", type ='lower', tl.col="black")