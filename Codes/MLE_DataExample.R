########################################################
# CHEP 805.3 - Biostatistics I
########################################################

# Doctorvisits data from AER package, 
# in which the variable visits will be our target variable
# source: https://www.r-bloggers.com/2020/01/count-data-models/

library(tidyverse)
library(AER)
library(broom)
library(ModelMetrics)

# Load data
data("DoctorVisits")
doc <- DoctorVisits
glimpse(doc)

doc$visits<-as.integer(doc$visits)
doc$illness <- as.factor(doc$illness)

tab <- table(doc$visits)
tab

# Plot response
plot(table(DoctorVisits$visits))


## Fit Poisson model
model11<-glm(visits~gender+age+income+illness+nchronic+lchronic, data=doc, 
             family ="poisson")
summary(model11)
glance(model11)


# Check theoretical and empirical distribution 

pos <- dpois(0:9,0.302)*5190
both <- numeric(20)
both[1:20 %% 2 != 0] <- tab
both[1:20 %% 2 == 0] <- pos
labels<-character(20)
labels[1:20 %% 2==0]<-as.character(0:9)
barplot(both,col=rep(c("red","yellow"),10),names=labels)


# Overdispersion test

dispersiontest(model11)

# since the p-value is very small then we have overdispersion problem, 
# suggesting the use of quasi-poisson model instead.

## Fit Quasi poisson model

model2<-glm(visits~gender+age+income+illness+nchronic+lchronic, 
            data=doc, family ="quasipoisson")
tidy(model2)
glance(model2)

# This model uses the quasi maximum likelihood which gives 
# the same coefficient estimates but with different (corrected) standard errors.


