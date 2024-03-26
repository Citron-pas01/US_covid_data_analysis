library(ggplot2)
library(tidyverse)
library(nycflights13)
library(gapminder)
library(data.table)
library(reshape2)
library(openintro)
library(readxl)
library(stringr)
library(gridExtra)
library(vip)
library(caret)
library(MASS)
library(LEAP)
library(dplyr)
library(glmnet)
library(alr4)
library(xtable)

#declaring path to save files if need arises
dir_path = "C:/Users/Bo_Ni/OneDrive - The Pennsylvania State University/PSU/1-Class/IE 582/Hw/Hw3/"

#importing datasets from web
day_dt = as_tibble(fread("http://personal.psu.edu/mzm6664/day.csv",header = T))
hr_dt = as_tibble(fread("http://personal.psu.edu/mzm6664/hour.csv",header = T))
#fwrite(day_dt,"C:\\Users\\mihir\\Desktop\\day.csv")
#fwrite(hr_dt,"C:\\Users\\mihir\\Desktop\\hour.csv")
#day_dt = as_tibble(fread("C:\\Users\\mihir\\Desktop\\day.csv",header = T) )
#hour_dt = as_tibble(fread("C:\\Users\\mihir\\Desktop\\hour.csv",header = T))


#remove primary keys as there not useful in regression
day_dt = day_dt %>% dplyr::select(-(instant))
  
hour_dt = hr_dt %>% dplyr::select(-(instant))

#meta data locations
#http://personal.psu.edu/mzm6664/Readme.txt

#quickly look at the datasets
colnames(day_dt)
head(day_dt)

#quickly look at the datasets
colnames(hour_dt)
head(hour_dt)

#creating 3 datasets for each datasets
casual_day = day_dt %>% dplyr::select(-(registered:cnt))
cnt_day = day_dt %>% dplyr::select(-(casual:registered))
registered_day = day_dt %>% dplyr::select(-casual, -cnt)

casual_hr = hour_dt %>% dplyr::select(-(registered:cnt))
cnt_hr = hour_dt %>% dplyr::select(-(casual:registered))
registered_hr = hour_dt %>% dplyr::select(-casual, -cnt)

#function which displays different information about for a given model
display_model <- function(mdl_nm,name)
{ print(name)
  print("Summary Statistics of the model as follows:")
  print(summary(mdl_nm))
  print("VIF Stats for the model as follows:")
  print(car::vif(mdl_nm))
  print("Now plotting Residual vs Fitted Values Graph")
  print(ggplot()+
    geom_point(aes(y=mdl_nm$residuals,x=mdl_nm$fitted.values),color="black")+
    labs(x="Fitted Values",y="Residual",
         title=" Residual vs Fitted Values",caption=name)+
    theme_bw())
  print("Now plotting Std Residual vs Fitted Values Graph")
  print(ggplot()+
    geom_point(aes(y=stdres(mdl_nm),x=mdl_nm$fitted.values),color="red")+
    labs(x="Fitted Values",y="Standardized Residual",
         title="Std Res vs Fitted Values",caption=name)+
    theme_bw())
  print("Now plotting Frequnecy Distribution of Std. residuals")
  print(ggplot()+
    geom_freqpoly(aes(x=stdres(mdl_nm)),bins=30,color="blue")+
    labs(x="Standardized Residual",y="Count",
         title="Frequnecy Distribution of Std. residuals",
         caption=name)+
    theme_bw())
}


#function which displays different information about for regularized regression
rid_las_ela_net <- function (mdl_nm,name)
{
  print(name)
  temp_res= mdl_nm$results
  temp_cv = mdl_nm$resample
  print("The best alpha and lambda values are as follows:")
  print(c(mdl_nm$bestTune$alpha,mdl_nm$bestTune$lambda))
  print("The final model RMSE and RSqaured values are as follows:")
  print(c(min(temp_res$RMSE),max(temp_res$Rsquared,na.rm = T)))
  print("Final Model coefficients are as below:")
  print(coef(mdl_nm$finalModel, mdl_nm$bestTune$lambda))
  
  #determining breaks dynamically
  print("Cross Validation Results are as below:")
  print(temp_cv)
  
  temp_res$alphaf=as.factor(temp_res$alpha)
  minE=min(temp_res$RMSE)
  maxE=max(temp_res$RMSE)
  diffE=(maxE-minE)/10
  minR=min(temp_res$Rsquared,na.rm = T)
  maxR=max(temp_res$Rsquared,na.rm = T)
  diffR=(maxR-minR)/10
  
  print("the minimum value of lambda")
  
  print(min(temp_res$lambda))
  
  print("Now plotting Lambda vs RMSE")
  
  print(ggplot(data=temp_res)+
          geom_line(aes(x=lambda,y=RMSE,color=alphaf))+
          scale_x_log10(name="Lambda")+
          scale_y_continuous(name="RMSE",breaks=round(seq(minE,maxE,diffE),0))+
          labs(title="lambda vs RMSE",
               ylab="RMSE",
               caption = name)+
          theme_bw()+
          theme(legend.position = "bottom"))
  
  print("Now plotting Lambda vs RSquared")
  
  print(ggplot(data=temp_res)+
          geom_line(aes(x=lambda,y=Rsquared*100,color=alphaf))+
          scale_x_log10(name="Lambda")+
          scale_y_continuous(name="RSqaured",breaks=round(seq(minR,maxR,diffR)*100,1))+
          labs(title="lambda vs RSquared",
               ylab="RMSE",caption = name)+
          theme_bw()+
          theme(legend.position = "bottom"))
  
  
  
  temp_cv$Resamplef=as.factor(temp_cv$Resample)
  
  print("Now plotting RMSE Distribution across CV")
 
  print(ggplot(data=temp_cv)+
          geom_boxplot(aes(y=RMSE),color="red",fill="green")+
          labs(y="RMSE",title="RMSE Distribution in 10 folds",caption = name)+
          theme_bw()+
          theme(axis.text.x = element_blank()))
  
  print("Now plotting Rsqaured Distribution across CV")
  
  print("Now printing the mean, median and std of Rsquare")
  print(mean(temp_cv$Rsquared))
  print(median(temp_cv$Rsquared))
  print(sd(temp_cv$Rsquared))
  
  print(ggplot(data=temp_cv)+
          geom_boxplot(aes(y=Rsquared*100),color="blue",fill="forestgreen")+
          labs(y="R Squared",title="RSqaured Distribution in 10 folds",caption = name)+
          theme_bw()+
          theme(axis.text.x = element_blank()))
  
  print("Now plotting Variable Importance Plot using vip library")
  
  print(vip(mdl_nm, num_features = 15, geom = "point")+theme_bw()+
          labs(y="Importance",x="Variable Name",title="Variable Importance Plot",
               caption=paste(name,"Importance is determined by magnitude of the standardized coefficients",sep=": ")))
  
  #print("Printing Distinct values of Alpha")
  #print(temp_res %>% distinct(alpha))
  
  #print("Printing Distinct values of Lambda")
  #print(temp_res %>% distinct(lambda))
}




#linear regression for "day" dataset
model_full <- lm(cnt ~ ., data = cnt_day)
model_null <- lm(cnt ~ 1, data = cnt_day)

#First Iteration using all the methods

#forward selection using AIC
fwd_AIC1 <- stepAIC(model_null, scope = list(lower=model_null,upper=model_full),
                   direction = "forward", trace = T)
display_model(fwd_AIC1,"fwd_AIC1")

#backward selection using AIC
bwd_AIC1 <- stepAIC(model_full, scope = list(upper=model_null,lower=model_full),
                   direction = "backward", trace = T)
display_model(bwd_AIC1,"bwd_AIC1")

#stepwise selection using AIC
step_AIC1 <- stepAIC(model_null, scope = list(lower=model_null,upper=model_full),
                    direction = "both", trace = T)
display_model(step_AIC1,"step_AIC1")



#second iteration of linear models
model_full <- lm(cnt ~ .-dteday, data = cnt_day)


#forward selection using AIC
fwd_AIC2 <- stepAIC(model_null, scope = list(lower=model_null,upper=model_full),
                   direction = "forward", trace = T)
display_model(fwd_AIC2,"fwd_AIC2")



#third iteration of linear models
model_full <- lm(cnt ~ .-dteday-atemp, data = cnt_day)

#backward selection using AIC
bwd_AIC3 <- stepAIC(model_full,
                   direction = "backward", trace = T)
display_model(bwd_AIC3,"bwd_AIC3")



#Application of Lasso, Ridge and Elastic Net
set.seed(582)

#linear regression for "hour" dataset

# Variable Assignment
Y=cnt_hr$cnt
X=data.matrix(cnt_hr %>% dplyr::select(-cnt))
lambda <- 10^seq(-6, 6, length = 1000)
alpha = seq(0,0.9,0.1)

#caret train package uses
# glmnet library
# performs 10 fold cross validation
# preprocessing data
# for varied values of alpha and lambda

ridge <- train(
  x=X,y=Y, method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)

set.seed(233)

lasso <- train(
  x=X,y=Y, method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)

set.seed(330)

elastic_net <- train(
  x = X,
  y = Y,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)




rid_las_ela_net(ridge,"ridge")

rid_las_ela_net(lasso,"lasso")

rid_las_ela_net(elastic_net,"elastic net")










