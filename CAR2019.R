car=read.csv("D:\\Data\\R_working\\datasets_Cars.csv")
nrow(car)
ncol(car)
names(car)
str(car)
summary(car)
#1.Data Preprocessing
#a.Identification and removal irrelevent variables 
#Checking where most of my data is located and spread out in all variables
table(factor(car$Make))
#Out of 856 observations in variable make, data is spread equally between all categories.
table(factor(car$Type))
#Out of 856 observations in variable type, data Sedan has 524 which is more than half compaired to others.
table(factor(car$Fuel))
#Out of 856 observations in variable fule, data is spreed most equal.
#So i will keep Make column and Fuel column and remove columns like Model,Type,Airpollution score,Invoice,Cert.Region,Origin,DrveTrain.
#AS these columns seems irrelavent for Price of the cars
library(dplyr)
car1=select(car,Make,Fuel,MSRP,EngineSize,Cylinders,Horsepower,MPG_City,MPG_Highway,Weight,Wheelbase,Length)

#b.Identification and removal duplicates.
dim(car1[duplicated(car1$MSRP),])[1]  #(row)
#there are 24 unique vaues which is removed.
car2=car1[!duplicated(car$MSRP),]

#C.Formatting and data type conversion
#We can see that MSRP (Price) variable in stored in string data type due to $ sign. So its imported to remove $ sign and convert the column to numeric data type
car2$MSRP=as.numeric(gsub("\\$|,","",car2$MSRP))
class(car2$MSRP)#check whether the data type has changed to numeric.

#2.Exploratory data annalysis

#a.Identification,visualizing and filling of missing values
any(is.na(car2))
colSums(is.na(car2))
library(Amelia)
missmap(car2,main = "Missing onces",col = c("Green","black"),legend = FALSE)
#we can see that there are 4 missing values in cylender variable
#Fill the missing rows with mean values
car2$Cylinders[is.na(car2$Cylinders)]=mean(car$Cylinders,na.rm=TRUE)
colSums(is.na(car2))

#b.Identification of Outliers and treating Outliers
#We will select variables only with numerical values
car3=drop(car2[,3:11])
boxplot(car)
#We can follow Zscore approch or IQR
#IQR approch
iqr=function(x){ 
qnt=quantile(x,probs = c(.25,.75))
v=1.5*IQR(x)
x[x<(qnt[1]-v)]=qnt[1]-v
x[x>(qnt[2]+v)]=qnt[2]+v
x
}
car4=car3%>%
  mutate_at(c("MSRP","EngineSize","Cylinders","Horsepower","MPG_City","MPG_Highway","Weight","Wheelbase","Length"),iqr)
boxplot(car4)
#We can see there are no outliers now
#or
#car5=car2%>%                     #with out droping the text column
 # mutate_if(is.numeric,iqr)

#c.Checking of correlation
cor(car4)
pairs(car4,upper.panel = NULL)
#to understand the correlation between Price and other variables we use correlation matrix
library(Hmisc)
#rcorr matric gives the the correlation and also P-Values
rcorr(as.matrix(car4))
#Correlation Matrix confirms there exhist a strong positive relationship between MSRP and Horsepower of 85%
#this relationship can be visiualised by scatterplot
ggplot(car4,aes(x=Horsepower,y=MSRP))+geom_point()
cor(car4[,c("Horsepower","MSRP")])

#Model Building
#simple lenior regression
#Prediction of Price when the Hoursepower is 225,250,275,300

#a.Visulising the best fit line(Mean line) passing through variables MSRP and Hoursepower
pairs(car4$MSRP~car4$Horsepower)
plot(car4$MSRP,car4$Horsepower)
abline(h=mean(car4$MSRP),v=mean(car4$Horsepower),col="red",untf = FALSE)

#b.Applying the linear regression
car_model=lm(MSRP~Horsepower,car4)
car_model
#Intercept value(b0)=-7133.667
#Slope(b1)=173.469
#Predicting when Hoursepower is 225,250,275,300 what could be the price of the car
-7133.667+(173.469*225)=31896.86
#Model Prediction
predict(car_model,data.frame(Horsepower=c(225,250,275,300)))
#Model prediction is accurate with the result
abline(car_model,col="green")#Thus my best fit line pass through the mean points
#same can be viewed in ggpplot2
p=ggplot(car4,aes(y=MSRP,x=Horsepower))+geom_point()+geom_smooth(method = lm,se=FALSE)
p+scale_color_continuous(low ="55D8CE", high="#FF6E2E")

summary(car_model)
#conclusion: Simple linear regression R2 = 71.62%

#Multi linear regression
#Prediction of MSRP(Price)when EngineSize=3.5 Cylinders=6 Horsepower=225 MPG_City=18 MPG_Highway=24 Weight=3880  Wheelbase= 115 Length= 19
car_model2=lm(MSRP~EngineSize+Cylinders+Horsepower+MPG_City+MPG_Highway+Weight+Wheelbase+Length,car4)
summary(car_model2)
#conclusion: Simple linear regression R2 = 77.11%
#Model Prediction when engine=3.5, cyl=6, Horse=225, MPG_city=18, MPG_highway=24, weight=3880, wheelbase=115, len=19
predict(car_model2,data.frame(EngineSize=c(3.5)
                             ,Cylinders=c(6)
                             ,Horsepower=c(225)
                             ,MPG_City=18   
                             ,MPG_Highway=24  
                             ,Weight=3880  
                             ,Wheelbase= 115 
                             ,Length= 19))
#63996.38