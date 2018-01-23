setwd("D:/Rworkspace")
getwd()

train=read.csv("train.csv")
test=read.csv("test.csv")

str(train)

#N_BEDROOM,N_BATHROOM and QS_OVERALL has NA values in train 
#inspecting values

temp=train[is.na(train$N_BEDROOM),]
#using mean of similar observations to impute the missing values
for(i in 1:nrow(temp) ){
  temp$N_BEDROOM=floor(mean(subset(train,train$N_BATHROOM==temp$N_BATHROOM[i] & train$N_ROOM==temp$N_ROOM[i],
                                   select = N_BEDROOM)$N_BEDROOM,na.rm = TRUE)+0.5)
}
for(i in 1:nrow(temp)){
  train$N_BEDROOM[train$PRT_ID==temp$PRT_ID[i]]=temp$N_BEDROOM[i]
}


#similaryly for bathrooms
temp=train[is.na(train$N_BATHROOM),]
nrow(temp) 
#5 missing values
temp
#using for loop to dynamically average the number of bathrooms using number of bedrooms and total rooms
for(i in 1:nrow(temp) ){
  temp$N_BATHROOM=floor(mean(subset(train,train$N_BEDROOM==temp$N_BEDROOM[i] & train$N_ROOM==temp$N_ROOM[i],
                                    select = N_BATHROOM)$N_BATHROOM,na.rm = TRUE)+0.5)
}
for(i in 1:nrow(temp)){
  train$N_BATHROOM[train$PRT_ID==temp$PRT_ID[i]]=temp$N_BATHROOM[i]
}
#QS_OVERALL imputing with average quality score
for(i in 1:nrow(train)){
  if(is.na(train$QS_OVERALL[i])){
    train$QS_OVERALL[i]=mean(train$QS_OVERALL,na.rm = TRUE)
  }
}

#repeating the same for the test data

#QS_OVERALL imputing with average quality score
for(i in 1:nrow(test)){
  if(is.na(test$QS_OVERALL[i])){
    test$QS_OVERALL[i]=mean(train$QS_OVERALL,na.rm = TRUE)
  }
}

#checking out the categorical variables
table(train$SALE_COND)
#many of them are the same but have diefferent spelling, so combining them into one
train$SALE_COND=as.character(train$SALE_COND)
train$SALE_COND[train$SALE_COND %in% c('Ab Normal', 'AbNormal')] ='Abnormal'
train$SALE_COND[train$SALE_COND %in% c('Adj Land', 'AdjLand')] ='AdjLand'
train$SALE_COND[train$SALE_COND %in% c('Partial', 'Partiall','PartiaLl')] ='Partial'

train$SALE_COND=as.factor(train$SALE_COND)

table(train$PARK_FACIL)

train$PARK_FACIL=as.character(train$PARK_FACIL)
train$PARK_FACIL[train$PARK_FACIL %in% c('Noo','No')] ='No'
train$PARK_FACIL=as.factor(train$PARK_FACIL)

table(train$AREA)

train$AREA=as.character(train$AREA)
train$AREA[train$AREA %in% c('Adyar','Adyr')]='Adyar'
train$AREA[train$AREA %in% c('Ana Nagar','Ann Nagar','Anna Nagar')]='Anna Nagar'
train$AREA[train$AREA %in% c('Chormpet','Chrmpet','Chrompet','Chrompt')]='Chrompet'
train$AREA[train$AREA %in% c('Karapakam','Karapakkam')]='Karapakam'
train$AREA[train$AREA %in% c('KK Nagar','KKNagar')]='KK Nagar'
train$AREA[train$AREA %in% c('T Nagar','TNagar')]='T Nagar'
train$AREA[train$AREA %in% c('Velachery','Velchery')]='Velachery'
train$AREA=as.factor(train$AREA)

table(train$BUILDTYPE)

train$BUILDTYPE=as.character(train$BUILDTYPE)
train$BUILDTYPE[train$BUILDTYPE %in% c('Comercial','Commercial')]='Commercial'
train$BUILDTYPE[train$BUILDTYPE %in% c('Other','Others')]='Others'
train$BUILDTYPE=as.factor(train$BUILDTYPE)

table(train$UTILITY_AVAIL)

train$UTILITY_AVAIL=as.character(train$UTILITY_AVAIL)
train$UTILITY_AVAIL[train$UTILITY_AVAIL %in% c('All Pub','AllPub')]='AllPub'
train$UTILITY_AVAIL[train$UTILITY_AVAIL %in% c('NoSeWa','NoSewr ')]='NoSewer'
train$UTILITY_AVAIL=as.factor(train$UTILITY_AVAIL)

table(train$STREET)

train$STREET=as.character(train$STREET)
train$STREET[train$STREET %in% c('No Access','NoAccess')]='No Access'
train$STREET[train$STREET %in% c('Pavd','Paved ')]='Paved'
train$STREET=as.factor(train$STREET)

#repeating the same for test

table(test$SALE_COND)
#many of them are the same but have diefferent spelling, so combining them into one
test$SALE_COND=as.character(test$SALE_COND)
test$SALE_COND[test$SALE_COND %in% c('Ab Normal', 'AbNormal')] ='Abnormal'
test$SALE_COND[test$SALE_COND %in% c('Adj Land', 'AdjLand')] ='AdjLand'
test$SALE_COND[test$SALE_COND %in% c('Partial', 'Partiall','PartiaLl')] ='Partial'

test$SALE_COND=as.factor(test$SALE_COND)

table(test$PARK_FACIL)

test$PARK_FACIL=as.character(test$PARK_FACIL)
test$PARK_FACIL[test$PARK_FACIL %in% c('Noo','No')] ='No'
test$PARK_FACIL=as.factor(test$PARK_FACIL)

table(test$AREA)

test$AREA=as.character(test$AREA)
test$AREA[test$AREA %in% c('Adyar','Adyr')]='Adyar'
test$AREA[test$AREA %in% c('Ana Nagar','Ann Nagar','Anna Nagar')]='Anna Nagar'
test$AREA[test$AREA %in% c('Chormpet','Chrmpet','Chrompet','Chrompt')]='Chrompet'
test$AREA[test$AREA %in% c('Karapakam','Karapakkam')]='Karapakam'
test$AREA[test$AREA %in% c('KK Nagar','KKNagar')]='KK Nagar'
test$AREA[test$AREA %in% c('T Nagar','TNagar')]='T Nagar'
test$AREA[test$AREA %in% c('Velachery','Velchery')]='Velachery'
test$AREA=as.factor(test$AREA)

table(test$BUILDTYPE)

test$BUILDTYPE=as.character(test$BUILDTYPE)
test$BUILDTYPE[test$BUILDTYPE %in% c('Comercial','Commercial','Commercil')]='Commercial'
test$BUILDTYPE[test$BUILDTYPE %in% c('Other','Others')]='Others'
test$BUILDTYPE=as.factor(test$BUILDTYPE)

table(test$UTILITY_AVAIL)

test$UTILITY_AVAIL=as.character(test$UTILITY_AVAIL)
test$UTILITY_AVAIL[test$UTILITY_AVAIL %in% c('All Pub','AllPub')]='AllPub'
test$UTILITY_AVAIL[test$UTILITY_AVAIL %in% c('NoSeWa','NoSewr ')]='NoSewer'
test$UTILITY_AVAIL=as.factor(test$UTILITY_AVAIL)

table(test$STREET)

test$STREET=as.character(test$STREET)
test$STREET[test$STREET %in% c('No Access','NoAccess')]='No Access'
test$STREET[test$STREET %in% c('Pavd','Paved ')]='Paved'
test$STREET=as.factor(test$STREET)



#converting date from string to date type
train$convertedDte=as.Date.factor(train$DATE_SALE,'%d-%m-%Y')
train$convertedBuilDate=as.Date.factor(train$DATE_BUILD,'%d-%m-%Y')
summary(train)

#calculating date for purchase in months from today
train$convertedDte<-as.Date(train$convertedDte,origin = '1970-01-01')
train$convertedBuilDate<-as.Date(train$convertedBuilDate,origin = '1970-01-01')
#Number of days since account opened
train$Days_SALE<-Sys.Date()- train$convertedDte
train$Days_BUILT=Sys.Date()- train$convertedBuilDate
train$Days_SALE<-as.numeric(train$Days_SALE)
train$Days_BUILT<-as.numeric(train$Days_BUILT)
#number of months since account has been opened
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

for(i in 1:nrow(train)){
  train$months_SALE[i]<-elapsed_months(Sys.Date(),train$convertedDte[i])
  train$months_BUILT[i]<-elapsed_months(Sys.Date(),train$convertedBuilDate[i])
}

#repeating same for test
test$convertedDte=as.Date.factor(test$DATE_SALE,'%d-%m-%Y')
test$convertedDtconvertedBuilDate=as.Date.factor(test$DATE_BUILD,'%d-%m-%Y')
summary(test)

#calculating date for purchase in months from today
test$convertedDte<-as.Date(test$convertedDte,origin = '1970-01-01')
test$convertedDtconvertedBuilDate<-as.Date(test$convertedDtconvertedBuilDate,origin = '1970-01-01')
#Number of days since account opened
test$Days_SALE<-Sys.Date()- test$convertedDte
test$Days_BUILT<-Sys.Date()- test$convertedDtconvertedBuilDate
test$Days_SALE<-as.numeric(test$Days_SALE)
test$Days_BUILT<-as.numeric(test$Days_BUILT)
#number of months since account has been opened
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

for(i in 1:nrow(test)){
  test$months_SALE[i]<-elapsed_months(Sys.Date(),test$convertedDte[i])
  test$months_BUILT[i]<-elapsed_months(Sys.Date(),test$convertedDtconvertedBuilDate[i])
  
}

#removing dates and days from today derived from it 
train1=train[,-c(4,11,26,25,24,23)]
test1=test[,-c(4,11,24,25,23,22)]

#seperating commercial buildings from others 

train3= subset(train1,BUILDTYPE=='Commercial')
train3$BUILDTYPE=as.character(train3$BUILDTYPE)
train3$BUILDTYPE=as.factor(train3$BUILDTYPE)
train4=subset(train1,BUILDTYPE!='Commercial')
train4$BUILDTYPE=as.character(train4$BUILDTYPE)
train4$BUILDTYPE=as.factor(train4$BUILDTYPE)

test3=subset(test1,BUILDTYPE=='Commercial')
test3$BUILDTYPE=as.character(test3$BUILDTYPE)
test3$BUILDTYPE=as.factor(test3$BUILDTYPE)
test4=subset(test1,BUILDTYPE!='Commercial')
test4$BUILDTYPE=as.character(test4$BUILDTYPE)
test4$BUILDTYPE=as.factor(test4$BUILDTYPE)

#building seperate model for Commercial and non-commercial

model1=step(lm(SALES_PRICE~.,data=train3[,-c(1,10)]),direction = c('both'))
summary(model1)
plot(model1)

model2=step(lm(SALES_PRICE~.,data=train4[,-1]),direction = c('both'))
summary(model2)
plot(model2)

#for commercial
pred1=predict(model1,newdata=test3[,-c(1,10)])
#for non commercial
pred2=predict(model2,newdata=test4[,-1])

part1=data.frame(PRT_ID=test3$PRT_ID,SALES_PRICE=pred1)
part2=data.frame(PRT_ID=test4$PRT_ID,SALES_PRICE=pred2)
solution=rbind(part1,part2)
write.csv(solution,"solution.csv",row.names = FALSE)
