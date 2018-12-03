library(rpart)
library(rpart.plot)

# Elimino todas las variables
rm(list=ls())

# Leo el dataset
df<- read.csv(file = '/Users/jpascual/Documents/Personal/Orga_datos_7506/TP2/tp2_orga_datos/df_grouped_v4.csv',stringsAsFactors = F,header = T,sep=',')
colnames(df)


df$fecha<-NULL



#df$label<-ifelse(df$conversion>0,1,0)
df$label<-factor(df$label)

df_person_ciego<-df[is.na(df$label),'person']
df.person<-df$person

df$person<-NULL

df$index<-NULL
df$X<-NULL


df.ciego<-df[is.na(df$label),]
df<-df[!is.na(df$label),]

table(df$label)

#particion
particion<-split(1:nrow(df),f = df$label)

##############################################################################################################
# Train y Test:

# Semilla:
set.seed(1234)

# índices para Train:
index.train<-c(sample(particion[[1]], floor(length(particion[[1]])*0.8)),sample(particion[[2]], floor(length(particion[[2]])*0.8)))

# Dataset train:
df.train<-df[index.train,]



# dataset Test (val):
df.val<-df[!(1:nrow(df)%in%index.train),]




# Corroboro que me de la misma proporción:
table(df.train$label)/nrow(df.train)
table(df.val$label)/nrow(df.val)

# Corroboro que no haya perdido casos:
nrow(df.train)+nrow(df.val)==nrow(df)


df$label<-as.factor(df$label)



m1<-rpart(label~.,data=df.train,
          control=rpart.control(maxdepth = 30,minsplit = 1,minbucket = 1,cp = 0.001,xval = 10) ,
          method = 'class')

m1
var<-m1$variable.importance
class(var)
df[,var[1:10]]



rpart.plot(x = m1,type = 2,extra = 1)

pred1<-predict(object = m1,newdata = df.val,type='prob')


predicho <- ifelse(pred1[,2]>0.5,1,0)
confusion <- table(df.val$label,predicho)

#Paso a Matriz para poder manipularla
confusion <- as.matrix(confusion)

#La tengo que ordenar 
confusion <- confusion[c(2,1),c(2,1)]
confusion




printcp(m1)
plotcp(m1)

m1$variable.importance


# Metricas:
# Accuracy:
accuracy<-sum(diag(confusion))/sum(confusion)

# Precisión
precision<-confusion[1,1]/sum(confusion[,1])

# Sensibilidad
sensibilidad<-confusion[1,1]/sum(confusion[1,])

# Especificidad
especificidad<-confusion[2,2]/sum(confusion[2,])

accuracy
precision
sensibilidad
especificidad

sensibilidad/especificidad

#########################
# aplico el modelo con todo el dataset para submitear


m2<-rpart(label~.,data=df,cp=0.0001,method = 'class')

rpart.plot(x = m2,type = 2,extra = 1)

pred2<-predict(object = m1,newdata = df.ciego,type='prob')

head(pred2,100)



res.ciego<-data.frame(a=ifelse(pred2>0.2,yes=1,no = 0))
table(res.ciego)


entrega<-cbind(df_person_ciego,res.ciego)
colnames(entrega)<-c('person','label')
table(entrega$label)
nrow(entrega)
head(entrega)

write.csv(file = '/Users/jpascual/Documents/Personal/Orga_datos_7506/TP2/tp2_orga_datos/submit_results_v5',x=entrega,row.names = F,quote = F) 
