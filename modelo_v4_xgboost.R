library(xgboost)
library(pROC) 
# Elimino todas las variables
rm(list=ls())

# Leo el dataset
df<- read.csv(file = '/Users/jpascual/Documents/Personal/Orga_datos_7506/TP2/tp2_orga_datos/df_grouped_v4.csv',stringsAsFactors = F,header = T,sep=',')
cols<- read.csv(file = '/Users/jpascual/Documents/Personal/Orga_datos_7506/TP2/tp2_orga_datos/colnames2.csv',stringsAsFactors = F,header = T,sep=',')

########################################################################
# elimino varibles que no deben estar:
df$fecha<-NULL
df$index<-NULL
df$X<-NULL

########################################################################
# armo dataset solo con las mejores columnas que dejó rpart
rownames(cols)<-cols$x
cols2<-unique(c(rownames(cols),'label','person'))


#df<-df[,colnames(df) %in% cols2]


########################################################################
# fuerzo a que todas las variables sean binarias 0-1:
# df.person<-df$person
# for(i in 1:ncol(df)){
#   #print(i)
#   df[,i]<-ifelse(df[,i]>0,1,0)
# }
# df$person<-df.person
########################################################################

# convierto a factor la clase
df$label<-factor(df$label)

# armo el dataset "ciego":
df_person_ciego<-df[is.na(df$label),'person']
df.person<-df$person
df$person<-NULL
df.ciego<-df[is.na(df$label),]
df.ciego$label<-NULL

# me quedo con el dataset para modelar:
df<-df[!is.na(df$label),]





#particion
particion<-split(1:nrow(df),f = df$label)


##############################################################################################################
# Train y Test:

# Semilla:
set.seed(83920)

# índices para Train:
PORC<-0.75
index.train<-c(sample(particion[[1]], floor(length(particion[[1]])*PORC)),sample(particion[[2]], floor(length(particion[[2]])*PORC)))

# Dataset train:
df.train<-df[index.train,]
################################################################################################
# oversampling
nrow(df[!is.na(df.train$label)&df.train$label==1,])

df.train_aumentar<-df.train[!is.na(df.train$label)&df.train$label==1,]

df.train_aumentar<-rbind(df.train_aumentar,df.train_aumentar,df.train_aumentar,df.train_aumentar,df.train_aumentar,df.train_aumentar,df.train_aumentar)

df.train<-rbind(df.train,df.train_aumentar)

table(df.train$label)

# 21% positivos (4 replicas +1)
# 0.241836 % positivos (5+1)
# 0.1753601% positivos (3+1)
################################################################################################


# dataset Test (val):
df.val<-df[!(1:nrow(df)%in%index.train),]

df.train_label<-df.train$label
df.train$label<-NULL

df.val_label<-df.val$label
df.val$label<-NULL

df.label<-df$label
df$label<-NULL
table(df.label)



# Corroboro que me de la misma proporción:
table(df.train_label)/nrow(df.train)
table(df.val_label)/nrow(df.val)

# Corroboro que no haya perdido casos:
nrow(df.train)+nrow(df.val)==nrow(df)



#######################
# Creo las matrices:
trMatrix <- data.matrix(df.train)
trMatrix[1:5,1:8]
#trMatrix <- cbind(trMatrix,pdv)
#trMatrix <-trMatrix[,60:100]

# Observación: xgboost solo trabaja con matrices
trTarget<-df.train_label
table(trTarget)

tsMatrix <- as.matrix(df.val)
#tsMatrix <-tsMatrix[,60:100]
tsTarget<-df.val_label
table(tsTarget)

####################################################################################

#sample(as.integer(trTarget),100)

p.max_depth=3
p.colsample_bytree=0.7
p.min_child_weight=7
p.nrounds=100
p.eta=0.1
p.gamma=10
p.lambda='x'
p.subsample='x'
# auc=0.852038	p.max_depth= 3	p.colsample_bytree= 0.9	p.min_child_weight= 7	p.eta= 0.1	p.lambda= x	p.gamma= 1	p.nrounds= 55

# con dataset "binario" completo
# auc=0.853401 ,p.max_depth= 3 ,p.colsample_bytree= 0.7 ,p.min_child_weight= 7 ,p.eta= 0.1 ,p.lambda= x ,p.gamma= 10 ,p.nrounds= 100

modelo5000 <- xgboost(data=trMatrix, 
                      label=as.integer(trTarget)-1,
                      eval_metric = "auc",
                      #nfold=10,metrics = list("auc","rmse"),
                      max_depth=p.max_depth,
                      print_every_n = 50,
                      #verbose = 0,
                      colsample_bytree=p.colsample_bytree,
                      min_child_weight=p.min_child_weight,
                      nrounds=p.nrounds,
                      gamma=p.gamma,
                      eta=p.eta,
                      nthread=4,
                      #lambda=p.lambda,
                      #early_stopping_rounds = 100,
                      objective = "binary:logistic")

modelo.basico <- xgboost(data=trMatrix,
                      label=as.integer(trTarget)-1,
                      eval_metric = "auc",
                      nrounds=120,
                      nthread=4,
                      objective = "binary:logistic")


prModelo5000<- predict(modelo5000,tsMatrix)
res5000<-data.frame(a=tsTarget,b=ifelse(prModelo5000>0.2,yes=1,no = 0))
table(res5000)

roc_test <- roc( tsTarget, prModelo5000, algorithm = 2) 
plot(roc_test) 
auc(roc_test)
?roc


# 60-40
# 0.862157 ,p.max_depth= 3 ,p.colsample_bytree= 0.5 ,p.min_child_weight= 3 ,p.eta= 0.2 ,p.nrounds= 28" 


####################################################################################
xgb.importance(feature_names = colnames(trMatrix) ,model = modelo5000)

xgb.plot.importance(xgb.importance(feature_names = colnames(trMatrix) ,model = modelo5000))

aa<-xgb.importance(feature_names = colnames(trMatrix) ,model = modelo5000)

features<-aa$Feature[1:50]
####################################################################################
# BEST:
# record AUC = 0.8712 
# seed=1234
# particion de train test: 60-40
# nrounds=1000, colsample_bytree=0.5, max_depth=5,min_child_weight=3, gamma=13,eta=0.2


####################################################################################
# OTHERS
# nrounds=1000, colsample_bytree=0.5, max_depth=10,min_child_weight=3 gamma=10
# AUC=0.858


########################################################################################
# Mejor modelo
# Lo aplico al dataset entero, entonces genero la matriz:
trCompleto <- data.matrix(df)
trTargetComp<-df.label
nrow(trCompleto)
modelo500.COMP <- xgboost(data=trCompleto, 
                          label=as.integer(trTargetComp)-1,
                          eval_metric = "auc",
                          max_depth=p.max_depth,
                          print_every_n = 50,
                          colsample_bytree=p.colsample_bytree,
                          min_child_weight=p.min_child_weight,
                          nrounds=p.nrounds,
                          eta=p.eta,
                          nthread=4,
                          #gamma=p.gamma,
                          #subsample=p.subsample,
                          #lambda=1000,
                          #early_stopping_rounds = 100,
                          objective = "binary:logistic")


modelo500.COMP <- xgboost(data=trCompleto,
                      label=as.integer(trTargetComp)-1,
                      eval_metric = "auc",
                      #nfold=10,metrics = list("auc","rmse"),
                      #max_depth=p.max_depth,
                      #print_every_n = 100,
                      #verbose = 0,
                      #colsample_bytree=p.colsample_bytree,
                      #min_child_weight=p.min_child_weight,
                      nrounds=120,
                      #gamma=p.gamma,
                      #eta=0.1,
                      nthread=4,
                      #lambda=p.lambda,
                      #early_stopping_rounds = 100,
                      objective = "binary:logistic")


# aplico el modelo al df ciego
trMatrix.ciego <- data.matrix(df.ciego)
prModelo5000.ciego<- predict(modelo500.COMP,trMatrix.ciego)
head(prModelo5000.ciego)


########################################################################################
# Para submitear lo resultados:
res.ciego<-data.frame(a=ifelse(prModelo5000.ciego>0.05,yes=1,no = 0))
table(res.ciego)


entrega<-cbind(df_person_ciego,res.ciego)
colnames(entrega)<-c('person','label')
table(entrega$label)
nrow(entrega)
head(entrega)

write.csv(file = '/Users/jpascual/Documents/Personal/Orga_datos_7506/TP2/tp2_orga_datos/submit_results_v6',x=entrega,row.names = F,quote = F) 





########################################################################################
# guardo las columnas que usé:

write.csv(file = '/Users/jpascual/Documents/Personal/Orga_datos_7506/TP2/tp2_orga_datos/colnames.csv',x=colnames(df),row.names = F,quote = F) 

write.csv(file = '/Users/jpascual/Documents/Personal/Orga_datos_7506/TP2/tp2_orga_datos/colnames2.csv',x=features,row.names = F,quote = F) 

