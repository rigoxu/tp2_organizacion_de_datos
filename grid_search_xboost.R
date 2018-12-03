p.max_depth=3
p.colsample_bytree=0.5
p.min_child_weight=3
p.nrounds=10
p.eta=0.3
p.gamma=1
p.lambda='x'
p.early_stopping_rounds = 1000
# auc=0.852038	p.max_depth= 3	p.colsample_bytree= 0.9	p.min_child_weight= 7	p.eta= 0.1	p.lambda= x	p.gamma= 1	p.nrounds= 55
library(pROC) 

#0.847261	p.max_depth= 3	p.colsample_bytree= 0.75	p.min_child_weight= 10	p.eta= 0.1	p.nrounds= 50

res<-0
res.total<-0
AUC.total<-0
AUC.total<-NULL
res.total<-NULL


for(p.colsample_bytree in c(0.9)){
    for(p.min_child_weight in c(7)){
      for(p.eta in c(0.01)){
        for(p.max_depth in c(3)){
          for(p.gamma in c(20,25,30)){
            #for(p.lambda in c(0,1,5,10,100,200)){
            for(p.nrounds in c(100)){
    
  modelo5000 <- xgboost(data=trMatrix, 
                       label=as.integer(trTarget)-1,
                       eval_metric = "auc",
                       #nfold=10,metrics = list("auc","rmse"),
                       max_depth=p.max_depth,
                       #print_every_n = 100,
                       verbose = 0,
                       colsample_bytree=p.colsample_bytree,
                       min_child_weight=p.min_child_weight,
                       nrounds=p.nrounds,
                       gamma=p.gamma,
                       eta=p.eta,
                       nthread=4,
                       #lambda=p.lambda,
                       #early_stopping_rounds = 100,
                       objective = "binary:logistic")
  
  prModelo5000<- predict(modelo5000,tsMatrix)
  
  roc_test <- roc( tsTarget, prModelo5000, algorithm = 2) 
  
  #res<-paste((auc(roc_test ),'p.max_depth=',p.max_depth,',','p.colsample_bytree',p.colsample_bytree)
  #res<-paste(round(auc(roc_test),6),
  res<-paste(round(auc(roc_test),6),
             ',p.max_depth=',p.max_depth,
             ',p.colsample_bytree=',p.colsample_bytree,
             ',p.min_child_weight=',p.min_child_weight,
             ',p.eta=',p.eta,
             ',p.lambda=',p.lambda,
             ',p.gamma=',p.gamma,
             ',p.nrounds=',p.nrounds
             )
  res.total<-rbind(res.total,res)
  
  AUC<-auc(roc_test)
  AUC.total<-c(AUC.total,AUC)
  print(res)
  
  write.csv(x = res.total,'../Personal/Orga_datos_7506/TP2/tp2_orga_datos/restotal.csv',quote = F,row.names = F)
  
            #}
          }
        }
      }  
    }
  }
}


max(AUC.total)
res.total<-rbind(res.total,res)

write.csv(x = res.total,'../Personal/Orga_datos_7506/TP2/tp2_orga_datos/restotal.csv',quote = F,row.names = F)
AUC<-auc(roc_test)
AUC.total<-AUC
AUC.total<-c(AUC.total,AUC)


modelo5000 <- xgboost(data=trMatrix, 
                      label=as.integer(trTarget)-1,
                      eval_metric = "auc",
                      #nfold=10,metrics = list("auc","rmse"),
                      max_depth=p.max_depth,
                      #print_every_n = 100,
                      verbose = 0,
                      colsample_bytree=p.colsample_bytree,
                      min_child_weight=p.min_child_weight,
                      nrounds=p.nrounds,
                      gamma=p.gamma,
                      eta=p.eta,
                      nthread=4,
                      #lambda=p.lambda,
                      #early_stopping_rounds = 100,
                      objective = "binary:logistic")

prModelo5000<- predict(modelo5000,tsMatrix)

roc_test <- roc( tsTarget, prModelo5000, algorithm = 2) 