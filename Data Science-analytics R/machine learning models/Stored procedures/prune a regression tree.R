
#prune a regression tree

prune<- function(digits,Independent,Dependent,Train,Test,ConTree){

  tree2 <- rpart(formula=Dependent~Independent, data = train, control=rpart.control(cp=0))

  #prune parameter
  mincp <- ConTree$cptable[which.min(Contree$cptable[,"xerror"]),"CP"]
  
  #prune tree
  prunedtree <- prune(tree2,mincp)
  
  #predict
  #tree with x constraints
  test$level3 <- predict(ConTree,Test,type = "vector")
  #fulltree
  test$full <- predict(tree2,Test,type = "vector")
  #prunedtree
  test$pruned <- predict(prunedtree,Test, type = "vector")

}


prune2(3,.,Collection,train,test,tree)
