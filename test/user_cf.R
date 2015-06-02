getRecMatrix<-function(){
    userCf<-read.csv("../data/userCf.csv",header = FALSE)
  names(userCf)<-c("userId","itemId","score")
  userCf$userId<-as.character(userCf$userId)
  userCf$itemId<-as.character(userCf$itemId)
  users<-unique(userCf$userId)
  items<-unique(userCf$itemId)
  users_rownum<-1:length(users)
  names(users_rownum)<-users
  items_rownum<-1:length(items)
  names(items_rownum)<-items
  mx<-matrix(0,nrow = length(users),ncol = length(items),dimnames = (list(users,items)))
  #使用坐标计算，将二维的坐标转换为一维的坐标
  #一维的坐标可以按点取值，二维的只能按行按列取
  mx[(items_rownum[userCf$itemId]-1)*length(users_rownum)+users_rownum[userCf$userId]]<-userCf$score
  
  similarity<-mx%*%t(mx)
  s<-rowSums(mx^2)
  #归一化
  similarity<-similarity/sqrt(s%*%t(s))
  similarity
  
  rec<-t(t(mx)%*%similarity)
  rec[(items_rownum[userCf$itemId]-1)*length(users_rownum)+users_rownum[userCf$userId]]<-0
  rec
}

