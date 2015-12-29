#计算矩阵相似性，最后一个参数为距离计算公式
matrixVecComputerFun<-function(a,b,disFun){
  if(nrow(a)!=ncol(b)){
    stop("行列不一致，请检查矩阵！")
  }
  
  apply(a,1,function(y) {apply(b,2,disFun,y)})
}

#使用欧几里得距离计算
euclideanDistanceSimilarity<-function(x,y){
  t<-y-x
  length(t[t!=0])/(1+sqrt(sum(t^2)))
}

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

  #user_similarity<-mx%*%t(mx)
  #s<-rowSums(mx^2)
  #归一化
  #user_similarity<-user_similarity/sqrt(s%*%t(s))
  user_similarity<-matrixVecComputerFun(mx,t(mx),euclideanDistanceSimilarity)
  
  #rec<-t(t(mx)%*%user_similarity)
  #根据矩阵转置的性质，(A'B)'=B'A，又因为B是对称矩阵，B'=B
  #故(A'B)'=BA
  rec<-user_similarity%*%mx
  rec[(items_rownum[userCf$itemId]-1)*length(users_rownum)+users_rownum[userCf$userId]]<-0
  rec
}

