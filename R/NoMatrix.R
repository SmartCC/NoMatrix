#所需的程序包列表
pl<-c("RNeo4j","plyr","XML")

#检查所需的包是否安装
if(length(setdiff(pl,.packages(all.available=T)))!=0){
  stop(paste("缺失包：",paste(setdiff(comment.df.names,names(df)),collapse = ",")))
}


library(RNeo4j)
library(plyr)
library(XML)

#读取配置文件
source("conf.R")
conf_path="../conf/configure.xml"
conf<-getConfigure(conf_path)

#创建neo4j连接，需要后边的username,否则会报302错误
#URL最后一定要加/，否则会找不到地址
graph<-startGraph(
  paste(getConfProperty("neo4j.host",conf),":",getConfProperty("neo4j.port",conf),getConfProperty("neo4j.db.path",conf),sep = ""),
  username=getConfProperty("neo4j.user",conf),
  password=getConfProperty("neo4j.password",conf))
clear(graph)

initGraph<-function(graph,conf){
  #读取数据
  gdata<-read.csv(getConfProperty("neo4j.init.data",conf))
  #将数据id转换为字符串
  gdata$user_id<-as.character(gdata$user_id)
  gdata$brand_id<-as.character(gdata$brand_id)
  
  #用户id列表
  users<-unique(gdata$user_id)
  #商品id列表
  brands<-unique(gdata$brand_id)
  
  addIndex(graph,"USERID","uid")
  #创建用户图节点
  userNodes<-llply(users,function(x) createNode(graph,"USERID",uid=x))
  #将返回结果给数据命名索引
  names(userNodes)<-users
  #添加限制函数，使插入的数据唯一
  addConstraint(graph, "USERID", "uid")
  
  #创建商品图节点
  addIndex(graph,"BRANDID","uid")
  brandNodes<-llply(brands,function(x) createNode(graph,"BRANDID",bid=x))
  names(brandNodes)<-brands
  addConstraint(graph,"BRANDID","bid")
  
  #创建联系
  a_ply(gdata,1,function(df) createRel(userNodes[[df$user_id]],df$type,brandNodes[[df$brand_id]]))
  
}

user_cf.recom<-function(userid){
  user_brands<-cypher(graph,paste("match (n:USERID {uid:'",userid,"'})-[:`1`]->(ms:BRANDID)<-[:`1`]-(ns:USERID) return ns.uid as uid ,count(ms) as fre",sep = ""))
  sample_user_brands<-cypher(graph,paste("match (n:USERID {uid:'",userid,"'})-[:`1`]->(m:BRANDID)<-[:`1`]-(ns:USERID)-[:`1`]->(ms:BRANDID) return ns.uid as uid ,count(ms) as fre",sep = ""))
  #
  user_brands$fre<-as.numeric(user_brands$fre)
  rownames(user_brands)<-user_brands$uid
  #
  sample_user_brands$fre<-as.numeric(sample_user_brands$fre)
  rownames(sample_user_brands)<-sample_user_brands$uid
  
  users<-unique(c(user_brands$uid,sample_user_brands))
  user_sample<-score=user_brands[users]/sample_user_brands[users]
  names(user_sample)<-users
  
  brands<-cypher(graph,paste("match (n:USERID {uid:'",userid,
                             "'})-[:`1`]->(m:BRANDID)<-[:`1`]-(ns:USERID)-[:`1`]->(ms:BRANDID) where m.bid<>ms.bid return ns.uid as uid ,ms as bid",sep = ""))
  bid_score<-ddply(brands,.(bid),function(df){
    #获取每个商品对应的uid
    uids<-unique(df$uid)
    #计算相似度
    data.frame(score=sum(user_sample[uids]))
  })
  #根据得分进行降序排序，可以根据设定的阈值进行筛选
  bid_score[order(bid_score$score,decreasing = TRUE),]
}