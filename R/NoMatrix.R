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
  
  #创建用户图节点
  userNodes<-llply(users,function(x) createNode(graph,"USERID",id=x))
  #将返回结果给数据命名索引
  names(userNodes)<-users
  #添加限制函数，使插入的数据唯一
  addConstraint(graph, "USERID", "id")
  #创建商品图节点
  brandNodes<-llply(brands,function(x) createNode(graph,"BRANDID",id=x))
  names(brandNodes)<-brands
  addConstraint(graph,"BRANDID","id")
  
  #创建联系
  a_ply(gdata,1,function(df) createRel(userNodes[[df$user_id]],df$type,brandNodes[[df$brand_id]]))
  
}

#获取推荐
getRecommends<-function(userid){
  brands<-cypher(graph,"match n-[]->ms<-[]-ns<-[]-ms2-[]->ns2 where n.id=userid return ms2.id as brandId,count(ns) as fre",userid=userid)
  #可以添加去重
  #给结果排序
  brands[order(-brands$fre),]
}