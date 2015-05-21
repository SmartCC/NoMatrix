#所需的程序包列表
pl<-c("RNeo4j","plyr")

#检查所需的包是否安装
if(length(setdiff(pl,.packages(all.available=T)))!=0){
  stop(paste("缺失包：",paste(setdiff(comment.df.names,names(df)),collapse = ",")))
}


library(RNeo4j)
library(plyr)

#创建neo4j连接，需要后边的username,否则会报302错误
#URL最后一定要加/，否则会找不到地址
graph<-startGraph("http://localhost:7474/db/data/",username="root")
clear(graph)

#读取数据
ali<-read.csv("t_alibaba_data.csv")
#将数据id转换为字符串
ali$user_id<-as.character(ali$user_id)
ali$brand_id<-as.character(ali$brand_id)

#用户id列表
users<-unique(ali$user_id)
#商品id列表
brands<-unique(ali$brand_id)

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
a_ply(ali,1,function(df) createRel(userNodes[[df$user_id]],df$type,brandNodes[[df$brand_id]]))
