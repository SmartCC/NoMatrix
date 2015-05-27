#从配置中获取对应的value，没有的抛异常
getConfProperty<-function(key,conf){
  if(is.na(conf[key])) out.error(paste("\n错误，配置文件没有key值", key,"\n",sep = ""))
  conf[key]
}


#处理配置文件
getConfigure<-function(configure_file){
  conf<-xmlParse(configure_file)
  keys<-unlist(sapply(getNodeSet(conf,"//name"),xmlValue))
  values<-unlist(sapply(getNodeSet(conf,"//value"),xmlValue))
  ifelse(length(keys)!=length(values),
         out.error(paste("配置文件key与value的值不对应，请检查配置文件",configure_file)),
         names(values)<-keys)
  values
}

out.error<-function(e){
  #此处可采用相应的处理，如计入日志等
  #使用stop函数终止程序运行
  stop(e)
}