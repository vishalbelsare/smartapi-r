library(rjson)
library(base64)
setClass("webSocket",
         representation(
           url = "character",
           feedToken = "character",
           clientCode ="character",
         )
         )
root_url<- "wss://omnefeeds.angelbroking.com/NestHtml5Mobile/socket/stream"

connect_object<-function(params){
  object = methods::new("webSocket")
  tryCatch({
    object@url=ifelse(is.null(params[['url']]),root_url,params[['url']])
    object@feedToken=ifelse(is.null(params[["feedToken"]]),message("feedtoken cannot be blank"),
                            params[["feedToken"]])
    object@clientCode=ifelse(is.null(params[["clientCode"]]),message("clientCode cannot be blank"),
                            params[["clientCode"]])
    }, error=function(e){
    message("in error function",e$message)
  })
  return(object)
}

webSocket.connect<-(function(object,script,task){
  ws <- websocket::WebSocket$new(object@url,autoConnect = FALSE)
  ws$connect()
  ws$onOpen(function(event){
    is_open<-TRUE
    message("connection is opened")
    ws$send(toJSON(list("task"="cn","channel"="","token"=object@feedToken,"user"=object@clientCode,"acctid"=object@clientCode)))
    ws$send(toJSON(list("task"=task,"channel"=script,"token"=object@feedToken,"user"=object@clientCode,"acctid"=object@clientCode)))
    send_ticks()
  })
  send_ticks<-function(){
   later::later(send_ticks,10)
    message("heart beat")
    ws$send(toJSON(list("task"="hb","channel"='',"token"=object@feedToken,"user"=object@clientCode,"acctid"=object@clientCode)))
  }
  ws$onMessage(function(event) {
    msg_data<-memDecompress(base64enc::base64decode(event$data), "gzip", asChar=TRUE)

    cat("Client received message:",msg_data, "\n")

  })
  ws$onError(function(){
    cat("Error in the connection")
  })
  ws$onClose(function(){
    is_open<-FALSE
    cat("Closing the connection")

  })
})






