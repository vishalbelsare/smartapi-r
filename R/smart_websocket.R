library(rjson)
library(base64)
setClass("smartwebSocket",
         representation(

           jwtToken = "character",
           clientCode ="character",
           apiKey="character",
           url = "character",
           feedType="character"

         )
)
r_url<- "wss://smartapisocket.angelbroking.com/websocket"

smart_connect_object<-function(params){
  object = methods::new("smartwebSocket")
  tryCatch({

    object@jwtToken=ifelse(is.null(params[["jwtToken"]]),message("jwtToken cannot be blank"),
                            params[["jwtToken"]])
    object@clientCode=ifelse(is.null(params[["clientCode"]]),message("clientCode cannot be blank"),
                             params[["clientCode"]])
    object@apiKey=ifelse(is.null(params[['apiKey']]),'',params[["apiKey"]])

    object@url=ifelse(is.null(params[['url']]),r_url,params[['url']])

    object@feedType=ifelse(is.null(params[["feedType"]]),message("feedType cannot be blank"),
                             params[["feedType"]])


  }, error=function(e){
    message("in error function",e$message)
  })
  return(object)
}

smartwebSocket.connect<-(function(object){
  message("___________________________",paste(object@url,"?jwttoken=",object@jwtToken,"&&clientcode=",object@clientCode,"&&apikey=",object@apiKey,sep = ""))
  ws <- websocket::WebSocket$new(paste(object@url,"?jwttoken=",object@jwtToken,"&&clientcode=",object@clientCode,"&&apikey=",object@apiKey,sep = ""),autoConnect = FALSE)

  ws$connect()
  is_open=NULL
  ws$onOpen(function(event){
    message(event)
    is_open<-TRUE
    message("connection is opened",ws)
    #ws$send(toJSON(list("task"="mw","channel"=object@script,"token"=object@feedToken,"user"=object@clientCode,"acctid"=object@clientCode)))
    send_ticks()
  })

  send_ticks<-function(){
    later::later(send_ticks,10)
    ws$send(toJSON(list("actiontype":"heartbeat","feedtype":object@feedType,"jwttoken":object@jwtToken,"clientcode":object@clientCode,"apikey":object@apiKey)))
  }

  #ws$send(toJSON(list("actiontype":object@actionType,"feedtype":object@feedType,"jwttoken":object@jwtToken,"clientcode":object@clientCode,"apikey":object@apiKey)))

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

fetch_data<-function(object,actiontype,feedtype){
  ws$send(toJSON(list("actiontype":actiontype,"feedtype":feedtype,"jwttoken":object@jwtToken,"clientcode":object@clientCode,"apikey":object@apiKey)))
}





