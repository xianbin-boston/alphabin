# dropbox API
require(httr,quiet=T)
require(jsonlite, quiet=T)

token_db <- "WTNF3uTknUEAAAAAAAADey1096aewefoETLhhTfQdZjbZ4U2MluyIv4IpcBsvJmX"

dropbox_mkdir <- function(path) {
  x<-POST("https://api.dropboxapi.com/2/files/create_folder",
          encode = "json",
          add_headers(Authorization=paste0("Bearer ",token_db)),
          body=list(path=path)
          )
  return(as.data.frame(content(x)))
}

dropbox_upload <- function(path,file){
  b<-POST("https://content.dropboxapi.com/2/files/upload",
          encode="json",
          add_headers(Authorization=paste0("Bearer ",token_db),
                      "Dropbox-API-Arg"=toJSON(
                        list(path = path,mode = "add",autorename = TRUE,mute = FALSE),
                        auto_unbox = T)
                      ),
          content_type("application/octet-stream"),
          body = upload_file(file)
          )
  return(as.data.frame(content(b)))
}



