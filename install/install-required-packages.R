# run this from R / RStudio to install required packages
# Install routine from https://stackoverflow.com/a/44660688

using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("devtools", "shinydashboard","shinyjs","shinyWidgets","DT","scales","RJSONIO","sodium")
devtools::install_github("mul118/shinyMCE")