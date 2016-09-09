#' Recommendation Engine Function
#'
#' This function allows you to build item based recommendation engine for customers
#' @param data provide the data frame with customer id and historical product purchase
#' @keywords recommendation
#' @export
#' @examples
#' recommendation_engine()


recommendation_engine <- function(data,top){
  #create value column that will be used during the melt and dcast function
  data$val <- 1
  colnames(data)[1:2] <- c("CUSTOMER_ID","PRODUCT")
  mydata.melt <- reshape2::melt(data = data,id.vars = c("CUSTOMER_ID","PRODUCT","val"))
  
  print("================= reshaping the data for processing ================")
  mydata.dcast <- reshape2::dcast(data=mydata.melt,CUSTOMER_ID~PRODUCT,value.var = "val",sum)
  mydata.dcast$CUSTOMER_ID <- as.character(mydata.dcast$CUSTOMER_ID)
  mydata.dcast[is.na(mydata.dcast)] <- 0
  
  #create function for cosine similarity 
  similarity.func <- function(x,y) 
  {
    sim <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
    return(sim)
  }
  
  ######################### GENERATE SIMILARITY MATRIX ###################
  similarity.matrix  <- matrix(NA, 
                               nrow=ncol(mydata.dcast[,-1]),
                               ncol=ncol(mydata.dcast[,-1]),
                               dimnames=list(colnames(mydata.dcast[,-1]),
                                             colnames(mydata.dcast[,-1])))
  
  mat <- mydata.dcast[,-1]
  cust <- mydata.dcast[,1]
  rm(mydata.dcast)
  
  print("================= create product affinity matrix using cosine similarity =======================")
  # this is the most disgusting part of the code. working on some efficient ways to avoid such o(n2) complexities
  pb <- txtProgressBar(min = 0, max = ncol(mat), style = 3)
  for(i in 1:ncol(mat)) {
    for(j in 1:ncol(mat)) {
      similarity.matrix[i,j] <- similarity.func(mat[,i],mat[,j])
    }
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  mat <- as.matrix(mat)
  # class(mat_comp) <- "numeric"
  
  # this part is to generate recommendation score
  reco_score <- mat %*% as.matrix(similarity.matrix)
  reco_score_final <- reco_score*(1-mat)
  reco_score_final_v2 <- cbind(CUST = cust, as.data.frame(reco_score_final))
  
  
  reco_score_final_v3 <- reshape2::melt(reco_score_final_v2, id.vars=c("CUST"))
  
  reco_score_final_v3 <- data.table::as.data.table(reco_score_final_v3)
  
  #sort by customer id and group class-- here variable refers to group class
  data.table::setkey(reco_score_final_v3,CUST,variable)
  #head(reco_score_final_v3)
  
  data.table::setkey(reco_score_final_v3,CUST,value)
  
  print(paste0("================= selecting top",top," products for each customers ================="))
  print("================== this might take a few minutes depending on the customer size======================")
  final_reco_v1 <- reco_score_final_v3[,tail(.SD,top),by=.(CUST)]
  final_reco_v1 <- as.data.frame(final_reco_v1)
  final_reco_v1
}
