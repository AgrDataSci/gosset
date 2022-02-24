# 
# 
# G <- rbca_ranks
# folds <- rbca_folds
# 
# # grouped rankings as matrix
# G <- G[1:length(G), ,as.grouped_rankings = FALSE]
# 
# # zeros into NAs
# G[G == 0] <- NA
# 
# n <- dim(G)[[1]]
# 
# # run over the rows and get the names of each item tested in a given fold
# tb <- NULL
# for(i in seq_len(n)){
#   tb <- rbind(tb, 
#               cbind(names(G[i, !is.na(G[i,])]), folds[[i]]))
# }
# 
# # table of tested items by folds
# # tb <- table(tb[,1], tb[,2])
# 
# # check if all items co-occur in all folds
# # which means that they should occur in both training and test sets 
# f <- unique(folds)
# 
# stability <- NULL
# for(i in seq_len(f)){
#   train <- tb[,2] != f[[i]]
#   test <-  tb[,2] == f[[i]]
#   
#   train <- unique(tb[train, 1])
#   test <- unique(tb[test, 1])
#  
#   data.frame(fold = f[[i]],
#              trained = length(train),
#              tested = length(test),
#              missing_in_training = paste(train[!train %in% test], collapse = ", "),
#              missing_in_test = paste(test[!test %in% train], collapse = ", "))
#   
# }




