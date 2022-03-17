# library(PlackettLuce)
# library(qvcalc)
# library(partykit)
# 
# #Function parameters
# #object fitted PlackettLuce tree used for predictions
# #newdata list of datasets used to make predictions (years)
# #pred_best predicted best performing genotypes by each cell in the target raster
# #nsim number of simulations for resampling, default = 1000, suggested 1000000
# entropy <- function(object, newdata, pred_best, nsim = 1000){
#   
#   .k_top <- length(pred_best[[1]])
#   
#   #get rank probabilities of fitted model
#   mod_rank_probs <- get_rankProb(object, n = nsim)
#   
#   #get entropies of simulated rankings
#   mod_entr <- get_entropies(mod_rank_probs)
#   
#   #predict in which node of the plt falls each cell 
#   pred_nodes <- prim_nodes <- lapply(newdata,
#                                      function(X) predict(object, 
#                                                          newdata = X, 
#                                                          type = "node"))
#   pred_nodes_df <- as.data.frame(pred_nodes)
#   
#   colnames(pred_nodes_df) <- paste("y", seq_along(newdata), sep = "_")
#   
#   #get average entropy of years in newdata
#   avg_entr <- calc_cell_entr(pred_nodes_df, mod_entr)
#   
#   #get entropy by each predicted genotype
#   res <- entropy_gen(avg_entr, pred_best)
#   
#   return(res)
#   
# }
# 
# #function -- create rank probability matrices
# get_rankProb <- function(plt, n=1000) {
#   
#   # Make a list with model outputs
#   node_ids <- nodeids(plt, terminal = TRUE)
#   qv_list <- list()
#   for (i in seq_along(node_ids)) {
#     qv <- qvcalc(plt[[node_ids[i]]]$node$info$object, ref = 1)
#     qv_list[[i]] <- qv
#   }
#   
#   rankProb_list <- list(length=length(node_ids))
#   
#   for(j in 1:length(qv_list)){
#     
#     qv_list_j <- qv_list[[j]]$qvframe
#     
#     #simulate n times the worth values
#     samples <- matrix(NA, nrow=nrow(qv_list_j), ncol=n)
#     for(i in 1:nrow(qv_list_j)) samples[i,] <- rnorm(n, mean=(qv_list_j[i,"estimate"]), sd=(qv_list_j[i,"quasiSE"]))
#     
#     #convert simulated values to a matrix of ranks
#     ranks <- apply(samples, 2, function(x) rank(-x, ties.method = "random")) #negative so that 1 is best
#     
#     #rankFreq <- matrix(0, nrow(x_j), nrow(x_j)) triggers an error because does not found x_j, changed to qv_list_j
#     rankFreq <- matrix(0, nrow(qv_list_j), nrow(qv_list_j))
#     
#     for(i in 1:nrow(qv_list_j)) {
#       
#       FreqTable <- table(ranks[i,])
#       rankFreq[i,as.integer(names(FreqTable))] <- FreqTable 
#       
#     }
#     
#     # calculate probabilities from rank frequencies
#     rankProb <- rankFreq * 0
#     for(i in 1:nrow(rankFreq)) rankProb[i,] <- rankFreq[i,]/sum(rankFreq[i,])
#     rankProb <- t(rankProb) #the entropy function requires a different format
#     colnames(rankProb) <- rownames(qv_list_j)
#     
#     rankProb_list[[j]] <- rankProb
#     
#   }
#   
#   names(rankProb_list) <- as.character(node_ids)
#   return(rankProb_list)
#   
# }
# 
# ### calculate rankings entropy
# 
# # Wu, Y.-C., Shih, M.-C., & Tu, Y.-K. (2021). Using Normalized Entropy to Measure 
# # Uncertainty of Rankings for Network Meta-analyses. Medical Decision Making, 41(6), 
# # 706-713. doi:10.1177/0272989x21999023
# #This is the R function for calculating Normalized Entropy. 
# #The input for this function is ranking probabilities (a), 
# #and number of treatments (k). 
# #The rows of ranking probabilities matrix a are rankings (1st, 2nd, 3rd,...), 
# #and each column is one treatment included in the NMA.
# entropy_arm_function <- function(a, k) {
#   e <- -a * log(a, 2)
#   e[!is.finite(e)] <- 0
#   e_colsum <- colSums(e)
#   e_max <- k * (-1/k) * log2(1/k)
#   e_prop <- e_colsum/e_max
#   return(e_prop)
# }
# 
# get_entropies <- function(rankProb){
#   
#   entropies_list <- rankProb
#   
#   for(i in 1:length(rankProb)) entropies_list[[i]] <- entropy_arm_function(rankProb[[i]], 
#                                                                            ncol(rankProb[[i]]))
#   
#   return(entropies_list)
#   
# }
# 
# 
# #calculate average entropy for the years in newdata
# calc_cell_entr <- function(.nodes_df, .mod_entr){
#   
#   nodes <- unique(unlist(.nodes_df))
#   
#   freq_nodes_cell <- matrix(0, nrow = nrow(.nodes_df), ncol = length(nodes))
#   
#   colnames(freq_nodes_cell) <- nodes
#   
#   nyears <- ncol(.nodes_df)
#   
#   ngen <- length(.mod_entr[[1]])
#   
#   gen_names <- names(.mod_entr[[1]])
#   
#   avg_entr_yrs <- matrix(nrow = nrow(.nodes_df), ncol = ngen)
#   
#   colnames(avg_entr_yrs) <- gen_names
#   
#   for(i in 1:nrow(freq_nodes_cell)){
#     
#     entropy_nodes <- unlist(.mod_entr[unique(unlist(.nodes_df[i,]))]) * rep(table(t(.nodes_df[i, ])) / nyears, each = ngen)
#     
#     names(entropy_nodes) <- gsub(pattern = "[0-9]\\.", replacement = "", x = names(entropy_nodes))
#     
#     avg_entr_yrs[i, names(entropy_nodes)] <- tapply(X = entropy_nodes, INDEX = names(entropy_nodes), FUN = sum)
#     
#   }
#   
#   return(avg_entr_yrs)
#   
# }
# 
# #get entropy by predicted best or top_k genotype
# entropy_gen <- function(avg_entr_gen, pred_gen){
#   
#   entr_best_gen <- vector(mode = "numeric", length = nrow(avg_entr_gen))
#   
#   #check if best genotype or top-k
#   if(length(pred_gen[[1]]) > 1){
#     message("Computing entropy for top_k")
#     for(i in seq_along(entr_best_gen)){
#       entr_best_gen[i] <- mean(avg_entr_gen[i, pred_gen[[i]]])
#       
#     }
#   }
#   else{
#     message("Computing entropy for best genotype")
#     for(i in seq_along(entr_best_gen)){
#       entr_best_gen[i] <- avg_entr_gen[i, pred_gen[[i]]]
#       
#     }
#   }
#   
#   return(entr_best_gen) 
# }
# 
