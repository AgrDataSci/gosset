#' Get node labels and rules used in a party tree
#' 
#' @param x an object of class \code{party}
#' @return a vector with the node labels, a data.frame with node rules
#' @author Kauê de Sousa
#' @examples 
#' if (require("PlackettLuce")) {
#' example("beans", package = "PlackettLuce")
#' G <- group(R, rep(seq_len(nrow(beans)), 4))
#' 
#' tree <- pltree(G ~ maxTN + season + lon, data = beans)
#' 
#' node_labels(tree)
#' 
#' node_rules(tree)
#' 
#' top_items(tree)
#' 
#' }
#' @importFrom stats model.frame
#' @importFrom partykit nodeids data_party node_party breaks_split partynode
#' @export
node_labels <- function(x) {
  
  rules <- .list.rules.party(x)
  rules <- paste(rules, collapse = "  ")
  
  var <- names(model.frame(x))
  
  labels <- sapply(var, function(x) {
    grepl(x, rules)
  })
  
  names(labels[labels == TRUE])
  
}

#' Get node rules 
#' @rdname node_labels
#' @export
node_rules <- function(x){
  
  node_ids <- partykit::nodeids(x, terminal = TRUE)
  
  result <- data.frame()
  
  for (i in seq_along(node_ids)) {
    r <- data.frame(node = node_ids[i],
                    rules = .list.rules.party(x, node_ids[i]))
    
    result <- rbind(result, r)
    
  }
  
  rule <- result$rules
  rule <- gsub("%in%","@", rule)
  rule <- gsub("[(]|[)]| c","", rule)
  rule <- gsub("  "," ", rule)
  rule <- gsub('"NA",',"", rule)
  rule <- gsub(', "NA"',"", rule)
  rule <- gsub(",", "COMMA", rule)
  rule <- gsub("[.]", "DOT", rule)
  rule <- gsub("@", "EQUAL", rule)
  rule <- gsub("&", " AND ", rule)
  rule <- gsub("<=", " LOWEQUAL ", rule)
  rule <- gsub("=>", " HIGHEQUAL ", rule)
  rule <- gsub("<", " LOW ", rule)
  rule <- gsub(">", " HIGH ", rule)
  
  # remove all other special characters
  rule <- gsub("[[:punct:]]", "", rule)
  
  # reposition the key special characters
  rule <- gsub("  ", " ", rule)
  rule <- gsub("LOWEQUAL", "<=", rule)
  rule <- gsub("HIGHEQUAL", "=>", rule)
  rule <- gsub("LOW", "<", rule)
  rule <- gsub("HIGH", ">", rule)
  rule <- gsub("EQUAL", "= ", rule)
  rule <- gsub("AND", "&", rule)
  rule <- gsub("COMMA", ",", rule)
  rule <- gsub("DOT", ".", rule)
  rule <- gsub(", NA", "", rule)
  rule <- gsub("NA,", "", rule)
  
  result$rules <- rule
  
  return(result)
  
}

#' Get the top items out of a decision tree
#' @param top an integer for the number of items to return
#' @rdname node_labels
#' @export 
top_items <- function(x, top = 5) {
  
  if (length(x) > 1) {
    
    coef_x <- coef(x, log = FALSE)
    
    bestitems <- apply(coef_x, 1 , function(y) {
      names(rev(sort(y)))[1:top]
    })
    
    bestitems <- as.data.frame(bestitems)
    
    names(bestitems) <- paste0("Node", 
                               nodeids(x, terminal = TRUE))
    
    return(bestitems)
    
  }
  
  if (length(x) == 1){
    
    coef_x <- coef(x, log = FALSE)
    
    bestitems <- names(rev(sort(coef_x)))[1:top]
    
    return(bestitems)
    
  }
  
}

#' Imported from partykit
#' @param i node ids
#' @noRd
.list.rules.party <- function (x, i = NULL, ...) 
{
  if (is.null(i)) 
    i <- partykit::nodeids(x, terminal = TRUE)
  if (length(i) > 1) {
    ret <- sapply(i, .list.rules.party, x = x)
    names(ret) <- if (is.character(i)) 
      i
    else names(x)[i]
    return(ret)
  }
  if (is.character(i) && !is.null(names(x))) 
    i <- which(names(x) %in% i)
  stopifnot(length(i) == 1 & is.numeric(i))
  stopifnot(i <= length(x) & i >= 1)
  i <- as.integer(i)
  dat <- partykit::data_party(x, i)
  if (!is.null(x$fitted)) {
    findx <- which("(fitted)" == names(dat))[1]
    fit <- dat[, findx:ncol(dat), drop = FALSE]
    dat <- dat[, -(findx:ncol(dat)), drop = FALSE]
    if (ncol(dat) == 0) 
      dat <- x$data
  }
  else {
    fit <- NULL
    dat <- x$data
  }
  rule <- c()
  recFun <- function(node) {
    if (id_node(node) == i) 
      return(NULL)
    kid <- sapply(kids_node(node), id_node)
    whichkid <- max(which(kid <= i))
    split <- split_node(node)
    ivar <- varid_split(split)
    svar <- names(dat)[ivar]
    index <- index_split(split)
    if (is.factor(dat[, svar])) {
      if (is.null(index)) 
        index <- ((1:nlevels(dat[, svar])) > breaks_split(split)) + 
          1
      slevels <- levels(dat[, svar])[index == whichkid]
      srule <- paste(svar, " %in% c(\"", paste(slevels, 
                                               collapse = "\", \"", sep = ""), "\")", sep = "")
    }
    else {
      if (is.null(index)) 
        index <- 1:length(kid)
      breaks <- cbind(c(-Inf, breaks_split(split)), c(breaks_split(split), 
                                                      Inf))
      sbreak <- breaks[index == whichkid, ]
      right <- right_split(split)
      srule <- c()
      if (is.finite(sbreak[1])) 
        srule <- c(srule, paste(svar, ifelse(right, ">", 
                                             ">="), sbreak[1]))
      if (is.finite(sbreak[2])) 
        srule <- c(srule, paste(svar, ifelse(right, "<=", 
                                             "<"), sbreak[2]))
      srule <- paste(srule, collapse = " & ")
    }
    rule <<- c(rule, srule)
    return(recFun(node[[whichkid]]))
  }
  node <- recFun(node_party(x))
  paste(rule, collapse = " & ")
}
