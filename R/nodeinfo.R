#' Get node labels and rules used in a party tree
#' 
#' Returns the covariates used to split a recursive
#'  partitioning tree and the rules that were applied 
#'  to build the tree 
#' 
#' 
#' @param x an object of class \code{party}
#' @return a vector with the node labels, 
#'  a data.frame with node rules, a ggplot
#' @author Kauê de Sousa
#' @examples 
#' \donttest{
#' library("PlackettLuce")
#' data("beans", package = "PlackettLuce")
#' G <- rank_tricot(data = beans,
#'                  items = c(1:3),
#'                  input = c(4:5),
#'                  group = TRUE,
#'                  additional.rank = beans[c(6:8)])
#'  
#' pld <- cbind(G, beans[,c("maxTN", "season", "lon")])
#'
#' tree <- pltree(G ~ maxTN + season + lon, data = pld)
#' 
#' node_labels(tree)
#' 
#' node_rules(tree)
#' 
#' top_items(tree)
#' 
#' plot(tree)
#' 
#' plot(tree, log = TRUE)
#' }
#' @importFrom stats model.frame
#' @importFrom partykit nodeids data_party node_party breaks_split partynode
#'  kids_node id_node split_node varid_split index_split right_split
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
    if (partykit::id_node(node) == i) 
      return(NULL)
    kid <- sapply(partykit::kids_node(node), partykit::id_node)
    whichkid <- max(which(kid <= i))
    split <- partykit::split_node(node)
    ivar <- partykit::varid_split(split)
    svar <- names(dat)[ivar]
    index <- partykit::index_split(split)
    if (is.factor(dat[, svar])) {
      if (is.null(index)) 
        index <- ((1:nlevels(dat[, svar])) > partykit::breaks_split(split)) + 
          1
      slevels <- levels(dat[, svar])[index == whichkid]
      srule <- paste(svar, " %in% c(\"", paste(slevels, 
                                               collapse = "\", \"", sep = ""), "\")", sep = "")
    }
    else {
      if (is.null(index)) 
        index <- 1:length(kid)
      breaks <- cbind(c(-Inf, partykit::breaks_split(split)), 
                      c(partykit::breaks_split(split), Inf))
      sbreak <- breaks[index == whichkid, ]
      right <- partykit::right_split(split)
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
  node <- recFun(partykit::node_party(x))
  paste(rule, collapse = " & ")
}


#' Plot party tree
#' @param log logical, if \code{TRUE} log-worth coefficients are 
#'  displayed instead of worth
#' @param ref optional, character for the reference item when
#'  \var{log} = \code{TRUE}
#' @param ci.level an integer for the confidence interval levels
#' @param ... additional arguments passed to methods
#' @importFrom stats update
#' @importFrom PlackettLuce freq
#' @importFrom partykit nodeids
#' @importFrom psychotools itempar
#' @importFrom qvcalc qvcalc
#' @importFrom stats coef
#' @importFrom ggplot2 ggplot aes geom_vline geom_point geom_errorbar scale_x_continuous 
#' theme_bw labs theme element_text element_blank element_rect element_line facet_grid
#' @importFrom ggparty ggparty geom_edge geom_edge_label geom_node_label
#' @import patchwork
#' @rdname node_labels
#' @method plot pltree
#' @export
plot.pltree <- function(x, 
                        log = TRUE, 
                        ref = NULL, 
                        ci.level = 0.95, ...){
  
  
  if (length(x) == 1) {
    return(NextMethod(x, ...))
  }
  
  dots <- list(...)
  
  font.size <- dots[["font.size"]]
  threshold <- dots[["threshold"]]
  terms     <- dots[["terms"]]
  adjust    <- dots[["adjust"]]
  nudge.x   <- dots[["nudge.x"]]
  nudge.y   <- dots[["nudge.y"]]
  letter.size <- dots[["letter.size"]]
  
  if(is.null(nudge.x)) nudge.x <- 0
  if(is.null(nudge.y)) nudge.y <- 0.35
  if(is.null(letter.size)) letter.size <- 14
  
  # Extract ids from terminal nodes
  node_id <- partykit::nodeids(x, terminal = TRUE)
  
  # get models from each node
  nodes <- list()
  for (i in seq_along(node_id)) {
    nodes[[i]] <- x[[ node_id[i] ]]$node$info$object
  }
  
  # get number of observations in each inner node
  nobs <- list()
  for (i in seq_along(node_id)) {
    nobs[[i]] <- as.integer(x[[ node_id[i] ]]$node$info$nobs) 
  }
  
  # get item names
  items <- dimnames(coef(x))[[2]]
  
  # get item parameters from model
  coeffs <- try(lapply(nodes, function(x) {
    z <- psychotools::itempar(x, vcov = TRUE, log = log, ref = ref)
    # get estimates from item parameters using qvcalc
    z <- qvcalc::qvcalc(z)$qvframe
  }), silent = TRUE)
  
  if (isTRUE("try-error" %in% class(coeffs))) {
    
    message("Unable to compute quasi-variance estimates with whole tree. \n",
            "Updating the model using rankings from each node \n")
    
    coeffs <- try(lapply(nodes, function(x) {
      psychotools::itempar(x, vcov = FALSE, log = log, ref = ref)
    }), silent = TRUE)
    
    # update the model, this will check if ranking is complete in each node 
    # and refit the rankings from each node to get the qvSE 
    qvSE <- try(lapply(nodes, function(x) {
      
      Z <- x$rankings
      
      Z <- Z[1:length(Z),, as.rankings = F]
      
      rmv <- which(colSums(Z) == 0)
      
      if (length(rmv) > 0) Z <- Z[, -rmv]
      
      Z <- stats::update(x, rankings = Z, 
                         weights = PlackettLuce::freq(Z), 
                         start = NULL)
      
      Z <- psychotools::itempar(Z, vcov = FALSE)
      
      # get estimates from item parameters using qvcalc
      Z <- qvcalc::qvcalc(Z)
      
      # extract data frames with estimates
      Z <- Z$qvframe
      
      Z$items <- rownames(Z)
      
      Z
      
    }), silent = TRUE)
    
    x <- list()
    for (i in seq_along(coeffs)) {
      
      xi <- data.frame(estimate = as.vector(coeffs[[i]]),
                       items = items)
      
      xi <- merge(xi, qvSE[[i]][,c("items", "quasiSE")], by = "items", all.x = TRUE)
      
      x[[i]] <- xi
      
    }
    
    coeffs <- x
    
  }
  
  # if the error persists then return an error 
  if (isTRUE("try-error" %in% class(coeffs))) {
    stop("Unable to compute worth estimates. Check for errors/warnings in ",
         "your modelparty object. \n You can try log = FALSE \n")
  }
  
  # Add limits in error bars and item names
  coeffs <- lapply(coeffs, function(X){
    X <- within(X, {
      bmax = X$estimate + stats::qnorm(1-(1-ci.level)/2) * X$quasiSE
      bmin = X$estimate - stats::qnorm(1-(1-ci.level)/2) * X$quasiSE
      items <- items
    })
    return(X)
  })
  
  # Add node information and number of observations
  for (i in seq_along(node_id)) {
    
    coeffs[[i]] <- within(coeffs[[i]], {
      
      nobs <- nobs[[i]]
      
      node <- node_id[i]}
    )
    
  }
  
  coeffs <- do.call("rbind", coeffs)
  
  if (isFALSE(log)) {
    coeffs$bmin <- ifelse(coeffs$bmin < 0, 0, coeffs$bmin)
    coeffs$bmax <- ifelse(coeffs$bmax > 1, 1, coeffs$bmax)
  }
  
  coeffs$id <- paste0(coeffs$node, "_", coeffs$items)
  
  groups <- ""
  
  coeffs$groups <- groups
  
  node_lev <- unique(paste0("Node ", coeffs$node, " (n=", coeffs$nobs, ")"))
  
  coeffs$id <- coeffs$node
  
  coeffs$node <- factor(paste0("Node ", coeffs$node, " (n=", coeffs$nobs, ")"),
                        levels = node_lev)
  
  coeffs$items <- factor(coeffs$items, levels = rev(sort(items)))
  
  splitvar <- 0L
  p.value <- 0L
  id <- 0L
  estimate <- 0L
  bmin <- 0L
  bmax <- 0L
  
  # get the tree structure
  if (length(node_id) > 1) {
    tree <- 
      ggparty::ggparty(x, terminal_space = 0) +
      ggparty::geom_edge() +
      ggparty::geom_edge_label() +
      ggplot2::theme(legend.position = "none") +
      ggparty::geom_node_label(line_list = list(
        ggplot2::aes(label = splitvar),
        ggplot2::aes(label = paste("p =",
                                   formatC(p.value,
                                           format = "e",
                                           digits = 1))),
        ggplot2::aes(label = ""),
        ggplot2::aes(label = id)),
        line_gpar = list(list(size = 12),
                         list(size = 10),
                         list(size = 10),
                         list(size = 10,
                              col = "black",
                              fontface = "bold",
                              alignment = "center")
        ),
        ids = "inner") +
      ggplot2::coord_cartesian(ylim = c(0.1, 1.1))
  }
  
  # Get max and min values for the x axis in the plot
  xmax <- round(max(coeffs$bmax, na.rm = TRUE) + 0.01, digits = 4)
  
  if (isFALSE(log)) {
    xmin <- 0
    xinter <- 1/length(items)
    xbreaks <- round(c(mean(c(0, xmax)), xmax), 2)
    xbreaks <- c(0, xbreaks)
  }
  
  if (isTRUE(log)) {
    xinter <- 0
    xmin <- min(coeffs$bmin, na.rm = TRUE)
    xbreaks <- round(c(mean(c(xmin, xmax)), xmax), 2)
    xbreaks <- c(xmin, xbreaks)
  }
  
  xlabs <- as.character(round(xbreaks, 2))
  
  # Check font size for axis X and Y, and plot title
  p <- 
    ggplot2::ggplot(coeffs, 
                    ggplot2::aes(x = estimate, 
                                 y = items)) +
    ggplot2::geom_vline(xintercept = xinter, 
                        colour = "#E5E7E9", size = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = groups),
                       position = ggplot2::position_nudge(y = nudge.y, x = nudge.x)) +
    ggplot2::geom_point(pch = 21, size = 2, 
                        fill = "black",colour = "black") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = bmin,
                                         xmax = bmax),
                            colour="black", height = 0.1) +
    ggplot2::scale_x_continuous(limits = c(xmin, xmax),
                                breaks = xbreaks,
                                labels = xlabs) +
    ggplot2::facet_grid(. ~ node) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, angle = 0,
                                                       hjust = 0.5, vjust = 1, 
                                                       face = "plain",
                                                       colour = "black"),
                   axis.text.y = ggplot2::element_text(size = 13, angle = 0,
                                                       hjust = 1, vjust = 0.5, 
                                                       face = "plain",
                                                       colour = "black"),
                   text = ggplot2::element_text(size = letter.size),
                   strip.background = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(colour = "black", size = 1),
                   axis.ticks = ggplot2::element_line(colour = "black", size = 0.5),
                   axis.ticks.length = grid::unit(0.3, "cm"))
  
  if(length(node_id) > 1){
    p <- (tree / p)
  }
  
  return(p)
  
}

