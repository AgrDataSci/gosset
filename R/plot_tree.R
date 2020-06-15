#' Plot recursive partitioning trees
#'
#' Make a ggplot2 chart from model-based recursive partitioning
#' trees with quasi-variance
#'
#' @param object an object of class modelparty
#' @param add.letters optional
#' @param ... additional arguments passed to ggplot2
#' @return an object of class ggplot
#' @seealso \code{\link[qvcalc]{qvcalc}} \code{\link[ggplot2]{ggplot}}
#' @examples
#' 
#' library("psychotree")
#' library("ggplot2")
#' library("ggparty")
#' library("patchwork")
#' 
#' 
#' ## Germany's Next Topmodel 2007 data
#' data("Topmodel2007", package = "psychotree")
#' 
#' ## BT tree
#' tm_tree <- bttree(preference ~ ., data = Topmodel2007, minsize = 5, alpha = 0.1)
#' 
#' gosset:::plot_tree(tm_tree)
#' 
#' library("PlackettLuce")
#' library("multcompView")
#' 
#' R <- as.grouped_rankings(Topmodel2007$preference)
#' 
#' # Topmodel2007[, -1] gives covariate values for each judge
#' print(Topmodel2007[1:2, -1])
#' 
#' # fit partition model based on all variables except preference
#' # set npseudo = 0 as all judges rank all models
#' tm_tree <- pltree(R ~ ., data = Topmodel2007[, -1], minsize = 5,
#'                   npseudo = 0)
#' 
#' gosset:::plot_tree(tm_tree, add.letters = TRUE)
#' 
#' @importFrom partykit nodeids
#' @importFrom stats pt p.adjust reorder
#' @importFrom psychotools itempar
#' @importFrom qvcalc qvcalc
#' @importFrom ggplot2 ggplot aes geom_vline geom_point geom_errorbar scale_x_continuous 
#' theme_bw labs theme element_text element_blank element_rect element_line facet_grid
#' @noRd
plot_tree <- function(object, add.letters = FALSE, ...){
  
  # Extract ids from terminal nodes
  node_id <- partykit::nodeids(object, terminal = TRUE)
  
  dots <- list(...)
  
  font.size <- dots[["font.size"]]
  
  # get node information
  nodes <- list()
  for (i in seq_along(node_id)) {
    nodes[[i]] <- object[[ node_id[i] ]]$node$info$object
  }
  
  # get number of observers in each node
  nobs <- list()
  for (i in seq_along(node_id)) {
    nobs[[i]] <- as.integer(object[[ node_id[i] ]]$node$info$nobs) 
  }
  
  # get item parameters from model
  coeffs <- lapply(nodes, psychotools::itempar)
  
  # get estimates from item parameters using qvcalc
  coeffs <- lapply(coeffs, qvcalc::qvcalc)
  
  # extract data frames with estimates
  coeffs <- lapply(coeffs, function(X){
    df <- X[]$qvframe }
  )
  
  # get item names
  items <- rownames(coeffs[[1]])
  
  # Add limits in error bars and item names
  coeffs <- lapply(coeffs, function(X){
    X <- within(X, {
      bmin <- X$estimate-(X$quasiSE)
      bmax <- X$estimate+(X$quasiSE)
      items <- items
    })
    
    X$bmax <- ifelse(X$bmax > 1, 0.991, X$bmax)
    
    X$bmin <- ifelse(X$bmin < 0, 0.001, X$bmin)
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
  
  if (isTRUE(add.letters)){
    groups <- try(lapply(nodes, function(x){
      x <- .multcompPL(x)
      x[sort(items), ".group"]
    }), silent = TRUE)
    groups <- unlist(groups)
    if (grepl("Error",groups[[1]])) {
      message("Unable to get letters for the plotting object.",
              " The issue has likely occurred in qvcalc::qvcalc() \n")
      groups <- ""
    }
    coeffs <- cbind(coeffs, groups = groups)
  }else{
    coeffs$groups <- ""
  }
  
  node_lev <- unique(paste0("Node ", coeffs$node, " (n=", coeffs$nobs, ")"))
  
  coeffs$id <- coeffs$node
  
  coeffs$node <- factor(paste0("Node ", coeffs$node, " (n=", coeffs$nobs, ")"),
                        levels = node_lev)
  
  coeffs$items <- factor(coeffs$items, levels = sort(items))
  
  splitvar <- 0L
  p.value <- 0L
  id <- 0L
  estimate <- 0L
  bmin <- 0L
  bmax <- 0L
  
  # get the tree structure
  if (length(node_id) > 1) {
    tree <- 
      ggparty::ggparty(object, terminal_space = 0) +
      ggparty::geom_edge() +
      ggparty::geom_edge_label() +
      ggplot2::theme(legend.position = "none") +
      ggparty::geom_node_label(line_list = list(
        aes(label = splitvar),
        aes(label = paste("p =",
                          formatC(p.value,
                                  format = "e",
                                  digits = 1))),
        aes(label = ""),
        aes(label = id)),
        line_gpar = list(list(size = 12),
                         list(size = 8),
                         list(size = 8),
                         list(size = 8,
                              col = "black",
                              fontface = "bold",
                              alignment = "center")
        ),
        ids = "inner") +
      ggplot2::coord_cartesian(ylim = c(0.1, 1.1))
  }
  
  # Get max and min values for the x axis in the plot
  xmax <- round(max(coeffs$bmax, na.rm = TRUE) + 0.01, digits = 4)
  xmin <- round(min(coeffs$bmin, na.rm = TRUE) - 0.01, digits = 4)
  xbreaks <- round(c(mean(c(0, xmax)), xmax), 2)
  xbreaks <- c(0, xbreaks)
  xlabs <- as.character(xbreaks)

  # Check font size for axis X and Y, and plot title
  s.axis <- 11
  
  p <- 
    ggplot2::ggplot(coeffs, 
                    ggplot2::aes(x = estimate, 
                                 y = items)) +
    ggplot2::geom_vline(xintercept = 1/length(items), 
                        colour = "#E5E7E9", size = 0.8) +
    geom_text(aes(label = groups),
              size = 2.5,
              nudge_y = 0.25) +
    ggplot2::geom_point(pch = 21, size = 2, 
                        fill = "black",colour = "black") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = bmin,
                                         xmax = bmax),
                            colour="black", height = 0.1) +
    ggplot2::scale_x_continuous(limits = c(0, xmax),
                                breaks = xbreaks,
                                labels = xlabs) +
    ggplot2::facet_grid(. ~ node) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = s.axis, angle = 0,
                                                       hjust = 0.5, vjust = 1, 
                                                       face = "plain",
                                                       colour = "black"),
                   axis.text.y = ggplot2::element_text(size = s.axis, angle = 0,
                                                       hjust = 1, vjust = 0.5, 
                                                       face = "plain",
                                                       colour = "black"),
                   text = element_text(size = 10),
                   strip.background = element_blank(),
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

library("PlackettLuce")
R <- matrix(c(1, 2, 0, 0,
              4, 1, 2, 3,
              2, 1, 1, 1,
              1, 2, 3, 0,
              2, 1, 1, 0,
              1, 0, 3, 2), nrow = 6, byrow = TRUE)
colnames(R) <- c("apple", "banana", "orange", "pear")

# create rankings object
R <- as.rankings(R)

# Standard maximum likelihood estimates
mod <- PlackettLuce(R, npseudo = 0)


.multcompPL<-function(mod, terms = NULL, threshold = 0.05, adjust = "none"){
  
  #get estimates with quasi-SEs
  qv1 <- qvcalc::qvcalc(mod)$qvframe
  
  #reduce frame to only selected terms if not all comparisons are desired
  if (!is.null(terms)) {
    qv1 <- subset(qv1, rownames(qv1) %in% terms)
    # give error if less than 2 terms can be identified
    if (nrow(qv1) < 3) {
      stop("Less than 2 terms selected")
    }
  }
  
  #set up matrices for all differences and pooled errors
  diffs <- mat.or.vec(nrow(qv1),nrow(qv1))
  ses <- mat.or.vec(nrow(qv1),nrow(qv1))
  
  for(i in 1:nrow(qv1)){
    for(j in 1:nrow(qv1)){
      #get differences and pooled ses
      diffs[i,j] <- qv1$estimate[i] - qv1$estimate[j]
      ses[i,j] <- sqrt(qv1$quasiVar[i] + qv1$quasiVar[j])
    }
  }
  
  #calculate z scores
  z <- diffs/ses
  #TO DO: What DF to use to use here? Is it just the resid DF?
  p <- 2 * (1 - stats::pt(abs(z), mod$df.residual))
  
  #adjust p-value if you want to adjust. make sure to only take each p once for adjustment
  p[upper.tri(p)] <- stats::p.adjust(p[upper.tri(p)], method = adjust)
  
  #make sure lower triangular is mirror of upper
  p[lower.tri(p)] <- t(p)[lower.tri(p)]
  
  #set rownames
  rownames(p) <- colnames(p) <- rownames(qv1)
  
  #re-order qv output to ensure letters are produced in a sensible order
  qv1$term <- stats::reorder(factor(rownames(qv1)), qv1$estimate, mean)
  qv1 <- qv1[order(qv1$estimate, decreasing = TRUE),]
  
  #get mean seperation letter groupings
  args <- list(formula = estimate ~ term, 
               x = p, 
               data = qv1,
               compare = "<",
               threshold =  threshold,
               Letters = letters,
               reversed = FALSE)
  
  let <- do.call("multcompLetters2", args)
  
  qv1$.group <- let$Letters
  
  return(qv1)
  
}
