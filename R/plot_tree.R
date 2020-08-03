#' Plot recursive partitioning trees
#'
#' Plot quasi-variance estimates from a model-based 
#' recursive partitioning tree
#'
#' @param object an object of class modelparty
#' @param add.letters logical, if \code{TRUE} add post-hoc letters
#' @param ... additional arguments passed to methods
#' @return An object of class ggplot
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
#' @importFrom psychotools itempar
#' @importFrom qvcalc qvcalc
#' @importFrom ggplot2 ggplot aes geom_vline geom_point geom_errorbar scale_x_continuous 
#' theme_bw labs theme element_text element_blank element_rect element_line facet_grid
#' @noRd
plot_tree <- function(object, add.letters = FALSE, ...){
  
  dots <- list(...)
  
  font.size <- dots[["font.size"]]
  threshold <- dots[["threshold"]]
  terms     <- dots[["terms"]]
  adjust    <- dots[["adjust"]]
  ref       <- dots[["ref"]]

  # Extract ids from terminal nodes
  node_id <- partykit::nodeids(object, terminal = TRUE)
  
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
  
  coeffs$id <- paste0(coeffs$node, "_", coeffs$items)
  
  if (isTRUE(add.letters)) {
    
    # try to compute the estimates and get the letters
    # sometimes it doesn't work, if it happens the return 
    # a message about the issue
    if (is.null(threshold)) {
      threshold <- 0.05
    }
    if (is.null(ref)) {
      ref <- 1
    }
    if (is.null(adjust)) {
      adjust <- "none"
    }
    
    groups <- tryCatch(
      {
        multcompPL(object, 
                   threshold = threshold,
                   terms = terms,
                   adjust = adjust,
                   ref = ref)
        
       }, error = function(cond){
        message("Unable to get letters for the plotting object.",
                " The issue has likely occurred in qvcalc::qvcalc() \n")
        return(NA)
      }
    )
    
    if (isTRUE(is.na(groups))) {
      coeffs <- cbind(coeffs, groups = "")
    }else{
      groups$id <- paste0(groups$node, "_", groups$term)
      coeffs <- merge(coeffs, groups[,c("id","group")], by = "id", all.x = TRUE)
      names(coeffs)[names(coeffs)=="group"] <- "groups"
    }
  }
  
  if (isFALSE(add.letters)) {
    
    coeffs$groups <- ""
    
  }
  
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
              nudge_y = 0.35) +
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

# library("PlackettLuce")
# R <- matrix(c(1, 2, 0, 0,
#               4, 1, 2, 3,
#               2, 1, 1, 1,
#               1, 2, 3, 0,
#               2, 1, 1, 0,
#               1, 0, 3, 2), nrow = 6, byrow = TRUE)
# colnames(R) <- c("apple", "banana", "orange", "pear")
# 
# # create rankings object
# R <- as.rankings(R)
# 
# # Standard maximum likelihood estimates
# mod <- PlackettLuce(R, npseudo = 0)


