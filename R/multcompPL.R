#' Post-hoc analysis of Plackett-Luce models
#' 
#' @param mod an object of class PlackettLuce
#' @param terms a vector with the models terms to be used
#' @param threshold a numeric value, second reference argument to compare
#' @param adjust p.value correction method, a character string
#' @param ... additional arguments passed to methods in qvcalc 
#' @param x an object of class 'multcompPL' 
#' @examples 
#' \donttest{
#' require("psychotree")
#' require("PlackettLuce")
#' require("multcompView")
#' 
#' data("Topmodel2007", package = "psychotree")
#' 
#' R <- as.grouped_rankings(Topmodel2007$preference)
#' 
#' plt <- pltree(R ~ ., data = Topmodel2007[, -1], minsize = 5, npseudo = 0)
#' 
#' multcompPL(plt, threshold = 0.05, ref = "Anni")
#' }
#' @importFrom partykit nodeids
#' @importFrom stats pt p.adjust reorder
#' @importFrom qvcalc qvcalc
#' @export
multcompPL <- function(mod, terms = NULL, threshold = 0.05, adjust = "none", ...) {
  UseMethod("multcompPL")
}

#' @rdname multcompPL
#' @export
multcompPL.default <- function(mod, terms = NULL, threshold = 0.05, adjust = "none", ...){
  
  #get estimates with quasi-SEs
  qv1 <- qvcalc::qvcalc(mod, ...)$qvframe
  
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
  
  qv1$group <- let$Letters
  
  qv1 <- qv1[, union("term", names(qv1))]
  
  row.names(qv1) <- seq_along(qv1$group)
  
  class(qv1) <- union("multcompPL", class(qv1))
  
  return(qv1)
  
}


#' @rdname multcompPL
#' @method multcompPL pltree
#' @export 
multcompPL.pltree <- function(mod, terms = NULL, threshold = 0.05, adjust = "none", ...){
  
  # Extract ids from terminal nodes
  node_id <- partykit::nodeids(mod, terminal = TRUE)
  
  # get node information
  nodes <- list()
  for(i in seq_along(node_id)) {
    nodes[[i]] <- mod[[node_id[i]]]$node$info$object
  }
  
  # get group information
  groups <- list()
  
  for(i in seq_along(node_id)){
    g <- multcompPL(mod = nodes[[i]], threshold = threshold, adjust = adjust,...)
    g$node <- node_id[[i]]
    groups[[i]] <- g
  }
  
  result <- do.call("rbind", groups)
  
  result <- result[, union(c("term", "node"), names(result))]
  
  row.names(result) <- seq_along(result$group)
  
  class(result) <- union("multcompPL", class(result))
  
  return(result)
  
}

#' @rdname multcompPL
#' @method plot multcompPL
#' @export
plot.multcompPL <- function(x, level = 0.95, ...){
  
  x$term <- .reduce(as.character(x$term), ...)
  items <- rev(sort(unique(x$term)))
  x$term <- factor(x$term, levels = items)
  
  estimate <- x$estimate
  term <- x$term
  group <- x$group
  quasiSE <- x$quasiSE
  
  p <- ggplot2::ggplot(data = x,
                       ggplot2::aes(x = estimate, 
                                    y = term,
                                    label = group, 
                                    xmax = estimate + stats::qnorm(1-(1-level)/2) * quasiSE,
                                    xmin = estimate - stats::qnorm(1-(1-level)/2) * quasiSE)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(width = 0.1) +
    ggplot2::geom_text(vjust = 1.2) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank()) +
    ggplot2::labs(x = "Estimate", y = "Item")
  
  node <- x$node
  
  if (isFALSE(is.null(node))) {
    p <- 
      p +
      ggplot2::facet_grid(. ~ node)
  }
  
  return(p)
  
}

