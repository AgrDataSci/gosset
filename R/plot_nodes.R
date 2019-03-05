#' Plot nodes from recursive partitioning trees
#'
#' Make a ggplot2 chart from model-based recursive partitioning trees with quasi-variance
#'
#' @param object an object of class modelparty
#' @param labels optional, new labels for the axis X in the plot
#' by default
#' @param ... additional arguments passed to ggplot2
#' @return a list of plots with probabilities of winning and 
#' intervals based on quasi-standard errors
#' @seealso \code{\link[qvcalc]{qvcalc}}
#' @examples
#' library("PlackettLuce")
#' library("qvcalc")
#' library("ggplot2")
#' 
#' data("breadwheat", package = "gosset")
#' 
#' # convert the tricot rankings from breadwheat data
#' # into a object of class 'rankings'
#' names(breadwheat)
#' 
#' G <- to_rankings(breadwheat,
#'                  items = c(1:3),
#'                  rankings = c(7:8),
#'                  type = "tricot",
#'                  grouped.rankings = TRUE)
#' 
#' # combine grouped rankings with lon lat from wheat dataset
#' mydata <- cbind(G, breadwheat[c("lon","lat")])
#' 
#' # fit a pltree model using lon and lat
#' mod <- pltree(G ~ ., data = mydata)
#' 
#' # plot nodes
#' plot_nodes(mod)
#' @importFrom psychotools itempar
#' @importFrom partykit nodeids
#' @import qvcalc
#' @export

plot_nodes <- function(object, labels = NULL, ...){

  # Extract ids from terminal nodes
  node_id <- partykit::nodeids(object, terminal = TRUE)

  dots <- list(...)
  
  if ("font.size" %in% names(dots)) {
    font.size <- dots[["font.size"]]
  } else {
    font.size <- NULL
  }
  
  
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

  # extract dataframes with estimates
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

  # Get max and min values for the x axis in the plot
  xmax <- round(max(do.call(rbind, coeffs)$bmax, na.rm = TRUE) + 0.01, digits = 4)
  xmin <- round(min(do.call(rbind, coeffs)$bmin, na.rm = TRUE), digits = 4)

  # Check font size for axis X and Y, and plot title
  if (is.null(font.size)) {
    s.title <- 13
    s.axis <- 15
  } else{
    s.title <- font.size[1]
    s.axis <- font.size[2]
  }

  # Check labels for axis Y
  if (is.null(labels)) {
    labels <- coeffs[[1]]$items
  }

  # Check dimensions of labels
  if (length(labels) != length(items)) {
    stop("wrong dimensions in labels \n")
  }


  # Plot winning probabilities
  plots <- lapply(coeffs, function(X){

    p <- ggplot2::ggplot(X, aes(x = X$estimate, y = labels)) +
      ggplot2::geom_vline(xintercept = 1/length(X$items), 
                 colour = "#E5E7E9", size = 0.8) +
      ggplot2::geom_point(pch = 21, size = 2, 
                 fill = "black",colour = "black") +
      ggplot2::geom_errorbarh(aes(xmin = X$bmin,
                         xmax = X$bmax),
                     colour="black", height = 0.2) +
      ggplot2::scale_x_continuous(limits = c(0, xmax)) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = NULL, y = NULL ,
           title = paste0("Node ", X$node[1], " (n= ", X$nobs[1], ")")) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = s.title),
            axis.text.x = ggplot2::element_text(size = s.axis, angle = 0,
                                       hjust = 0.5, vjust = 1, face = "plain",
                                       colour = "black"),
            axis.text.y = ggplot2::element_text(size = s.axis, angle = 0,
                                       hjust = 1, vjust = 0.5, face = "plain",
                                       colour = "black"),
            plot.background = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(colour = "black", size = 1),
            axis.ticks = ggplot2::element_line(colour = "black", size = 0.5),
            axis.ticks.length=unit(0.3, "cm"))

    p

  })

  names(plots) <- paste0("node",node_id)

  return(plots)
}