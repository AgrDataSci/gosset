#' Plot worth parameters
#' 
#' Produces plots to highlight worth
#'  coefficients of items in a party tree of 
#'  a list of PlackettLuce models 
#' 
#' @param object an object of class \code{party} or 
#'  an object of class \code{PlackettLuce} or
#'  a list objects of class \code{PlackettLuce}
#' @param labels a vector with the name of models in \var{object}
#' @param ... additional arguments passed to methods
#' @examples 
#' library("psychotree")
#' library("PlackettLuce")
#' data("Topmodel2007", package = "psychotree")
#' R <- as.grouped_rankings(Topmodel2007$preference)
#' 
#' tm_tree <- pltree(R ~ ., data = Topmodel2007[, -1], 
#'                   minsize = 5,
#'                   npseudo = 0)
#' 
#' worth_map(tm_tree)
#' 
#' ##########################################
#' 
#' # Ranking of preference on four fruits 
#' # based on traits taste, texture, 
#' # price and storability
#' 
#' # taste
#' R1 <- matrix(c(1, 2, 3, 4,
#'                4, 1, 3, 2,
#'                4, 1, 2, 3,
#'                1, 2, 0, 3), nrow = 4, byrow = TRUE)
#' colnames(R1) <- c("apple", "banana", "orange", "pear")
#' mod1 <- PlackettLuce(R1)
#' 
#' # texture
#' R2 <- matrix(c(1, 4, 2, 3,
#'                1, 4, 3, 2,
#'                1, 4, 2, 3,
#'                1, 4, 2, 3), nrow = 4, byrow = TRUE)
#' colnames(R2) <- c("apple", "banana", "orange", "pear")
#' mod2 <- PlackettLuce(R2)
#' 
#' # price
#' R3 <- matrix(c(2, 4, 3, 1,
#'                4, 1, 2, 3,
#'                3, 4, 2, 1,
#'                4, 3, 1, 2), nrow = 4, byrow = TRUE)
#' colnames(R3) <- c("apple", "banana", "orange", "pear")
#' mod3 <- PlackettLuce(R3)
#' 
#' # storability
#' R4 <- matrix(c(1, 4, 3, 2,
#'                3, 4, 1, 2,
#'                1, 3, 2, 4,
#'                2, 3, 4, 1), nrow = 4, byrow = TRUE)
#' colnames(R4) <- c("apple", "banana", "orange", "pear")
#' mod4 <- PlackettLuce(R4)
#' 
#' # models in a list
#' mods <- list(mod1, mod2, mod3, mod4)
#' 
#' # name for each model
#' labels <- c("Taste", "Texture", "Price", "Storability")
#' 
#' worth_map(mods, labels)
#' 
#' # plot only one model as bar 
#' worth_bar(mod1)
#' 
#' @importFrom methods addNextMethod asMethodDefinition assignClassDef
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_distiller 
#'  theme_bw theme element_text element_blank labs 
#'  theme_minimal geom_bar scale_fill_manual
#' @importFrom partykit nodeids
#' @importFrom psychotools itempar
#' @rdname worth_map
#' @export
worth_map <- function(object, ...) {
  
  UseMethod("worth_map")
  
}

#' @rdname worth_map
#' @export
worth_map.default <- function(object, ...) {
  # Extract ids from terminal nodes
  node_id <- partykit::nodeids(object, terminal = TRUE)
  
  # get models from each node
  nodes <- list()
  for (i in seq_along(node_id)) {
    nodes[[i]] <- object[[ node_id[i] ]]$node$info$object
  }
  
  labs <- paste("Node", node_id)
  
  worth_map.list(nodes, labels = labs, ...)
  
}


#' @method worth_map list
#' @rdname worth_map
#' @export
worth_map.list <- function(object, labels, ...) {
  
  winprobs <- .combine_coeffs(object, log = TRUE, vcov = FALSE)
  
  # add name of features
  names(winprobs) <- labels
  
  winprobs <- data.frame(items = rep(dimnames(winprobs)[[1]], times = ncol(winprobs)),
                         labels = rep(dimnames(winprobs)[[2]], each = nrow(winprobs)),
                         winprob = as.numeric(unlist(winprobs)))
  
  winprobs$labels <- factor(winprobs$labels, levels = labels)
  
  items <- winprobs$items
  winprob <- winprobs$winprob
  
  lims <- max(abs(winprob)) * c(-1, 1)
  
  angle <- 0
  if (any(nchar(labels) > 30)) {
    angle <- 40  
  }
  
  p <- ggplot2::ggplot(winprobs, 
                       ggplot2::aes(x = items, 
                  y = labels,
                  fill = winprob,
                  label = as.character(round(winprob, 2)))) +
    ggplot2::geom_tile() + 
    ggplot2::scale_fill_distiller(palette = "RdBu", limit = lims,
                                  direction = 1,
                                  na.value = "white") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.background = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(color = "grey20"),
          strip.text.x = ggplot2::element_text(color = "grey20"),
          axis.text.x = ggplot2::element_text(angle = 40, vjust = 1, hjust=1),
          axis.text.y = ggplot2::element_text(angle = angle, vjust = 1, hjust=1),
          panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(x = "", 
                  y = "",
                  fill = "")
  
  return(p)
  
  
}

#'Combine coefficients from PlackettLuce models
#' @param x a list with PlackettLuce models
#' @param na.replace logical, to replace or keep NAs
#' @param ... further arguments passed to methods
#' @noRd
.combine_coeffs <- function(x, na.replace = TRUE, ...) {
  
  coeffs <- lapply(x, function(y) {psychotools::itempar(y, ...)})
  
  items <- unique(unlist(lapply(coeffs, names)))
  
  r <- data.frame(items)
  
  rownames(r) <- items
  
  r <- r[,-1]
  
  for(i in seq_along(coeffs)) {
    c_i <- coeffs[[i]]
    r[names(c_i),i] <- c_i
  }
  
  if (isTRUE(na.replace)) {
    r[is.na(r)] <- 0
  }
  
  return(r)
  
}

#' @rdname worth_map
#' @export
worth_bar <- function(object, ...){

  palette <- list(colors = c("#FFFF80", "#38E009","#1A93AB", "#0C1078"))
  
  palette <- do.call("colorRampPalette", palette)
  
  object <- coef(object, log = FALSE)
  
  object <- data.frame(group = names(object),
                       value = as.vector(object))
  
  nr <- dim(object)[[1]]
  
  object <- object[rev(order(object$value)), ] 
  
  # get order of players based on their performance
  player_levels <- rev(.player_order(object, "group", "value"))
  
  object$group <- factor(object$group, levels = player_levels)
  
  value <- object$value
  group <- as.character(object$group)
  
  maxv <- .round5(round(max(value) + .02, 2), 0.05)
  
  angle <- 0
  if (any(nchar(group) > 30)) {
    angle <- 40  
  }
  
  p <- ggplot2::ggplot(data = object, 
                       ggplot2::aes(x = value,
                                    y = group, 
                                    fill = group)) +
    ggplot2::geom_bar(stat = "identity", 
                      position = "dodge",
                      show.legend = FALSE,
                      width = 1, 
                      color = "#ffffff") + 
    ggplot2::scale_fill_manual(values = palette(nr)) + 
    ggplot2::scale_x_continuous(labels = paste0(seq(0, maxv, by = 0.05)),
                                breaks = seq(0, maxv, by = 0.05),
                                limits = c(0, maxv)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
                   legend.text = ggplot2::element_text(size = 9),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(color = "grey20"),
                   axis.text.y = ggplot2::element_text(angle = angle, vjust = 1,
                                                       hjust=1, color = "grey20")) +
    ggplot2::labs(y = "",
                  x = "") 
  
  return(p)
  
}



