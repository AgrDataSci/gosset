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
#' G = rank_tricot(data = beans,
#'                  items = c(1:3),
#'                  input = c(4:5),
#'                  group = TRUE,
#'                  additional.rank = beans[c(6:8)])
#'  
#' pld = cbind(G, beans[,c("maxTN", "season", "lon")])
#'
#' tree = pltree(G ~ maxTN + season + lon, data = pld)
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
#' @importFrom stats model.frame reorder
#' @importFrom partykit nodeids data_party node_party breaks_split partynode
#'  kids_node id_node split_node varid_split index_split right_split
#' @export
node_labels = function(x) {
  
  rules = .list.rules.party(x)
  rules = paste(rules, collapse = "  ")
  
  var = names(model.frame(x))
  
  labels = sapply(var, function(x) {
    grepl(x, rules)
  })
  
  names(labels[labels == TRUE])
  
}

#' Get node rules 
#' @rdname node_labels
#' @export
node_rules = function(x){
  
  node_ids = partykit::nodeids(x, terminal = TRUE)
  
  result = data.frame()
  
  for (i in seq_along(node_ids)) {
    r = data.frame(node = node_ids[i],
                    rules = .list.rules.party(x, node_ids[i]))
    
    result = rbind(result, r)
    
  }
  
  rule = result$rules
  rule = gsub("%in%","@", rule)
  rule = gsub("[(]|[)]| c","", rule)
  rule = gsub("  "," ", rule)
  rule = gsub('"NA",',"", rule)
  rule = gsub(', "NA"',"", rule)
  rule = gsub(",", "COMMA", rule)
  rule = gsub("[.]", "DOT", rule)
  rule = gsub("@", "EQUAL", rule)
  rule = gsub("&", " AND ", rule)
  rule = gsub("<=", " LOWEQUAL ", rule)
  rule = gsub("=>", " HIGHEQUAL ", rule)
  rule = gsub("<", " LOW ", rule)
  rule = gsub(">", " HIGH ", rule)
  
  # remove all other special characters
  rule = gsub("[[:punct:]]", "", rule)
  
  # reposition the key special characters
  rule = gsub("  ", " ", rule)
  rule = gsub("LOWEQUAL", "<=", rule)
  rule = gsub("HIGHEQUAL", "=>", rule)
  rule = gsub("LOW", "<", rule)
  rule = gsub("HIGH", ">", rule)
  rule = gsub("EQUAL", "= ", rule)
  rule = gsub("AND", "&", rule)
  rule = gsub("COMMA", ",", rule)
  rule = gsub("DOT", ".", rule)
  rule = gsub(", NA", "", rule)
  rule = gsub("NA,", "", rule)
  
  result$rules = rule
  
  return(result)
  
}

#' Get the top items out of a decision tree
#' @param top an integer for the number of items to return
#' @rdname node_labels
#' @export 
top_items = function(x, top = 5) {
  
  if (length(x) > 1) {
    
    coef_x = coef(x, log = FALSE)
    
    bestitems = apply(coef_x, 1 , function(y) {
      names(rev(sort(y)))[1:top]
    })
    
    bestitems = as.data.frame(bestitems)
    
    names(bestitems) = paste0("Node", 
                               nodeids(x, terminal = TRUE))
    
    return(bestitems)
    
  }
  
  if (length(x) == 1){
    
    coef_x = coef(x, log = FALSE)
    
    bestitems = names(rev(sort(coef_x)))[1:top]
    
    return(bestitems)
    
  }
  
}

#' Imported from partykit
#' @param i node ids
#' @noRd
.list.rules.party = function (x, i = NULL, ...) 
{
  if (is.null(i)) 
    i = partykit::nodeids(x, terminal = TRUE)
  if (length(i) > 1) {
    ret = sapply(i, .list.rules.party, x = x)
    names(ret) = if (is.character(i)) 
      i
    else names(x)[i]
    return(ret)
  }
  if (is.character(i) && !is.null(names(x))) 
    i = which(names(x) %in% i)
  stopifnot(length(i) == 1 & is.numeric(i))
  stopifnot(i <= length(x) & i >= 1)
  i = as.integer(i)
  dat = partykit::data_party(x, i)
  if (!is.null(x$fitted)) {
    findx = which("(fitted)" == names(dat))[1]
    fit = dat[, findx:ncol(dat), drop = FALSE]
    dat = dat[, -(findx:ncol(dat)), drop = FALSE]
    if (ncol(dat) == 0) 
      dat = x$data
  }
  else {
    fit = NULL
    dat = x$data
  }
  rule = c()
  recFun = function(node) {
    if (partykit::id_node(node) == i) 
      return(NULL)
    kid = sapply(partykit::kids_node(node), partykit::id_node)
    whichkid = max(which(kid <= i))
    split = partykit::split_node(node)
    ivar = partykit::varid_split(split)
    svar = names(dat)[ivar]
    index = partykit::index_split(split)
    if (is.factor(dat[, svar])) {
      if (is.null(index)) 
        index = ((1:nlevels(dat[, svar])) > partykit::breaks_split(split)) + 
          1
      slevels = levels(dat[, svar])[index == whichkid]
      srule = paste(svar, " %in% c(\"", paste(slevels, 
                                               collapse = "\", \"", sep = ""), "\")", sep = "")
    }
    else {
      if (is.null(index)) 
        index = 1:length(kid)
      breaks = cbind(c(-Inf, partykit::breaks_split(split)), 
                      c(partykit::breaks_split(split), Inf))
      sbreak = breaks[index == whichkid, ]
      right = partykit::right_split(split)
      srule = c()
      if (is.finite(sbreak[1])) 
        srule = c(srule, paste(svar, ifelse(right, ">", 
                                             ">="), sbreak[1]))
      if (is.finite(sbreak[2])) 
        srule = c(srule, paste(svar, ifelse(right, "<=", 
                                             "<"), sbreak[2]))
      srule = paste(srule, collapse = " & ")
    }
    rule <<- c(rule, srule)
    return(recFun(node[[whichkid]]))
  }
  node = recFun(partykit::node_party(x))
  paste(rule, collapse = " & ")
}


#' Plot PlackettLuce tree
#' @param log logical, if \code{TRUE} log-worth coefficients are 
#'  displayed instead of worth
#' @param ref optional, character for the reference item when
#'  \var{log} = \code{TRUE}
#' @param ci.level an integer for the confidence interval levels
#' @param ... additional arguments passed to methods. See details
#' @details 
#' Argument multcomp = TRUE adds multi-comparison letters from multcompView
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
#' @import methods
#' @rdname node_labels
#' @method plot pltree
#' @export
plot.pltree = function(x, 
                        log = TRUE, 
                        ref = NULL, 
                        ci.level = 0.95, ...){
  
  
  if (length(x) == 1) {
    return(NextMethod(x, ...))
  }
  
  # Extract ids from terminal nodes
  node_id = partykit::nodeids(x, terminal = TRUE)
  
  # get number of observations in each inner node
  nobs = integer(0L)
  for (i in seq_along(node_id)) {
    nobs = c(nobs, as.integer(x[[ node_id[i] ]]$node$info$nobs))
  }
  
  # get models from each node
  nodes = list()
  for (i in seq_along(node_id)) {
    nodes[[i]] = x[[ node_id[i] ]]$node$info$object
  }
  
  # make panels
  p = try(build_tree_nodes(nodes, 
                       log = log,
                       ci.level = ci.level,
                       ref = ref,
                       node.ids = node_id,
                       n.obs = nobs, 
                       ...), silent = TRUE)
  
  if (isTRUE("try-error" %in% class(p))) {
    warning("Error in generating tree from gosset, using plot.party instead \n")
    return(NextMethod(x))
  }
  
  # get the tree structure
  tree = build_tree_branches(x, ...)
  
  # put branches and nodes together 
  p = (tree / p)
  
  return(p)
  
}

#' Build tree
#' This function builds the party tree
#' @param x a party object
#' @noRd
build_tree_branches = function(x, ...){
  
  splitvar = 0L
  p.value = 0L
  id = 0L
  
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

#' Build tree nodes
#' This function makes the panels 
#' @param x a list with PlackettLuce objects
#' @param node.ids a vector of integers with node ids
#' @param n.obs a vector of integers with N per node
#' @param multcomp TRUE adds multi-comparison groups
#' @inheritParams plot.pltree
#' @noRd
build_tree_nodes = function(x, 
                            log = TRUE, 
                            ref = NULL, 
                            ci.level = 0.95, 
                            node.ids = NULL,
                            n.obs = NULL,
                            multcomp = FALSE, 
                            ...){
  
  if (isFALSE(log)) ref = NULL
  
  if (is.null(node.ids)) {
    node.ids = 1:length(x)
  }
  
  estimate = 0L
  bmin = 0L
  bmax = 0L
  
  # get item names
  items = names(coef(x[[1]]))
  
  # get item parameters from model
  coeffs = try(lapply(x, function(y) {
    z = psychotools::itempar(y, log = log, ref = ref)
    # get estimates from item parameters using qvcalc
    z = qvcalc::qvcalc(z, ref = ref)$qvframe
  }), silent = TRUE)
  
  if (isTRUE("try-error" %in% class(coeffs))) {
    
    # is likely that the error id due to missing items in one the nodes
    # so we apply the function pseudo_ranking() to add these missing items
    # extract the original rankings, add pseudo_ranking and refit the model
    coeffs = lapply(x, function(y){
      r = y$rankings
      r = pseudo_rank(r)
      stats::update(y, rankings = r)
    })
    
    coeffs = try(lapply(coeffs, function(y) {
      z = psychotools::itempar(y, log = log, ref = ref)
      # get estimates from item parameters using qvcalc
      z = qvcalc::qvcalc(z)$qvframe
    }), silent = TRUE)
    
  }
  
  # if the error persists then return an error 
  if (isTRUE("try-error" %in% class(coeffs))) {
    stop("Unable to compute worth estimates. Check for errors/warnings in ",
         "your modelparty object. \n You can try log = FALSE \n")
  }
  
  # Add limits in error bars and item names
  coeffs = lapply(coeffs, function(X){
    X = within(X, {
      bmax = X$estimate + stats::qnorm(1-(1-ci.level)/2) * X$quasiSE
      bmin = X$estimate - stats::qnorm(1-(1-ci.level)/2) * X$quasiSE
      items = items
    })
    return(X)
  })
  
  # Add node information and number of observations
  # and if required add multicomp letters
  for (i in seq_along(node.ids)) {
    coeffs[[i]] = within(coeffs[[i]], {
      nobs = n.obs[i]
      node = node.ids[i]}
    )
    
    if(isTRUE(multcomp)) {
      mc = multcompPL(x[[i]])
      coeffs[[i]] = merge(coeffs[[i]], mc[,c("items", "group")], by = "items")
    }else{
      coeffs[[i]]$group = ""
    }
  }
  
  coeffs = do.call("rbind", coeffs)
  
  if (isFALSE(log)) {
    coeffs$bmin = ifelse(coeffs$bmin < 0, 0, coeffs$bmin)
    coeffs$bmax = ifelse(coeffs$bmax > 1, 1, coeffs$bmax)
  }
  
  coeffs$id = paste0(coeffs$node, "_", coeffs$items)
  
  groups = ""
  
  node_lev = unique(paste0("Node ", coeffs$node, " (n=", coeffs$nobs, ")"))
  
  coeffs$id = coeffs$node
  
  coeffs$node = factor(paste0("Node ", coeffs$node, " (n=", coeffs$nobs, ")"),
                        levels = node_lev)
  
  coeffs$items = factor(coeffs$items, levels = rev(sort(items)))
  
  # Get max and min values for the x axis in the plot
  xmax = round(max(coeffs$bmax, na.rm = TRUE) + 0.01, digits = 4)
  
  if (isFALSE(log)) {
    xmin = 0
    xinter = 1/length(items)
    xbreaks = round(c(mean(c(0, xmax)), xmax), 2)
    xbreaks = c(0, xbreaks)
  }
  
  if (isTRUE(log)) {
    xinter = 0
    xmin = min(coeffs$bmin, na.rm = TRUE)
    xbreaks = round(c(mean(c(xmin, xmax)), xmax), 2)
    xbreaks = c(xmin, xbreaks)
  }
  
  xlabs = as.character(round(xbreaks, 2))
  xmin = xmin + (xmin * 0.15)
  xmax = xmax + (xmax * 0.15)
  
  # Check font size for axis X and Y, and plot title
  ggplot2::ggplot(coeffs, 
                  ggplot2::aes(x = estimate, 
                               y = items,
                               label = group)) +
    ggplot2::geom_vline(xintercept = xinter, 
                        colour = "#E5E7E9", size = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = group),
                       hjust = 0.5, vjust = -1) +
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
                                                       hjust = 0.5, vjust = 0.5, 
                                                       face = "plain",
                                                       colour = "black"),
                   axis.text.y = ggplot2::element_text(size = 13, angle = 0,
                                                       hjust = 1, vjust = 0.5, 
                                                       face = "plain",
                                                       colour = "black"),
                   text = ggplot2::element_text(size = 14),
                   strip.background = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(colour = "black", size = 1),
                   axis.ticks = ggplot2::element_line(colour = "black", size = 0.5),
                   axis.ticks.length = grid::unit(0.3, "cm"))
  
  
}

# coeffs = data.frame(estimate = c(-0.6, 0, 0.4, 0.2, -0.1, 0.4),
#                      items = rep(c("banana", "apple", "orange"), 2),
#                      groups = "",
#                      node = rep(c(1,2), each = 3))
# 
# coeffs
# 
# xinter = 0
# bmin = 0
# bmax = 0
# xbreaks = c(-0.78, 0, 0.89)
# xlabs = xbreaks
# xmin = -0.78
# xmin = xmin + (xmin * 0.15)
# xmax = 0.89
# xmax = xmax + (xmax * 0.15)


#' @rdname multcompPL
#' @noRd
multcompPL = function(mod, items = NULL, threshold = 0.05, adjust = "none", ...){
  
  #get estimates with quasi-SEs
  qv1 = qvcalc::qvcalc(mod, ...)$qvframe
  
  #reduce frame to only selected items if not all comparisons are desired
  if (!is.null(items)) {
    qv1 = subset(qv1, rownames(qv1) %in% items)
    # give error if less than 2 items can be identified
    if (nrow(qv1) < 3) {
      stop("Less than 2 items selected")
    }
  }
  
  #set up matrices for all differences and pooled errors
  diffs = mat.or.vec(nrow(qv1),nrow(qv1))
  ses = mat.or.vec(nrow(qv1),nrow(qv1))
  
  for(i in 1:nrow(qv1)){
    for(j in 1:nrow(qv1)){
      #get differences and pooled ses
      diffs[i,j] = qv1$estimate[i] - qv1$estimate[j]
      ses[i,j] = sqrt(qv1$quasiVar[i] + qv1$quasiVar[j])
    }
  }
  
  #calculate z scores
  z = diffs/ses
  #TO DO: What DF to use to use here? Is it just the resid DF?
  p = 2 * (1 - stats::pt(abs(z), mod$df.residual))
  
  #adjust p-value if you want to adjust. make sure to only take each p once for adjustment
  p[upper.tri(p)] = stats::p.adjust(p[upper.tri(p)], method = adjust)
  
  #make sure lower triangular is mirror of upper
  p[lower.tri(p)] = t(p)[lower.tri(p)]
  
  #set rownames
  rownames(p) = colnames(p) = rownames(qv1)
  
  #re-order qv output to ensure letters are produced in a sensible order
  qv1$items = stats::reorder(factor(rownames(qv1)), qv1$estimate, mean)
  qv1 = qv1[order(qv1$estimate, decreasing = TRUE), ]
  
  #get mean seperation letter groupings
  args = list(formula = estimate ~ items, 
               x = p, 
               data = qv1,
               compare = "<",
               threshold =  threshold,
               Letters = letters,
               reversed = FALSE)
  
  let = do.call("multcompLetters2", args)
  
  qv1$group = let$Letters
  
  qv1 = qv1[, union("items", names(qv1))]
  
  row.names(qv1) = seq_along(qv1$group)
  
  class(qv1) = union("multcompPL", class(qv1))
  
  return(qv1)
  
}

