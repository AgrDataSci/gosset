#' Probability of outperforming a check
#' 
#' Measures the precision of estimated values, and
#'  the potential response to selection on those 
#'  estimated values compared to a check
#'  
#' @param x a numeric vector, or an object  
#'  of class \code{PlackettLuce} or \code{pltree}
#' @param y numeric, the reference value
#' @param ref a character or integer for indexing the
#'  element containing reference values in \var{x}
#' @param ... additional arguments passed to methods
#' @return the reliability based on the worth parameters
#' @author Kauê de Sousa, David Brown, Jacob van Etten
#' @references 
#'  Eskridge and Mumm (1992). Theoret. Appl. Genetics 84, 494–500 \doi{10.1007/BF00229512}.
#' @examples
#' # Case 1. vector example
#' 
#' x = c(9.5, 12, 12.3, 17)
#' 
#' y = 11.2
#' 
#' reliability(x, y)
#' 
#' # Case 2. PlackettLuce model
#' 
#' library("PlackettLuce") 
#' 
#' R = matrix(c(1, 2, 4, 3,
#'               4, 1, 2, 3,
#'               2, 3, 1, 4,
#'               4, 2, 3, 1,
#'               2, 1, 4, 3,
#'               1, 4, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) = c("apple", "banana", "orange", "pear")
#' 
#' mod = PlackettLuce(R)
#' 
#' reliability(mod, ref = "orange")
#' 
#' \donttest{
#' # Case 3. PlackettLuce tree
#' 
#' data("beans", package = "PlackettLuce")
#' 
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
#' reliability(tree, ref = "Local")
#' 
#' }
#' @importFrom methods addNextMethod asMethodDefinition assignClassDef
#' @importFrom qvcalc qvcalc
#' @importFrom stats update pnorm
#' @export
reliability = function(x, ...) {
  
  UseMethod("reliability")
  
}

#' @rdname reliability
#' @export
reliability.default = function(x, y = NULL, ...) {
  
  dots = list(...)
  
  ref = dots[["ref"]]
  
  worth = x
  
  if (is.null(ref)) {
    ref_worth = y
  }
  
  if (is.null(y)) {
    ref_worth = worth[ref]
  }
  
  worth / (ref_worth + worth)
  
}

#' @rdname reliability
#' @method reliability PlackettLuce
#' @export
reliability.PlackettLuce = function(x, ref, ...) {
  
  x = qvcalc::qvcalc(x, ref = ref)$qvframe
  
  items = rownames(x)
  
  whichref = which(items %in% ref)
  
  worth = exp(x$estimate) / sum(exp(x$estimate)) 
  
  rel = worth / (worth[whichref] + worth)
  
  x$reliability = rel
  
  rel_upper = (exp(log(worth) + x$quasiSE) / (worth[whichref] + exp(log(worth) + x$quasiSE)))  
  
  rel_lower = (exp(log(worth) - x$quasiSE) / (worth[whichref] + exp(log(worth) - x$quasiSE)))  
  
  rel_se =  rel - rel_lower
  
  x$rel_se = rel_se
  
  x$worth = worth
  
  # #formulas from summary Plackett-Luce package
  # #https://github.com/hturner/PlackettLuce/blob/master/R/summary.R
  x$Z = x$estimate/x$SE
  
  x$p_value = 2 * stats::pnorm(-abs(x$Z))
  
  x$item = rownames(x)
  
  x = x[,c("item", "reliability", "rel_se","worth", "Z", "p_value")]
  
  x[is.na(x)] = NA
  
  rownames(x) = 1:nrow(x)
  
  names(x) = c("item", "reliability", "reliabilitySE", "worth", "Zvalue", "Pr(>|z|)")
  
  class(x) = union("gosset_df", class(x))
  
  return(x)
  
}

#' @rdname reliability
#' @method reliability pltree
#' @export 
reliability.pltree = function(x, ref, ...) {
  
  # Extract ids from terminal nodes
  node_id = partykit::nodeids(x, terminal = TRUE)
  
  # get models from each node
  nodes = list()
  for (i in seq_along(node_id)) {
    mod_i = x[[ node_id[i] ]]$node$info$object
    # r = mod_i$rankings
    # r = pseudo_rank(r)
    # mod_i = stats::update(mod_i, rankings = r)
    nodes[[i]] = mod_i
  }
  
  rel = lapply(nodes, function(y){
    reliability.PlackettLuce(y, ref = ref)
  })
  
  nitems = nrow(rel[[1]])
  
  rel = do.call("rbind", rel)
  
  rel = cbind(node = rep(node_id, each = nitems), rel)
  
  class(rel) = union("gosset_df", class(rel))
  
  return(rel)
  
}
