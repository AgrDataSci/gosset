#' Rank decimal numbers
#' 
#' @param object a vector with floating point numbers
#' @param id optional, a vector with ids to group values
#' @param bindwith optional, a data.frame to cbind with ranked values
#' @return A data frame with ranked floating point numbers
#' @examples 
#' # without id
#' .rank_decimal(c(0.2, -1.2, 2.3, 0.2, 0.4, -3.3))
#' 
#' # with id
#' .rank_decimal(c(0.2, -1.2, 2.3, 0.2, 0.4, -3.3),
#'               id = c(1,1,1,2,2,2))
#'
#' @noRd
.rank_decimal = function(object, id = NULL, bindwith = NULL){
  
  isdf = is.data.frame(object)
  
  if (!isdf) {
    object = as.data.frame(object)
    names(object) = "rank"
  }
  
  if (!is.null(bindwith)) {
    object = cbind(object, bindwith)
  }
  
  if(is.null(id)) {
    id = rep(1, nrow(object))
  }
  
  object = cbind(id = id, object)
  
  object = split(object, id)
  
  object = lapply(object, function(x) {
    x$rank = rank((x$rank - 1) * -1, na.last = "keep")
    return(x)
  })
  
  object = do.call("rbind", object)
  
  object = as.data.frame(object, stringsAsFactors = FALSE) 
  
  object[,c("id","rank")] = lapply(object[,c("id","rank")], 
                                   as.integer)
  
  return(object)
  
}


#' Set data.frame into long format
#' 
#' @param object a data.frame in the wide format
#' @param id a character for the id
#' @return a data.frame in the long format
#' @examples
#' data = data.frame(
#'   id = 1:10,
#'   x = LETTERS[1:10],
#'   y = letters[11:20],
#'   z = colors()[1:10],
#'   w = colors()[401:410]
#' )
#' 
#' .set_long(data, "id")
#' @noRd
.set_long = function(object, id) {
  
  object = split(object, object[, id])
  
  object = lapply(object, function(x){
    
    x = t(x)
    
    x = cbind(row.names(x), x)
    
    i = x[id, 2]
    
    x = cbind(x, id = i)
    
    x = x[ x[,1] != id , ]
    
    x = as.data.frame(x, stringsAsFactors = FALSE)
    
    names(x) = c("variable", "value", "id")
    
    x = x[ ,c("id", "variable", "value")]
    
    return(x)
    
  })
  
  object = do.call("rbind", object)
  
  object = as.data.frame(object, stringsAsFactors = FALSE)
  
  object = with(object, object[order(as.integer(id)), ])
  
  rownames(object) = 1:nrow(object)
  
  return(object)
  
}


#' Set a data.frame into wide format
#' 
#' @param object a data.frame in the long format
#' @param id a character for the id
#' @return a data.frame in the wide format
#' @examples
#' data = data.frame(
#'   id = 1:10,
#'   x = LETTERS[1:10],
#'   y = letters[11:20],
#'   z = colors()[1:10],
#'   w = colors()[401:410]
#' )
#' 
#' data_l = .set_long(data, "id")
#' 
#' .set_wide(data_l, "id")
#' @noRd
.set_wide = function(object, id) {
  
  object = split(object, object[, id])
  
  object = lapply(object, function(x){
    
    x = t(x)
    
    i = as.integer(x[1, 1])
    
    nams = c("id", x[2, ])
    
    val = as.vector(c(i, x[3, ]))
    
    names(val) = nams
    
    return(val)
    
  })
  
  object = do.call("rbind", object)
  
  object = as.data.frame(object, stringsAsFactors = FALSE)
  
  object = with(object, object[order(as.integer(id)), ])
  
  return(object)
}

#' Find integer values in a vector with decimals
#' @param x a vector with numbers
#' @return a logical vector where TRUE indicates integer numbers
#' @examples
#' .is_wholenumber(c(1,3.1,4,0.9))
#' @noRd
.is_wholenumber = function(x) {
  x = as.vector(t(x))
  x = x %% 1 == 0
  return(x)
}

#' Find a decimal number in a vector with integers
#' @param x a vector with numbers
#' @return a logical vector where TRUE indicates floating point numbers
#' @examples
#' .is_decimal(c(1,3.1,4,0.9))
#' @noRd
.is_decimal = function(x) {
  x = as.vector(t(x))
  x = !x %% 1 == 0
  return(x)
}

#' Is a grouped_rankings object
#' @param object an object to have its class tested
#' @return a logical value where TRUE indicates that the 
#' object is of class "grouped_rankings"
#' @examples
#' .is_grouped_rankings(airquality)
#' @noRd
.is_grouped_rankings = function(object) {
  
  c("grouped_rankings") %in% class(object)
  
}

#' Is a rankings object
#' @param object an object to have its class tested
#' @return a logical value where TRUE indicates that the 
#' object is of class "rankings"
#' @examples
#' .is_rankings(airquality)
#' @noRd
.is_rankings = function(object) {
  
  c("rankings") %in% class(object)
  
}

#' Is a paircomp object
#' @param object an object to have its class tested
#' @return a logical value where TRUE indicates that 
#' the object is of class "paircomp"
#' @examples
#' .is_paircomp(airquality)
#' @noRd
.is_paircomp = function(object) {
  
  c("paircomp") %in% class(object)
  
}

#' Is a tibble object
#' @param object an object to have its class tested
#' @return a logical value where TRUE indicates that the 
#' object is of class "tbl_df"
#' @examples
#' .is_tibble(airquality)
#' @noRd
.is_tibble = function(object) {
  
  c("tbl_df") %in% class(object)
  
}


#' Check if objects has the same class
#' @param x an object to be compared with y
#' @param y an object to be compared with x
#' @return a logical value where TRUE indicates that x has the same class as y
#' @examples
#' .same_class(airquality, mtcars)
#' @noRd
.same_class = function(x, y) {
  
  class_x = class(x)
  
  any(class(x) %in% class(y))
  
}


#' Logical function for > greater 
#' Test a paircomp object
#' @param x a vector of class numeric or integer
#' @param y a vector of class numeric or integer
#' @return a logical vector TRUE indicates whether x is greater than y
#' @examples
#' .is_greater(c(1,2,3), c(0,4,6))
#' @noRd
.is_greater = function(x, y) {
  
  x > y
  
}

#' Logical function for < lower 
#' @param x a vector of class numeric or integer
#' @param y a vector of class numeric or integer
#' @return a logical vector TRUE indicates whether x is lower than y
#' @examples
#' .is_lower(c(1,2,3), c(0,4,6))
#' @noRd
.is_lower = function(x, y) {
  
  x < y
  
}


#' Round to the nearest base value
#' @param x a vector of class numeric
#' @param a number for the base value to round
#' @return a vector of class numeric with rounded number to its near base
#' @examples 
#' x = c(1.6,0.3, 0.2, 2)
#' .round5(x, 0.5)
#' @noRd
.round5 = function(x, base.value) {
  
  base.value * round( x / base.value )
  
}

#' Sort order of players based on their performance in contests
#' @param data an object with player based values
#' @param split a character or index of 'data' to indicate the 
#' variable to split the data
#' @param value a character or index of 'data' to indicate the 
#' variable to sort data
#' @return a character vector with the player order
#' @examples
#' data("breadwheat", package = "gosset")
#' R = rank_tricot(data = breadwheat,
#'                  items = c(1:3),
#'                  input = c(18:19))
#' d = dominance(R)
#' .player_order(d, "player1", "dominance")
#' @noRd
.player_order = function(data, split, value) {
  
  # get sums of victories to order items from higher to lower
  p_order = split(data, data[, split])
  
  p_order = lapply(p_order, function(z) {
    sum(z[, value])
  })
  
  p_order = sort(unlist(p_order))
  
  # get order
  p_order = rev(names(p_order))
  
  return(p_order)
  
}

#' Decode rankings
#' @param items a matrix with items
#' @param rankings a matrix with rankings
#' @return a matrix with decoded rankings 
#' @examples
#' i = as.data.frame(matrix(NA, nrow = 10, ncol = 5))
#' names(i) = paste0("Item",1:5)
#' 
#' r = as.data.frame(matrix(NA, nrow = 10, ncol = 5))
#' names(r) = paste0("Position_Item",1:5)
#' 
#' for(s in 1:10) {
#'   i[s,] = sample(LETTERS[1:5])
#'   r[s,] = sample(1:5)
#' }
#' 
#' .decode_ranking(i, r)
#' @noRd
.decode_ranking = function(items, rankings) {
  
  nc = ncol(rankings)
  nr = nrow(rankings)
  
  rankings = split(rankings, rownames(rankings))
  
  index = lapply(rankings, function(y) {
    
    order(y, na.last = NA)
    
  })
  
  index = do.call("rbind", index)
  
  ranks = matrix(NA, nrow = nr, ncol = nc)
  
  for (z in seq_len(nc)) {
    
    ranks[, z ] = items[cbind(1:nr, index[, z])]
    
  }
  
  return(ranks)
  
}

#' Nearest neighbour
#' Check the nearest point using the Eucledian method
#' @param xy1 a numeric vector indicating one single coordinate lonlat
#' @param xy2 a data.frame with coordinates lonlat
#' @return the index in nrow 'xy2' for the nearest point to 'xy1'
#' @examples
#' lonlat1 = c(11.572197, 57.57921)
#' 
#' lonlat2 = data.frame(lon = runif(10, 11, 12),
#'                       lat = runif(10, 55, 58))
#' 
#' nn = .nearest(lonlat1, lonlat2)
#' 
#' lonlat2[nn, ]
#' @noRd
.nearest = function(xy1, xy2) {
  
  x1 = xy1[1]
  y1 = xy1[2]
  
  x2 = xy2[,1]
  y2 = xy2[,2]
  
  x = (x1 - x2)^2
  
  y = (y1 - y2)^2
  
  xy = sqrt(x + y)
  
  index_xy = which.min(xy)
  
  return(index_xy)
  
}

#' Make pairwise comparisons of items
#' 
#' @param x vector source for combinations
#' @param m integer for the number of elements to choose
#' @examples 
#' x = LETTERS[1:5]
#' .combn2(x, 2)
#' 
#' @noRd
.combn2 = function(x, m)
{
  cc = utils::combn(rev(x), m)
  cc = cc[c(2,1), ncol(cc):1]
  return(cc)    
}


#' Abbreviate characters
#' 
#' @param x a vector with characters
#' @param nchars an integer for the min length of characters allowed in \code{x}
#' @param ... additional arguments passed to methods
#' @examples 
#' muni = c("New York", "Oslo", "Rio de Janeiro", "Punjab",
#'           "Sao Jose dos Campos", "Rio Preto da Eva")
#' 
#' .reduce(muni)
#' 
#' .reduce(muni, minlength = 7)
#' @noRd
.reduce = function(x, nchars = 8, ...){
  
  reduc = nchar(x) > nchars
  
  abb = abbreviate(x[reduc], ...)
  
  x[reduc] = abb
  
  return(x)
}



#' Color pallete
#' @param x a integer
#' @return the color pallete
#' @noRd
.col_pallet = function(x, ...) {
  
  p = c('#d73027','#4575b4', '#f46d43','#74add1', 
        '#fdae61','#abd9e9', '#fee090', '#762a83',
        '#a6dba0','#9970ab','#5aae61', '#c2a5cf', 
        '#1b7837')
  
  v = p[1:x]
  
  return(v)
  
}
