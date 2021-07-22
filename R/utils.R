'%||%' <- function(lhs, rhs){
  if(!is.null(lhs)) lhs
  else rhs
}

'%|null|%' <- function(lhs, rhs){
  if(!is.null(lhs)) return(rhs)
  #else as.null()
}



# #x %|null|% 1+5
# x <- NULL
# j <- x %|null|% x+5
# class(j)

# x <- NULL
# x <- x %|null|% data.frame(x,x)
# class(x) <- "NULL"
# str(x)

add_checkpoint <- function(x, clean_null = TRUE){
  attr(x, "notion_check") <- TRUE
  if(clean_null) return(rlist::list.clean(x))
  else x
}

parse_result <- function(... , clean_null = TRUE){
  x <- tibble::lst(...)
  attr(x, "notion_check") <- TRUE
  if(clean_null) return(rlist::list.clean(x))
  else x
}


validate_checkpoint <- function(x){
  if(is.null(attr(x, "notion_check"))){
    warning("Please use obj_*** functions to create a complex JSON argurments \n to ensure the correctness")
  }
  x
}

un_RNO <- function(x){
  as.list(vec_data(x))
}

