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

# unpack_notion <- function(x){
#   print(length(x))
#   if(length(x) == 1) {
#     unbox <- purrr::transpose(vec_data(x))
#     return(unbox[[1]])
#   }
#   purrr::transpose(vec_data(x))
#   #purrrly::by_row(vec_data(x), function(v) list(v)[[1L]], .collate = "list")$.out
# }

# unpack_a_notion <- function(x){
#   purrr::flatten(x)
# }

unpack_notion <- function(x){
  #print("we are here1")
  if(length(x) == 1) {
    #print("we are here2")
    tmp <- purrr::flatten(x)
    if(assertive.types::is_inherited_from(x, "notion_sort") &
       tmp$property == "*Search_API"){
      #print("we are here3")
      tmp$property <- NULL
    }
    return(tmp)
  }

  else lapply(x,purrr::flatten)
}

wrap <- function(...) {
  tibble::lst(...)
}

# Wrapper function
a_wrap <- \(x, .names = "sort"){
  list(unpack_notion(x)) |>
    (\(x, nm){
      names(x)[1] <- nm
      return(x)
    })(nm = .names)
}
