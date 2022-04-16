#' Append multiple list items
#'
#' Recursively iterate through each of the list items to make one simplified list.
#' Flattens all lists without a name, while preserving lists with a name. Also 
#' keeps the names of all provided names and ignores non-existent list objects.
#' 
#' @param ... Feed values of any type to have it return a single list
#' @return A single list of all of the lists flattened in a certain manner
#' @seealso [base::append()], [base::unlist()]
#' @export
#' @examples
#' a <- "x"
#' x <- matrix(c(c(1,2),c(3,4)), ncol = 2)
#' testList <- list(test = "abc", test2 = 12, test3 = list(testA = "xyz", testB = 2), test5 = x, test6 = a)
#' testList2 <- list(test7 = "xt", test8 = "yes")
#' testList3 <- list(test9 = x)
#' t <- appends(testList, testx = testList2, testList3, testList$xyz)
#' 
#' ## List of 7
#' ## _names: chr [1:7] "test" "test2" "test3" "test5" "test6" "testx" "test9"
#' ## test:"abc"
#' ## test2:12
#' ## test3:List of 2
#' ## test5: num [1:2, 1:2] 1 2 3 4
#' ## test6:"x"
#' ## testx:List of 2
#' ## test9: num [1:2, 1:2] 1 2 3 4
appends <- function(...){
    out.list <- list()
    params <- list(...)
    if (length(params) == 1) params <- params[[1]]
    i <- 0
    for (param in params){
        i <- i + 1
        name <- names(params)[i]
        if (is.null(name)) name <- ""
        # Recursive loop through only unnamed list elements
        if (typeof(param) == "list" & name=="") {
            out.list <- append(out.list, appends(param))
        } else if (!is.null(param)) {
            out.list <- append(out.list, list(param))
            names(out.list)[length(out.list)] <- name
        }
    }
    return(out.list)
}