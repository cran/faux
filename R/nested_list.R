#' Output a nested list in RMarkdown list format
#'
#' @param x The list
#' @param pre Text to prefix to each line (e.g., if you want all lines indented 4 spaces to start, use "    ")
#' @param quote Text to quote values with (e.g., use "`" to make sure values are not parsed as markdown
#'
#' @return A character string
#' @export
#'
#' @examples
#' x <- list(
#'   a = list(a1 = "Named", a2 = "List"),
#'   b = list("Unnamed", "List"),
#'   c = c(c1 = "Named", c2 = "Vector"),
#'   d = c("Unnamed", "Vector"),
#'   e = list(e1 = list("A", "B", "C"),
#'            e2 = list(a = "A", b = "B"),
#'            e3 = c("A", "B", "C"),
#'            e4 = 100),
#'   f = "not a list or vector"
#' )
#' nested_list(x)
nested_list <- function(x, pre = "", quote = "") {
  txt <- c()
  if (is.list(x) | length(x) > 1) {
    if (is.null(names(x))) {
      # unnamed list
      for (i in 1:length(x)) {
        y <- x[[i]]
        if (is.list(y) | length(y) > 1) {
          txt[length(txt)+1] <- paste0(pre, "* ", i, ": ")
          subtxt<- nested_list(y, paste(pre, "  "), quote)
        } else {
          subtxt<- nested_list(y, pre, quote)
        }
        txt <- c(txt, subtxt)
      }
    } else {
      # named list
      for (y in names(x)) {
        if (is.list(x[[y]]) | length(x[[y]]) > 1) {
          # non-terminal named list entry
          txt[length(txt)+1] <- paste0(pre, "* ", y, ": ")
          subtxt <- nested_list(x[[y]], paste0(pre, "  "), quote)
          txt <- c(txt, subtxt)
        } else {
          # terminal named list entry
          entry <- paste(x[[y]], collapse = ", ")
          txt[length(txt)+1] <-
            paste0(pre, "* ", y, ": ", quote, entry, quote)
        }
      }
    }
  } else {
    # terminal unnamed list entry
    collapse <- paste0(quote, ", ", quote)
    txt[length(txt)+1] <- paste0(pre, "* ", quote,
                                 paste(x, collapse = collapse), quote)
  }
  
  list_txt <- paste(txt, collapse = "\n")
  class(list_txt) <- c("nested_list", "character")
  
  list_txt
}

#' Print Nested List
#'
#' @param x The nested_list string
#' @param ... Additional parameters for print
#'
#' @export
#'
print.nested_list <- function(x, ...) {
  cat(x)
}


