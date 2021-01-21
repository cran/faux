#' Validates the specified design
#' 
#' Specify any number of within- and between-subject factors with any number of levels. 
#' 
#' Specify n for each between-subject cell; mu and sd for each cell, and r for the within-subject cells for each between-subject cell.
#' 
#' This function returns a validated design list for use in sim_data to simulate a data table with this design, or to archive your design.
#' 
#' See \href{../doc/sim_design.html}{\code{vignette("sim_design", package = "faux")}} for details.
#' 
#' @param within a list of the within-subject factors
#' @param between a list of the between-subject factors
#' @param n the number of samples required
#' @param mu a vector giving the means of the variables
#' @param sd the standard deviations of the variables
#' @param r the correlations among the variables (can be a single number, full correlation matrix as a matric or vector, or a vector of the upper right triangle of the correlation matrix
#' @param dv the name of the DV column list(y = "value")
#' @param id the name of the ID column list(id = "id")
#' @param vardesc a list of variable descriptions having the names of the within- and between-subject factors
#' @param plot whether to show a plot of the design
#' @param design a design list including within, between, n, mu, sd, r, dv, id
#' @param fix_names fix variable names so special characters become . or _ (default TRUE)
#' 
#' @return list
#' 
#' @examples 
#' 
#' within <- list(time = c("day", "night"))
#' between <- list(pet = c("dog", "cat"))
#' mu <- list(dog = 10, cat = 5)
#' vardesc <- list(time = "Time of Day", pet = "Type of Pet")
#' check_design(within, between, mu = mu, vardesc = vardesc)
#' 
#' between <- list(language = c("dutch", "thai"),
#'                 pet = c("dog", "cat"))
#' mu <- list(dutch_dog = 12, dutch_cat = 7, thai_dog = 8, thai_cat = 3)
#' check_design(within, between, mu = mu)
#' @export
#' 
check_design <- function(within = list(), between = list(), 
                         n = 100, mu = 0, sd = 1, r = 0, 
                         dv = list(y = "value"), 
                         id = list(id = "id"), 
                         vardesc = list(),
                         plot = faux_options("plot"), 
                         design = NULL, fix_names = TRUE) {
  # design passed as design list
  if (!is.null(design)) {
    # double-check the entered design
    list2env(design, envir = environment())
  } else if ("design" %in% class(within)) {
    # design given as first argument: not ideal but handle it
    list2env(within, envir = environment())
  }
  
  # name anonymous factors ----
  if (is.numeric(within) && all(within %in% 2:10)) { # vector of level numbers
    within_names <- LETTERS[1:length(within)]
    indices <- lapply(within, function(.) seq(1:.))
    within <- mapply(paste0, within_names, indices, SIMPLIFY = FALSE)
  }
  if (is.numeric(between) && all(between %in% 2:10)) { # vector of level numbers
    between_names <- LETTERS[(length(within)+1):(length(within)+length(between))]
    indices <- lapply(between, function(.) seq(1:.))
    between <- mapply(paste0, between_names, indices, SIMPLIFY = FALSE)
  }
  
  # check factor specification ----
  if (!is.list(within) || !is.list(between)) {
    stop("within and between must be lists")
  }
  
  # if within or between factors are named vectors, 
  # use their names as column names and values as labels for plots
  pattern <- NULL
  if (fix_names) pattern <- "_"
  between <- lapply(between, fix_name_labels, pattern = pattern)
  within <- lapply(within, fix_name_labels, pattern = pattern)
  dv <- fix_name_labels(dv, pattern = NULL)
  id <- fix_name_labels(id, pattern = NULL)
  
  # check for duplicate factor names ----
  all_names <- c(names(within), names(between))
  factor_overlap <- duplicated(all_names)
  if (sum(factor_overlap) > 0) {
    dupes <- unique(all_names[factor_overlap])
    stop("You have multiple factors with the same name (", 
         paste(dupes, collapse = ", "),
         "). Please give all factors unique names.")
  }
  
  # check for duplicate level names within any factor ----
  dupes <- c(within, between) %>%
    lapply(duplicated) %>%
    lapply(sum) %>%
    lapply(as.logical) %>%
    unlist()
  
  if (sum(dupes)) {
    dupelevels <- c(within, between) %>% 
      names() %>% 
      `[`(dupes) %>% 
      paste(collapse = ", ")
    stop("You have duplicate levels for factor(s): ", dupelevels)
  }
  
  # define columns ----
  cells_w <- cell_combos(within, names(dv))
  cells_b <- cell_combos(between, names(dv)) 
  
  # convert n, mu and sd  ----
  cell_n  <- convert_param(n,  cells_w, cells_b, "Ns")
  for (i in names(cell_n)) {
    cell_n[[i]] <- cell_n[[i]][[1]]
  }
  cell_mu <- convert_param(mu, cells_w, cells_b, "means")
  cell_sd <- convert_param(sd, cells_w, cells_b, "SDs")
  
  # set up cell r from r ----
  # (number, vector, matrix or list styles)
  cell_r <- list()
  if (length(within)) {
    for (cell in cells_b) {
      cell_cor <- if(is.list(r)) r[[cell]] else r
      mat <- cormat(cell_cor, length(cells_w))
      rownames(mat) <- cells_w
      colnames(mat) <- cells_w
      cell_r[[cell]] <- mat
    }
  }
  
  # check n ----
  n <- suppressWarnings(lapply(n, as.numeric)) # make sure all cells are numbers
  if (unlist(n) %>% is.na() %>% sum()) { stop("All n must be numbers") }
  if (sum(unlist(n) %% 1 > 0)) {
    warning("Some cell Ns are not integers. They have been rounded up to the nearest integer.")
    n <- lapply(n, ceiling)
  }
  if (sum(unlist(n) < 0)) { stop("All n must be >= 0") }
  if (sum(unlist(n) == 0)) {
    warning("Some cell Ns are 0. Make sure this is intentional.")
  }
  
  # check mu ----
  mu <- suppressWarnings(lapply(mu, as.numeric)) # make sure all cells are numbers
  if (unlist(mu) %>% is.na() %>% sum()) { stop("All mu must be numbers") }
  
  # check sd ----
  sd <- suppressWarnings(lapply(sd, as.numeric)) # make sure all cells are numbers
  if (unlist(sd) %>% is.na() %>% sum()) { stop("All sd must be numbers") }
  if (sum(unlist(sd) < 0)) { stop("All sd must be >= 0") }
  
  # start param table ----
  fnames <- lapply(c(between, within), names)
  d <- do.call(expand.grid, rev(fnames))
  
  if (nrow(d) == 0 & ncol(d) == 0) {
    d <- as.data.frame(dv)
  } else {
    d <- d[,ncol(d):1] %>% as.data.frame()
  }
  
  if (length(within)) {
    rmat <- cell_r %>% unlist() %>% unname() %>% 
      matrix(ncol = length(cells_w), byrow = TRUE)
    colnames(rmat) <- cells_w
    
    for (w in cells_w) { d[w] <- rmat[,w] }
  }
  
  d$n <- unlist(cell_n) %>% rep(each = length(cells_w))
  d$mu <- unlist(cell_mu)
  d$sd <- unlist(cell_sd)
  
  design <- list(
    within = within,
    between = between,
    dv = dv,
    id = id,
    n = cell_n,
    mu = cell_mu,
    sd = cell_sd,
    r = cell_r,
    params = d
  )
  
  class(design) <- c("design", "list")
  
  if (plot) { plot_design(design) %>% print() }
  
  invisible(design)
}

#' Print Design List
#'
#' @param x The design list
#' @param ... Additional parameters for print
#'
#' @export
#'
print.design <- function(x, ...) {
  if (!"quote" %in% names(list(...))) quote <- ""
  
  txt <- "Design\n\n"
  txt <- sprintf("%s* [DV] %s: %s%s%s  \n", 
                 txt, names(x$dv), quote, x$dv, quote)
  txt <- sprintf("%s* [ID] %s: %s%s%s  \n\n", 
                 txt, names(x$id), quote, x$id, quote)
  txt <- sprintf("%sWithin-subject variables:\n\n%s\n\n", 
                 txt, nested_list(x$within, quote = quote))
  txt <- sprintf("%sBetween-subject variables:\n\n%s\n\n", 
                 txt, nested_list(x$between, quote = quote))
  txt <- knitr::kable(x$params) %>%
    paste(collapse = "\n") %>%
    sprintf("%sParameters:\n\n%s\n\n",  txt, .)
  
  cat(txt)
}

