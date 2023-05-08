install.packages("devtools")
devtools::install_github("dgrtwo/stackr")


# API utilities

#' Parse the results of a Stack Exchange API query into a data.frame.
#'
#' The additional metadata, such as "has_more", "quota_max", and
#' "quota_remaining" is included in a list as the attribute "metadata".
#'
#' @param req a request from httr::GET
#'
stack_parse <- function(req) {
  text <- httr::content(req, as = "text")
  
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  
  j <- jsonlite::fromJSON(text)
  if (!is.null(j$error_id)) {
    stop(paste0("Error ", j$error_id, ": ", j$error_message))
  }
  items <- j$items
  
  if (length(items) == 0 || nrow(items) == 0) {
    return(NULL)
  }
  
  # fix tags to be comma-separated
  if (!is.null(items$tags)) {
    items$tags <- sapply(items$tags, paste, collapse = ",")
  }
  # "shallow user" ends up being a data.frame. Turn it into separate
  # columns
  if (any(sapply(items, is.data.frame))) {
    items <- jsonlite::flatten(items)
  }
  # replace dots, as in owner.user_id, with _
  colnames(items) <- gsub("\\.", "_", colnames(items))
  # convert all dates, which fortunately always end in _date
  for (col in colnames(items)) {
    if (grepl("_date$", col)) {
      items[[col]] <- as.POSIXct(items[[col]], origin = "1970-01-01")
    }
  }
  
  # add metadata as an attribute
  attr(items, "metadata") <- j[-1]
  
  if (!is.null(j[-1]$backoff)) {
    message("Response has backoff parameter: must wait ",
            j[-1]$backoff, " seconds before performing same method")
  }
  
  items
}


#' Make a GET request to the Stack Exchange API
#'
#' @param path the query path, such as "answers/" or "users/{id}"
#' @param site site to query; by default Stack Overflow
#' @param page which page to start from
#' @param num_pages number of consecutive pages to query; by default 1
#' @param ... additional parameters to the method
stack_GET <- function(path, site = "stackoverflow", page = 1, num_pages = 1400, ...) {
  # auth <- github_auth(pat)
  base_path <- "https://api.stackexchange.com/2.2/"
  query <- list(site = site, page = page, ...)
  
  Sys.setenv(STACK_EXCHANGE_KEY = "k5keDy8akn7x5gh5bkM5Pg")
  stack_key <- Sys.getenv("STACK_EXCHANGE_KEY")
  
  
  if (stack_key != "") {
    query$key <- stack_key
  }
  
  tbls <- NULL
  tbl <- NULL
  while (num_pages > 0) {
    req <- httr::GET(base_path, path = path, query = query)
    
    tbl <- stack_parse(req)
    tbls <- c(tbls, list(tbl))
    
    metadata <- attr(tbl, "metadata")
    
    if (!is.null(metadata$backoff)) {
      Sys.sleep(metadata$backoff)
    }
    
    if (!metadata$has_more) {
      # finished pagination, can quit
      break
    }
    
    # set up for next iteration
    query$page <- query$page + 1
    num_pages <- num_pages - 1
  }
  
  # combine them all
  ret <- as.data.frame(dplyr::bind_rows(tbls))
  attr(ret, "metadata") <- attr(tbl, "metadata")
  ret
}


#' construct a query URL for a request, including checking special
#' operations
#'
#' @param base base of query, such as "answers" or "questions"
#' @param id vector of IDs to search
#' @param special special parameter, which specifies the action (such as
#' retrieving an associated object with an ID)
#' @param special_ids vector of possible special parameters that require IDs
#' @param special_no_ids vector of possible special parameters that don't
#' require IDs
combine_url <- function(base, id, special = NULL, special_ids = c(),
                        special_no_ids = c()) {
  url <- paste0(base, "/")
  
  if (!is.null(id)) {
    url <- paste0(url, paste(id, collapse = ";"), "/")
  }
  
  if (!is.null(special)) {
    special <- match.arg(special, c(special_ids, special_no_ids))
    
    if (is.null(id)) {
      if (!(special %in% special_no_ids)) {
        stop(paste(special, "requires one or more IDs"))
      }
    } else {
      if (!(special %in% special_ids)) {
        stop(paste(special, "does not accept IDs"))
      }
    }
    url <- paste0(url, special)
  }
  
  url
}

# getting posts

stack_posts <- function(id = NULL, special = NULL, ...) {
  special_ids <- c("comments", "revisions", "suggested-edits")
  url <- combine_url("posts", id, special, special_ids)
  stack_GET(url, ...)
}

posts <- stack_posts()
write.csv(posts, posts.csv)

# getting users

stack_users <- function(id = NULL, special = NULL, ...) {
  special_id<- c("top-answer-tags", "top-question-tags", "top-tags",
                 "privileges", "notifications")
  special_ids <- c("answers", "badges", "comments", "favorites", "mentioned",
                   "network-activity", "posts", "questions", "reputation",
                   "reputation-history", "suggested-edits", "tags",
                   special_id)
  
  special_no_ids <- c("moderators")
  
  if ((!is.null(special) && (special %in% special_id)) && length(id) > 1) {
    stop(paste(special, "can be used only with a single ID"))
  }
  
  url <- combine_url("users", id, special, special_ids, special_no_ids)
  
  stack_GET(url, ...)
}

users <- stack_users()
write.csv(users, users.csv)

# getting tags

stack_tags <- function(name = NULL, special = NULL, ...) {
  if (!is.null(name) && is.null(special)) {
    # tags has a different naming convention, where "info" extracts from tags
    special <- "info"
  }
  special_ids <- c("faq", "related", "synonyms", "wikis", "info")
  special_no_ids <- c("moderator-only", "required", "synonyms")
  
  # TODO: top answerers/askers in a tag
  
  url <- combine_url("tags", name, special, special_ids, special_no_ids)
  stack_GET(url, ...)
}

tags <- stack_tags()
write.csv(tags, tags.csv)
