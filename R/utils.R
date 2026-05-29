gm_modify_thread_fixed <- function (id,
                                    add_labels = character(0),
                                    remove_labels = character(0),
                                    user_id = "me") {

  dots <- function(...) {
    eval(substitute(alist(...)))
  }

  name_map <- c(
    "user_id" = "userId",
    "search" = "q",
    "num_results" = "maxResults",
    "add_labels" = "addLabelIds",
    "remove_labels" = "removeLabelIds",
    "page_token" = "pageToken",
    "include_spam_trash" = "includeSpamTrash",
    "start_history_id" = "startHistoryId",
    "label_list_visibility" = "labelListVisibility",
    "message_list_visibility" = "messageListVisibility",
    "upload_type" = "uploadType",
    "label_ids" = "labelIds",
    NULL
  )

  gmailr_rename <- function (...)
  {
    args <- dots(...)
    arg_names <- names(args)
    missing <- if (is.null(arg_names)) {
      rep(TRUE, length(args))
    }
    else {
      !nzchar(arg_names)
    }
    arg_names[missing] <- vapply(args[missing], deparse, character(1))
    to_rename <- arg_names %in% names(name_map)
    arg_names[to_rename] <- name_map[arg_names[to_rename]]
    vals <- list(...)
    names(vals) <- arg_names
    vals
  }

  body <- gmailr_rename(add_labels = add_labels, remove_labels = remove_labels)
  req <- httr::POST(gmailr:::gmail_path(gmailr_rename(user_id), "threads", id, "modify"),
                    body = body, encode = "json", gmailr::gm_token())
  httr::stop_for_status(req)
  invisible(httr::content(req, "parsed"))
}
