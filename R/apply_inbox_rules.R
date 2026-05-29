my_threads <- gmailr::gm_threads(search = "is:unread")

seq(1, 23) |>
  purrr::map(function(i){

    thread <- gmailr::gm_thread(gmailr::gm_id(my_threads)[[i]])

    tibble::tibble(from = gmailr::gm_from(thread$messages[[1]]),
                   to = gmailr::gm_to(thread$messages[[1]]),
                   subject = gmailr::gm_subject(thread$messages[[1]]),
                   date = gmailr::gm_date(thread),
                   snippet = thread$messages[[1]]$snippet,
                   labels = thread$messages[[1]]$labelIds |>
                     unlist() |>
                     stringr::str_c(collapse = ";"),
                   id = thread$id) |>
      dplyr::mutate(year = stringr::str_extract(date, "\\d{4}$"),
                    date = stringr::str_remove(date, "\\d{4}$"),
                    date = stringr::str_remove(date, "^\\w{3} "),
                    date = stringr::str_c(year, " ", date),
                    date = lubridate::ymd_hms(date))
  }) |>
  purrr::list_rbind() ->
  result

library(tidyverse)
result |>
  mutate(labels = str_split(labels, ";")) |>
  unnest_longer(labels) |>
  count(labels, sort = TRUE)

inbox_rules <- read_csv("../../otter_space/inbox_rules.csv")


seq_along(inbox_rules$id) |>
  walk(function(i){
    result |>
      filter(from %in% inbox_rules$from[i],
             to %in% inbox_rules$to[i],
             str_detect(subject, inbox_rules$str_detect_subject[i])) |>
      pull(id) |>
      walk(ids, function(id){
        gm_modify_thread_fixed(id = id,
                               add_labels = inbox_rules$add_labels,
                               remove_labels = inbox_rules$remove_labels)
      })
  })
