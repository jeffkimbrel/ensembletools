#' make a tibble from NMDS point data
#'
#' @export

nmds_to_df <- function(nmds) {
  vegan::scores(nmds) |>
    as.data.frame() |>
    tibble::rownames_to_column("model") |>
    dplyr::mutate(R = as.numeric(stringr::str_remove(model, "model")))
}
