#' Ensemble data class
#'
#' @export

ensemble <- S7::new_class(
  "ensemble",
  properties = list(
    path = S7::class_character,
    solutions = S7::class_data.frame,
    scale = S7::class_logical,
    distance = S7::class_character,
    ordination = S7::class_list,
    clusters = S7::class_list
  ),
  constructor = function(path, scale = FALSE, quiet = FALSE) {
    # make sure file at path exists
    if (!file.exists(path)) {
      stop(glue::glue("No file not found at <{path}>"), call. = FALSE)
    }

    solutions <- parse_solutions(path,
      scale = scale,
      quiet = quiet
    )

    S7::new_object(S7::S7_object(),
      path = path,
      solutions = solutions,
      scale = scale,
      distance = character(),
      ordination = list(),
      clusters = list()
    )
  },
  validator = function(self) {

  }
)


#' Get information about an ensemble
#'
#' @export
#'

info <- S7::new_generic("info", "x")
S7::method(info, ensemble) <- function(x) {
  e@solutions |>
    dplyr::select(RC, type, class) |>
    unique() |>
    dplyr::count(class, type) |>
    dplyr::arrange(type)
}

#' NMDS stress value
#'
#' @export

stress <- S7::new_generic("stress", "x")
S7::method(stress, ensemble) <- function(x) {
  x@ordination$nmds$stress
}

#' Get Medioids
#'
#' @export

medioids <- S7::new_generic("medioids", "x")
S7::method(medioids, ensemble) <- function(x) {
  rownames(x@clusters$pam$medoids)
}


#' Get SIL values
#'
#' @export

sil_widths <- S7::new_generic("sil_widths", "x")
S7::method(sil_widths, ensemble) <- function(x) {
  x@clusters$pam$silinfo$widths |>
    as.data.frame() |>
    tibble::rownames_to_column("model") |>
    tibble::as_tibble()
}

