#' parse reactions and compounds
#'
#' @export

parse_solutions <- function(file_path,
                            min_value = 0,
                            scale = FALSE,
                            quiet = FALSE) {
  if (isFALSE(quiet)) {
    message(glue::glue("=== Parsing {basename(file_path)} ==="))
    message(glue::glue("Data scaling: {scale}"))
  }

  compounds <- parse_rc(file_path,
    type = "compounds",
    min_value = min_value,
    scale = scale
  )

  if (isFALSE(quiet)) {
    message(glue::glue("Found {length(unique(compounds$RC))} compounds"))
  }

  reactions <- parse_rc(file_path,
    type = "reactions",
    min_value = min_value,
    scale = scale
  )

  if (isFALSE(quiet)) {
    message(glue::glue("Found {length(unique(reactions$RC))} reactions"))
  }

  biomass <- parse_rc(file_path,
    type = "biomass",
    min_value = min_value,
    scale = scale
  )

  if (isFALSE(quiet)) {
    message(glue::glue("Found {length(unique(biomass$RC))} biomass"))
  }

  RC <- rbind(
    compounds,
    reactions,
    biomass
  )

  return(RC)
}


#' parse reactions or compounds
#'
#' @export

parse_rc <- function(file_path,
                     type = "compounds",
                     min_value = 0,
                     scale = FALSE) {
  # check that file is found at file_path
  if (!file.exists(file_path)) {
    stop("file not found at file_path", call. = FALSE)
  }


  if (!type %in% c("compound", "compounds", "reaction", "reactions", "biomass")) {
    stop("type must be one of: compounds, reactions or biomass", call. = FALSE)
  }

  # check that scale is a boolean value
  if (!is.logical(scale)) {
    stop("scale must be a either TRUE or FALSE", call. = FALSE)
  }

  # check that min_value is 0 or greater
  if (min_value < 0) {
    stop("min_value must be 0 or greater", call. = FALSE)
  }


  # pull the correct portion of the json object
  if (type %in% c("compound", "compounds")) {
    j <- jsonlite::fromJSON(file_path)[["data"]][["FBACompoundVariables"]]
    type <- "compounds" # change to "compounds" in case "compound"
  } else if (type %in% c("reaction", "reactions")) {
    j <- jsonlite::fromJSON(file_path)[["data"]][["FBAReactionVariables"]]
    type <- "reactions" # change to "reactions" in case "reaction"
  } else if (type %in% c("biomass")) {
    j <- jsonlite::fromJSON(file_path)[["data"]][["FBABiomassVariables"]]
    type <- "biomass"
  }

  df <- j |>
    tibble::as_tibble() |>
    tidyr::unnest(other_values)

  if (type == "compounds") {
    df <- dplyr::rename(df, RC = modelcompound_ref)
  } else if (type == "reactions") {
    df <- dplyr::rename(df, RC = modelreaction_ref)
  } else if (type == "biomass") {
    df <- dplyr::rename(df, RC = biomass_ref)
  }

  df <- df |>
    dplyr::select(RC, class, variableType, other_values, lowerBound, upperBound, min, max, value) |>
    dplyr::group_by(RC) |>
    dplyr::mutate(model_no = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      other_values = ifelse(abs(other_values) < min_value, 0, other_values),
      type = type
    )

  if (scale == TRUE) {
    df <- df |>
      dplyr::group_by(RC) |>
      dplyr::mutate(other_values = dplyr::case_when(
        max(abs(other_values)) == 0 ~ 0,
        TRUE ~ other_values / max(abs(other_values))
      )) |>
      dplyr::ungroup()
  }


  return(df)
}
