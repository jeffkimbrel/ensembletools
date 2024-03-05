#' Plot a histogram of the biomass
#'
#' @export



plot_biomass = function(solutions) {

  # solutions must be class ensemble or a dataframe
  if ("ensemble" %in%class(solutions)) {
    s = solutions@solutions
  } else if ("data.frame" %in% class(solutions)) {
    s = solutions
  } else {
    stop(glue::glue("solutions must be class <ensemble> or a <data.frame>, not <{class(solutions)[1]}>."), call. = FALSE)
  }

  # warning if scaled
  if (isTRUE(solutions@scale)) {
    message("Warning: data is scaled so biomass values are likely not useful")
  }




  bio = s |>
    dplyr::filter(type == "biomass")

  bio |>
    #dplyr::filter(other_values > median(other_values)) |>
    ggplot2::ggplot(ggplot2::aes(x = other_values)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::geom_vline(xintercept = median(bio$other_values),
                        linetype = "dashed",
                        color = "red")
  #ggplot2::geom_density()

}





#' Plot Clusters
#'
#' @export

plot_clusters = function(x) {
  scores <- x@ordination$scores

  if (nrow(sil_widths(x)) > 0) { # add pam data if clustered
    scores <- scores |>
      dplyr::left_join(sil_widths(x), by = "model") |>
      dplyr::select(model, dplyr::starts_with("NMDS"), PAM = cluster, sil_width) |>
      dplyr::mutate(PAM = as.factor(PAM))
  } else { # add NA data if not clustered
    scores <- scores |>
      dplyr::mutate(PAM = "NA")
  }

  p <- scores %>%
    ggplot2::ggplot(ggplot2::aes(x = NMDS1, y = NMDS2, fill = PAM)) +
    ggplot2::geom_point(pch = 21, alpha = .5, size = 2) +
    ggplot2::labs(
      title = glue::glue("Distance: {x@distance}, Scaled: {x@scale}"),
      fill = "Cluster"
    ) +
    ggplot2::theme_minimal()

  if (nrow(sil_widths(x)) > 0) {
    p <- p +
      ggplot2::geom_point(
        data = scores |>
          dplyr::filter(model %in% medioids(x)),
        pch = 3, size = 2, stroke = 2, ggplot2::aes(color = PAM), show.legend = FALSE
      )
  }

  return(p)
}


