#library(tidyverse)

solutions = parse_solutions("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Biofuels_SFA/Computational/ProbAnnotation/k-medioids/SampleFBA.json",
                             scale = F) |>
  dplyr::filter(model_no > 400 | model_no <= 200)


j = ensemble("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Biofuels_SFA/Computational/ProbAnnotation/k-medioids/SampleFBA.json")

# parse_rc("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Biofuels_SFA/Computational/ProbAnnotation/k-medioids/SampleFBA.json",
#          type = "biomass")


solutions |> dplyr::count(type)
solutions |> dplyr::filter(other_values == 0) |> dplyr::count(type)

# biomass histogram
plot_biomass(solutions)


# formatting the data for NMDS

# matrify_solutions(solutions, scale = FALSE)
# matrify_solutions(solutions, scale = TRUE)


# NMDS

scaled.e = matrify_solutions(solutions, scale = TRUE) |>
  ordinate_solutions(distance = "manhattan", dim = 2)

scaled.e.df = nmds_to_df(scaled.e)

unscaled.e = matrify_solutions(solutions, scale = FALSE) |>
  ordinate_solutions(distance = "manhattan", dim = 2)

unscaled.e.df = nmds_to_df(unscaled.e)


plot(scaled.e)
plot(unscaled.e)

vegan::scores(scaled.e)
vegan::scores(unscaled.e)



# PAM

scaled.pam = matrify_solutions(solutions, scale = TRUE) |>
  cluster_solutions(k = 4, metric = "euclidean")

scaled.pam$silinfo$widths






scaled.e.nmds = scaled.e.df %>%
  dplyr::left_join(scaled.pam$silinfo$widths %>%
  as.data.frame() %>%
  tibble::rownames_to_column("model"),
            by = "model") %>%
  dplyr::select(model, dplyr::starts_with("NMDS"), PAM = cluster, sil_width) %>%
  dplyr::mutate(PAM = paste0("PAM", PAM))


scaled.pam.medioids = scaled.e.nmds %>%
  dplyr::filter(model %in% rownames(scaled.pam$medoids))





scaled.e.nmds %>%
  ggplot2::ggplot(ggplot2::aes(x = NMDS1, y = NMDS2, fill = PAM)) +
  ggplot2::geom_point(pch = 21, alpha = .5, size = 2) +
    #scale_fill_manual(values = palette_jak$bay(5)) +
  ggplot2::geom_point(data = scaled.pam.medioids, pch = 4, size = 5, stroke = 2, ggplot2::aes(color = PAM, fill = NA), show.legend = FALSE) +
    ggrepel::geom_text_repel(data = scaled.pam.medioids, size = 5, ggplot2::aes(label = PAM, color = PAM, )) +
 #ggplot2::scale_color_manual(values = palette_jak$bay(5)) +
  ggplot2::labs(title = "Scaled Manhattan")
