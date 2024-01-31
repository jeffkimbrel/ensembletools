library(tidyverse)

solutions = parse_solutions("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Biofuels_SFA/Computational/ProbAnnotation/k-medioids/SampleFBA.json")

parse_rc("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Biofuels_SFA/Computational/ProbAnnotation/k-medioids/SampleFBA.json",
         type = "biomass")


solutions |> count(type)


# biomass histogram
bio = parse_rc("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Biofuels_SFA/Computational/ProbAnnotation/k-medioids/SampleFBA.json",
         type = "biomass") |>
  mutate(R = row_number()) |>
  filter(R > 400 | R <= 200)

median(bio$other_values)

bio |>
  filter(other_values > median(other_values)) |>
  ggplot(aes(x = other_values)) + geom_histogram()


bio |>
  ggplot(aes(x = model_no, y = other_values)) +
    geom_point()





# formatting the data for NMDS

## Unscaled
unscaled = solutions %>%
  select(RC, class, variableType, other_values) %>%
  mutate(other_values = ifelse(abs(other_values) < 1e-10, 0, other_values)) %>%
  group_by(RC) %>%
  mutate(R = paste0("model", row_number())) %>% # convert model # to a string
  tidyr::pivot_wider(names_from = R, values_from = other_values)

## Scaled
scaled = solutions %>%
  select(RC, class, variableType, other_values) %>%
  mutate(other_values = ifelse(abs(other_values) < 1e-10, 0, other_values)) %>%
  group_by(RC) %>%
  mutate(other_values = case_when(
    max(abs(other_values)) == 0 ~ 0,
    TRUE ~ other_values / max(abs(other_values))
  )) %>%
  mutate(R = paste0("model", row_number())) %>% # convert model # to a string
  tidyr::pivot_wider(names_from = R, values_from = other_values)






# NMDS

run_nmds = function(df, distance) {
  df %>%
    select(-class, -variableType) %>%
    column_to_rownames("RC") %>%
    t() %>%
    as.matrix() %>%
    vegan::metaMDS(distance = distance)
}

scaled.e = run_nmds(scaled, distance = "euclidean")




plot(scaled.e)

j = vegan::scores(scaled.e)
j
