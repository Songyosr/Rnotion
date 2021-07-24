library(data.table)
fdt <- fread("test_site/filter_table.csv")
fdt
names(fdt) <- c("Property", "Type", "Description", "Example")

# Belong to
fdt[, group := as.character(NA)]
header <- fdt[,.I[Property == "Property"] - 3]
fdt[header, group := gsub(" filter condition","", Property) |>
      tolower() |> (\(x)gsub("-","_",x))()]

# Extract
fdt <- fdt |>
  tidyr::fill(group) |>
  dplyr::filter(!is.na(Type) & Type != "" &  Type != "Type") |>
  dplyr::select(group, Property) |>
  dplyr::mutate()|>
  dplyr::add_row(group = "object", Property = "value")

write.csv(fdt,"data/property_filter_reference.csv")
