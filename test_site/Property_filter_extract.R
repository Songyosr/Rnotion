library(data.table)
fdt <- fread("test_site/filter_table.csv")
fdt
names(fdt) <- c("Property", "webType", "Description", "Example")

# Belong to
fdt[, group := as.character(NA)]
header <- fdt[,.I[Property == "Property"] - 3]
fdt[header, Type := gsub(" filter condition","", Property) |>
      tolower() |> (\(x)gsub("-","_",x))()]

# Extract
fdt <- fdt |>
  tidyr::fill(Type) |>
  dplyr::filter(!is.na(webType) & webType != "" &  webType != "Type") |>
  dplyr::select(Type, Property) |>
  #dplyr::mutate()|>
  dplyr::add_row(Type = "*Search_API", Property = "value")

write.csv(fdt,"data/property_filter_reference.csv")
