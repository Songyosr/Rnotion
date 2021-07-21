library("RefManageR")
library("bibtex")

#5902815
j <- ReadZotero(
  user = 5902815,
  group = 2395361,
  .params = list(
    q = "Nutrition",
    key = "eiwk2ti9iol9Bhzdy1lf0lr5",
    limit = 5)
)

RefManageR::fields(j)

as.data.frame(j) |> View()


fields(j)
