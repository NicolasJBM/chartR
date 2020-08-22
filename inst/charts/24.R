library(ComplexHeatmap)
library(cluster)

base <- scale(mtcars)

pa = pam(base, k = 3)

chart <- Heatmap(
  base,
  name = "mtcars",
  split = paste0("pam", pa$clustering),
  row_names_gp = gpar(fontsize = 7)
)

