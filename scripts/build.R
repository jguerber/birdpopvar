print(paste(
  "Current library path:",
  renv::paths$library()
))

# renv::install("here", library = renv::paths$library())
devtools::load_all()

targets::tar_make()

# print worker info
print(targets::tar_crew())
