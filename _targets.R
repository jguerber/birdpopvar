library(targets)
library(tarchetypes)

list(
  tar_target(
    hello,
    print("hello world")
  )
)
