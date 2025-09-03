# Make the subdirectory ./data mirror the structure and contents of ../data (used to quickly import processed data in the code project)

message(
  "WARNING: this will remove the contents of ./data\n",
  "and replace them with ../data.\n"
)

check_ok <- readline("Proceed ? type 'yes'")

if (check_ok == "yes") {
  unlink(here::here("data"), recursive = T)

  dir.create(here::here("data"), recursive = T)
  file.copy(here::here("../data"), here::here(""), recursive = T)

} else {
  print("Cancelled by user")
}

