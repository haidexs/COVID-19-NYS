aa = read_csv("./NYC_dataset.csv", col_names = TRUE)

for (i in 1:297) {
  aa[i, 1] = paste0(aa[i, 1], "/20")
}

for (i in 298:nrow(aa)) {
  aa[i, 1] = paste0(aa[i, 1], "/21")
}

write.csv(aa, './NYC_dataset_mmddyy.csv', row.names = FALSE)
