library(BiocManager)
BiocManager::install(version = "3.18", update = FALSE, ask = FALSE)

if ("RcppEigen" %in% rownames(installed.packages())) {
  remove.packages("RcppEigen")
}
BiocManager::install("DESeq2", ask = FALSE)

if (packageVersion("DESeq2") == "1.24.0") {
  print("DESeq2 VERSION IS 1.24.0 !")
} else {
  print("PROBLEM: DESeq2 VERSION IS NOT 1.24.0 !")
}
