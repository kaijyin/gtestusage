# 设置 CRAN 镜像
options(repos = c(CRAN = "https://cran.r-project.org"))
install.packages('IRkernel')
install.packages("languageserver")
install.packages("purrr")
install.packages("hash")
IRkernel::installspec()
