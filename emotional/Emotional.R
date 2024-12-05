# install.packages("readr")
# install.packages("dplyr")
# install.packages("jiebaR")
library(dplyr)
library(jiebaR)
library(readr)
library(parallel)
library(stringr)
# 读取 CSV 文件
dataframe <- read_csv('precessed.csv')
# 查看数据框
# print(dataframe)

# 定义函数

# 标题文本处理
process_title <- function(data) {
  titles <- c()
  print(paste("数据长度:", length(data)))
  it<-0
  for (title in data) {
    if (!is.na(title)&&is.character(title)) {
      if (substr(title, 1, 4) == "慈善募捐") {
        titles <- c(titles, substr(title, 8, nchar(title) - 7))
      } else {
        titles <- c(titles, title)
      }
    } else {
      titles <- c(titles, title)
    }
    it <- it + 1  # 增加计数器
    if (it %% 1000 == 0) {
      print(paste("已处理:", it, "条记录"))
    }
  }
  return(titles)
}

# 项目介绍文本处理
process_detail <- function(data) {
  patterns <- c('（[^（）]*?图[^（）]*?）', '【[^【】]*?图[^【】]*?】', '（[^（）]*?照片[^（）]*?）', '【[^【】]*?照片[^【】]*?】')
  photo <- c() # 照片数量
  detail_ <- c() # 剔除这类信息后的文本
  details <- c() # 最终返回的文本
  for (detail in data) {
    if (is.character(detail)) {
      match <- c()
      for (pattern in patterns) {
        match <- c(match, str_extract_all(detail, pattern)[[1]])
        detail <- str_replace_all(detail, pattern, "")
      }
      photo <- c(photo, length(match))
      detail_ <- c(detail_, detail)
    } else {
      detail_ <- c(detail_, "")
      photo <- c(photo, 0)
    }
  }
  for (detail in detail_) {
    if (detail != "") {
      text <- str_replace_all(detail, "', '", "")
      details <- c(details, text)
    } else {
      details <- c(details, detail)
    }
  }
  return(list(photo = photo, details = details))
}

# jieba分词，返回分出来的词，同时剔除非中文非数字字符；处理对象为文本
clean <- function(text) {
  pattern <- '[^\u4e00-\u9fa5\\d]' # 用于剔除非中文非数字字符
  clean_words <- c()
  if (is.character(text)) {
    words <- segment(text, jiebaR::worker())
    for (word in words) {
      clean_word <- gsub(pattern, "", word)
      if (clean_word != "") {
        clean_words <- c(clean_words, clean_word)
      }
    }
  }
  return(clean_words)
}


# # 信息量,计算clean后分词的数量
get_info <- function(lst) {
  info <- c()
  for (item in lst) {
    clean_words <- clean(item)
    info <- c(info, length(clean_words))

  }
  return(info)
}

# 筛选强烈情感的符号
get_mark <- function(data) {
  Exclamation <- c()
  QMark <- c()
  for (text in data) {
    words <- segment(text, jiebaR::worker())
    Exclamation <- c(Exclamation, sum(words == '？') + sum(words == '?'))
    QMark <- c(QMark, sum(words == '！'))
  }
  return(list(Exclamation = Exclamation, QMark = QMark))
}

# 统计文档频率
document_frequency <- function(list_of_text) {
  # 初始化文档频率字典
  document_frequency <- list()

  # 遍历每个文本并更新文档频率字典
  for (text in list_of_text) {
    if (is.character(text)) {
      words <- segment(text, jiebaR::worker())
      for (word in words) {
        if (word %in% names(document_frequency)) {
          document_frequency[[word]] <- document_frequency[[word]] + 1
        } else {
          document_frequency[[word]] <- 1
        }
      }
    }
  }

  return(document_frequency)
}

# 计算熵
calculate_entropy <- function(text, list_of_text, document_frequency) {
  # 返回输入内容的熵
  if (is.character(text)) {
    words <- segment(text, jiebaR::worker())
    word_counts <- table(words)
    total_words <- length(words)
    entropy <- 0.0

    for (word in names(word_counts)) {
      count <- word_counts[[word]]
      probability <- count / total_words
      inverse_document_frequency <- log((length(list_of_text) + 1) / (document_frequency[[word]] + 1))
      entropy <- entropy + inverse_document_frequency * probability * log(probability, 2)
    }

    entropy <- -entropy
  } else {
    entropy <- NA
  }

  return(entropy)
}

# 数据处理

# Title
titles <- process_title(dataframe$项目名称)
TInfo <- get_info(titles)
dataframe <- dataframe %>%
  mutate(TInfo = TInfo)

# Brief
briefs <- dataframe$项目简介
BInfo <- get_info(briefs)
dataframe <- dataframe %>%
  mutate(BInfo = BInfo)

# Detail
detail_result <- process_detail(dataframe$项目介绍)
photo <- detail_result$photo
details <- detail_result$details
DInfo <- get_info(details)
marks <- get_mark(details)
Exclamation <- marks$Exclamation
QMark <- marks$QMark

document_frequency <- document_frequency(details)
entropy <- sapply(details, calculate_entropy, details = details,document_frequency = document_frequency)

dataframe <- dataframe %>%
  mutate(photo = photo, DInfo = DInfo, Exclamation = Exclamation, QMark = QMark, entropy = entropy)

# 数据存到新的csv里面
write_csv(dataframe, "precessed_1.csv")