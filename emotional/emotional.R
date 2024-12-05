
library(dplyr)
library(readr)
library(jiebaR)
library(purrr)
library(hash)
library(parallel)
library(stringr)

# 初始化 jiebaR 分词器,精确模式等于lcut
cutter <- worker(type = "mix")

# 标题文本处理
process_title <- function(data) {
  titles <- c()
  for (title in data) {
    if (!is.na(title) && title!="" && is.character(title)) {
      if (substr(title, 1, 4) == "慈善募捐") {
        titles <- c(titles, substr(title, 8, nchar(title) - 7))
      } else {
        titles <- c(titles, title)
      }
    } else {
      titles <- c(titles, title)
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
    if (!is.na(detail) && detail!="" && is.character(detail)) {
      match <- c()
    #   print(paste("here detail:", detail))
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

  # 处理 detail_ 中的文本
  for (detail in detail_) {
    if (!is.na(detail) && detail != "") {
      text <- str_replace_all(detail, "', '", "")
      details <- c(details, text)
    } else {
      details <- c(details, detail)
    }
  }

  return(list(photo = photo, details = details))
}


# jieba分词，返回分出来的词，同时剔除非中文非数字字符；处理对象为文本
# 清洗文本并进行分词
clean <- function(text) {
  # 去除非中文和非数字字符
  pattern <- '[^\u4e00-\u9fa5\\d]'  # 匹配非中文字符和非数字字符
  clean_words <- c()

  if (!is.na(text) && text!="" && is.character(text)) {
    # 使用 jiebaR 的分词器进行中文分词
    words <- segment(text, cutter)

    # 对每个词进行处理
    for (word in words) {
      clean_word <- gsub(pattern, "", word)  # 去掉非中文和非数字字符
      if (clean_word != "") {
        clean_words <- c(clean_words, clean_word)  # 保存有效的词汇
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
    if (!is.na(text) && text != "" && is.character(text)) {
      exclamation_count <- str_count(text, fixed("？")) + str_count(text, fixed("?"))
      qmark_count <- str_count(text, fixed("！"))
      Exclamation <- c(Exclamation, exclamation_count)
      QMark <- c(QMark, qmark_count)
    } else {
      Exclamation <- c(Exclamation, 0)
      QMark <- c(QMark, 0)
    }
  }
  return(list(Exclamation = Exclamation, QMark = QMark))
}

# 计算文档频率
document_frequency <- function(list_of_text) {
  # 初始化文档频率哈希表
  document_frequency <- hash()

  # 定义处理单个文本的函数
  process_text <- function(text) {
    if (!is.na(text) && text!="" && is.character(text)) {
      words <- segment(text, cutter)
      return(words)
    }
    return(NULL)
  }

  # 使用 map 函数处理文本
  words_list <- map(list_of_text, process_text)

  # 定义合并文档频率的函数
  update_document_frequency <- function(document_frequency, words) {
    if (!is.null(words)) {
      for (word in words) {
        if (has.key(word, document_frequency)) {
          document_frequency[[word]] <- document_frequency[[word]] + 1
        } else {
          document_frequency[[word]] <- 1
        }
      }
    }
    return(document_frequency)
  }

  # 使用 reduce 函数合并结果
  document_frequency <- reduce(words_list, update_document_frequency, .init = document_frequency)

  return(document_frequency)
}

# 计算熵
calculate_entropy <- function(text, list_of_text) {
  # 需要先有 document_frequency，可以用上一个函数获取。
  # 返回的是输入内容的熵
  if (!is.na(text) && text != "" && is.character(text)) {
    words <- segment(text,cutter)
    word_counts <- table(words)
    total_words <- length(words)
    entropy <- 0.0

    for (word in names(word_counts)) {
      count <- word_counts[[word]]
      probability <- count / total_words
      inverse_document_frequency <- log((length(list_of_text) + 1) / (document_frequencys[[word]] + 1))
      entropy <- entropy + inverse_document_frequency * probability * log(probability, 2)
    }

    entropy <- -entropy
  } else {
    entropy <- NA
  }

  return(entropy)
}


# 数据处理
data <- read_csv("precessed.csv")

# Title
titles <- process_title(data$项目名称)
data$TInfo <- get_info(titles)

# Brief
briefs <- data$项目简介
data$BInfo <- get_info(briefs)

# Detail
detail_result <- process_detail(data$`项目介绍`)
data$photo <- detail_result$photo

details <- detail_result$details
data$DInfo <- get_info(details)

marks <- get_mark(details)
data$Exclamation <- marks$Exclamation
data$QMark <- marks$QMark
document_frequencys <- document_frequency(details)
data$entropy <- sapply(details, calculate_entropy,list_of_text = details)

# 数据存到新的csv里面
write_csv(data, "precessed_need_python.csv")
# 后续的情绪因子还需要python生成
