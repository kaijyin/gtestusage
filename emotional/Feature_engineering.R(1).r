# 加载必要的库
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(openxlsx)
library(jiebaR)
library(parallel)
library(stringr)


## Part 1: Rationality
setwd("/Users/jimingshang/R_codes/project")# 更改为你的目录
data <- read_csv("./data/original.csv")

# 转换为数值类型并计算成功率
data$`已筹金额` <- as.numeric(data$`已筹金额`)
data$`筹款目标` <- as.numeric(data$`筹款目标`)

# 提取省份信息
get_province <- function(location) {
  # 定义一个常见的省份列表
  provinces <- c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区", "辽宁省", "吉林省", "黑龙江省",
                 "上海市", "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "河南省",
                 "湖北省", "湖南省", "广东省", "广西壮族自治区", "海南省", "重庆市", "四川省",
                 "贵州省", "云南省", "西藏自治区", "陕西省", "甘肃省", "青海省", "宁夏回族自治区",
                 "新疆维吾尔自治区", "香港特别行政区", "澳门特别行政区", "台湾省")

  # 遍历省份列表，检查地点中是否包含某个省份
  for (province in provinces) {
    if (grepl(province, location)) {
      return(province)  # 找到匹配的省份，返回
    }
  }

  return(NULL)  # 如果没有匹配到任何省份，返回 NULL
}

data$province <- sapply(data$`项目介绍`, get_province)

# 人均GDP字典
gdp_dict <- c(
  '北京市' = 132678, '天津市' = 89765, '河北省' = 43562, '山西省' = 38547,
  '内蒙古自治区' = 39875, '辽宁省' = 56433, '吉林省' = 45261, '黑龙江省' = 37255,
  '上海市' = 144259, '江苏省' = 93214, '浙江省' = 90875, '安徽省' = 43871,
  '福建省' = 57562, '江西省' = 41255, '山东省' = 54632, '河南省' = 42985,
  '湖北省' = 50875, '湖南省' = 47981, '广东省' = 87456, '广西壮族自治区' = 37895,
  '海南省' = 65478, '重庆市' = 54876, '四川省' = 43258, '贵州省' = 37896,
  '云南省' = 39567, '西藏自治区' = 28756, '陕西省' = 47985, '甘肃省' = 36897,
  '青海省' = 38964, '宁夏回族自治区' = 41532, '新疆维吾尔自治区' = 46987,
  '台湾省' = 95321, '香港特别行政区' = 135789, '澳门特别行政区' = 136598
)

# 根据省份获取人均GDP
city2gdp <- function(city) {
  if (!is.null(city)) {
    return(gdp_dict[city])
  }
  return(NA)
}

data$gdp <- sapply(data$province, city2gdp)

# 判断是否有位置信息
loc_null <- function(gdp) {
  if (is.null(gdp)) {
    return(0)
  } else {
    return(1)
  }
}

data$is_location <- sapply(data$gdp, loc_null)

# 获取项目名称的省份
data$Tprovince <- sapply(data$`项目名称`, get_province)
data$Tis_location <- sapply(data$Tprovince, loc_null)

# 提取年份
data$year <- as.numeric(substr(data$`筹款时间开始`, 1, 4))

# 计算图片数量
num_photo <- function(detail) {
  pattern1 <- '图片'
  pattern2 <- '照片'
  match1 <- str_count(detail, pattern1)
  match2 <- str_count(detail, pattern2)
  return(match1 + match2)
}

data$photo <- sapply(data$`项目介绍`, num_photo)

# 处理日期验证
is_valid_date <- function(date_string) {
  tryCatch({
    as.Date(date_string, format="%Y/%m/%d")
  }, error = function(e) {
    return(NA)
  })
}

data$flag1 <- sapply(as.character(data$`筹款时间开始`), is_valid_date)
data$flag2 <- sapply(as.character(data$`筹款时间结束`), is_valid_date)

# 计算日期差
data$begin <- as.Date(data$`筹款时间开始`, format="%Y/%m/%d")
data$end <- as.Date(data$`筹款时间结束`, format="%Y/%m/%d")
data$duration <- as.numeric(difftime(data$end, data$begin, units = "days"))

# 计算与财务相关的词汇
money_dict <- c('钱', '家境', '经济', '贫穷', '财务状况', '不富裕', '钱财', '贫寒', '穷困',
                '贫苦', '困苦', '清寒', '清贫', '困穷', '穷苦', '贫困')

money_related <- function(text) {
  money_times <- 0
  word_list <- str_split(text, "")[[1]]
  for (money in money_dict) {
    if (money %in% word_list) {
      money_times <- money_times + 1
    }
  }
  return(ifelse(money_times >= 1, 1, 0))
}

data$Dmoney <- sapply(data$`项目介绍`, money_related)
data$Tmoney <- sapply(data$`项目名称`, money_related)


## Part 2: Credibility

# 创建统计结果
result_df <- data %>%
  count(项目执行方, name = "donor_authority")

data <- merge(result_df, data, by = "项目执行方", all = FALSE)

# 自定义Gunning Fog Index函数
gunning_fog_index <- function(text) {
  text <- as.character(text)

  # 分割句子
  sentences <- strsplit(text, split = "[.!?]")[[1]]
  sentence_count <- length(sentences)

  # 分割单词
  words <- strsplit(text, split = "\\s+")[[1]]
  word_count <- length(words)

  # 计算复杂单词数（这里简化为0）
  complex_word_count <- 0

  # 计算Gunning Fog Index
  gfi <- 0.4 * ((word_count / sentence_count) + (complex_word_count / word_count * 100))

  return(gfi)
}

# 计算Gunning Fog Index
data$note_GFI <- sapply(data$`票据说明`, gunning_fog_index)

# 定义单位换算比例
unit_mapping <- c(元 = 1, 千元 = 1000, 万元 = 10000, 亿元 = 100000000)

# 定义提取和求和金额的函数
extract_and_sum_amounts <- function(text) {
  pattern <- "(\\d+(?:\\.\\d+)?)(元|千元|万元|亿元)"
  matches_df <- strcapture(pattern, text, data.frame(amount = numeric(), unit = character()))

  if (nrow(matches_df) == 0) {
    return(0)
  }

  # Convert amount to numeric
  matches_df$amount <- as.numeric(as.character(matches_df$amount))

  # Check for valid units
  valid_units <- matches_df$unit %in% names(unit_mapping)
  if (any(!valid_units)) {
    warning(paste("Invalid units found:", matches_df$unit[!valid_units]))
  }

  # Sum only valid amounts
  total_amount <- sum(matches_df$amount[valid_units] * unit_mapping[matches_df$unit[valid_units]])
  if (total_amount > 0){
    return (1)
  }
  else return (0)
}

# 提取“执行计划”列并转换为字符型
data$`执行计划` <- as.character(data$`执行计划`)

# 计算每个文本的捐款总额
data$disclosure <- sapply(data$`执行计划`, function(x) {
  if (!is.na(x) && x != "") {
    extract_and_sum_amounts(x)
  } else {
    0
  }
})

# 定义需要的词性集合
categories <- c("n", "a", "d", "v")  # jiebaR 中的词性标签

# 初始化 jiebaR 分词器
cutter <- worker(type = "tag")

# 初始化exec_LD变量
data$exec_LD <- NA

# 循环处理每个描述，计算词汇密度，但是由于该变量缺失值过多，最终没有加入回归
for (idx in 1:length(data$`执行计划`)) {
  description <- capability_description[idx]
  if (!is.na(description)) {
    # 中文分词和词性标注
    tokenized_sent <- segment(description, mix_cutter)

    # 确保tokenized_sent包含token和POS列
    if (length(tokenized_sent) > 0) {
      # 计算符合条件的词数
      total_count <- sum(names(tokenized_sent) %in% categories)

      # 计算词汇密度
      LD_value <- (total_count / length(tokenized_sent)) * 100
    } else {
      LD_value <- 0
    }

    # 将结果添加到结果数据框
    data$exec_LD[idx] <- LD_value
  }
}


## Part 3: Emotional
# 初始化 jiebaR 分词器,mix精确模式等于lcut
mix_cutter <- worker(type = "mix")

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
    words <- segment(text, mix_cutter)

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
      words <- segment(text, mix_cutter)
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
    words <- segment(text,mix_cutter)
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

