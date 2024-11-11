library(tidyverse) 
#(dplyr：简化数据的操作，常用于过滤、排序、变换数据)(tidyr:数据整理)(readr)(ggplot2:数据可视化)(stringr)
library(sf) # 用于读取空间数据
library(here)
library(janitor)#整理所有内容
library(readr)# 用于读取 CSV 文件
library(readxl)
library(dplyr)
#read data
Global_gender_inequality<- read_csv(here::here("hdr-data-2.csv"))
Spatial_data_world<- st_read("World_Countries_(Generalized)_9029012925078512962.geojson")

#检查数据类型
Datatypelist <- Global_gender_inequality %>% 
  summarise_all(class) %>% #summarise_all(class) 就是一个快速查看数据集里每一列是什么类型的工具。
  pivot_longer(everything(),
              names_to="All_variables", 
               values_to="Variable_class")


# 清理列名并合并数据
mergedgii_data <- Spatial_data_world %>%
  clean_names() %>%
  left_join(Global_gender_inequality, by = "country")  # 注意：左连接时，"country" 列名应在两表中一致

# 将数据转换为宽格式
  gii_data <- mergedgii_data %>%
  pivot_wider(names_from = year, values_from = value)

# 计算不平等指数的差异并创建新列
  gii_data <- gii_data %>%
    mutate(inequality_diff = `2010` - `2019`)
  
  #画图
  ggplot(gii_data) +
    geom_sf(aes(fill = inequality_diff)) +
    scale_fill_viridis_c(name = "GII Difference", option = "C") +
    labs(title = "Global Gender Inequality Index Difference ") +
    theme_minimal()