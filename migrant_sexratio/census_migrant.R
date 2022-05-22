library(ggplot2)
library(dplyr)
path = 'C:/Users/WXY/Documents/project/project_census/'
list.files(path)
# 女性倾向于居住于外来人口少的地方，是因为那些地方more safe? more expensive? 
# more occupations that are female-dominated/gender-neutral?
# more
census_bj = openxlsx::read.xlsx(paste0(path, 'census_beijing_2010.xlsx'), sheet = 1)
colnames(census_bj)

streetpop_sh = openxlsx::read.xlsx(paste0(path, 'census_shanghai_2010.xlsx'), sheet='nonlocal')

# create new variable disctrict names
streetpop_sh['district'] = NA
grep('^[\u4e00-\u9fff]{1,3}区$', streetpop_sh$地区, value=TRUE)
idx_district = grep('^[\u4e00-\u9fff]{1,3}区$', streetpop_sh$地区)
idx_district = idx_district[-length(idx_district)]
streetpop_sh[idx_district, 'district'] = streetpop_sh[idx_district, '地区']
streetpop_sh[,'district'] = c("上海市", zoo::na.locf(streetpop_sh[, 'district']))

streetpop_sh %>%
  filter(`log(本地人口性别比)` < -0.3)

# delete streets where population is far too less
streetpop_sh = streetpop_sh[streetpop_sh['常住人口_合计'] > 500,]
streetpop_sh = streetpop_sh[streetpop_sh['常住外来人口_合计'] > 50,]
streetpop_sh = streetpop_sh[streetpop_sh['常住外来人口_合计'] > 50,]
streetpop_sh = streetpop_sh %>%
  filter(`常住人口_合计` > 500 & (`常住外来人口_合计` > 50) & (`常住人口_合计` - `常住外来人口_合计` > 50))

colnames(streetpop_sh)

ggplot(streetpop_sh %>% filter(地区 != district)) +
  geom_point(
    aes(`常住外来人口占比`, `log(外来人口性别比)`, color = 'red')
  ) +
  geom_hline(yintercept = 0, linetype='dashed') +
  geom_point(
    aes(`常住外来人口占比`, `log(本地人口性别比)`, color = 'blue')
  )

# wide to long

# 非家庭户比例

# 离开户籍地的原因（仅有区县层面数据）
streetpop_sh['reason_migrant'] = 
  
# 不同学历的外来人口男性和女性居住的模式

# 分解外来人口对各个地区（区县一级，以及较大的街道）的性别比升高贡献率
# 可以考虑年龄，地域，学历等情况

# beijing -----------------------------------------------------

census_bj['district'] = NA
census_bj[!grepl(' ', census_bj$地区), 'district'] = census_bj[!grepl(' ', census_bj$地区), '地区']
census_bj[, 'district'] = zoo::na.locf(census_bj[, 'district'])

census_bj[, 'standardized_ratio'] = (census_bj[, '常住外来人口_男']/census_bj[, '常住人口_男'])/(
  census_bj[, '常住外来人口_女']/census_bj[, '常住人口_女']
)

census_bj %>%
  filter(district == '西城区' & 常住外来人口中男性占比 > 0.6)

census_bj %>%
  filter(district == 地区 & 地区 != '总计') %>%
  ggplot(data = .,
         aes(常住外来人口占比, 常住外来人口中男性占比)) +
  geom_point() +
  geom_text(aes(label=地区))

ggplot(census_bj %>% filter(地区 != district)) +
  geom_point(
    aes(常住外来人口_合计, 常住外来人口中男性占比, color = district)
  ) +
  geom_hline(yintercept = 0.5, linetype='dashed')
  # geom_smooth(aes(常住外来人口占比, 常住外来人口中男性占比, color = district), method = lm, se = FALSE) +
  # facet_wrap(vars(district), nrow = 4, strip.position = "top")

# micro census ------------------------------------------------------------

census_15 <- haven::read_dta('D:/data/census/2015_mini_census/2015census.dta', encoding = "utf-8")
adm_cn <- readRDS("D:/data/DATA_R/adm_cn.rds")

# labels
vars_labels = sapply(census_15,
                     function(x){ attr(x,"label") })
vars_labels = do.call(rbind, vars_labels)
vars_labels = data.frame(vars=row.names(vars_labels), labels=vars_labels, row.names = NULL)


# 外来常住人口
# 问题 1 您家现住房的详细地址？
#'m1 区
#'m2 省地县码
#'m38	户口登记地址？1: 相同
#'m39	2、户口登记地址与本户现住房地址不同（填省地县码）      
#'m40	调查时点居住地址？
#'m41	2、其他地区（填写省地县码）
#'m42	在本市居住时间？
#'m43	离开户口登记地的时间？ 
#'m44	离开户口登记地的原因？

# 居住时间半年以上，户口不在本地的人
head(census_15[c('m38', 'm42')], n=20)
sum(census_15['m38'] == 2)
table(census_15[census_15['m38'] == 2, c('m42')])
table(census_15[census_15['m38'] == 2 & census_15['m42'] != 1, c('m44')])

n_beijing = nrow(census_15[census_15['provcode'] == '11', ])  # 
nrow(census_15[census_15['provcode'] == '11' & census_15['m38'] == '1', ])
n_beijingMigrant = nrow(census_15[census_15['provcode'] == '11' &  # 居住地在
                                    census_15['m38'] == 2 &
                                    substring(census_15$m39, 1, 2) != '11' &  # 户口登记地在省外
                                    !census_15$m43 %in% c(2), ])  # 离开户口登记地时间半年以上
n_beijingMigrant = nrow(census_15[census_15['provcode'] == '11' &  # 居住地在
                                    census_15['m38'] == 2 &
                                    !census_15$m43 %in% c(2), ])  # 离开户口登记地时间半年以上
n_beijingMigrant/n_beijing
n_beijingMigrant/15196
substr('123', 1, 2)
census_15[1:20, c('m38', 'm39', 'm40', 'm41')]

# shanghai
n_shanghai = nrow(census_15[census_15['provcode'] == '31', ])
n_shanghaiMigrant = nrow(census_15[census_15['provcode'] == '31' & census_15['m38'] == 2 & census_15['m42'] != 1, ])
n_shanghaiMigrant/n_shanghai
180600/335775
119128/335775


