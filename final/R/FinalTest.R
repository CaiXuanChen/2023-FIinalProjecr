library(tidyverse)
library(readxl)
library(tidyr)
library(tidyselect)
# import helper functions
source("R/helpers.R") 

#資料匯入
dataForAll <- list()

### 各區

dataForAll$各類別<- read.csv(
  "C:/Users/caisyuan/OneDrive/桌面/2023-FIinalProjecr/final/R/臺南市各區各類別之診所、醫院.csv")
dataForAll$各類別<- dataForAll$各類別[, -c(2,4)]

#### correct columnames

correctDFColumnNames(df=dataForAll$各類別) ->
  dataForAll$各類別
correctDFColumnNames(dataForAll$countyMayor) ->
  dataForAll$countyMayor

### 藥局

dataForAll$藥局<- read.csv(
  "C:/Users/caisyuan/OneDrive/桌面/2023-FIinalProjecr/final/R/臺南市健保特約藥局.csv")
dataForAll$藥局<- dataForAll$藥局[, -c(3)]
dataForAll$診所<- read.csv(
  "C:/Users/caisyuan/OneDrive/桌面/2023-FIinalProjecr/final/R/臺南市醫療院所.csv")
dataForAll$診所<- dataForAll$診所[, -c(3)]


dataForAll$藥局 |> 
  dplyr::glimpse()

saveRDS(dataForAll, file="dataForAll.Rds")
dataForAll=readRDS("dataForAll.Rds")

# 拉長

dataForAll$municipalityMayor |>
  dplyr::glimpse()

dataForAllLong <- list()
dataForAll$municipalityMayor |> tidyr::pivot_longer(
  cols = c(2:9), names_to ="黨派", values_to ="得票數"
) -> dataForAllLong$municipalityMayor

# dataForAllLong$municipalityMayor |> View()


dataForAll$countyMayor |> tidyr::pivot_longer(
  cols = c(2:8), names_to ="黨派", values_to ="得票數"
) -> dataForAllLong$countyMayor
dataForAll$municipalityCouncillors |> tidyr::pivot_longer(
  cols = c(2:11), names_to ="黨派", values_to ="席次"
) -> dataForAllLong$municipalityCouncillors
dataForAll$countyCouncillors |> tidyr::pivot_longer(
  cols = c(2:12), names_to ="黨派", values_to ="席次"
) -> dataForAllLong$countyCouncillors


View(dataForAllLong$藥局)
View(dataForAllLong$診所)
View(dataForAllLong$醫院)




## 台南市各區域哪類別診所最多

### dplyr::filter

dataForAllLong$municipalityMayor |> dplyr::glimpse()

address_df <- data.frame()
address_df$address <- c("地址1", "地址2", "地址3", ..., "地址600")  # 替換 "地址1", "地址2", "地址3", ..., "地址600" 為實際的地址資料，需按照相應的順序

# 假設你有一個地址的資料框 data.frame，其中包含一個名為 "地址" 的欄位
addressData <- data.frame(
  地址 = c("台北市中正區忠孝西路一段", "台北市大同區民權西路", "台北市中山區南京東路一段", "台北市中正區林森南路")
)

# 使用正則表達式從地址中提取區名稱
districts <- sub(".*([市縣].+[區鄉鎮市區]).*", "\\1", addressData$地址)

# 計算每個區的資料筆數
districtCounts <- table(districts)

# 輸出結果
print(districtCounts)




targetDF = dataForAllLong$municipalityMayor
# filter 台北市
targetDF |>
  dplyr::filter(
    行政區別 == "臺北市" # logical expression
  )  -> dfTaipei


### dplyr::summarise

#### summarise 等號右邊


dfTaipei$黨派
dfTaipei$得票數
{
  pos<-which.max(dfTaipei$得票數)
  dfTaipei$黨派[[pos]]
}

party = dfTaipei$黨派
voteShare = dfTaipei$得票數
{
  pos<-which.max(voteShare)
  party[[pos]]
}


getWinningParty = function(party, voteShare){
  pos<-which.max(voteShare)
  return(party[[pos]])
}

getWinningParty(dfTaipei$黨派, dfTaipei$得票數)


dfTaipei |>
  dplyr::summarise(
    winningParty = getWinningParty(黨派,得票數)
  )


## 各行政區市長黨派

### dplyr::group_by

## replace `filter` with `group_by`


dataForAllLong$municipalityMayor |>
  # dplyr::filter(
  #   行政區別 == "臺北市" # logical expression
  # ) |>
  dplyr::group_by(
    行政區別
  ) |>
  dplyr::summarise(
    winningParty = {
      pos<-which.max(得票數)
      黨派[[pos]]
    }
    # getWinningParty(黨派,得票數)
  ) -> dataForAll$winningParty$municipalMayor



getWinningParty = function(party, voteShare){
  pos<-which.max(voteShare)
  return(party[[pos]])
}


dataForAllLong$countyMayor |> View()



dataForAllLong$countyMayor |>
  dplyr::group_by(
    行政區別
  ) |>
  dplyr::summarise(
    winningParty = 
      getWinningParty(黨派,得票數)
  ) -> dataForAll$winningParty$countyMayor

saveRDS(dataForAll, "dataForAll.Rds")





## 議會

dataForAllLong$municipalityCouncillors |> View()

dataForAllLong$municipalityCouncillors |>
  dplyr::group_by(
    行政區別
  ) |>
  dplyr::summarise(
    councilMajorityParty = getWinningParty(黨派,席次),
    totalSeats = sum(席次, na.rm=T),
    majoritySeatNumber = {
      max(席次, na.rm=T)
    }
  ) -> dataForAll$winningParty$municipalityCouncillors

dataForAllLong$countyCouncillors |>
  dplyr::group_by(
    行政區別
  ) |>
  dplyr::summarise(
    councilMajorityParty = getWinningParty(黨派,席次),
    totalSeats = sum(席次, na.rm=T),
    majoritySeatNumber = {
      max(席次, na.rm=T)
    }
  ) -> dataForAll$winningParty$countyCouncillors
View(dataForAll$winningParty$countyCouncillors)


## 合併

dataForAll$winningParty$municipalMayor |> View()


### dplyr::xxx_join


dataForAll$winningParty$municipalMayor |>
  dplyr::left_join(
    dataForAll$winningParty$municipalityCouncillors,
    by=c("行政區別")
  ) ->
  dataForAll$result$municipal

View(dataForAll$result$municipal)

dataForAll$winningParty$countyMayor |>
  dplyr::left_join(
    dataForAll$winningParty$countyCouncillors,
    by="行政區別"
  ) ->
  dataForAll$result$county

View(dataForAll$result$county)

dataForAll$countyCouncillors |> View()


### 格式更正


dataForAll$municipalityCouncillors |> View()


View(mayorTable)
View(councillorTable)


# 整理-市長

#行列再次互換
spread(mayorTable, key = "行政區別", value = "得票數")->mayorTableNew
#補入嘉義市選舉結果
mayorTableNew$"嘉義市" =c(59874,0,0,0,0,0,0,0,32790,0,0)
#下行名
rownames(mayorTableNew) <-mayorTableNew$黨派[1:11]
mayorTableNew <- mayorTableNew[-1]
#處理數值
mayorTableNew <- mutate_all(mayorTableNew, ~replace(., is.na(.), 0))
#把character選出來
char_columns <- sapply(mayorTableNew, is.character)
#character 變成numeric
mayorTableNew[ , char_columns] <- as.data.frame(apply(mayorTableNew[ , char_columns], 2, as.numeric))
mayorTableNew <- data.frame(mayorTableNew, stringsAsFactors = F)
#排序縣市
mayorTableNew[,order(names(mayorTableNew))] -> mayorTableNew


View(mayorTableNew)
str(mayorTableNew)

# 整理-議員

#議員 行列再次互換
spread(councillorTable, key = "行政區別", value = "席次")->councillorTableNew
#下行名
rownames(councillorTableNew) <- councillorTableNew$黨派[1:14]
councillorTableNew <- councillorTableNew[-1]
#處理數值
councillorTableNew <- mutate_all(councillorTableNew, ~replace(., is.na(.), 0))
char_columns2 <- sapply(councillorTableNew, is.character)
councillorTableNew[ , char_columns2] <- as.data.frame(apply(councillorTableNew[ , char_columns2], 2, as.numeric))
councillorTableNew <- data.frame(councillorTableNew, stringsAsFactors = F)
#排序縣市
councillorTableNew[,order(names(councillorTableNew))] ->councillorTableNew



View(councillorTableNew)
str(councillorTableNew)

sapply ( mayorTableNew, max )
sapply ( councillorTableNew, max )

names(mayorTableNew)
names(councillorTableNew)

# 合併

summarizedList =list()

#議員
for(.x in colnames(mayorTableNew)){
  mayorTableNew[[.x]] -> targetFeature
  councillorTableNew[[.x]] -> targetFeature2
  summarizedList[[.x]] <-list(
    "市長當選人黨派" = rownames(mayorTableNew[which(targetFeature == max(targetFeature)),]),
    # "市長當選人黨派" = rownames(mayorTableNew[1,]),
    "議員多數黨派" = rownames(councillorTableNew[which(targetFeature2 == max(targetFeature2)),]),
    "是否全面執政" = identical(rownames(mayorTableNew[which(targetFeature == max(targetFeature)),]) , rownames(councillorTableNew[which(targetFeature2 == max(targetFeature2)),]))
  )
}

View(summarizedList)
#summarizedList <- data.frame(summarizedList)

#二次合併

#方法一
summarizedList2 <- mapply(c, summarizedList$南投縣,summarizedList$嘉義市,summarizedList$嘉義縣,summarizedList$基隆市,summarizedList$宜蘭縣,summarizedList$屏東縣,summarizedList$彰化縣,summarizedList$新北市,summarizedList$新竹市,summarizedList$新竹縣,summarizedList$桃園市,summarizedList$澎湖縣,summarizedList$臺中市,summarizedList$臺北市,summarizedList$臺南市,summarizedList$臺東縣,summarizedList$花蓮縣,summarizedList$苗栗縣,summarizedList$連江縣,summarizedList$金門縣,summarizedList$雲林縣,summarizedList$高雄市 ,SIMPLIFY = F)
summarizedList2 <- data.frame(summarizedList2)

#方法二
summarizedList2 <- summarizedList$南投縣
summarizedList2 <- data.frame(summarizedList2)
summarizedList2[2,] <- summarizedList$嘉義市
summarizedList2[3,] <- summarizedList$嘉義縣
summarizedList2[4,] <- summarizedList$基隆市
summarizedList2[5,] <- summarizedList$宜蘭縣
summarizedList2[6,] <- summarizedList$屏東縣
summarizedList2[7,] <- summarizedList$彰化縣
summarizedList2[8,] <- summarizedList$新北市
summarizedList2[9,] <- summarizedList$新竹市
summarizedList2[10,] <- summarizedList$新竹縣
summarizedList2[11,] <- summarizedList$桃園市
summarizedList2[12,] <- summarizedList$澎湖縣
summarizedList2[13,] <- summarizedList$臺中市
summarizedList2[14,] <- summarizedList$臺北市
summarizedList2[15,] <- summarizedList$臺南市
summarizedList2[16,] <- summarizedList$臺東縣
summarizedList2[17,] <- summarizedList$花蓮縣
summarizedList2[18,] <- summarizedList$苗栗縣
summarizedList2[19,] <- summarizedList$連江縣
summarizedList2[20,] <- summarizedList$金門縣
summarizedList2[21,] <- summarizedList$雲林縣
summarizedList2[22,] <- summarizedList$高雄市
rownames(summarizedList2) <- colnames(mayorTableNew)



