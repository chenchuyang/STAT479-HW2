# STAT479-HW2


Name: CHUYANG CHEN


# data cleaning 
```{r}
#install.packages("blscrapeR")
library(blscrapeR)
library(tidyverse)

# get the lastest nemployment data.
df <- get_bls_county()
WIunemployment = df %>% filter(fips_state == 55)

# get the unemployment data from November 2019 (the data from previous month)
df2 = get_bls_county("November 2019")
WIunemployment_Nov = df2 %>% filter(fips_state == 55)

# rename the column names to aovid confusion 
colnames(WIunemployment_Nov)[colnames(WIunemployment_Nov) == "unemployed"] = "unemployed_Nov"
colnames(WIunemployment_Nov)[colnames(WIunemployment_Nov) == "unemployed_rate"] = "unemployed_rate_Nov"

# get the bridges data in WI.
bridges = read_csv("https://www.fhwa.dot.gov/bridge/nbi/2018/delimited/WI18.txt")  

# selecting varibles which are relevent to the unemployment number / rate. 
table2 = bridges %>% 
  group_by(COUNTY_CODE_003) %>% 
  left_join(WIunemployment,by = c("COUNTY_CODE_003" = "fips_county")) %>%
  select(area_title,unemployed,unemployed_rate,APPR_WIDTH_MT_032,PERCENT_ADT_TRUCK_109,APPR_ROAD_EVAL_072,ADT_029,
         FUNCTIONAL_CLASS_026)

# Add the unemployed data from November 2019 into table2
table3 = table2 %>% 
  group_by(COUNTY_CODE_003) %>% 
  left_join(WIunemployment_Nov,by = c("COUNTY_CODE_003" = "fips_county"))
```


# bulding linear model with relevent variables 
This linear model has 4 independent variables.

Continous variables:
  - APPR_WIDTH_MT_03 represents the width of usable roadway approaching the bridge. 
  - PERCENT_ADT_TRUCK_109 represents the percentage of average daily traffic that is trunk. 
  - ADT_029 represents the overall average daily traffic 

Dummy variables (categorical variables):
  - FUNCTIONAL_CLASS_026 represents the location and function of the bridge. 01-09 indicates the bridge is in a 
    rural area, and 11 - 19 means the bridge is in an urban area. 
```{r}
# Build a linear model to predict unempolyment rate 
lm_unemployed_rate = lm(data = table2, unemployed_rate ~ APPR_WIDTH_MT_032 + PERCENT_ADT_TRUCK_109 + ADT_029 + as.character(FUNCTIONAL_CLASS_026))
summary(lm_unemployed_rate)
```
The result of this linear model shows that larger width of usable roadway approaching the bridge is associated with lower unemployment rate. As the percentage of trunk traffic increases, the unemployment rate will also rise. The average daily traffic tells a different story: more traffic in gerenal leads to lower unemployment rate. The location of bridges shows that rural area is associated with higher unemployment rate, which makes sense. Even though the coefficients of urban area are also postive, we cannot say urabn area is associated with higher unemployment rate since their p-values are very large, which means that the coefficients do not have significant different with zero. 


```{r}
# Build a linear model to predict unempolyed number
lm_unemployed = lm(data = table2, unemployed ~ APPR_WIDTH_MT_032 + PERCENT_ADT_TRUCK_109 + ADT_029 + as.character(FUNCTIONAL_CLASS_026))
summary(lm_unemployed)
```



# Using the unemployed number and rate from the previous month as additional predictors to the linear model
```{r}
# linear model predicting unempolyment rate
lm_unemployed_rate2 = lm(data = table3, unemployed_rate ~ unemployed_rate_Nov + unemployed_Nov + APPR_WIDTH_MT_032 + PERCENT_ADT_TRUCK_109 + ADT_029 + as.character(FUNCTIONAL_CLASS_026))
summary(lm_unemployed_rate2)
```
The result shows that a higher unemployment rate of previous month are likely to lead to a higher unemployment rate of future month. For some reason, the predictor unemployed number from previous month tells a different story, and I am not sure why this is happening. A larger width of usable roadway approaching the bridge is associated with even lower unemployment rate in this model compared to the model without unemployment data from previous month. A larger percetage of trunk traffic is associated with lower unemployment rate, which is contradict to the first model. Higher average daily traffic in gereanl leads to lower unemployment rate, which totally makes sense, but unfortunately the p-value here is way too large which means we cannot get this conclusion. Rural areas are associated with higher unemployment rate, and the p-value for urban areas are too large so being in urban area might not be an effective predictor. 

```{r}
# linear model predicting unempolyed number
lm_unemployed2 = lm(data = table3, unemployed ~ unemployed_rate_Nov + unemployed_Nov + APPR_WIDTH_MT_032 + PERCENT_ADT_TRUCK_109 + ADT_029 + as.character(FUNCTIONAL_CLASS_026))
summary(lm_unemployed_rate2)
```




