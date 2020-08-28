library(ggplot2)
library(dplyr)
library(readxl)
library(gt)

# read table
df  <- read_xlsx("C:/Users/Игорь/Dropbox/Игорь — маркетолог/new.xlsx", 1)

# delete columns
df <- select(df, -c(`Курс РФ`, `Курс $`, "НДС", "Сумма без НДС", "Google $",
              "Соц. Сети" , "УНП", "Яндекс", "Остаток Google $",
              "Пришло Яндекс BYN", "Google", "Остаток", "Соц. сети", 
              "Остаток Яндекс"))

# rename columns
df <- rename(df, 'date' = 'Дата прихода', "tariff" = "Тариф",
             "manager" = "Продажа", "marketolog" = "Маркетолог",
             "site" = "Сайт", "summa_prihoda" = "Сумма",
             "axora_fix" = "Axora фикс", "sale" = "Sale",
             "percent_yandex" = "Axora %...17", 
             "percent_google" = "Axora %...21",
             "percent_smm" = "Axora %...25", "service" = "% Обслуж")

# negative to positive
df$summa_prihoda <- abs(df$summa_prihoda)

## Data Analysis
## count general data by department

# groupping by month and year
df_total <- df %>% mutate(month = format(date, "%m"), 
                          year = format(date, "%Y")) %>%
                  group_by(month, year) %>% 
                  summarise(summa_prihoda = sum(summa_prihoda, na.rm = TRUE),
                            commission = sum(c(percent_yandex, percent_google), 
                                             na.rm = TRUE, digits = 2),
                            service = sum(service, na.rm = TRUE, digits = 2),
                            selling = sum(axora_fix, na.rm = TRUE, digits = 2)) %>% 
                  mutate_if(is.numeric, ~round(., 2))

# greate total data
df_total <- df_total %>% 
            rowwise() %>% 
            mutate(total = sum(service, commission, selling,  
                               na.rm = TRUE),
                   margin = (total/summa_prihoda)*100) %>% 
            mutate_if(is.numeric, ~round(., 2)) %>% 
            arrange(df_total$year)

# delete empty rows
df_total <- df_total[complete.cases(df_total),]

## Count by specialist

# marketer table 
df_marketolog <- select(df, -c(6:8))

# sort site, marketer
df_group_site <- df_marketolog %>% mutate(month = format(date, "%m"), 
                                          year = format(date, "%Y")) %>%
                group_by(site, marketolog, month, year) %>% 
                summarise(service = sum(c(percent_yandex, percent_google, service), 
                                        na.rm = TRUE))

# sales table
df_group_sale <- df %>% mutate(month = format(date, "%m"), 
                               year = format(date, "%Y")) %>%
                        group_by(month, year, site, sale) %>% 
                        summarise(coast = sum(axora_fix, na.rm = TRUE))
  
df_group_sale <- rename(df_group_sale, marketolog = sale)
# or
# colnames(df_group_sale)[4] <- c("marketolog")

# merge marketer table and sales table
df_mark_service_coast <- rbind(df_group_site, df_group_sale)

# other way
#df_total_coast <- merge(df_group_site, df_group_sale, 
#by = c("month", "year", "site", "marketolog"), all.x = TRUE)

# revenue by months
df_revenue <- df_mark_service_coast %>% 
              group_by(month, year, marketolog) %>% 
              summarise(service = sum(service, na.rm = T), 
                        coast = sum(coast, na.rm = T), 
                        revenue = sum(c(service, coast), na.rm = T)) %>% 
              mutate_if(is.double, list(~ round(., 1)))

# delete empty rows
df_revenue <- df_revenue[complete.cases(df_revenue),]

# count sales by months
df_group_sale %>% group_by(month) %>% 
                  summarise(selling = sum(coast, na.rm = T),
                            amt = length(which(coast > 0)),
                            mean_coast = (selling/amt)) %>% 
                  filter(month != "NA") %>% 
                  mutate_if(is.numeric, ~round(., 2)) %>% 
                  gt() %>% 
                  fmt_currency(
                    columns = vars(mean_coast, selling),
                    currency = "BYR")

df_revenue %>% gt()
df_total %>% gt() %>% 
              fmt_currency(
                columns = vars(total),
                currency = "BYR") 


## Graphs
# revenue marketer by months
ggplot(data = df_revenue, aes(x = reorder(marketolog,-revenue),
                              y = revenue, fill = month)) + 
        geom_bar(position = 'dodge', stat = 'identity') +
        geom_text(aes(label = floor(revenue)), position = position_dodge(width = 1), 
                  vjust = -0.25) +
        labs(fill = "month") +
        ggtitle('revenue marketer by months',
                subtitle = '') +
        xlab('marketolog') +
        ylab('revenue, byn') +
        theme_bw()

# sales by months
ggplot(df_revenue, mapping = aes(x = reorder(marketolog, -coast), 
                                 y = coast, fill = month)) +
          geom_bar(position = 'dodge', stat = 'identity') +
          geom_text(aes(label = floor(coast)), 
                    position = position_dodge(width = 1), 
                    vjust = -0.25) +
          ggtitle('sales') +
          xlab('marketolog') +
          ylab('revenue, byn') +
          theme_bw()

# revenue by months
ggplot(data = df_total, aes(x = month, y = total, fill = total)) + 
          geom_bar(position = 'dodge', stat = 'identity') +
          geom_text(aes(label = floor(total)), 
                    position = position_dodge(width = 1), 
                    vjust = -0.25) +
          ggtitle('revenue by months',
                  subtitle = '') +
          xlab('months') +
          ylab('revenue, byn') +
          theme_bw() +
          theme(legend.position = "none")

# revenue by weeks
df_period <- df %>% mutate(month = format(date, "%m"),
              week = format(date, "%W")) %>%
            group_by(month, week) %>% 
            summarise(summa_prihoda = sum(summa_prihoda, na.rm = TRUE)) %>% 
            mutate_if(is.numeric, ~round(., 2))

df_period <- df_period[complete.cases(df_period),]
df_period$mean <- mean(df_period$summa_prihoda)

g <- ggplot(df_period,
       aes(week, summa_prihoda, fill = month)) +
      geom_col(position = 'dodge')  +
      geom_text(aes(label = floor(summa_prihoda)), 
                position = position_dodge(width = 1), 
                vjust = -0.25) +
      ggtitle('',
              subtitle = '') +
      labs(fill = "months") +
      xlab('week') +
      ylab('total, byn') +
      theme_bw()

g + geom_line(aes(x = week, y = mean, group = month), stat = 'identity', 
              position = 'dodge', 
          color = 'blue', size = 1)
