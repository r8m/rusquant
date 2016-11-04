# Rusquant
Это форк пакета [rusquant](http://r-forge.r-project.org/projects/rusquant/)  
Что нового:
* Добавлены функции для загрузки тикеров  mfd.ru, moex.com:  
  + *loadStockListMfd()*
  + *loadStockListMoex(market=c("MOEX", "FORTS"))*
* Добавлены демо скрипты


# Install
```
library(devtools)
install_github("r8m/rusquant")
```

# Demo
```
demo("SI_BR_Regression",package="rusquant")
```