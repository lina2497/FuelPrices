pacman::p_load(stringr,purrr,RSelenium)

driver<- rsDriver(browser=c("chrome"))
remDr <- driver[["client"]]
remDr$navigate("https://www.allstarcard.co.uk/fuel-card-services/uk-fuel-price-information/")


#Type in post code
PostCode<-remDr$findElement(using = 'css',  ".text-1")
PostCode$sendKeysToElement(list("HP27 0HY"))

#Click button
Button<-remDr$findElement(using = 'css',"button")
Button$clickElement()

#get data
t <- remDr$findElements(using = 'css', "tbody")



Fuel<-as.character(sapply(t, function(x){x$getElementText()})[2])

Fuel<-strsplit(Fuel,"\n")
Fuel2<-data.frame(Station = unlist(Fuel), stringsAsFactors = FALSE)

FuelSplit<-do.call(rbind,strsplit(Fuel2$Station," miles "))

Prices<-do.call(rbind,strsplit(FuelSplit[,2],"\ "))
Fuel2$Petrol <- Prices[,1]
Fuel2$Diesel <- Prices[,2]

Location<-FuelSplit[,1]

FindLastSpace<-function(x){
  LastSpace<-rev(gregexpr("\\ ", x)[[1]])[1]
  return(LastSpace)
}
  



Split<-map(Location,~FindLastSpace(.x))


str_trunc(FuelSplit[[1]][1],width = 21)

map2(Split,Location,~str_trunc(.y,width = .x))

