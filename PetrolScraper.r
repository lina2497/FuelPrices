###Packages
pacman::p_load(stringr,purrr,RSelenium)

##############################FUNCTIONS####################################

######Scrape Data function###########  

ScrapeFuel<-function(driver,PC){
  
  remDr <- driver[["client"]]
  
  #Open URL
  remDr$navigate("https://www.allstarcard.co.uk/fuel-card-services/uk-fuel-price-information/")
  
  Sys.sleep(2)
  
  #Type in post code
  PostCode<-remDr$findElement(using = 'css',  ".text-1")
  PostCode$sendKeysToElement(list(PC))
  
  
  
  #Click 10 mile radius radio button
  radio<-remDr$findElement( using = "xpath", "//input[@type='radio' and @value='10']")
  radio$clickElement()
  
  
  
  #Click button
  Button<-remDr$findElement(using = 'css',"button")
  Button$clickElement()
  
  Sys.sleep(2)
  #get data
  t <- remDr$findElements(using = 'css', "tbody")
  
  ##Functions
  FindLastSpace<-function(x){
    LastSpace<-rev(gregexpr("\\ ", x)[[1]])[1]
    return(LastSpace)
  }
  
  return(t)
}

######Wrangle Scaped Data into tidy dataframe function########

Wrangle<-function(Scraped){
  Fuel2<-NA
  #Wrangle Output into tidy table
  Fuel<-as.character(sapply(Scraped, function(x){x$getElementText()})[2])
  if (is.character(Fuel)){
    Fuel<-strsplit(Fuel,"\n")
    Fuel2<-data.frame(Station = unlist(Fuel), stringsAsFactors = FALSE)
    FuelSplit<-do.call(rbind,strsplit(Fuel2$Station," miles "))
    Prices<-do.call(rbind,strsplit(FuelSplit[,2],"\ "))
    Fuel2$Petrol <- Prices[,1]
    Fuel2$Diesel <- Prices[,2]
    Location<-FuelSplit[,1]
    Split<-map(Location,~FindLastSpace(.x))
    Fuel2$Station<-trimws(map2(Split,Location,~str_trunc(.y,width = .x,ellipsis = "")))
  }
  return(Fuel2)
}


##############################wORKFLOW####################################

##Open browser using selenium
driver<- rsDriver(browser=c("chrome"))

##Scrape data
Scraped<-ScrapeFuel(driver,"HP27 0HY")

##Tidy data
Wrangle(Scraped)

