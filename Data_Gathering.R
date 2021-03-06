# Set working directory
setwd('C:/Users/a6p/Desktop/Uni 2014/E1161 - Collaborative Research/GitHub Clone/Assignment3/')

require("devtools")
library("rsdmx")


# Pull OECD city patent data
dataURL <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CITIES/AT001+AT002+AT003+BE001+BE002+BE003+BE005+DE001+DE002+DE003+DE004+DE005+DE006+DE007+DE008+DE009+DE010+DE011+DE012+DE013+DE015+DE027+DE033+DE034+DE035+DE501+DE502+DE507+DK001+EE001+ES001+ES002+ES003+ES004+ES005+ES006+ES008+ES019+FI001+FR001+FR003+FR004+FR006+FR007+FR008+FR009+FR010+FR013+FR026+FR032+FR203+FR205+FR215+IT001+IT002+IT003+IT004+IT005+IT006+IT007+IT008+IT009+IT010+IT011+JP026+JP030+JP031+JP034+JP036+JP038+JP050+JP051+JP052+JP053+JP054+MEX19+MEX38+NL001+NL002+NL003+NL004+NO001+PT001+PT002+SE001+SE002+SE003+US003+US012+US014+US033+US035+US038+US044+US045+US048+US055+US060+US065+US069+US077+US081+US097+US103+US106+US107+US114+US115+US117+US122+US124+US125+US141+US147+US149+US155+US160+US161+US170+US178+US180+US181+US195+US196+US202+US209+US213+US223+US234+US237+US242+US250+US252+US259.PCT_INTENSITY?startTime=2008&endTime=2012"

sdmx <- readSDMX(dataURL)
city_pat <- as.data.frame(sdmx)

# Clean table by deleting and renaming rows
city_pat$VAR <- NULL # Delete columns
city_pat$attrs.df <- NULL

colnames(city_pat) <- c("METRO_ID" , "Year" , "Patents") # Rename columns

# Pull OECD GDP per capita data
dataURL2 <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CITIES/AT001+AT002+AT003+BE001+BE002+BE003+BE005+DE001+DE002+DE003+DE004+DE005+DE006+DE007+DE008+DE009+DE010+DE011+DE012+DE013+DE015+DE027+DE033+DE034+DE035+DE501+DE502+DE507+DK001+EE001+ES001+ES002+ES003+ES004+ES005+ES006+ES008+ES019+FI001+FR001+FR003+FR004+FR006+FR007+FR008+FR009+FR010+FR013+FR026+FR032+FR203+FR205+FR215+IT001+IT002+IT003+IT004+IT005+IT006+IT007+IT008+IT009+IT010+IT011+JP026+JP030+JP031+JP034+JP036+JP038+JP050+JP051+JP052+JP053+JP054+MEX19+MEX38+NL001+NL002+NL003+NL004+NO001+PT001+PT002+SE001+SE002+SE003+US003+US012+US014+US033+US035+US038+US044+US045+US048+US055+US060+US065+US069+US077+US081+US097+US103+US106+US107+US114+US115+US117+US122+US124+US125+US141+US147+US149+US155+US160+US161+US170+US178+US180+US181+US195+US196+US202+US209+US213+US223+US234+US237+US242+US250+US252+US259.GDP_PC?startTime=2008&endTime=2008"

sdmx <- readSDMX(dataURL2)
city_gdp <- as.data.frame(sdmx)

# Clean table by deleting and renaming rows
city_gdp$VAR <- NULL # Delete columns
city_gdp$attrs.df <- NULL

colnames(city_gdp) <- c("METRO_ID" , "Year" , "GDP") # Rename columns

# Pull OECD population data
dataURL3 <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CITIES/AT001+AT002+AT003+BE001+BE002+BE003+BE005+DE001+DE002+DE003+DE004+DE005+DE006+DE007+DE008+DE009+DE010+DE011+DE012+DE013+DE015+DE027+DE033+DE034+DE035+DE501+DE502+DE507+DK001+EE001+ES001+ES002+ES003+ES004+ES005+ES006+ES008+ES019+FI001+FR001+FR003+FR004+FR006+FR007+FR008+FR009+FR010+FR013+FR026+FR032+FR203+FR205+FR215+IT001+IT002+IT003+IT004+IT005+IT006+IT007+IT008+IT009+IT010+IT011+JP026+JP030+JP031+JP034+JP036+JP038+JP050+JP051+JP052+JP053+JP054+MEX19+MEX38+NL001+NL002+NL003+NL004+NO001+PT001+PT002+SE001+SE002+SE003+US003+US012+US014+US033+US035+US038+US044+US045+US048+US055+US060+US065+US069+US077+US081+US097+US103+US106+US107+US114+US115+US117+US122+US124+US125+US141+US147+US149+US155+US160+US161+US170+US178+US180+US181+US195+US196+US202+US209+US213+US223+US234+US237+US242+US250+US252+US259.POP?startTime=2008&endTime=2008"

sdmx <- readSDMX(dataURL3)
city_pop <- as.data.frame(sdmx)

# Clean table by deleting and renaming rows
city_pop$VAR <- NULL # Delete columns
city_pop$attrs.df <- NULL

colnames(city_pop) <- c("METRO_ID" , "Year" , "Population") # Rename columns


# Pull OECD urban pollution (Estimated population exposure to air pollution PM2.5 expressed in mirco gram per cubic metre, annual average)
dataURL4 <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CITIES/AT001+AT002+AT003+BE001+BE002+BE003+BE005+DE001+DE002+DE003+DE004+DE005+DE006+DE007+DE008+DE009+DE010+DE011+DE012+DE013+DE015+DE027+DE033+DE034+DE035+DE501+DE502+DE507+DK001+EE001+ES001+ES002+ES003+ES004+ES005+ES006+ES008+ES019+FI001+FR001+FR003+FR004+FR006+FR007+FR008+FR009+FR010+FR013+FR026+FR032+FR203+FR205+FR215+IT001+IT002+IT003+IT004+IT005+IT006+IT007+IT008+IT009+IT010+IT011+JP026+JP030+JP031+JP034+JP036+JP038+JP050+JP051+JP052+JP053+JP054+MEX19+MEX38+NL001+NL002+NL003+NL004+NO001+PT001+PT002+SE001+SE002+SE003+US003+US012+US014+US033+US035+US038+US044+US045+US048+US055+US060+US065+US069+US077+US081+US097+US103+US106+US107+US114+US115+US117+US122+US124+US125+US141+US147+US149+US155+US160+US161+US170+US178+US180+US181+US195+US196+US202+US209+US213+US223+US234+US237+US242+US250+US252+US259.AIR_POLLUTION?startTime=2005&endTime=2005"

sdmx <- readSDMX(dataURL4)
city_pol <- as.data.frame(sdmx)

# Clean table by deleting and renaming rows
city_pol$VAR <- NULL # Delete columns
city_pol$attrs.df <- NULL
city_pol$obsTime <- NULL

colnames(city_pol) <- c("METRO_ID" , "Pollution") # Rename columns

# Pull OECD Greenspace (Estimated population exposure to air pollution PM2.5 expressed in mirco gram per cubic metre, annual average)
dataURL5 <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CITIES/AT001+AT002+AT003+BE001+BE002+BE003+BE005+DE001+DE002+DE003+DE004+DE005+DE006+DE007+DE008+DE009+DE010+DE011+DE012+DE013+DE015+DE027+DE033+DE034+DE035+DE501+DE502+DE507+DK001+EE001+ES001+ES002+ES003+ES004+ES005+ES006+ES008+ES019+FI001+FR001+FR003+FR004+FR006+FR007+FR008+FR009+FR010+FR013+FR026+FR032+FR203+FR205+FR215+IT001+IT002+IT003+IT004+IT005+IT006+IT007+IT008+IT009+IT010+IT011+JP026+JP030+JP031+JP034+JP036+JP038+JP050+JP051+JP052+JP053+JP054+MEX19+MEX38+NL001+NL002+NL003+NL004+NO001+PT001+PT002+SE001+SE002+SE003+US003+US012+US014+US033+US035+US038+US044+US045+US048+US055+US060+US065+US069+US077+US081+US097+US103+US106+US107+US114+US115+US117+US122+US124+US125+US141+US147+US149+US155+US160+US161+US170+US178+US180+US181+US195+US196+US202+US209+US213+US223+US234+US237+US242+US250+US252+US259.GREEN_AREA_PC?startTime=2008&endTime=2008"

sdmx <- readSDMX(dataURL5)
city_green <- as.data.frame(sdmx)

# Clean table by deleting and renaming rows
city_green$VAR <- NULL # Delete columns
city_green$attrs.df <- NULL

colnames(city_green) <- c("METRO_ID" , "Year" , "Greenspace") # Rename columns

# Pull OECD city employment of metropolitan area as % of national value 2008
dataURL6 <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/CITIES/AT001+AT002+AT003+BE001+BE002+BE003+BE005+DE001+DE002+DE003+DE004+DE005+DE006+DE007+DE008+DE009+DE010+DE011+DE012+DE013+DE015+DE027+DE033+DE034+DE035+DE501+DE502+DE507+DK001+EE001+ES001+ES002+ES003+ES004+ES005+ES006+ES008+ES019+FI001+FR001+FR003+FR004+FR006+FR007+FR008+FR009+FR010+FR013+FR026+FR032+FR203+FR205+FR215+IT001+IT002+IT003+IT004+IT005+IT006+IT007+IT008+IT009+IT010+IT011+JP026+JP030+JP031+JP034+JP036+JP038+JP050+JP051+JP052+JP053+JP054+MEX19+MEX38+NL001+NL002+NL003+NL004+NO001+PT001+PT002+SE001+SE002+SE003+US003+US012+US014+US033+US035+US038+US044+US045+US048+US055+US060+US065+US069+US077+US081+US097+US103+US106+US107+US114+US115+US117+US122+US124+US125+US141+US147+US149+US155+US160+US161+US170+US178+US180+US181+US195+US196+US202+US209+US213+US223+US234+US237+US242+US250+US252+US259.EMP_SHARE?startTime=2008&endTime=2008"

sdmx <- readSDMX(dataURL6)
city_emp <- as.data.frame(sdmx)

# Clean table by deleting and renaming rows
city_emp$VAR <- NULL # Delete columns
city_emp$attrs.df <- NULL

colnames(city_emp) <- c("METRO_ID" , "Year" , "Employment") # Rename columns

# Merge all OECD citydata sets
oecd_pat_gdp <- merge(city_pat , city_gdp , by=c("METRO_ID" , "Year"))
oecd_pat_gdp_pop <- merge(oecd_pat_gdp , city_pop , by=c("METRO_ID" , "Year"))
oecd_pat_gdp_pop_green <- merge(oecd_pat_gdp_pop , city_green , by=c("METRO_ID" , "Year"))
oecd_pat_gdp_pop_green_emp <- merge(oecd_pat_gdp_pop_green , city_emp , by=c("METRO_ID" , "Year"))
oecd <- merge(oecd_pat_gdp_pop_green_emp , city_pol , by=c("METRO_ID"))


# Clean city table, insert city and country names
oecd$Year <- NULL

oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "AT001" , "Vienna")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "AT002" , "Graz")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "AT003" , "Linz")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "BE001" , "Brussels")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "BE002" , "Antwerp")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "BE003" , "Ghent")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "BE005" , "Liege")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE001" , "Berlin")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE002" , "Hamburg")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE003" , "Munich")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE004" , "Cologne")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE005" , "Frankfurt")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE006" , "Essen")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE007" , "Stuttgart")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE008" , "Leipzig")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE009" , "Dresden")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE010" , "Dortmund")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE011" , "Dusseldorf")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE012" , "Bremen")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE013" , "Hanover")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE015" , "Bochum")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE027" , "Freiburg")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE033" , "Augsburg")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE034" , "Bonn")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE035" , "Karlsruhe")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE501" , "Duisburg")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE502" , "Mannheim")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DE507" , "Aachen")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "DK001" , "Copenhagen")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "EE001" , "Tallinn")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "ES001" , "Madrid")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "ES002" , "Barcelona")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "ES003" , "Valencia")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "ES004" , "Seville")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "ES005" , "Zaragoza")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "ES006" , "Malaga")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "ES008" , "Palmas")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "ES019" , "Bilbao")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FI001" , "Helsinki")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR001" , "Paris")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR003" , "Lyon")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR004" , "Toulouse")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR006" , "Strasbourg")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR007" , "Bordeaux")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR008" , "Nantes")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR009" , "Lille")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR010" , "Montpellier")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR013" , "Rennes")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR026" , "Grenoble")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR032" , "Toulon")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR203" , "Marseille")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR205" , "Nice")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "FR215" , "Rouen")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "IT001" , "Rome")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "IT002" , "Milan")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "IT003" , "Naples")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "IT004" , "Turin")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "IT005" , "Palermo")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "IT006" , "Genova")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "IT007" , "Florence")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "IT008" , "Bari")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "IT009" , "Bologna")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "IT010" , "Catania")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "IT011" , "Venice")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "JP026" , "Mito")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "JP030" , "Tokyo")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "JP031" , "Kofu")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "JP034" , "Nagoya")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "JP036" , "Numazu")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "JP038" , "Osaka")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "JP050" , "Okayama")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "JP051" , "Kurashiki")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "JP052" , "Fukuyama")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "JP053" , "Hiroshima")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "JP054" , "Takamatsu")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "MEX19" , "Monterrey")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "MEX38" , "Irapuato")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "NL001" , "Hague")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "NL002" , "Amsterdam")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "NL003" , "Rotterdam")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "NL004" , "Utrecht")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "NO001" , "Oslo")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "PT001" , "Lisbon")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "PT002" , "Porto")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "SE001" , "Stockholm")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "SE002" , "Gothenburg")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "SE003" , "Malmo")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US003" , "Seattle")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US012" , "Portland")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US014" , "Minneapolis")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US033" , "Milwaukee")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US035" , "Madison")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US038" , "Buffalo")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US044" , "Albany")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US045" , "Detroit")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US048" , "Boston")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US055" , "Chicago")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US060" , "Providence")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US065" , "Toledo")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US069" , "Cleveland")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US077" , "Omaha")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US081" , "Akron")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US097" , "Pittsburgh")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US103" , "Harrisburg")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US106" , "Philadelphia")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US107" , "Columbus")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US114" , "Denver")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US115" , "Indianapolis")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US117" , "Dayton")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US122" , "Baltimore")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US124" , "Cincinnati")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US125" , "Washington")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US141" , "Louisville")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US147" , "Wichita")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US149" , "Richmond")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US155" , "Fresno")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US160" , "Nashville")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US161" , "Tulsa")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US170" , "Raleigh")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US178" , "Charlotte")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US180" , "Albuquerque")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US181" , "Memphis")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US195" , "Columbia")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US196" , "Atlanta")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US202" , "Phoenix")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US209" , "Dallas")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US213" , "Charleston")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US223" , "Tucson")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US234" , "Austin")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US237" , "Jacksonville")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US242" , "Houston")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US250" , "Orlando")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US252" , "Tampa")
oecd$METRO_ID <- replace(oecd$METRO_ID , oecd$METRO_ID == "US259" , "Miami")



# Pull API data from GitHub network
library(httr)
library(dplyr)
library(rjson)


# Pull user numbers of GitHub users without followers

# Create locations vector
locations <- c('Vienna', 'Graz', 'Linz', 'Brussels', 'Antwerp', 'Ghent', 'Liege', 'Berlin', 'Hamburg', 'Munich', 'Cologne', 'Frankfurt', 'Essen', 'Stuttgart', 'Leipzig', 'Dresden', 'Dortmund', 'Dusseldorf', 'Bremen', 'Hanover', 'Bochum', 'Freiburg', 'Augsburg', 'Bonn', 'Karlsruhe', 'Duisburg', 'Mannheim', 'Aachen', 'Copenhagen', 'Tallinn', 'Madrid', 'Barcelona', 'Valencia', 'Seville', 'Zaragoza', 'Malaga', 'Palmas', 'Bilbao', 'Helsinki', 'Paris', 'Lyon', 'Toulouse', 'Strasbourg', 'Bordeaux', 'Nantes', 'Lille', 'Montpellier', 'Rennes', 'Grenoble', 'Toulon', 'Marseille', 'Nice', 'Rouen', 'Rome', 'Milan', 'Naples', 'Turin', 'Palermo', 'Genova', 'Florence', 'Bari', 'Bologna', 'Catania', 'Venice', 'Mito', 'Tokyo', 'Kofu', 'Nagoya', 'Numazu', 'Osaka', 'Okayama', 'Kurashiki', 'Fukuyama', 'Hiroshima', 'Takamatsu', 'Monterrey', 'Irapuato', 'Hague', 'Amsterdam', 'Rotterdam', 'Utrecht', 'Eindhoven', 'Oslo', 'Lisbon', 'Porto', 'Stockholm', 'Gothenburg', 'Malmo', 'Seattle', 'Portland', 'Minneapolis', 'Milwaukee', 'Madison', 'Buffalo', 'Albany', 'Detroit', 'Boston', 'Chicago', 'Providence', 'Toledo', 'Cleveland', 'Omaha', 'Akron', 'Pittsburgh', 'Harrisburg', 'Philadelphia', 'Columbus', 'Denver', 'Indianapolis', 'Dayton', 'Baltimore', 'Cincinnati', 'Washington', 'Louisville',  'Wichita', 'Richmond', 'Fresno', 'Nashville', 'Tulsa', 'Raleigh', 'Charlotte', 'Albuquerque', 'Memphis', 'Columbia', 'Atlanta', 'Phoenix',  'Dallas', 'Charleston', 'Tucson', 'Austin', 'Jacksonville', 'Houston', 'Orlando', 'Tampa', 'Miami')

# Create empty vector for user counts
user_counts0 <- vector()

# Loop through the locations and download the user counts
## Note: the %23E turns into a greater than sign >
for (i in locations){
  # Download raw data
  URL_temp0 <- paste0('https://api.github.com/search/users?q=location:', i,
                     '+followers:0')
  users0 <- GET(URL_temp0) %>% 
    content(as = 'text') %>% 
    fromJSON()               
  
  # Extract counts of users that meet criteria for the location
  user_counts_temp0 <- users0$total_count
  
  # Add to user_counts vector
  user_counts0 <- c(user_counts0, user_counts_temp0)
  
  # Sleep R for 7 seconds so you don't overload the API
  Sys.sleep(2); message('-')
}

# Combine locations and user counts data
location_counts0 <- data.frame(locations, user_counts0)

# Clean API data
colnames(location_counts0) <- c("METRO_ID" , "zeroFollowers") # Rename columns


# Pull user numbers of GitHub users with more than one followers

# Create empty vector for user counts
user_counts1 <- vector()

# Loop through the locations and download the user counts
## Note: the %23E turns into a greater than sign >
for (i in locations){
  # Download raw data
  URL_temp1 <- paste0('https://api.github.com/search/users?q=location:', i,
                     '+followers:%3E1')
  users1 <- GET(URL_temp1) %>% 
    content(as = 'text') %>% 
    fromJSON()               
  
  # Extract counts of users that meet criteria for the location
  user_counts_temp1 <- users1$total_count
  
  # Add to user_counts vector
  user_counts1 <- c(user_counts1, user_counts_temp1)
  
  # Sleep R for 7 seconds so you don't overload the API
  Sys.sleep(2); message('-')
}

# Combine locations and user counts data
location_counts1 <- data.frame(locations, user_counts1)


# Clean API data
colnames(location_counts1) <- c("METRO_ID" , "oneFollowers") # Rename columns



# Pull user numbers of GitHub users with more than 25 followers

# Create empty vector for user counts
user_counts2 <- vector()

# Loop through the locations and download the user counts
## Note: the %23E turns into a greater than sign >
for (i in locations){
  # Download raw data
  URL_temp2 <- paste0('https://api.github.com/search/users?q=location:', i,
                     '+followers:%3E25')
  users2 <- GET(URL_temp2) %>% 
    content(as = 'text') %>% 
    fromJSON()               
  
  # Extract counts of users that meet criteria for the location
  user_counts_temp2 <- users2$total_count
  
  # Add to user_counts vector
  user_counts2 <- c(user_counts2, user_counts_temp2)
  
  # Sleep R for 7 seconds so you don't overload the API
  Sys.sleep(2); message('-')
}

# Combine locations and user counts data
location_counts2 <- data.frame(locations, user_counts2)


# Clean API data
colnames(location_counts2) <- c("METRO_ID" , "twentyfiveFollowers")

# Merge different follower number dataframes
location_counts3 <- merge(location_counts0 , location_counts1 , by=c("METRO_ID"))     
location_counts <- merge(location_counts3 , location_counts2 , by=c("METRO_ID"))     

# Change follower numbers to categorical
location_counts$oneFollowers <- location_counts$oneFollowers-location_counts$twentyfiveFollowers

# Change column names
colnames(location_counts) <- c("METRO_ID" , "nofollowing" , "medfollowing" , "hifollowing")

# Merge all OECD and API data set
dataset <- merge(oecd , location_counts , by=c("METRO_ID"))                 

# Change total counts into counts per population
dataset$nofollowing <- dataset$nofollowing/dataset$Population
dataset$nofollowing <- dataset$nofollowing*500000

dataset$medfollowing <- dataset$medfollowing/dataset$Population
dataset$medfollowing <- dataset$medfollowing*500000

dataset$hifollowing <- dataset$hifollowing/dataset$Population
dataset$hifollowing <- dataset$hifollowing*500000



# Save dataframe as .csv file to save computing time
write.csv(dataset, file = "Dataset.csv")
