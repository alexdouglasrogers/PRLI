library(readr)
library(dplyr)
library(readxl)

setwd("/Users/alexrogers/Desktop/NBSAP_project")

### cleaning and merging data sets ###

dat <- read_csv("Inputs/Red_List.csv")
dat <- dat[dat$VAR == "INDEX",][,c(2,6,7)]
dat <- dat[22:5040,]
dat$Country <- as.factor(dat$Country)

#Adding Variable for year National Biodiversity Strategies and Action Plan submitted.
dat$NBSAP <- rep(0,5019)


dat[dat$Country == "Monaco" & dat$Year >= 2021|
      dat$Country == "Haiti" & dat$Year >= 2020|
      dat$Country == "South Sudan" & dat$Year >= 2018| #
      dat$Country == "San Marino" & dat$Year >= 2018|
      dat$Country == "Andorra" & dat$Year >= 2016|
      dat$Country == "Nauru" & dat$Year >= 2010|
      dat$Country == "Somalia" & dat$Year >= 2015|
      dat$Country == "Iraq" & dat$Year >= 2015|
      dat$Country == "Brunei Darussalam" & dat$Year >= 2015|
      dat$Country == "Antigua and Barbuda" & dat$Year >= 2014|
      dat$Country == "Greece" & dat$Year >= 2014|
      dat$Country == "United Arab Emirates" & dat$Year >= 2014|
      dat$Country == "Afghanistan" & dat$Year >= 2014|
      dat$Country == "Liechtenstein" & dat$Year >= 2014|
      dat$Country == "Tuvalu" & dat$Year >= 2013|
      dat$Country == "Malta" & dat$Year >= 2012|
      dat$Country == "Myanmar" & dat$Year >= 2011|#
      dat$Country == "Timor-Leste" & dat$Year >= 2011|
      dat$Country == "Dominican Republic" & dat$Year >= 2011|
      dat$Country == "Serbia" & dat$Year >= 2011|
      dat$Country == "Italy" & dat$Year >= 2010|
      dat$Country == "Tanzania" & dat$Year >= 2004|
      dat$Country == "Montenegro" & dat$Year >= 2010|
      dat$Country == "Kuwait" & dat$Year >= 1997| ##completed not adopted
      dat$Country == "Solomon Islands" & dat$Year >= 2009|
      dat$Country == "Israel" & dat$Year >= 2010|
      dat$Country == "Cabo Verde" & dat$Year >= 1999|
      dat$Country == "Saudi Arabia" & dat$Year >= 2005|
      dat$Country == "Saint Kitts and Nevis" & dat$Year >= 2004|
      dat$Country == "Saint Vincent and the Grenadines" & dat$Year >= 2004|
      dat$Country == "Equatorial Guinea" & dat$Year >= 2005|
      dat$Country == "Azerbaijan" & dat$Year >= 2004|
      dat$Country == "Malawi" & dat$Year >= 2006|
      dat$Country == "Palau" & dat$Year >= 2005|
      dat$Country == "Mauritius" & dat$Year >= 2006|
      dat$Country == "Ethiopia" & dat$Year >= 2006|
      dat$Country == "Togo" & dat$Year >= 2003|
      dat$Country == "Germany" & dat$Year >= 2007|
      dat$Country == "Tonga" & dat$Year >= 2006|
      dat$Country == "Belgium" & dat$Year >= 2007|
      dat$Country == "Uganda" & dat$Year >= 2002|
      dat$Country == "Angola" & dat$Year >= 2006|
      dat$Country == "Nigeria" & dat$Year >= 2006|
      dat$Country == "Guinea-Bissau" & dat$Year >= 2001|
      dat$Country == "Mali" & dat$Year >= 2001|
      dat$Country == "Türkiye" & dat$Year >= 2001|
      dat$Country == "South Africa" & dat$Year >= 2005|
      dat$Country == "Syrian Arab Republic" & dat$Year >= 2002|
      dat$Country == "Sierra Leone" & dat$Year >= 2003|
      dat$Country == "Surinam" & dat$Year >= 2013| #strategy released in 2006
      dat$Country == "Czechia" & dat$Year >= 2005|
      dat$Country == "Trinidad and Tobago" & dat$Year >= 2001|
      dat$Country == "Lao People's Democratic Republic" & dat$Year >= 2004|
      dat$Country == "Qatar" & dat$Year >= 2004|
      dat$Country == "Sao Tome and Principe" & dat$Year >= 2005|
      dat$Country == "Poland" & dat$Year >= 2003|
      dat$Country == "Yemen" & dat$Year >= 2005|
      dat$Country == "Algeria" & dat$Year >= 2005|
      dat$Country == "Liberia" & dat$Year >= 2003|
      dat$Country == "Eritrea" & dat$Year >= 2000|
      dat$Country == "Gabon" & dat$Year >= 1999|
      dat$Country == "Chile" & dat$Year >= 2003|
      dat$Country == "Lesotho" & dat$Year >= 2000|
      dat$Country == "Democratic People's Republic of Korea" & dat$Year >= 1998|
      dat$Country == "Tajikistan" & dat$Year >= 2003|
      dat$Country == "Sudan" & dat$Year >= 2000|
      dat$Country == "Sweeden" & dat$Year >= 1995|
      dat$Country == "Jamaica" & dat$Year >= 2003|
      dat$Country == "Honduras" & dat$Year >= 2001|
      dat$Country == "Oman" & dat$Year >= 2001|
      dat$Country == "Comoros" & dat$Year >= 2000|
      dat$Country == "El Salvador" & dat$Year >= 1999|
      dat$Country == "Zambia" & dat$Year >= 1999|
      dat$Country == "Latvia" & dat$Year >= 2000|
      dat$Country == "Thailand" & dat$Year >= 1997|
      dat$Country == "Rwanda" & dat$Year >= 2003|
      dat$Country == "Marshall Islands" & dat$Year >= 2000|
      dat$Country == "Turkmenistan" & dat$Year >= 2002|
      dat$Country == "Georgia" & dat$Year >= 2005|
      dat$Country == "Barbados" & dat$Year >= 2002|
      dat$Country == "Maldives" & dat$Year >= 2002|
      dat$Country == "Ghana" & dat$Year >= 2016| ## strategy in 2002
      dat$Country == "Madagascar" & dat$Year >= 2002|
      dat$Country == "Bahamas" & dat$Year >= 1999|
      dat$Country == "Morocco" & dat$Year >= 2002|
      dat$Country == "Cambodia" & dat$Year >= 2002|
      dat$Country == "Nepal" & dat$Year >= 2002|
      dat$Country == "Moldova" & dat$Year >= 2000|
      dat$Country == "Djibouti" & dat$Year >= 2001|
      dat$Country == "Slovenia" & dat$Year >= 2001| ## strategy only
      dat$Country == "Ireland" & dat$Year >= 2002|
      dat$Country == "Micronesia" & dat$Year >= 2002|
      dat$Country == "Guinea" & dat$Year >= 2001|
      dat$Country == "Russia" & dat$Year >= 2001|
      dat$Country == "Nicaragua" & dat$Year >= 2001|
      dat$Country == "Vanuatu" & dat$Year >= 2000|
      dat$Country == "Niue" & dat$Year >= 2001|
      dat$Country == "Democratic Republic of the Congo" & dat$Year >= 2000|
      dat$Country == "Guatemala" & dat$Year >= 1999|
      dat$Country == "Peru" & dat$Year >= 2001|
      dat$Country == "Portugal" & dat$Year >= 2001|
      dat$Country == "Argentina" & dat$Year >= 2003|
      dat$Country == "Spain" & dat$Year >= 2011|
      dat$Country == "Croatia" & dat$Year >= 1999|
      dat$Country == "Grenada" & dat$Year >= 2000|
      dat$Country == "Panama" & dat$Year >= 2000|
      dat$Country == "Mexico" & dat$Year >= 2000|
      dat$Country == "Cameroon" & dat$Year >= 1999|
      dat$Country == "Kenya" & dat$Year >= 1999|
      dat$Country == "Zimbabwe" & dat$Year >= 2002|
      dat$Country == "Estonia" & dat$Year >= 1999|
      dat$Country == "Mauritania" & dat$Year >= 1999|
      dat$Country == "Dominica" & dat$Year >= 2002|
      dat$Country == "Indonesia" & dat$Year >= 1993|
      dat$Country == "Costa Rica" & dat$Year >= 1999|
      dat$Country == "Denmark" & dat$Year >= 2004| ## strategy in 1996
      dat$Country == "Romania" & dat$Year >= 1996|
      dat$Country == "India" & dat$Year >= 1999|
      dat$Country == "Sri Lanka" & dat$Year >= 1998|
      dat$Country == "Guyana" & dat$Year >= 2015| ## action plan in 1999
      dat$Country == "Uruguay" & dat$Year >= 1999|
      dat$Country == "Armenia" & dat$Year >= 1999|
      dat$Country == "Albania" & dat$Year >= 1999|
      dat$Country == "Canada" & dat$Year >= 1996|
      dat$Country == "Kyrgyzstan" & dat$Year >= 1998|
      dat$Country == "New Zealand" & dat$Year >= 2000|
      dat$Country == "Belize" & dat$Year >= 1998|
      dat$Country == "Lithuania" & dat$Year >= 1998|
      dat$Country == "Lebanon" & dat$Year >= 1998|#
      dat$Country == "Belarus" & dat$Year >= 1997|
      dat$Country == "Austria" & dat$Year >= 1998|
      dat$Country == "Burkina Faso" & dat$Year >= 1998|
      dat$Country == "Egypt" & dat$Year >= 1998|
      dat$Country == "Ukraine" & dat$Year >= 2000|
      dat$Country == "Malaysia" & dat$Year >= 1998|
      dat$Country == "Seychelles" & dat$Year >= 2008|
      dat$Country == "Korea" & dat$Year >= 1997|
      dat$Country == "Bhutan" & dat$Year >= 1997|
      dat$Country == "United Kingdom" & dat$Year >= 1994|
      dat$Country == "Australia" & dat$Year >= (1996)|
      dat$Country == "Finland" & dat$Year >= (1997)|
      dat$Country == "Philippines" & dat$Year >= (1997)|
      dat$Country == "Japan" & dat$Year >= (1995)|
      dat$Country == "Benin" & dat$Year >= (2002)|
      dat$Country == "Bolivia" & dat$Year >= (2001)|
      dat$Country == "Bosnia and Herzegovina" & dat$Year >= (2011)|
      dat$Country == "Botswana" & dat$Year >= (2005)|
      dat$Country == "Brazil" & dat$Year >= (2002)|
      dat$Country == "Bulgaria" & dat$Year >= (2000)|
      dat$Country == "Burundi" & dat$Year >= (2000)|
      dat$Country == "Central African Republic" & dat$Year >= (2003)|
      dat$Country == "Chad" & dat$Year >= (1999)|
      dat$Country == "China (People's Republic of)" & dat$Year >= (1994)|
      dat$Country == "Colombia" & dat$Year >= (2005)|
      dat$Country == "Congo" & dat$Year >= (2001)|
      dat$Country == "Cook Islands" & dat$Year >= (2002)|
      dat$Country == "Côte d'Ivoire" & dat$Year >= (2002)|
      dat$Country == "Cuba" & dat$Year >= (1997)|
      dat$Country == "Ecuador" & dat$Year >= (2001)|
      dat$Country == "Eswatini" & dat$Year >= (2001)|
      dat$Country == "Fiji" & dat$Year >= (2003)|
      dat$Country == "France" & dat$Year >= (2004)|
      dat$Country == "Gambia" & dat$Year >= (1999)|
      dat$Country == "Hungary" & dat$Year >= (2008)|
      dat$Country == "Iran" & dat$Year >= (2006)|
      dat$Country == "Jordan" & dat$Year >= (2001)|
      dat$Country == "Kazakhstan" & dat$Year >= (1999)|#NBSAP
      dat$Country == "Kiribati" & dat$Year >= (1996)|
      dat$Country == "Luxembourg" & dat$Year >= (2007)|
      dat$Country == "Mongolia" & dat$Year >= (1996)|
      dat$Country == "Mozambique" & dat$Year >= (2001)|
      dat$Country == "Namibia" & dat$Year >= (2002)|
      dat$Country == "Niger" & dat$Year >= (2000)|
      dat$Country == "North Macedonia" & dat$Year >= (2005)|
      dat$Country == "Norway" & dat$Year >= (2001)|
      dat$Country == "Pakistan" & dat$Year >= (1999)|
      dat$Country == "Papua New Guinea" & dat$Year >= (2007)|
      dat$Country == "Paraguay" & dat$Year >= (2003)|
      dat$Country == "Saint Lucia" & dat$Year >= (2000)|
      dat$Country == "Samoa" & dat$Year >= (2001)|
      dat$Country == "Senegal" & dat$Year >= (1998)|
      dat$Country == "Slovak Republic" & dat$Year >= (1998)|
      dat$Country == "Switzerland" & dat$Year >= (2006)|
      dat$Country == "Tunisia" & dat$Year >= (1998)|
      dat$Country == "Venezuela" & dat$Year >= (2001)|
      dat$Country == "Uzbekistan" & dat$Year >= (1998)|
      dat$Country == "Singapore" & dat$Year >= (1992)|
      dat$Country == "Netherlands" & dat$Year >= (1995)|
      dat$Country == "Iceland" & dat$Year >= (2010)| ## strategy in 2008
      dat$Country == "Cyprus" & dat$Year >= (2020)|
      dat$Country == "Bangladesh" & dat$Year >= (2006)|
      dat$Country == "Bahrain" & dat$Year >= (2007)|
      dat$Country == "Zambia" & dat$Year >= (1999)|
      dat$Country == "Suriname" & dat$Year >= (2013)| ## strategy in 2006
      dat$Country == "Sweden" & dat$Year >= (1995)|
      dat$Country == "Viet Nam" & dat$Year >= 1994,]$NBSAP <- 1

# Creating 5 year time lag variable
dat$NBSAP_Lag_5 <- rep(0, length(dat$Country))

dat[dat$Country == "Monaco" & dat$Year >= (2021+5)|
      dat$Country == "Haiti" & dat$Year >= (2020+5)|
      dat$Country == "South Sudan" & dat$Year >= (2018+5)| #
      dat$Country == "San Marino" & dat$Year >= (2018+5)|
      dat$Country == "Andorra" & dat$Year >= (2016+5)|
      dat$Country == "Nauru" & dat$Year >= (2010+5)|
      dat$Country == "Somalia" & dat$Year >= (2015+5)|
      dat$Country == "Iraq" & dat$Year >= (2015+5)|
      dat$Country == "Brunei Darussalam" & dat$Year >= (2015+5)|
      dat$Country == "Antigua and Barbuda" & dat$Year >= (2014+5)|
      dat$Country == "Greece" & dat$Year >= (2014+5)|
      dat$Country == "United Arab Emirates" & dat$Year >= (2014+5)|
      dat$Country == "Afghanistan" & dat$Year >= (2014+5)|
      dat$Country == "Liechtenstein" & dat$Year >= (2014+5)|
      dat$Country == "Tuvalu" & dat$Year >= (2013+5)|
      dat$Country == "Malta" & dat$Year >= (2012+5)|
      dat$Country == "Myanmar" & dat$Year >= (2011+5)|#
      dat$Country == "Timor-Leste" & dat$Year >= (2011+5)|
      dat$Country == "Dominican Republic" & dat$Year >= (2011+5)|
      dat$Country == "Serbia" & dat$Year >= (2011+5)|
      dat$Country == "Italy" & dat$Year >= (2010+5)|
      dat$Country == "Tanzania" & dat$Year >= (2004+5)|
      dat$Country == "Montenegro" & dat$Year >= (2010+5)|
      dat$Country == "Kuwait" & dat$Year >= (1997+5)| ##completed not adopted
      dat$Country == "Solomon Islands" & dat$Year >= (2009+5)|
      dat$Country == "Israel" & dat$Year >= (2010+5)|
      dat$Country == "Cabo Verde" & dat$Year >= (1999+5)|
      dat$Country == "Saudi Arabia" & dat$Year >= (2005+5)|
      dat$Country == "Saint Kitts and Nevis" & dat$Year >= (2004+5)|
      dat$Country == "Saint Vincent and the Grenadines" & dat$Year >= (2004+5)|
      dat$Country == "Equatorial Guinea" & dat$Year >= (2005+5)|
      dat$Country == "Azerbaijan" & dat$Year >= (2004+5)|
      dat$Country == "Malawi" & dat$Year >= (2006+5)|
      dat$Country == "Palau" & dat$Year >= (2005+5)|
      dat$Country == "Mauritius" & dat$Year >= (2006+5)|
      dat$Country == "Ethiopia" & dat$Year >= (2006+5)|
      dat$Country == "Togo" & dat$Year >= (2003+5)|
      dat$Country == "Germany" & dat$Year >= (2007+5)|
      dat$Country == "Tonga" & dat$Year >= (2006+5)|
      dat$Country == "Belgium" & dat$Year >= (2007+5)|
      dat$Country == "Uganda" & dat$Year >= (2002+5)|
      dat$Country == "Angola" & dat$Year >= (2006+5)|
      dat$Country == "Nigeria" & dat$Year >= (2006+5)|
      dat$Country == "Guinea-Bissau" & dat$Year >= (2001+5)|
      dat$Country == "Mali" & dat$Year >= (2001+5)|
      dat$Country == "Türkiye" & dat$Year >= (2001+5)|
      dat$Country == "South Africa" & dat$Year >= (2005+5)|
      dat$Country == "Syrian Arab Republic" & dat$Year >= (2002+5)|
      dat$Country == "Sierra Leone" & dat$Year >= (2003+5)|
      dat$Country == "Surinam" & dat$Year >= (2013+5)| #strategy released in (2006
      dat$Country == "Czechia" & dat$Year >= (2005+5)|
      dat$Country == "Trinidad and Tobago" & dat$Year >= (2001+5)|
      dat$Country == "Lao People's Democratic Republic" & dat$Year >= (2004+5)|
      dat$Country == "Qatar" & dat$Year >= (2004+5)|
      dat$Country == "Sao Tome and Principe" & dat$Year >= (2005+5)|
      dat$Country == "Poland" & dat$Year >= (2003+5)|
      dat$Country == "Yemen" & dat$Year >= (2005+5)|
      dat$Country == "Algeria" & dat$Year >= (2005+5)|
      dat$Country == "Liberia" & dat$Year >= (2003+5)|
      dat$Country == "Eritrea" & dat$Year >= (2000+5)|
      dat$Country == "Gabon" & dat$Year >= (1999+5)|
      dat$Country == "Chile" & dat$Year >= (2003+5)|
      dat$Country == "Lesotho" & dat$Year >= (2000+5)|
      dat$Country == "Democratic People's Republic of Korea" & dat$Year >= (1998+5)|
      dat$Country == "Tajikistan" & dat$Year >= (2003+5)|
      dat$Country == "Sudan" & dat$Year >= (2000+5)|
      dat$Country == "Sweeden" & dat$Year >= (1995+5)|
      dat$Country == "Jamaica" & dat$Year >= (2003+5)|
      dat$Country == "Honduras" & dat$Year >= (2001+5)|
      dat$Country == "Oman" & dat$Year >= (2001+5)|
      dat$Country == "Comoros" & dat$Year >= (2000+5)|
      dat$Country == "El Salvador" & dat$Year >= (1999+5)|
      dat$Country == "Zambia" & dat$Year >= (1999+5)|
      dat$Country == "Latvia" & dat$Year >= (2000+5)|
      dat$Country == "Thailand" & dat$Year >= (1997+5)|
      dat$Country == "Rwanda" & dat$Year >= (2003+5)|
      dat$Country == "Marshall Islands" & dat$Year >= (2000+5)|
      dat$Country == "Turkmenistan" & dat$Year >= (2002+5)|
      dat$Country == "Georgia" & dat$Year >= (2005+5)|
      dat$Country == "Barbados" & dat$Year >= (2002+5)|
      dat$Country == "Maldives" & dat$Year >= (2002+5)|
      dat$Country == "Ghana" & dat$Year >= (2016+5)| ## strategy in 2002
      dat$Country == "Madagascar" & dat$Year >= (2002+5)|
      dat$Country == "Bahamas" & dat$Year >= (1999+5)|
      dat$Country == "Morocco" & dat$Year >= (2002+5)|
      dat$Country == "Cambodia" & dat$Year >= (2002+5)|
      dat$Country == "Nepal" & dat$Year >= (2002+5)|
      dat$Country == "Moldova" & dat$Year >= (2000+5)|
      dat$Country == "Djibouti" & dat$Year >= (2001+5)|
      dat$Country == "Slovenia" & dat$Year >= (2001+5)| ## strategy only
      dat$Country == "Ireland" & dat$Year >= (2002+5)|
      dat$Country == "Micronesia" & dat$Year >= (2002+5)|
      dat$Country == "Guinea" & dat$Year >= (2001+5)|
      dat$Country == "Russia" & dat$Year >= (2001+5)|
      dat$Country == "Nicaragua" & dat$Year >= (2001+5)|
      dat$Country == "Vanuatu" & dat$Year >= (2000+5)|
      dat$Country == "Niue" & dat$Year >= (2001+5)|
      dat$Country == "Democratic Republic of the Congo" & dat$Year >= (2000+5)|
      dat$Country == "Guatemala" & dat$Year >= (1999+5)|
      dat$Country == "Peru" & dat$Year >= (2001+5)|
      dat$Country == "Portugal" & dat$Year >= (2001+5)|
      dat$Country == "Argentina" & dat$Year >= (2003+5)|
      dat$Country == "Spain" & dat$Year >= (2011+5)|
      dat$Country == "Croatia" & dat$Year >= (1999+5)|
      dat$Country == "Grenada" & dat$Year >= (2000+5)|
      dat$Country == "Panama" & dat$Year >= (2000+5)|
      dat$Country == "Mexico" & dat$Year >= (2000+5)|
      dat$Country == "Cameroon" & dat$Year >= (1999+5)|
      dat$Country == "Kenya" & dat$Year >= (1999+5)|
      dat$Country == "Zimbabwe" & dat$Year >= (2002+5)|
      dat$Country == "Estonia" & dat$Year >= (1999+5)|
      dat$Country == "Mauritania" & dat$Year >= (1999+5)|
      dat$Country == "Dominica" & dat$Year >= (2002+5)|
      dat$Country == "Indonesia" & dat$Year >= (1993+5)|
      dat$Country == "Costa Rica" & dat$Year >= (1999+5)|
      dat$Country == "Denmark" & dat$Year >= (2004+5)| ## strategy in (1996
      dat$Country == "Romania" & dat$Year >= (1996+5)|
      dat$Country == "India" & dat$Year >= (1999+5)|
      dat$Country == "Sri Lanka" & dat$Year >= (1998+5)|
      dat$Country == "Guyana" & dat$Year >= (2015+5)| ## action plan in (1999
      dat$Country == "Uruguay" & dat$Year >= (1999+5)|
      dat$Country == "Armenia" & dat$Year >= (1999+5)|
      dat$Country == "Albania" & dat$Year >= (1999+5)|
      dat$Country == "Canada" & dat$Year >= (1996+5)|
      dat$Country == "Kyrgyzstan" & dat$Year >= (1998+5)|
      dat$Country == "New Zealand" & dat$Year >= (2000+5)|
      dat$Country == "Belize" & dat$Year >= (1998+5)|
      dat$Country == "Lithuania" & dat$Year >= (1998+5)|
      dat$Country == "Lebanon" & dat$Year >= (1998+5)|#
      dat$Country == "Belarus" & dat$Year >= (1997+5)|
      dat$Country == "Austria" & dat$Year >= (1998+5)|
      dat$Country == "Burkina Faso" & dat$Year >= (1998+5)|
      dat$Country == "Egypt" & dat$Year >= (1998+5)|
      dat$Country == "Ukraine" & dat$Year >= (2000+5)|
      dat$Country == "Malaysia" & dat$Year >= (1998+5)|
      dat$Country == "Seychelles" & dat$Year >= (2008+5)|
      dat$Country == "Korea" & dat$Year >= (1997+5)|
      dat$Country == "Bhutan" & dat$Year >= (1997+5)|
      dat$Country == "United Kingdom" & dat$Year >= (1994+5)|
      dat$Country == "Australia" & dat$Year >= (1996+5)|
      dat$Country == "Finland" & dat$Year >= (1997+5)|
      dat$Country == "Philippines" & dat$Year >= (1997+5)|
      dat$Country == "Japan" & dat$Year >= (1995+5)|
      dat$Country == "Benin" & dat$Year >= (2002+5)|
      dat$Country == "Bolivia" & dat$Year >= (2001+5)|
      dat$Country == "Bosnia and Herzegovina" & dat$Year >= (2011+5)|
      dat$Country == "Botswana" & dat$Year >= (2005+5)|
      dat$Country == "Brazil" & dat$Year >= (2002+5)|
      dat$Country == "Bulgaria" & dat$Year >= (2000+5)|
      dat$Country == "Burundi" & dat$Year >= (2000+5)|
      dat$Country == "Central African Republic" & dat$Year >= (2003+5)|
      dat$Country == "Chad" & dat$Year >= (1999+5)|
      dat$Country == "China (People's Republic of)" & dat$Year >= (1994+5)|
      dat$Country == "Colombia" & dat$Year >= (2005+5)|
      dat$Country == "Congo" & dat$Year >= (2001+5)|
      dat$Country == "Cook Islands" & dat$Year >= (2002+5)|
      dat$Country == "Côte d'Ivoire" & dat$Year >= (2002+5)|
      dat$Country == "Cuba" & dat$Year >= (1997+5)|
      dat$Country == "Ecuador" & dat$Year >= (2001+5)|
      dat$Country == "Eswatini" & dat$Year >= (2001+5)|
      dat$Country == "Fiji" & dat$Year >= (2003+5)|
      dat$Country == "France" & dat$Year >= (2004+5)|
      dat$Country == "Gambia" & dat$Year >= (1999+5)|
      dat$Country == "Hungary" & dat$Year >= (2008+5)|
      dat$Country == "Iran" & dat$Year >= (2006+5)|
      dat$Country == "Jordan" & dat$Year >= (2001+5)|
      dat$Country == "Kazakhstan" & dat$Year >= (1999+5)|#NBSAP
      dat$Country == "Kiribati" & dat$Year >= (1996+5)|
      dat$Country == "Luxembourg" & dat$Year >= (2007+5)|
      dat$Country == "Mongolia" & dat$Year >= (1996+5)|
      dat$Country == "Mozambique" & dat$Year >= (2001+5)|
      dat$Country == "Namibia" & dat$Year >= (2002+5)|
      dat$Country == "Niger" & dat$Year >= (2000+5)|
      dat$Country == "North Macedonia" & dat$Year >= (2005+5)|
      dat$Country == "Norway" & dat$Year >= (2001+5)|
      dat$Country == "Pakistan" & dat$Year >= (1999+5)|
      dat$Country == "Papua New Guinea" & dat$Year >= (2007+5)|
      dat$Country == "Paraguay" & dat$Year >= (2003+5)|
      dat$Country == "Saint Lucia" & dat$Year >= (2000+5)|
      dat$Country == "Samoa" & dat$Year >= (2001+5)|
      dat$Country == "Senegal" & dat$Year >= (1998+5)|
      dat$Country == "Slovak Republic" & dat$Year >= (1998+5)|
      dat$Country == "Switzerland" & dat$Year >= (2006+5)|
      dat$Country == "Tunisia" & dat$Year >= (1998+5)|
      dat$Country == "Venezuela" & dat$Year >= (2001+5)|
      dat$Country == "Uzbekistan" & dat$Year >= (1998+5)|
      dat$Country == "Singapore" & dat$Year >= (1992+5)|
      dat$Country == "Netherlands" & dat$Year >= (1995+5)|
      dat$Country == "Iceland" & dat$Year >= (2010+5)| ## strategy in 2008
      dat$Country == "Cyprus" & dat$Year >= (2020+5)|
      dat$Country == "Bangladesh" & dat$Year >= (2006+5)|
      dat$Country == "Bahrain" & dat$Year >= (2007+5)|
      dat$Country == "Zambia" & dat$Year >= (1999+5)|
      dat$Country == "Suriname" & dat$Year >= (2013+5)|
      dat$Country == "Sweden" & dat$Year >= (1995+5)|
      dat$Country == "Viet Nam" & dat$Year >= (1994+5),]$NBSAP_Lag_5 <- 1

# Creating 10 year time lag variable
dat$NBSAP_Lag_10 <- rep(0, length(dat$Country))

dat[dat$Country == "Monaco" & dat$Year >= (2021+10)|
      dat$Country == "Haiti" & dat$Year >= (2020+10)|
      dat$Country == "South Sudan" & dat$Year >= (2018+10)| #
      dat$Country == "San Marino" & dat$Year >= (2018+10)|
      dat$Country == "Andorra" & dat$Year >= (2016+10)|
      dat$Country == "Nauru" & dat$Year >= (2010+10)|
      dat$Country == "Somalia" & dat$Year >= (2015+10)|
      dat$Country == "Iraq" & dat$Year >= (2015+10)|
      dat$Country == "Brunei Darussalam" & dat$Year >= (2015+10)|
      dat$Country == "Antigua and Barbuda" & dat$Year >= (2014+10)|
      dat$Country == "Greece" & dat$Year >= (2014+10)|
      dat$Country == "United Arab Emirates" & dat$Year >= (2014+10)|
      dat$Country == "Afghanistan" & dat$Year >= (2014+10)|
      dat$Country == "Liechtenstein" & dat$Year >= (2014+10)|
      dat$Country == "Tuvalu" & dat$Year >= (2013+10)|
      dat$Country == "Malta" & dat$Year >= (2012+10)|
      dat$Country == "Myanmar" & dat$Year >= (2011+10)|#
      dat$Country == "Timor-Leste" & dat$Year >= (2011+10)|
      dat$Country == "Dominican Republic" & dat$Year >= (2011+10)|
      dat$Country == "Serbia" & dat$Year >= (2011+10)|
      dat$Country == "Italy" & dat$Year >= (2010+10)|
      dat$Country == "Tanzania" & dat$Year >= (2004+10)|
      dat$Country == "Montenegro" & dat$Year >= (2010+10)|
      dat$Country == "Kuwait" & dat$Year >= (1997+10)| ##completed not adopted
      dat$Country == "Solomon Islands" & dat$Year >= (2009+10)|
      dat$Country == "Israel" & dat$Year >= (2010+10)|
      dat$Country == "Cabo Verde" & dat$Year >= (1999+10)|
      dat$Country == "Saudi Arabia" & dat$Year >= (2005+10)|
      dat$Country == "Saint Kitts and Nevis" & dat$Year >= (2004+10)|
      dat$Country == "Saint Vincent and the Grenadines" & dat$Year >= (2004+10)|
      dat$Country == "Equatorial Guinea" & dat$Year >= (2005+10)|
      dat$Country == "Azerbaijan" & dat$Year >= (2004+10)|
      dat$Country == "Malawi" & dat$Year >= (2006+10)|
      dat$Country == "Palau" & dat$Year >= (2005+10)|
      dat$Country == "Mauritius" & dat$Year >= (2006+10)|
      dat$Country == "Ethiopia" & dat$Year >= (2006+10)|
      dat$Country == "Togo" & dat$Year >= (2003+10)|
      dat$Country == "Germany" & dat$Year >= (2007+10)|
      dat$Country == "Tonga" & dat$Year >= (2006+10)|
      dat$Country == "Belgium" & dat$Year >= (2007+10)|
      dat$Country == "Uganda" & dat$Year >= (2002+10)|
      dat$Country == "Angola" & dat$Year >= (2006+10)|
      dat$Country == "Nigeria" & dat$Year >= (2006+10)|
      dat$Country == "Guinea-Bissau" & dat$Year >= (2001+10)|
      dat$Country == "Mali" & dat$Year >= (2001+10)|
      dat$Country == "Türkiye" & dat$Year >= (2001+10)|
      dat$Country == "South Africa" & dat$Year >= (2005+10)|
      dat$Country == "Syrian Arab Republic" & dat$Year >= (2002+10)|
      dat$Country == "Sierra Leone" & dat$Year >= (2003+10)|
      dat$Country == "Surinam" & dat$Year >= (2013+10)| #strategy released in (2006
      dat$Country == "Czechia" & dat$Year >= (2005+10)|
      dat$Country == "Trinidad and Tobago" & dat$Year >= (2001+10)|
      dat$Country == "Lao People's Democratic Republic" & dat$Year >= (2004+10)|
      dat$Country == "Qatar" & dat$Year >= (2004+10)|
      dat$Country == "Sao Tome and Principe" & dat$Year >= (2005+10)|
      dat$Country == "Poland" & dat$Year >= (2003+10)|
      dat$Country == "Yemen" & dat$Year >= (2005+10)|
      dat$Country == "Algeria" & dat$Year >= (2005+10)|
      dat$Country == "Liberia" & dat$Year >= (2003+10)|
      dat$Country == "Eritrea" & dat$Year >= (2000+10)|
      dat$Country == "Gabon" & dat$Year >= (1999+10)|
      dat$Country == "Chile" & dat$Year >= (2003+10)|
      dat$Country == "Lesotho" & dat$Year >= (2000+10)|
      dat$Country == "Democratic People's Republic of Korea" & dat$Year >= (1998+10)|
      dat$Country == "Tajikistan" & dat$Year >= (2003+10)|
      dat$Country == "Sudan" & dat$Year >= (2000+10)|
      dat$Country == "Sweeden" & dat$Year >= (1995+10)|
      dat$Country == "Jamaica" & dat$Year >= (2003+10)|
      dat$Country == "Honduras" & dat$Year >= (2001+10)|
      dat$Country == "Oman" & dat$Year >= (2001+10)|
      dat$Country == "Comoros" & dat$Year >= (2000+10)|
      dat$Country == "El Salvador" & dat$Year >= (1999+10)|
      dat$Country == "Zambia" & dat$Year >= (1999+10)|
      dat$Country == "Latvia" & dat$Year >= (2000+10)|
      dat$Country == "Thailand" & dat$Year >= (1997+10)|
      dat$Country == "Rwanda" & dat$Year >= (2003+10)|
      dat$Country == "Marshall Islands" & dat$Year >= (2000+10)|
      dat$Country == "Turkmenistan" & dat$Year >= (2002+10)|
      dat$Country == "Georgia" & dat$Year >= (2005+10)|
      dat$Country == "Barbados" & dat$Year >= (2002+10)|
      dat$Country == "Maldives" & dat$Year >= (2002+10)|
      dat$Country == "Ghana" & dat$Year >= (2016+10)| ## strategy in (2002
      dat$Country == "Madagascar" & dat$Year >= (2002+10)|
      dat$Country == "Bahamas" & dat$Year >= (1999+10)|
      dat$Country == "Morocco" & dat$Year >= (2002+10)|
      dat$Country == "Cambodia" & dat$Year >= (2002+10)|
      dat$Country == "Nepal" & dat$Year >= (2002+10)|
      dat$Country == "Moldova" & dat$Year >= (2000+10)|
      dat$Country == "Djibouti" & dat$Year >= (2001+10)|
      dat$Country == "Slovenia" & dat$Year >= (2001+10)| ## strategy only
      dat$Country == "Ireland" & dat$Year >= (2002+10)|
      dat$Country == "Micronesia" & dat$Year >= (2002+10)|
      dat$Country == "Guinea" & dat$Year >= (2001+10)|
      dat$Country == "Russia" & dat$Year >= (2001+10)|
      dat$Country == "Nicaragua" & dat$Year >= (2001+10)|
      dat$Country == "Vanuatu" & dat$Year >= (2000+10)|
      dat$Country == "Niue" & dat$Year >= (2001+10)|
      dat$Country == "Democratic Republic of the Congo" & dat$Year >= (2000+10)|
      dat$Country == "Guatemala" & dat$Year >= (1999+10)|
      dat$Country == "Peru" & dat$Year >= (2001+10)|
      dat$Country == "Portugal" & dat$Year >= (2001+10)|
      dat$Country == "Argentina" & dat$Year >= (2003+10)|
      dat$Country == "Spain" & dat$Year >= (2011+10)|
      dat$Country == "Croatia" & dat$Year >= (1999+10)|
      dat$Country == "Grenada" & dat$Year >= (2000+10)|
      dat$Country == "Panama" & dat$Year >= (2000+10)|
      dat$Country == "Mexico" & dat$Year >= (2000+10)|
      dat$Country == "Cameroon" & dat$Year >= (1999+10)|
      dat$Country == "Kenya" & dat$Year >= (1999+10)|
      dat$Country == "Zimbabwe" & dat$Year >= (2002+10)|
      dat$Country == "Estonia" & dat$Year >= (1999+10)|
      dat$Country == "Mauritania" & dat$Year >= (1999+10)|
      dat$Country == "Dominica" & dat$Year >= (2002+10)|
      dat$Country == "Indonesia" & dat$Year >= (1993+10)|
      dat$Country == "Costa Rica" & dat$Year >= (1999+10)|
      dat$Country == "Denmark" & dat$Year >= (2004+10)| ## strategy in (1996
      dat$Country == "Romania" & dat$Year >= (1996+10)|
      dat$Country == "India" & dat$Year >= (1999+10)|
      dat$Country == "Sri Lanka" & dat$Year >= (1998+10)|
      dat$Country == "Guyana" & dat$Year >= (2015+10)| ## action plan in (1999
      dat$Country == "Uruguay" & dat$Year >= (1999+10)|
      dat$Country == "Armenia" & dat$Year >= (1999+10)|
      dat$Country == "Albania" & dat$Year >= (1999+10)|
      dat$Country == "Canada" & dat$Year >= (1996+10)|
      dat$Country == "Kyrgyzstan" & dat$Year >= (1998+10)|
      dat$Country == "New Zealand" & dat$Year >= (2000+10)|
      dat$Country == "Belize" & dat$Year >= (1998+10)|
      dat$Country == "Lithuania" & dat$Year >= (1998+10)|
      dat$Country == "Lebanon" & dat$Year >= (1998+10)|#
      dat$Country == "Belarus" & dat$Year >= (1997+10)|
      dat$Country == "Austria" & dat$Year >= (1998+10)|
      dat$Country == "Burkina Faso" & dat$Year >= (1998+10)|
      dat$Country == "Egypt" & dat$Year >= (1998+10)|
      dat$Country == "Ukraine" & dat$Year >= (2000+10)|
      dat$Country == "Malaysia" & dat$Year >= (1998+10)|
      dat$Country == "Seychelles" & dat$Year >= (2008+10)|
      dat$Country == "Korea" & dat$Year >= (1997+10)|
      dat$Country == "Bhutan" & dat$Year >= (1997+10)|
      dat$Country == "United Kingdom" & dat$Year >= (1994+10)|
      dat$Country == "Australia" & dat$Year >= (1996+10)|
      dat$Country == "Finland" & dat$Year >= (1997+10)|
      dat$Country == "Philippines" & dat$Year >= (1997+10)|
      dat$Country == "Japan" & dat$Year >= (1995+10)|
      dat$Country == "Benin" & dat$Year >= (2002+10)|
      dat$Country == "Bolivia" & dat$Year >= (2001+10)|
      dat$Country == "Bosnia and Herzegovina" & dat$Year >= (2011+10)|
      dat$Country == "Botswana" & dat$Year >= (2005+10)|
      dat$Country == "Brazil" & dat$Year >= (2002+10)|
      dat$Country == "Bulgaria" & dat$Year >= (2000+10)|
      dat$Country == "Burundi" & dat$Year >= (2000+10)|
      dat$Country == "Central African Republic" & dat$Year >= (2003+10)|
      dat$Country == "Chad" & dat$Year >= (1999+10)|
      dat$Country == "China (People's Republic of)" & dat$Year >= (1994+10)|
      dat$Country == "Colombia" & dat$Year >= (2005+10)|
      dat$Country == "Congo" & dat$Year >= (2001+10)|
      dat$Country == "Cook Islands" & dat$Year >= (2002+10)|
      dat$Country == "Côte d'Ivoire" & dat$Year >= (2002+10)|
      dat$Country == "Cuba" & dat$Year >= (1997+10)|
      dat$Country == "Ecuador" & dat$Year >= (2001+10)|
      dat$Country == "Eswatini" & dat$Year >= (2001+10)|
      dat$Country == "Fiji" & dat$Year >= (2003+10)|
      dat$Country == "France" & dat$Year >= (2004+10)|
      dat$Country == "Gambia" & dat$Year >= (1999+10)|
      dat$Country == "Hungary" & dat$Year >= (2008+10)|
      dat$Country == "Iran" & dat$Year >= (2006+10)|
      dat$Country == "Jordan" & dat$Year >= (2001+10)|
      dat$Country == "Kazakhstan" & dat$Year >= (1999+10)|#NBSAP
      dat$Country == "Kiribati" & dat$Year >= (1996+10)|
      dat$Country == "Luxembourg" & dat$Year >= (2007+10)|
      dat$Country == "Mongolia" & dat$Year >= (1996+10)|
      dat$Country == "Mozambique" & dat$Year >= (2001+10)|
      dat$Country == "Namibia" & dat$Year >= (2002+10)|
      dat$Country == "Niger" & dat$Year >= (2000+10)|
      dat$Country == "North Macedonia" & dat$Year >= (2005+10)|
      dat$Country == "Norway" & dat$Year >= (2001+10)|
      dat$Country == "Pakistan" & dat$Year >= (1999+10)|
      dat$Country == "Papua New Guinea" & dat$Year >= (2007+10)|
      dat$Country == "Paraguay" & dat$Year >= (2003+10)|
      dat$Country == "Saint Lucia" & dat$Year >= (2000+10)|
      dat$Country == "Samoa" & dat$Year >= (2001+10)|
      dat$Country == "Senegal" & dat$Year >= (1998+10)|
      dat$Country == "Slovak Republic" & dat$Year >= (1998+10)|
      dat$Country == "Switzerland" & dat$Year >= (2006+10)|
      dat$Country == "Tunisia" & dat$Year >= (1998+10)|
      dat$Country == "Venezuela" & dat$Year >= (2001+10)|
      dat$Country == "Uzbekistan" & dat$Year >= (1998+10)|
      dat$Country == "Singapore" & dat$Year >= (1992+10)|
      dat$Country == "Netherlands" & dat$Year >= (1995+10)|
      dat$Country == "Iceland" & dat$Year >= (2010+10)| ## strategy in 2008
      dat$Country == "Cyprus" & dat$Year >= (2020+10)|
      dat$Country == "Bangladesh" & dat$Year >= (2006+10)|
      dat$Country == "Bahrain" & dat$Year >= (2007+10)|
      dat$Country == "Zambia" & dat$Year >= (1999+10)|
      dat$Country == "Suriname" & dat$Year >= (2013+10)|
      dat$Country == "Sweden" & dat$Year >= (1995+10)|
      dat$Country == "Viet Nam" & dat$Year >= (1994+10),]$NBSAP_Lag_10 <- 1

# Creating variable for change in Red List Index score

dat$Value_change <- rep(0, length(dat$Country))

for (i in 1:(length(dat$Country)-1)){
  dat$Value_change[i+1] <- dat$Value[i+1] - dat$Value[i]
}

dat <- dat[dat$Year != 2001,]
dat$Year <- as.numeric(dat$Year)
dat$Value <- as.numeric(dat$Value)
dat$Value_change <- as.numeric(dat$Value_change)
dat$NBSAP <- as.numeric(dat$NBSAP)
dat$NBSAP_Lag_5 <- as.numeric(dat$NBSAP_Lag_5)
dat$NBSAP_Lag_10 <- as.numeric(dat$NBSAP_Lag_10)

#sub-setting for countries that have implemented NBSAPs (check that n = 196)
dat <- dat[dat$Country %in% dat[dat$NBSAP==1 & dat$Year == 2021|
                                  dat$Country == "Libya"|
                                  dat$Country == "United States"|
                                  dat$Country == "Palestinian Authority or West Bank and Gaza Strip",]$Country,]


#length(unique(dat_plus_3$Country)) #should be 196
length(unique(dat$Country)) #should be 196

#### Preparing to join qog and datasets. ###

qog <- read_csv("Inputs/qog_std_ts_jan24.csv")
setdiff(dat$Country, as.factor(qog$cname_qog))
setdiff(dat$Country, as.factor(qog$cname_qog))

qog[qog$cname_qog == "Sudan (-2011)",]$cname_qog <- "Sudan"
qog[qog$cname_qog == "Sudan (2012-)",]$cname_qog <- "Sudan"
qog[qog$cname_qog == "Brunei",]$cname_qog <- "Brunei Darussalam"
qog[qog$cname_qog == "Cape Verde",]$cname_qog <- "Cabo Verde"
qog[qog$cname_qog == "China",]$cname_qog <- "China (People's Republic of)"
qog[qog$cname_qog == "Congo, Democratic Republic",]$cname_qog <- "Democratic Republic of the Congo"
qog[qog$cname_qog == "Cyprus (1975-)",]$cname_qog <- "Cyprus"
qog[qog$cname_qog == "Czech Republic",]$cname_qog <- "Czechia"
qog[qog$cname_qog == "Ethiopia (1993-)",]$cname_qog <- "Ethiopia"
qog[qog$cname_qog == "France (1963-)",]$cname_qog <- "France"
qog[qog$cname_qog == "Cote d'Ivoire",]$cname_qog <- "Côte d'Ivoire"
qog[qog$cname_qog == "Korea, North",]$cname_qog <- "Democratic People's Republic of Korea"
qog[qog$cname_qog == "Korea, South",]$cname_qog <- "Korea"
qog[qog$cname_qog == "Laos",]$cname_qog <- "Lao People's Democratic Republic"
qog[qog$cname_qog == "Malaysia (1966-)",]$cname_qog <- "Malaysia"
qog[qog$cname_qog == "Pakistan (1971-)",]$cname_qog <- "Pakistan"
qog[qog$cname_qog == "St Kitts and Nevis",]$cname_qog <- "Saint Kitts and Nevis"
qog[qog$cname_qog == "St Lucia",]$cname_qog <- "Saint Lucia"
qog[qog$cname_qog == "St Vincent and the Grenadines",]$cname_qog <- "Saint Vincent and the Grenadines"
qog[qog$cname_qog == "Slovakia",]$cname_qog <- "Slovak Republic"
qog[qog$cname_qog == "Vietnam",]$cname_qog <- "Viet Nam"
qog[qog$cname_qog == "Eswatini (former Swaziland)",]$cname_qog <- "Eswatini"
qog[qog$cname_qog == "Syria",]$cname_qog <- "Syrian Arab Republic"
qog[qog$cname_qog == "Turkey",]$cname_qog <- "Türkiye"
qog[qog$cname_qog == "Pakistan (1971-)",]$cname_qog <- "Pakistan"
qog[qog$cname_qog == "Brazil",]$cname_qog <- "Brazil"
qog[qog$cname_qog == "Congo (the)",]$cname_qog <- "Congo"
qog[qog$cname_qog == "Honduras",]$cname_qog <- "Honduras"

# qog[qog$cname_qog == "Turkey",]$cname_qog <- "Cook Islands" does not exist in QOG
# qog[qog$cname_qog == "Turkey",]$cname_qog <- "Niue" does not exist in QOG
# qog[qog$cname_qog == "Turkey",]$cname_qog <- "Palestinian Authority or West Bank and Gaza Strip" does not exist in QOG

colnames(dat)[1] <- "cname_qog"
setdiff(dat$cname_qog, as.factor(qog$cname_qog))
setdiff(as.factor(qog$cname_qog),dat$cname_qog)


qog <- qog %>%
  filter(!cname_qog %in% c('Czechoslovakia', 'USSR', "Tibet", "Serbia and Montenegro", "Taiwan"))

qog <- qog[qog$year < 2022 & qog$year > 2001,]
qog$Value <- rep(NA, length(qog$cname))
qog$Value_change <- rep(NA, length(qog$cname))
qog$NBSAP <- rep(NA, length(qog$cname))
qog$NBSAP_Lag_5 <- rep(NA, length(qog$cname))
qog$NBSAP_Lag_10 <- rep(NA, length(qog$cname))


setdiff(dat$cname_qog, qog$cname_qog)
setdiff(qog$cname_qog, dat$cname_qog) 

qog_benchmark <- qog

qog <- qog %>%
  left_join(dat, by = c("cname_qog" = "cname_qog", "year" = "Year")) %>%
  mutate(NBSAP = coalesce(NBSAP.x, NBSAP.y),
         NBSAP_Lag_5 = coalesce(NBSAP_Lag_5.x, NBSAP_Lag_5.y),
         NBSAP_Lag_10 = coalesce(NBSAP_Lag_10.x, NBSAP_Lag_10.y),
         Value = coalesce(Value.x, Value.y),
         Value_change = coalesce(Value_change.x, Value_change.y)) %>%
  select(-ends_with(".x"), -ends_with(".y"))

colnames(qog)[3] <- "Year"

### Region ###

wb_dat <- read_csv("Inputs/wb_regions.csv")
wb_dat <- wb_dat[,c(1,4)]
colnames(wb_dat)[1] <- "cname_qog"
setdiff(as.factor(qog$cname_qog),wb_dat$cname_qog)

wb_dat[wb_dat$cname_qog == "Brunei",]$cname_qog <- "Brunei Darussalam"
wb_dat[wb_dat$cname_qog == "Cape Verde",]$cname_qog <- "Cabo Verde"
wb_dat[wb_dat$cname_qog == "North Korea",]$cname_qog <- "Democratic People's Republic of Korea"
wb_dat[wb_dat$cname_qog == "Laos",]$cname_qog <- "Lao People's Democratic Republic"
wb_dat[wb_dat$cname_qog == "Slovakia",]$cname_qog <- "Slovak Republic"
wb_dat[wb_dat$cname_qog == "Turkey",]$cname_qog <- "Türkiye" 
wb_dat[wb_dat$cname_qog == "China",]$cname_qog <- "China (People's Republic of)"
wb_dat[wb_dat$cname_qog == "Democratic Republic of Congo",]$cname_qog <- "Democratic Republic of the Congo" 
wb_dat[wb_dat$cname_qog == "Cote d'Ivoire",]$cname_qog <- "Côte d'Ivoire"
wb_dat[wb_dat$cname_qog == "South Korea",]$cname_qog <- "Korea" 
wb_dat[wb_dat$cname_qog == "East Timor",]$cname_qog <- "Timor-Leste"
wb_dat[wb_dat$cname_qog == "Vietnam",]$cname_qog <- "Viet Nam"
wb_dat[wb_dat$cname_qog == "Syria",]$cname_qog <- "Syrian Arab Republic" 

qog <- left_join(qog,wb_dat, by = "cname_qog")

colnames(qog)[1997] <- "wb_region"
qog$wb_region <- as.factor(qog$wb_region)

#### RLI start value ####
qog$start_value <- rep(0, length(qog$cname))

for (i in 1:length(qog$cname)){
  qog$start_value[i] <- qog$Value[20*ceiling(i*(1/20))-19]
}

#### Percent protected ###

protected <- read.csv("Inputs/terrestrial-protected-areas.csv")
length(unique(intersect(qog$ccodealp,protected$Code)))
setdiff(qog$ccodealp,protected$Code) # San Marino missing

qog$pr_protected <- rep(NA, length(qog$cname))
for(i in 1:length(qog$cname)){
  if(qog$ccodealp[i] %in% protected$Code){
    qog$pr_protected[i] <- protected[protected$Code == qog$ccodealp[i] & protected$Year == 2021,]$Terrestrial.protected.areas....of.total.land.area./100
  }else{
    qog$pr_protected[i] <- NA
  }
}

### Island nation status ###

islands <- data.frame(c("ATG", "BHS", "BHR", "BRB", "BRN", "CPV","COM","CUB","CYP","DOM","DMA",
             "TLS","FJI","GRD","HTI","ISL","ISL","IDN", "IRL", "JAM","JPN",
             "KIR","MDG","MDV","MLT","MHL","MUS","FSM","NRU","NZL","PLW","PNG",
             "PHL","KNA","LCA","VCT","WSM","STP","SYC","SGP","SLB","LKA","TON",
             "TTO","TUV","GBR","VUT")) #had to add Dominica

length(unique(intersect(qog$ccodealp,islands))) #is 0 so all island nations represented
qog$island <- rep(NA, length(qog$cname))

for(i in 1:length(qog$cname)){
  if(qog$ccodealp[i] %in% islands[,1]){
    qog$island[i] <- 1
  }else{
    qog$island[i] <- 0
  } 
}

## Year Indicator ##

qog$year_ind <- (qog$Year - 2002)

#### Percent KBA covered ###

KBA_protected <- read.csv("Inputs/per_KBA_protected.csv")
colnames(KBA_protected)[4] <- "KBA_per_pro"
setdiff(qog$ccodealp,KBA_protected$Code) #Albania, Monaco, San Marino, and Tuvalu missing 

qog$KBA_protected <- rep(NA, length(qog$cname))
for(i in 1:length(qog$cname)){
  if(qog$ccodealp[i] %in% KBA_protected$Code){
    qog$KBA_protected[i] <- KBA_protected[KBA_protected$Code == qog$ccodealp[i] & KBA_protected$Year == qog$Year[i],]$KBA_per_pro/100
  }else{
    qog$KBA_protected[i] <- NA
  }
}

#### Percent of land indigenous ####

ind_dat <- read_excel("Inputs/percent_indigenous.xls")

# IC_T is percent of land controlled by indigenous populations (legally recognized or defacto)
ind_dat <- ind_dat[,c(1,4,6,9,12)]
colnames(ind_dat)[2] <- "cname"
ind_dat <- ind_dat[ind_dat$IC_T != "No data" ,] # Using full observations only

# Cleaning/standardizing IC_T and converting to numeric.

for (i in 1:length(ind_dat$IC_T)){
  if(ind_dat$IC_T[i] > 1){
    ind_dat$IC_T[i] <- round((as.numeric(gsub("%","", ind_dat$IC_T[i]))/100),4)
  }else{
    ind_dat$IC_T[i] <- round(as.numeric(gsub("%","", ind_dat$IC_T[i])),4)
  }
}

ind_dat$IC_T <- as.numeric(ind_dat$IC_T)
setdiff(ind_dat$ISO_Code, qog$ccodealp) #American Samoa, Bermuda, Cook Islands,
#Greenland, Niue, Taiwan, Tokelau, Western Sahara left out.

qog$indig_pcnt <- rep(NA,length(qog$cname))

for(i in 1:length(qog$cname)){
  if(qog$ccodealp[i] %in% ind_dat$ISO_Code){
    qog$indig_pcnt[i] <- ind_dat[qog$ccodealp[i] == ind_dat$ISO_Code,]$IC_T
  }else{
    qog$indig_pcnt[i] <- NA
  }
}

### Income into groups # https://blogs.worldbank.org/en/opendata/new-world-bank-country-classifications-income-level-2021-2022

qog$income_group <- cut(qog$wdi_gdpcapcur, breaks = c(-Inf,1045,4095,12695,Inf), labels = c("LIC", "LMIC", "MIC", "UIC"))


### FINAL STEPS ###

# Variables of interest:
# "Value_change","NBSAP","Value","wdi_gdpcapcur" or "income_group", "wdi_pop","wdi_popden", "wb_region", "br_com",
# fao_luagrcrop, fao_luforest, 


qog_short <- qog[,c("cname","Value", "Value_change","NBSAP","NBSAP_Lag_5", "NBSAP_Lag_10",
                   "wdi_gdpcapcur", "wdi_gdpcapgr","income_group", 
                    "wdi_pop","wdi_popden", "wb_region","fao_luagr","fao_luagrcrop","fao_lucrop",
                    "fh_fog","dr_ig", "year_ind","start_value",
                    "pr_protected", "KBA_protected", "indig_pcnt","island", "wdi_gini", "ccodealp", "fao_luforest",
                   "wdi_empagr")]

qog_short$fao_lucrop <- qog_short$fao_lucrop/100
qog_short$fao_luagr <- (as.numeric(qog_short$fao_luagr))/100
qog_short$fh_fog <- (as.numeric(qog_short$fh_fog))/12
qog_short$wdi_pop <- as.numeric(qog_short$wdi_pop)/1000000
qog_short$dr_ig <- (qog_short$dr_ig - min(na.omit(qog_short$dr_ig))) / (max(na.omit(qog_short$dr_ig)) - min(na.omit(qog_short$dr_ig)))
qog_short$fao_luagrcrop <- as.numeric(qog_short$fao_luagrcrop)/100
qog_short$wdi_gdpcapcur <- as.numeric(qog_short$wdi_gdpcapcur)/10000

summary(qog_short)

### Total Change ###
qog_short$total_change <- rep(NA, length(qog$cname))
for (i in 1:(length(qog$cname)/20)){
  sums <- 0
  for(j in 1:20){
    sums <- sum(qog_short[((i-1)*20)+(1:20),]$Value_change)
    qog_short[((i-1)*20)+(1:20),]$total_change <- sums
  }
}

hist(qog_short[qog_short$year_ind == 0,]$total_change,breaks = 80)
qog_short$wb_region <- as.factor(qog_short$wb_region)

summary(qog_short)

unique(qog_short$cname) #Verify this is 194
qog_short[qog_short$cname == "Sudan (the)",]$cname <- "Sudan"
unique(qog_short$cname) #Verify this is 193

qog_short$wb_region <- as.factor(qog_short$wb_region)
levels(qog_short$wb_region)
qog_short$wb_region <- relevel(qog_short$wb_region, ref = "Latin America and Caribbean")

write_csv(qog_short, "Outputs/qog_ts_short.csv")
