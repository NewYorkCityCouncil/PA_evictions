
# **PA_EVICTIONS**

This project creates the data behind the carto dashboards for evictions in NYC and other datasets that show tenant protections and distress for the acting public adovcate's page on evictions.

## Key Findings

The Bronx and central Brooklyn were experiencing the highest rate of evictions. Of properties with evictions, close to half (45%) have a rent stabilized unit, the multifamily dwellings (60% )are the most common building type and half (54%) were built between 1910-1930.

## Methodology and Sources

### Boro, Block & Lot (BBL)
The BBL field is used as the key column to match across all the datasets in the analysis. It is a unique property identifier for every lot in New York City. The BBL is composed of the borough number, block number and lot number. You may find the BBL for an address here. The NYC Department of City Planning’s [MapPLUTO](https://www1.nyc.gov/site/planning/data-maps/open-data.page) is used to map properties and to obtain property information.

### Evictions
The evictions in the analysis are City Marshal executed evictions from 2017 to the present. The dataset is updated daily on the NYC Open Data Portal and is provided by the Department of Investigations.

Eviction addresses are geocoded to obtain latitude, longitude and BBL information. Eviction addresses are cleaned before geocoding. For more details on how the addresses are cleaned, see [script](https://github.com/NewYorkCityCouncil/PA_evictions/blob/master/script.R). The addresses are geocoded with Geoclient API, a RESTful web service interface of the NYC Department of City Planning’s Geosupport system. In the case where the Geoclient API is unable to provide latitude, longitude, and BBL information, the eviction is removed from the analysis.

On the residential evictions map, the evictions point layer displays eviction address locations. In the case where there is more than one eviction at an address the point will appear darker, but information for only one eviction will be available in the pop up. Aggregated eviction information for an address is available on the building history map.

The eviction rate is calculated by dividing the total number evictions a property has ever had by the total number of residential units. Since the evictions are at the unit or apartment level, the assumption is made that an eviction can occur for a specific unit more than once over the time period of the dataset. It is rare for the same apartment to have had more than one tenant be evicted in a year. Apartment or unit information is left out to avoid the possibility of identifying anyone. Eviction rates for the district boundaries are calculated by dividing the aggregated total number of evictions and residential units in that district.

### Universal Language Access Law
The Universal Access Law layer in the analysis are the current [zipcodes](https://www.righttocounselnyc.org/faq) where income eligible New Yorkers can have access to free legal aid. In 2022, income eligible New Yorkers in all of NYC will have access.

### HPD Housing Maintenance Code Violations
The [Housing Maintenance Code Violations](https://data.cityofnewyork.us/Housing-Development/Housing-Maintenance-Code-Violations/wvxf-dwi5) in the analysis are class C violations that have been issued this year and the previous year. [Class C]() violations are the most serious. The dataset is updated daily on the NYC Open Data Portal and is provided by the Department of Housing Preservation & Development.

### DOB OATH/ECB Violations
The [DOB ECB violations](https://data.cityofnewyork.us/Housing-Development/DOB-ECB-Violations/6bgk-3dad) in the analysis are class 1 violations that have been issued this year and the previous year. [Class 1](https://www1.nyc.gov/site/buildings/safety/ecb-violations.page) violations are the most serious. The Department of Building’s [penalty schedule](https://www1.nyc.gov/assets/buildings/excel/penalty_schedule_with_codes.xlsx) was used to filter the infraction codes classified as class 1. The dataset is updated every weekday on the NYC Open Data Portal and is provided by the Department of Buildings.

### 311 Service Requests
The [311 Service Requests](https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9) in the analysis are Heat & Hot Water requests that have been made this year and the previous year. Heat and Hot Water complaints contain more detail on whether the incident affects the entire building or the apartment. The dataset is updated daily on the NYC Open Data Portal by 311.

### Certificate of No Harassment (CONH) Pilot Building List
The [CONH Pilot Building List](https://data.cityofnewyork.us/Housing-Development/Certification-of-No-Harassment-CONH-Pilot-Building/bzxi-2tsw) is a dataset that lists the buildings where property owners are required to file for Certificate of No Harassment before beginning certain construction work. The dataset is updated monthly on the NYC Open Data Portal by the Department of Housing Preservation and Development.

### Speculation Watch List
The [Speculation Watch List](https://data.cityofnewyork.us/Housing-Development/Speculation-Watch-List/adax-9mit) is a dataset that lists rent-regulated multiple dwelling properties that have a potential for speculation due to their recent sales. The dataset is updated quarterly on the NYC Open Data Portal by the Department of Housing Preservation and Development.

### Subsidies or Rent Stabilization
The Subsidies or Rent Stabilization layer is made up of the [Subsidized Housing Database](http://coredata.nyc/) from the NYU Furman Center’s CoreData.nyc and the [NYC Rent Stabilized Building Listings](https://www1.nyc.gov/site/rentguidelinesboard/resources/rent-stabilized-building-lists.page#tables) from the NYC Rent Guidelines Board. The Subsidized Housing Database is up to date as of June 27, 2018. For more information on a specific subsidy program, [visit](http://furmancenter.org/coredata/directory) Furman Center’s Directory of New York City Housing Programs. The Rent Stabilized Building Listings is updated every year. The Building Listing for 2017 is being used for the analysis while waiting for the 2018 listing to be posted.

### District Boundaries
Eviction totals are aggregated for zip codes, City Council, Community Board, State Senate, and State Assembly districts. District boundary shapefiles are available on the open data portal [here](https://data.cityofnewyork.us/browse?q=district%20boundaries&sortBy=relevance).

## Data Prep

The analysis was performed in R and visualized with Carto. Libraries used are stringr, data.table, RSocrata, anytime, sf, dplyr, and RCurl. An environment variable was created to make the api key for geocding hidden.

Across all datasets used, date and BBL fields were cleaned to analyze/subset the data by time and insure data merging, respectively.

There was significant text cleaning of addresses in the evictions dataset. The regular expressions `gsub()` and `strsplit()` were used to complete this task.


## Limitations & Data Quality

As mentioned above, eviction addresses that were unable to be geocoded were left out from the analysis. The number of evictions left out from the whole dataset is 6.5%. Next step to improve the geocoding of all addresses is to try out [GeoSearch API](https://geosearch.planninglabs.nyc/) from NYC Planning Labs.



