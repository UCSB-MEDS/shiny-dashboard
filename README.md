# An interactive dashboard for exploring Bren student career outcomes and admissions data

The [Bren Student Data Exporer](https://shinyapps.bren.ucsb.edu/student-data-explorer/) is an interactive dashboard that showcases career outcomes and admissions data for current students and recent alumni. It was developed with the intention of supporting:

- Prospective students in their decision-making as they explore the different degree programs at the Bren School
- Bren departments and staff with their reporting requirements
- All Bren communities and stakeholders from the past, present, and future by upholding data transparency and data integrity principles through an accessible application

## Dashboard updates & maintenance

Data are updated annually by Bren staff. Visit the [wiki](https://github.com/UCSB-MEDS/shiny-dashboard/wiki) to review important information and detailed instructions for updating and maintaining the Bren Student Data Explorer.

## Repository structure

```
.
├── bren-student-data-explorer/   # app directory
│   ├── r/                        # fxns for building inputs & outputs
│   ├── text/                     # static text elements
│   ├── www/                      # app images, styles, google analytics
│   ├── global.R
│   ├── server.R
│   └── ui.R
│
├── data-cleaning/                # data cleaning scripts for processing application data
│
├── .gitignore                      
├── README.md            
└── shiny-dashboard.Rproj 
```

## Update log
* **February 2023, updates by [Sam Shanny-Csik](https://github.com/samanthacsik):** refactored code base, added career data for MEDS and MESM graduating classes of 2022
* **July 2024, updates by [Sam Shanny-Csik](https://github.com/samanthacsik), [Jamie Montgomery](https://github.com/jamiecmontgomery), & [Kat Le](https://github.com/katleyq):** added career data for MEDS and MESM graduating classes of 2023, added admissions data for the 2023 entering classes, refactored code for maps (`{tmap}` > `{leaflet}` + removed data wrangling from server to improve loading speeds)
* **October 2024, updates by [Sam Shanny-Csik](https://github.com/samanthacsik):** redesigned career plots so that they are a bit easier to interpret, added a secondary table of job titles, and continued refactoring code (i.e. simplifying and removing unncessary code)
* **November 2024, updates by [Sam Shanny-Csik](https://github.com/samanthacsik):** updated demographics tab with 2024 incoming student data
* **May 2025, updates by [Sam Shanny-Csik](https://github.com/samanthacsik):** added career outcomes data for the graduating classes of 2024 and admissions data for the incoming classes of 2025; completed a major refactor to reorganize the file structure, establish a consistent data processing and cleaning pipeline, remove redundant code, and generalize functionality by replacing hard-coded values and text with dynamic, data- or user-defined inputs throughout.
