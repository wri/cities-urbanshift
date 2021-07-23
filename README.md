# cities-urbanshift

Welcome to tue UrbanSHift project repository.

**This is a work in progress repository! It is not yet an official WRI repository. 
For more information about the project status, please contact Eric.Mackres@wri.org.**


UrbanShift is a global program transforming cities for people and planet through sustainable, integrated and inclusive approaches to urban development.

UrbanShift is a GEF-funded program within [Urban Development](https://www.wri.org/cities/urban-development) and the [WRI Ross Center for Sustainable Cities](https://www.wri.org/cities). It is co-managed with UNEP, C40 Cities and ICLEI.

For more details and information about the project, please visit [the UrbanShift web page](https://www.wri.org/initiatives/urbanshift).

# Description activities

World Resources Institute (WRI) is providing several types of data-related assistance to participating cities:   

- **A suite of key geospatial data layers:** Administrative boundaries​  , Land cover (e.g., built-up/impervious, vegetation, trees, water)​ , Topography​ , Population density​, Built-up density​ , 
Land use (e.g., residential, commercial, industrial, vacant, agriculture)​, Amenities/service locations (e.g., jobs, health care, recreation)​ , Roads​ , Transit stops, routes and service, 
Buildings and other infrastructure,Emissions of air pollutants​, Natural hazards​.
- **Baseline measurements of indicators for core UrbanShift goals**: Green space / Land restoration , Greenhouse gas emissions  , Biodiversity 
- **Geospatial analysis on selected thematic area:** Climate and air pollution emissions mitigation, Accessibility to urban services, Urban expansion and land development, 
Climate hazards, vulnerability, risks and adaptation, Nature-based solutions, Biodiversity, Social, gender and environmental equity  
- **Capacity building and technical assistance** on data governance and as part of the City Academy and Labs modules of UrbanShift

# Repository structure

The repository is organised into 3 main folders with respect to the previously presenetd deliverables

## geospatial-layers

This folder hosts the different scripts, datasets and metadata description for addressing the first objective which consists of identifying and providing theme-based geospatial layers 
extracted from global open datasets.

The structure of this folder is as follows:

    geospatial-layers/
    ├── README.md                      # overview of the project
    ├── scripts/                       # scripts for exploring, extractinf and referencing datasets 
   	    ├── theme 1: administrative-boundaries        
	        ├──  explore.py             # data exploration scripts
		├──  extract.py             # data extraction and integration scripts
	        └── catalog.py             # metadata referencing scripts
	    ├── theme 2: population 
		├──  explore.py             # data exploration scripts
		├──  extract.py             # data extraction and integration scripts
	        ├──  catalog.py             # metadata referencing scripts
		└── ...
	    └── theme n
    ├── data/                                                  # data used for the project organised by stage
   	    ├── raw                                                # sample of raw data
	        ├── theme 1: administrative-boundaries             # identifies administrative boundaries datasets 
			    └── geoBoundaries.geojson
		├── theme 2: population                            # identifies population datasets 
			    └── worldPop.geojson                      
		└──...
	    └── mapped
	        ├── urbanshift-administrative-boundaries.geojson   # extracted administrative boundaries datasets for UrbanShift cities
		├── urbanshift-population.geojson                  # extracted administrative boundaries datasets for UrbanShift cities                
		└── ...
    └── data-catalog/                                          # metadata description of used datasets
   	    ├── dc-urbanshift-dataset-desc.csv                     # description of datasets properties                     
	    └── dc-urbanshift-dataset-field-desc.csv               # description of datasets fileds

## baseline-indicators

## thematic-analysis

# Branch management

We choosed a feature-based approach to manage the different branches of this repository (eg. `geolayers-administrative-boundaries` branch corresponds
to the different treatments concerning the exploration and extraction of administrative boundaries).
Every feature has its own branch and when a significant progress is the feature development, a 
pull request should be made in order to review the changes and control the integration into
the main.

The `gh-pages` branch concerns the github page website providing a global description of 
the advances in the whole project.

# Project management

We use `github project` as a backlog and task manager tool. It contains the different issues we are working on.
An automatic kanban backlog is provided with 4 possible status: `Backlog`, `To Do`, `In Progress`, and `Done`. 
With the automated kanban, cards automatically move between the different status when a commit reference the associated issue.
You can access our projects [here](https://github.com/wri/cities-urbanshift/projects).

The features we are working on are grouped into different milestones/sprints that provides a list and description of the 
planned deliverables. You can our milestones [here](https://github.com/wri/cities-urbanshift/milestones).

# Documentation

We provide two main sources of documentation:

- **Github wiki**: It shares knowledge and information needed to understand the main concepts, data providers, tools we 
use in thos project. You can explore our wiki [here](https://github.com/wri/cities-urbanshift/wiki).
- **Github pages**: A website that synthesize the major results and advances in the project. 
You can explore our github page [here](https://wri.github.io/cities-urbanshift/).

# License

# Contact information