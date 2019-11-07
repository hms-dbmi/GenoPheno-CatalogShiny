# GenoPheno-CatalogShiny

`GenoPheno-CatalogShiny` is an R Shiny App developed to provide a dynamic on-line catalog with datasets containing both, genomic sequencing and clinical data for the same patients. 

## What is this repository for?

The `GenoPheno-CatalogShiny` is a dynamic catalog that puts together a descriptive and informative summary of datasets with genomic and phenotypic data, to help investigators to identify resources for their research. The table provides information of the country where the data was collected, the number of patients for each dataset, the number of samples collected, the type of phenotypic data (electronic health records -EHR-, questionnaires, clinical notes), the type of genomic data (at least one of the following types of Next Generation Sequencing -NGS-: whole genome sequencing -WGS-, whole exome sequencing data -WES-) and the disease focus (general or disease specific). Filtering and searching features are available. Users can submit new entries via the Shiny App and also correct the existing ones via this repository (See details below). 

## Inclusion criteria
This catalog only includes those data repositories that comply with the following:
1. Over 500 human subjects
2. Contain both genotype and phenotype data for **the same subjects**
3. Include WGS and/or WES data as part of the genomic data content
4. Include at least 100 recorded phenotypic variables per subject
5. The dataset has to be accessible through a website or by collaboration with investigators
6. All the resources (data and website) have to be available in English

All five criteria must be met for a dataset to be considered. 

## On line
The on line version of the GenoPheno-CatalogShiny Shiny App is available at: 

<a href="https://avillachlab.shinyapps.io/GenoPheno" target="_blank">https://avillachlab.shinyapps.io/GenoPheno</a>


## Submitting a new entry

Go to the [GenoPheno-CatalogShiny website](https://avillachlab.shinyapps.io/GenoPheno) and click on the "Submit a new dataset" tab at the top of the table. Fill the form accordingly (fields marked with `*` are mandatory) and click the `Submit` button. The new entry submitted has to fulfill the inclusion criteria described above. All new submissions will be validated before being added to the table. 

## Correcting an existing entry

Corrections of existing entries and fields can be done via [pull requests](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests).


## License
Licensed under Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

## Citation 

