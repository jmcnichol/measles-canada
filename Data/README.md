# measles-canada
Simulated measles outbreaks in Canada

## Vaccination data
There are six .csv files.
1. statcan 2-years old coverage: This is the coverage data for 2 year old individuals in Canada by province for 2013, 2017, 2021. The data is sorted by region and we have coverage for each year for the provinces. First, we have the overall data for Canada. The data is gathered from https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310087001 

2. 2022 NIH paper (2009 - 2013 data): This is the data from testing 11176 sera from 2009 - 2013 in canada. We have seropositivity data for three different regions: ON, QC and others (NFLD, NS, NB, MB, AB, BC) and total. There is a column for seropositivity, and two columns for lower bound and upper bound of 95% confidence interval. The data is gathered from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9246716/ (Table 3). We can add the data for differen age groups, sex, and whether born in Canada or not if needed.

3. Alberta childhood coverage by geography: This is the coverage data for year 2022 for all regions of Alberta for MMR Dose 1 by Age 2 and MMR Dose 2 by Age 7. Immunization percent, standard error, and standard score is provided for each. The data is gathered from http://www.ahw.gov.ab.ca/IHDA_Retrieval/selectSubCategory.do (The region for each code used is defined there) and the data notes can be found in http://www.ahw.gov.ab.ca/IHDA_Retrieval/ShowMetaDataNotesServlet?3948

4. Alberta school coverage by geography: This is the coverage data for students for school year 2022-2023 for all regions of Alberta (not as specific as Alberta childhood coverage data) for MMR in Grade 1 and MMR in Grade 6. The data is by each sex and both.Immunization percent, standard error, and standard score is provided for each. The data is gathered from http://www.ahw.gov.ab.ca/IHDA_Retrieval/selectSubCategory.do (The region for each code used is defined there) and the data notes can be found in http://www.ahw.gov.ab.ca/IHDA_Retrieval/ShowMetaDataNotesServlet?4005

5. VCH school coverage: This the immunization coverage data for kindergarten - measles vaccine in VCH (Vancouver Coastal Health) for school year 2018-2019 for all schools. There is a column for the region of the schools. The data is gathered from https://public.tableau.com/app/profile/phsu.dashboard/viz/VCHSchoolImmunizationCoverageDashboard/Dashboard

6. Saskatchewan coverage 2018: This is the coverage data for different ages and doses in SK by health regions for 2018. In the first column, we have jurisdiction (with former health region by Peer Group): "Regina Qu'Appelle" and "Saskatoon" are in peer group A, "Cypress" to "Sunrise" are in peer group D, "Athabasca Health Authority" to "Mamawetan Churchill River" are in peer group F, and "Prairie North" and "Prince Albert Parkland" are in peer group H. The data is gathered from https://pubsaskdev.blob.core.windows.net/pubsask-prod/111793/Vaccine-Preventable-Disease-Monitoring-Report-Measles-2017-and-2018.pdf

