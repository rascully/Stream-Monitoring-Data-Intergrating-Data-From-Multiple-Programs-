#Integrating Stream Habitat Metrics 
This repository contains the code to integrat stream habitat metrics from three stream habitat monitoring programs EPA National Aquatic Resources Surveys (NARS); BLM Aquatic Assessment, Inventory, and Monitoring; and USFS Aquatic and Riparian Effective Monitoring Program (AREMP). The data are intergrated using the data exchange specifications in the [Stream Monitoring Data Exchange Specifications repository](https://github.com/rascully/Stream-Monitoring-Data-Exchange-Specifications). 

#Purpose
Improving data sharing will enable timely access to data, enhance the quality of data, and create clear channels for better management decisions. Healthy aquatic habitat is critical to fishes, aquatic species, and water quality. Across the US, long-term large-scale stream habitat monitoring programs collect data for their specific objectives and within their jurisdictional boundaries. Streams cross jurisdictional boundaries and face unprecedented pressure from changing climate, multi-use public lands, and development. To meet these stressors, we integrate data from multiple sources to create a data set of stream metrics across jurisdictional boundaries. This pilot, focus on integrating data from the EPA National Aquatic Resources Surveys (NARS); BLM Aquatic Assessment, Inventory, and Monitoring; and USFS Aquatic and Riparian Effective Monitoring Program (AREMP) and Pacfish/Infish Biological Opinion Effectiveness Monitoring Program (PIBO). This code integrate a subset of metrics collected on public lands in the Western United States and document metadata in MonitoringResources.org based on the data exchange specifications and crosswalk outlined in the [Stream Monitoring Data Exchange Specifications repository](https://github.com/rascully/Stream-Monitoring-Data-Exchange-Specifications). 


#Inputs 
We built the intergrated data set based on this workflow with the following inputs:  
<ul>
<li>Origian data sets from each program. We documented data set metadata in documented in [ScienceBase](www.sciencebase.gov) </li>
<li>[Stream Data Exechange Specifications]()</li>
<li>[Field Crosswalk]()</li>
<li>[MonitoringResources.org Methods](MonitoringResources.org)</li>
<li></li>
<li></li>
</ul>


#Outputs 
The code produces mutiple outputs stored in the Data file:  

We pre-process the AREMP and EPA NRSA data sets to flatten them and save the ouputs in the Data file. 
<ul>
<li>[Tity AREMP Data Set]() </li>
<li>[Tity NRSA Data Set]()</li>
</ul>

We intergrated a subset of metrics and metadata from the four programs the following data files are produced and saved in the Data file of this respository and uploaded to [ScienceBase](). 
<ul>
<li>Unique Locations-this is a list of the unique location of data collection from the three programs included in the intergrated data sets</li>
<li>Integrated Data Set- the data set bulit using the data[Stream Monitoring Data Exchange Specifications](https://github.com/rascully/Stream-Monitoring-Data-Exchange-Specifications)</li>
<li>Flat Integrated Data Set- a flat file of the integrated data sets.  </li>
<li></li>
</ul>

The working group decided to not combind Macroinverterbrate data from the orgian data sets. This decision is documented in !!!!. For the programs who collect data using similary methology and process samples at the Utah State University (USU) bug lab we get data from the USU Bug Lab and intergrated it into the data set using the XX code. and output xx data sets. 

Additionally the working gropu agree temperature data is an important variable to include, but programs already submit terperature data to a the USFS NorWest data base and based on internal anaysis we decided the temperature interpolations produced by the Norwest !!. We extract the termperature values for all data collection events in the intergrated data set, then join them to the master data set and produce a final intergrated data sets including metrics from the controlled vocabular where the is a field cross walk, Macroinverterbrate data from the 




