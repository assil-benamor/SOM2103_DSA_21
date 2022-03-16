
<div id="top"></div>




<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/lilos404/SOM2103_DSA_21">
    <img src="https://data.humdata.org/image/2015-11-05-231341.581365REACHlogo_300x125_grey.png" alt="Logo">
  </a>

<h3 align="center">Somalia detailed site assessment</h3>

  <p align="center">
    Aggregation and Analaysis scripts for SOM DSA 21 
    <br />
    <br />
    <a href="https://github.com/lilos404/SOM2103_DSA_21/issues">Report Bug</a>
    ·
    <a href="https://github.com/lilos404/SOM2103_DSA_21/issues">Request Feature</a>
  </p>
</div>



<!-- ABOUT THE PROJECT -->
## About The Project

The Detailed Site Assessment (DSA) was initiated in coordination with the Camp Coordination and Camp Management (CCCM) Cluster in order to provide the humanitarian community with up-to-date information on the location of IDP sites, the conditions and capacity of the sites, and an estimate of the severity of humanitarian needs of residents.

This project contains the aggregation and analysis scripts of the 2021 DSA. 

<p align="right">(<a href="#top">back to top</a>)</p>


### Built With

* [R](https://www.r-project.org/)

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- GETTING STARTED -->

## Steps

1. Clone the repo
   ```sh
   git clone https://github.com/lilos404/SOM2103_DSA_21
   ```
   
2. Add the input files (clean data, gps_coordinates, cccm_master_list) under input/data
   ```sh
   mkdir input/data
   cd input/data
   cp "[path_where_data_file_is_located]/input_data.zip" .
   unzip input_data.zip
   ```

3. Run the aggregation
   ```r
   source("src/1_aggregation.R")
   ```

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- ROADMAP -->
## Roadmap

- [x] Aggregation
    - [x] Select multiple
    - [x] Select multiple constraints
    - [x] Select one
    - [x] Numerical
    - [x] Skip logic
    - [x] GPS coordinates
- [x] Indicators
- [x] Output production
    - [x] Results table
    - [x] InDesign Data Merge

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTRIBUTING -->
## Contributing

If you have a suggestion that would make this better, please fork the repo and create a pull request. 

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/NewFeature`)
3. Commit your Changes (`git commit -m 'Add some NewFeature'`)
4. Push to the Branch (`git push origin feature/NewFeature`)
5. Open a Pull Request

<p align="right">(<a href="#top">back to top</a>)</p>



<!-- CONTACT -->
## Contact

Assil BEN AMOR - assil.benamor@reach-initiative.org


<p align="right">(<a href="#top">back to top</a>)</p>

