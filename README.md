# habs-proxies-forecast-chl
Collection of code from the HABs Proxies group to forecast chl-a for [Ecological Forecasting Challenge 2022](https://projects.ecoforecast.org/neon4cast-docs/theme-aquatic-ecosystems.html).

## How to work in this repository 
NOTE: A lot of the details on standardized environment and automation come from https://github.com/eco4cast/neon4cast-example 

### Setup the correct environment on your local machine 

1) Run `install.packages("renv")` in R. https://rstudio.github.io/renv/index.html
2) After the installation of `renv` finishes, run `renv::activate()` and `renv::restore()`. `renv` uses the `renv.lock` file committed to this repository to download and install the necessary packages to run this pipeline. `renv` installs the packages to a project-specific location (i.e. it doesn't update R packages installed elsewhere on your machine). _NOTE_: this might take a while to install all the necessary packages for this project. We also use `renv` in GitHub to automatically run the pipeline using GitHub actions. 
3) If you make changes to the pipeline, especially if you're using a new package not already in use (or updating an existing package to a new version), run `renv::snapshot()` to update the `renv.lock` file with any new packages that you have added.  
4) Commit and push any changes to your forked version of the repository and open a pull request.  

### Ready to submit a forecast automatically?

1) Uncomment the `p6_submit` target. 
2) Uncomment the `schedule:` and `- cron: "0 20 * * *"` lines in the `.github/workflow/do_prediction.yml` file. This cron job will run the forecast in this repository daily at 20:00 UTC, and the execution of the forecast occurs on GitHub's servers, so your local computer does not need to be turned on. You can update this to run on a different schedule based on timing codes found in https://crontab.guru
3) Commit and push the changes to Github. 

### Running the pipeline in GitHub actions manually 

1) Under the actions tab, click on ".github/workflows/do_prediction.yml" on the left side.
2) Click `Run workflow`, and then the green `Run workflow` button. 



## Disclaimer 
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided “AS IS.”
