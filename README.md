# Bachelors Thesis
## A Study of Bias in the Implementation of Recidivism Risk Algorithms
### If you have any questions, please feel free to reach out to me at gregorytadams@gmail.com.

README.md -- This file.

setup.sh --  A bash file that clones the necessary repositories (including this one), creating the filestructure assumed by the default filepaths in the analysis functions.  

setup.R -- A file that installs the necessary libraries for the analysis.  To avoid an overflow of errors saying you already have the libraries installed, I recommend you open the file and comment out any libraries you already have.  With the default libraries on the UChicago computers' RStudio, you only need to install dplyr (i.e. run "install.packages("dplyr")" sans outside quotes).

analysis_functions.R -- A file filled with functions used in do_analysis.R.  This draws heavily on the code published by ProPublica in their analysis (to see that code, run "jupyter notebook compas-analysis/Compas\ Analysis.ipynb" sans quotes, assuming you used setup.sh to download the files).

do_analysis.R -- High-level script that runs the full analysis and generates all outputs.  Best run in RStudio.  Draws heavily on analysis_functions.R.  Does not interpret the outputs for you.

Adams_Gregory_BA_Draft.docx -- the polished draft of my thesis submitted 2/6/17.

current_draft.docx -- the working draft of my thesis.  This is likely to be very messy at times.

Given that this is an ongoing project, any other files are likely unfinished.  Please ignore them until they are included in this README.


