# TEPandCSP-Scripts
R-Scripts for the Analysis of imageJ-output
What the scripts need:
-input table with unique fliter-IDs (no duplicates)
-the name of the imageJ-output file for each filter (including at least the column area, outputs for each of the three analysis in the case of TSP)
-information on the images made by your microscope for correct scaling (add in the script TEP-functions)

What the script provides.
-results-table (or several in the case of TSP), containing particle numbers and Areas per liter, as well as information on the size distribution) as "fake-Excel" if desired

Setup:
TEP-functions is the helper script, it is mandatory for all analysis versions, then add

normal TEP/CSP analysis:
Particle analysis (masterscript), optional: TEP_graphics_plugin, if you whish to see the size distribution plotted
as template for your input table you can use input_empty.xlsx

TSP-analysis:
TSP-master (masterscript, still a bit untidy [october 2023], will tidy up soon), couple with Particle-Analysis-TSP (slightly addapted version of the masterscript for individual analysis, to allow for multiple calls to the script from the new masterscript)
as template for your input table you can use TSP-input-empty.xlsx

Also will upload an updated version of the manual for the individual analysis script soon, to facilitate script setup and use
