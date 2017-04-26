Introduction
============
Within this archive you find the replication package for the paper A Dynamic Model for the Measurement and Prediction of Developers' Contribution Behavior by Verena Herbold, Steffen Herbold, Daniel Honsel, Jens Grabowski, and Stephan Waack which is currently under review. The aim of this replication package is to allow other researchers to replicate our results with minimal effort. 

Requirements
============
- Java 7.
- R (tested with version 3.3.2, other should work). 
- Only tested on Windows 8 and Windows 10. Should work on Linux, but may require minor adoptions (different paths, differences between .sh and .bat scripts, etc.). 

Contents
========
This replication kit contains:
- The eval_all.R script, which is the main R script for the execution of all experiments reported on in the paper.
- The hmm-helpers.R script, which contains some functions required by eval_all.R. 
- The compiled rectanglelearner.jar file for the execution of the threshold learning algorithm (source code may be added upon request). 
- The create_thresholds.bat Windows batch file that contains an example for the execution of the treshold learning algorithm on the data from the Ant project. 
- Eigth subfolders with the data for each project used in the case study. 

How does it work?
=================
You must update the path in the eval_all.R script to match the location of the replication kit on your local machine (Line 5 of the file). 

Now you should be able to run the contents of the eval_all.R and replicate our results.
