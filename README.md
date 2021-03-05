# Readme <a href='https://osf.io/zcvbs/'><img src='worcs_icon.png' align="right" height="139" /></a>


[![WORCS](https://img.shields.io/badge/WORCS-limited-orange)](https://osf.io/zcvbs/)

<!-- Please add a brief introduction to explain what the project is about    -->

## Where do I start?

You can load this project in RStudio by opening the file called 'Mortality and hospital costs.Rproj'.
The file for running the analysis and creating the presentation is the following: "/presentation/DGGOE.Rmd"
The current manuscript file (manuscript/manuscript.rmd) is not up to date and is not compiling, but contains some further data descriptions.


## Project structure

<!--  You can add rows to this table, using "|" to separate columns.         -->
File                               | Description                      | Usage         
---------------------------------- | -------------------------------- | --------------
README.md                          | Description of project           | Human editable
Mortality and hospital costs.Rproj | Project file                     | Loads project 
LICENSE                            | User permissions                 | Read only     
.worcs                             | WORCS metadata YAML              | Read only     
prepare_data.R                     | Script to process raw data       | Human editable
manuscript/manuscript.rmd          | Source code for paper            | Human editable
manuscript/references.bib          | BibTex references for manuscript | Human editable
renv.lock                          | Reproducible R environment       | Read only     
presentation/DGGOE.rmd             | Presentation and script          | Human editable

<!--  You can consider adding the following to this file:                    -->
<!--  * A citation reference for your project                                -->
<!--  * Contact information for questions/comments                           -->
<!--  * How people can offer to contribute to the project                    -->
<!--  * A contributor code of conduct, https://www.contributor-covenant.org/ -->

# Reproducibility

This project uses the Workflow for Open Reproducible Code in Science (WORCS) to
ensure transparency and reproducibility. The workflow is designed to meet the
principles of Open Science throughout a research project. 

* To learn how WORCS helps researchers meet the TOP-guidelines and FAIR principles, read the preprint at https://osf.io/zcvbs/
* To get started with `worcs`, see the [setup vignette](https://cjvanlissa.github.io/worcs/articles/setup.html)
* For detailed information about the steps of the WORCS workflow, see the [workflow vignette](https://cjvanlissa.github.io/worcs/articles/workflow.html)
* For a brief overview of the steps of the WORCS workflow, see below.
