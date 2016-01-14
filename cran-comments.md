## Release summary

This is my first submission.

## Test environments

* ubuntu 14.04, R 3.2.3
* windows 7 64bit, R 3.0.2, R 2.15.3
* online win-builder.r-project.org

## R CMD check results

There was 2 NOTEs 

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Stéphane Laurent <laurent_step@yahoo.fr>'
New submission

* The second NOTE is generated on win.builder with R Under development (unstable) (2016-01-13 r69941): 

  checking R code for possible problems ... NOTE
Vectorize_bigq: no visible global function definition for 'formalArgs'
arrange_names: no visible global function definition for 'setNames'
ejoinings: no visible global function definition for 'model.matrix'
Undefined global functions or variables:
  formalArgs model.matrix setNames
Consider adding
  importFrom("methods", "formalArgs")
  importFrom("stats", "model.matrix", "setNames")
to your NAMESPACE (and ensure that your DESCRIPTION Imports field
contains 'methods').

However I get an error if I import `formalArgs`: Namespace dependency not required: ‘methods’
