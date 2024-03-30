# Release version 3.2.0 (2024-04-01)

## Test environments

* Windows 10, R 4.3.2
* winbuilder devel
* Github actions

## R CMD check results

OK
___


# Release version 3.0.2 (2023-08-22)

## Test environments

* Windows 10, R 4.3.1
* winbuilder devel

## R CMD check results

OK
___


# Release version 3.0.1 (2022-08-10)

## Test environments

* Linux Mint, R 4.2.1
* winbuilder devel

## R CMD check results

OK
___


# Release version 3.0.0 (2020-10-28)

## Test environments

* ubuntu 18.04, R 3.6.3
* online win-builder.r-project.org (devel & release)

## R CMD check results

OK
___


# Release version 2.0.1 (2020-08-25)

## Release summary

This package was archived. I fixed the errors raised by the CRAN checks. 
The problems were caused by code such as `class(dist) == "function"`, which 
I replaced with `is(dist, "function")`. 
I am using a new email address as compared to the previous release. 

## Test environments

* ubuntu 18.04, R 3.6.3
* online win-builder.r-project.org (devel & release)

## R CMD check results

There was 1 NOTE. 

* checking CRAN incoming feasibility ... NOTE

New submission

___


# Release version 2.0.0 (2016-05-25)

## Release summary

Nothing particular. 

## Test environments

* ubuntu 14.04, R 3.2.5
* online win-builder.r-project.org (using `devtools::build_win()`)

## R CMD check results

Ok.

___


# Release version 1.1.0 (2016-02-04)

## Release summary

Nothing particular. The differences with the previous version are minor.

## Test environments

* ubuntu 14.04, R 3.2.3
* online win-builder.r-project.org (using `devtools::build_win()`)

## R CMD check results

There was 1 NOTE with winbuilder, due to an accented letter:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Stéphane Laurent <laurent_step@yahoo.fr>'

New maintainer:
  Stéphane Laurent <laurent_step@yahoo.fr>
Old maintainer(s):
  StÃ©phane Laurent <laurent_step@yahoo.fr>

## CRAN Package Check Results for the previous version

There is an error reported on https://cran.rstudio.com/web/checks/check_results_kantorovich.html for the r-patched-solaris-sparc flavor, related to one test. I do not know how to deal with it. 
___


# Release version 1.0.0 (2016-01-15)

## Release summary

This is the first submission.

## Test environments

* ubuntu 14.04, R 3.2.3
* windows 7 64bit, R 3.0.2, R 2.15.3
* online win-builder.r-project.org (using `devtools::build_win()`)

## R CMD check results

There was 1 NOTE 

* checking CRAN incoming feasibility ... NOTE

Maintainer: 'Stéphane Laurent <laurent_step@yahoo.fr>'

New submission
