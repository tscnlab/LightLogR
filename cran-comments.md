## Submission Version 0.10.0

### R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Submission Version 0.9.2

### R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 note ✖

* Note 1: unable to verify current time. 
  Time server seems to be unavailable

## Resubmission #1 (Version 0.5.3)

This is a resubmission. In this version I put some examples for gg_doubleplot and gg_photoperiod in /dontrun to avoid long runtime. I further rebuilt the package with the latest R-devel to correct the discrepancies with Authors@R and Author field.

## Submission Version 0.5.3

### R CMD check results

0 errors ✔ | 0 warnings ✔ | 3 notes ✖

* Note 1: Imports includes 22 non-default packages. 
  This is correct and necessary

* Note 2: unable to verify current time. 
  Time server seems to be unavailable

* Note 3: Found HTML validation problems. 
  This note seems related to the testing environment and not the actual package.

### Windevel results

2 Notes

* Note: Author field differs from that derived from Authors@R.
  As far as I can tell, the only difference is that the Authors@R field has (ORCID: *Orcid URL*) behind the authors, where as the Author field has (*Orcid URL*). As this is automatically generated based on Description, I don't think I can influence that.

* Note: Examples with CPU (user + system) or elapsed time > 10s
                user system elapsed
  gg_photoperiod 12.52   1.72   14.25
  gg_doubleplot   9.45   1.39   10.85

  These examples are expected to take a longer time, as they are plotting functions and several variants are shown in the examples.

## Resubmission #3 (Version 0.3.8)

This is a resubmission. In this version I corrected the error in a symlog_trans example.

## Resubmission #2 (Version 0.3.8)

This is a resubmission. In this version I removed the last erroneous link.

## Resubmission #1 (Version 0.3.8)

This is a resubmission. In this version I have:

* changed the \link{} targets only referring to packages and not to functions within other packages to \pkg{}.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Windevel results

Maintainer: 'Johannes Zauner <johannes.zauner@tum.de>'

* New submission

* Found the following (possibly) invalid URLs:
    URL: https://lists.lrz.de/mailman/listinfo/lightlogr-users
      From: README.md
      Status: 403
      Message: Forbidden
    
This URL is valid, but the server does not allow the Windevel server to access it. The URL leads to the mailing list for the package.