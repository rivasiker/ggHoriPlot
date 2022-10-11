## 2nd resubmission
This is a re-submission. In this version I have corrected minor bugs:

* Removed functions from other packages called with `:::`. 
* Added dropped aesthetics argument to `ggproto`.

## 1st resubmission
This is a re-submission. In this version I have:

* Added \value to all .Rd that missed it. 
* Added missing arguments to the .Rd files.

## Test environments

* GitHub Actions (ubuntu-20.04): release, devel
* GitHub Actions (macOS-latest): release
* GitHub Actions (windows-latest): release
* R-hub builder (Fedora Linux): devel
* R-hub builder (Ubuntu Linux): release
* R-hub builder (Windows): devel
* win-builder: devel
* local R installation (macOS): R 4.2.1

## R CMD check results

0 errors | 0 warnings | 1 note

The note is only found on Windows (devel): 

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this is
probably a MiKTeX bug and can likely be ignored. 
