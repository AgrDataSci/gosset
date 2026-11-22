<!-- Before submitting, confirm the win-builder, local and revdepcheck results below and edit them if they differ. -->

## Resubmission

This release, 2.0.0, fixes the check ERRORs for 1.5.1 on
r-devel-linux (debian and fedora, clang and gcc) and r-release-linux.
Rebuilding vignette 1 failed with "`mapping` must be created by `aes()`".
The error came from the dependency ggparty, which fails its own checks 
under current ggplot2. gosset no longer depends on ggparty and draws 
tree plots with ggplot2 and partykit.

The release also removes DescTools, tidyr and lifecycle from Imports,
moves BradleyTerry2 and ggrepel to Suggests, and corrects several 
statistical functions. NEWS.md lists every change.

## Test environments

* GitHub Actions, ubuntu-latest, macos-latest and windows-latest, R release
* win-builder, R-devel
* local, R release

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

Checked with revdepcheck. No new problems.
