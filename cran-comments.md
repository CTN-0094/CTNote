## Resubmission 1 (v 0.1.0)

**R CMD check results**: 0 ERRORs, 0 WARNINGs, and 0 NOTEs.

**Comments**: U.L. marked the following problems (which we resolved):

- The Description field should not start with the package name, 'This package' or similar.
- Found the following URLs which should use `\doi` (with the DOI name only):
    + File 'weight_positive_visits.Rd'
- Fixed miscellaneous spelling errors



## Initial Submission (v 0.1.0)
First submission of the CTNote package.

**R CMD check results**: 0 ERRORs, 0 WARNINGs, and 0 NOTEs.

**Comments**: We use GitHub Actions for continuous integration. This package builds cleanly on ubuntu R devel, R release, and R old release; this also builds on windows and macOS R release.

