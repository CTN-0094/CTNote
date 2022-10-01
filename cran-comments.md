## Resubmission 3 (v 0.1.0)

**R CMD check results**: 0 ERRORs, 0 WARNINGs, and 0 NOTEs.

**Comments**: V.W. marked the following problem:

- Please always explain all acronyms in the description text. e.g.: CTN

**Response**: we updated the Description field of the DESCRIPTION file to define all acronyms before use. The paper with our method is still in the editing phase. We expect to submit it to PLoS One within the next month.




## Resubmission 2 (v 0.1.0)

**R CMD check results**: 0 ERRORs, 0 WARNINGs, and 0 NOTEs.

**Comments**: U.L. marked the following problem:

- Found the following (possibly) invalid URLs:
     URL: https://journals.lww.com/psychopharmacology/Abstract/1996/02000/Buprenorphine_Versus_Methadone_in_the_Treatment_of.10.aspx
(moved to https://journals.lww.com/psychopharmacology/Fulltext/1996/02000/Buprenorphine_Versus_Methadone_in_the_Treatment_of.10.aspx)
     From: inst/doc/library_reduction_20220630.html

Please change http --> https, add trailing slashes, or follow moved content as appropriate.

**Response**: we changed the URL in the .bib file to a DOI and rebuilt all documentation. However, we note that this particular publication is behind a paywall, and we do not have institutional access to the full text or the full text hyperlink.




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

