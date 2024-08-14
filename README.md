
# SEERdb <a href="https://github.com/BCC-Biostats/SEERdb"><img src="man/figures/SEERdbhex.png" align="right" height="138" /></a>

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![GitHub
issues](https://img.shields.io/github/issues/BCC-Biostats/SEERdb)
![GitHub closed
issues](https://img.shields.io/github/issues-closed/BCC-Biostats/SEERdb)
![GitHub
contributors](https://img.shields.io/github/contributors/BCC-Biostats/SEERdb)
![GitHub last
commit](https://img.shields.io/github/last-commit/BCC-Biostats/SEERdb)
![GitHub repo
size](https://img.shields.io/github/repo-size/BCC-Biostats/SEERdb)

## Overview

SEERdb is a package for accessing data from the SEER medicare dataset.
It expects a directory where SEER medicare zipped files are stored.
Functions can open these files or build a SQLite database.

## File types

Thse are the given file types:

- hsp
  - demo
  - occurence
  - revenue
  - span
  - value
- msbf
  - ab.summary
  - abcd.summary
  - cc.summary
  - oth.cc.summary
- medpar
- nch
  - base
  - line
  - demo
- outpat
  - base
  - condition
  - occurence
  - revenue
  - span
  - value
- SEER
  - colon
  - lung
- census
  - tract.encr
  - zipcode.unencr
