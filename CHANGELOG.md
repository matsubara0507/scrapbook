# Changelog for scrapbook

## Unreleased changes

## 0.4.0

- Refactor: update resolver to lts-14.4
  - update extensible package to 0.6.1
  - update req package to 2.1.0
  - update feed package to 1.2.0
  - update rio package to 0.1.12
- Refactor: update config to build Docker image with stack v2
- Refactor: change deps package to githash from gitrev
- Refactor: remove deps package default-data

### 0.3.3

- Misc: update package.yaml info for Hackage

### 0.3.2

- Misc: remove deps lib
    - extensible-instances
    - data-default-instances-text
- Misc: update extensible to 0.5

### 0.3.1

- Refactor: update resolver to lts-12.26
- Misc: support docker image
- Misc: add TravisCI

## 0.3.0

- Refactor: update resolver to lts-12

## 0.2.0

- Feat: version option
- Feat: RSS 2.0
- Fix: remove namespace in xml tag
- Feat: summary
- Feat: mltiple input files
- Fix: occur error when write file on no exist directory
- Feat: default output file name is input file name
- Fix: help message
- Feat: add json output format
- Feat: add config to filter links with attr on Atom feed
- Refactor: use `rio` library
- Refactor: change several functions to polymorphic with `extensible`
- Fix: don't exit whole program when raise fetch exception

## Alpha

- alpha release
