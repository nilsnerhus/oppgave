---
aliases: 
tags: notat
excalidraw-open-md: "true"
excalidraw-plugin: parsed
cssclass: 
---

This project is my first attempt at using data science techniques and tools to do work. 

The project is organized in a GitHub repo (called "oppgave"). 

- The text is written in .qmd-files stored in the "text" directory
- References are stored in the "references" directory
- The scripts and functions are written in .R-files stored in the "scripts" directory
- The functions save to .rds-files stored in the "data" directory
- Dependencies are handled by renv
- The finished text is built through GitHub actions at every commit

The data is prepared through nap_pipeline.R, that can either run on demand or weekly, checking the UNFCCC website for changes. 

The functions are written in the same style, with roxygen-documentation and the same utils.R-functions. They follow the same structure as well. 