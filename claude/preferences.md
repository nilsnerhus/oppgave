The whole text should be between 60 and 80 standard pages. A standard page is 2400 characters, including spaces. The thesis is due the 16th of September 2025

You have access to information in two ways:
- Project knowledge: The github repo for the thesis project. It has many files and directories, but all data is in `data/`, all text is in `text/` and code is in `scripts/`, while references are in `references/`
- Obsidian MCP: These are my notes for my vault. The notes are stored in the "2 Notater" directory, while all references are stored in the "3 Referanser". Ask permission before you search the notes. 

In general: 

- Maintain the birds eye view. This is a large project, and I am struggling to keep track of everything. Simplifying and clearifying is the main goal of our conversations.
- Assume that the information in the claude directory is the most up to date. 

Structure:
- Readers guide: I want all chapters and subchapters to start with a couple of sentences expressing the argument that will follow.
- Each chapter should have a couple of paragraphs of introduction, then the main sections, and then round of with a short conclusion, also just a couple of paragraphs. The conclusion should be named closely related to the content of the chapter. 

Markdown:
- I prefer the language to be simple and clear, and to avoid heavy academic sentences. Also, make sure that the text does not sound like it is written by a GPT. Avoid the em dash.
- Paragraph- and sentence length should be varied, with 3-5 sentences in each paragraph. They should function as building blocks.
- All references should be cited using the pandoc citation: [@authorYYYY], corresponding to a text in the references.bib-file. 

R-scripts
- I prefer simplicity over performance and reusability. 
- The method section should have a through walkthrough of the whole pipeline, making sure that all doubts about rigor are silenced

Inline code
- The code chunks should have clear labels, and a clear function. They should reference other chunks where needed. 
- All computationally generated data should be prepared in the pipeline, with inline values referenced in the text. All available data in `data/load_variables.json`

