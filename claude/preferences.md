
In general: 

- Maintain the birds eye view. This is a large project, and I am struggling to keep track of everything. Simplifying and clearifying is the main goal of our conversations.
- Assume that the information in the claude directory is the most up to date. 
- Use the Socratic method, ask questions and help me think, do not do other work for me, unless I ask you specifically

Structure:

- Three part structure: For the thesis as a whole, the case (climate adaptation), the research design (control) and the analysis should be self contained. 
- Readers guide: I want all chapters and subchapters to start with a couple of sentences expressing the argument that will follow.
- Each chapter should have a couple of paragraphs of introduction, then the main sections, and then round of with a short conclusion, also just a couple of paragraphs. The conclusion should be named closely related to the content of the chapter. 

Markdown:

- I prefer the language to be simple and clear, and to avoid heavy academic sentences. Also, make sure that the text does not sound like it is written by a GPT
- Paragraphs should be of varied length, with 3-5 sentences each. They should function as building blocks. The sentence length should also be varied
- All references should be cited using the pandoc citation: [@authorYYYY], corresponding to a text in the references.bib-file. 

R-scripts

- I did not know how to code before I started this project. I am still unsure of how best to do it. Therefore, the code should rely as much as possible on known and documented packages.
- I prefer simplicity over performance and reusablility. 
- I am unsure of if my approach is the best. If a problem is solved easier by another package, or using another approach entierly, suggest that. 
- Choosing a different clustering algorithm, maybe one that is based on sentences rather than words/texts would be simpler to implement and explain?

Inline code

- The code chunks should have clear labels, and a clear function. They should reference other chunks where needed. 
- All computationally generated data (like topics and numbers) should be prepared in the code chunk and referenced inline. 