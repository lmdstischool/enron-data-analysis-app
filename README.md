# enron-data-analysis-app
Analysis dashboard app, written in R, representing Enron dataset.

### Read this Readme before launching the application for the first time ### 

The Readme is an index that explains the purpose of each file and helps you understand how to launch the application.

## The different files

1. enron_notebook.rmd: 
	- A comprehensive notebook that explains the process and thinking behind the project.
	- Open it and don't run it in the same environment as the application.

2. enron_notebook.html:
	- Enron's notebook in HTML format.

3. back_end_workload.R:
	- The file to be launched first in the same environment as the application.
	- Used to contain most of the workload in an attempt to make the application run faster.

4. app.R: 
	- The Shiny Application.

5. Download the dataset: https://www.kaggle.com/datasets/wcukierski/enron-email-dataset or https://www.cs.cmu.edu/~enron/

## How to Install and Run the App

1. It's important to have in the same directory: 
	- The Enron.Rdata file
	- The back_end_workload.R file
	- The app.R file
2. Open the back_end_workload.R file.
3. One the app.R file in the same environment.
4. Run the back_end_workload.R file (takes approximately 5-7 minutes. The most time-consuming part is word processing.)
5. Launch the application, in the same environment, by clicking on the “Run App” button.


## How to improve the app?

- Try lightening the code in the workload file.
- Improve application performance for specific graphics.
- Enhance the user interface with further customization.
- Create a model to predict e-mail activity based on the status.
