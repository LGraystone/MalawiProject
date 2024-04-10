# Malawi Project

#This project was completed as part of TMU's _Data Analytics, Big Data, and Predictive Analytics_
#certificate program (CIND 820: Big Data Analytics Project)
#It uses survey data from the 2018 WALA program evaluation to explore what health and eating practices
#can be used to identify Malawi children who are at risk of becoming malnourished
#data can be found at: #https://data.usaid.gov/Evaluation/WALA-2018-Follow-up-Health-Dataset/ty8s-z2zp/about_data

#The original program report can be found at: https://pdf.usaid.gov/pdf_docs/PA00K2WJ.pdf

#Information on WHO's Anthro Analyzer can be found at: https://www.who.int/tools/child-growth-standards/software

#Files and content include:

#1DataCleaningPreDep.R:
  #Variable management (renaming, changing data types, standardizing responses, compiling related variables)
  #Data cleaning (identifying and managing outliers, correcting survey and data entry errors, visual inspections, managing NA counts)
  #Preparing dataset for processing through WHO's Anthro Analyzer

#2PostDep.R:
  #Further cleaning (to correct changes made by Anthro Analyzer, identifying and managing additional outliers, identifying and managing extreme z scores)
  #Creating the dependent variables (stunted, wasting, underweight, malnourished) from the calculated Z scores
  #Exploratory data analysis (summary statistics and graphs)
  #Bivariate analysis (Kendell's correlation with visualization, removing highly correlated variables or variables that were used to develop the dependent variables)
  #Multivariate analysis (Kruskal-Wallis/Dunne or Wilcox Rank Sum tests, decision rules/attribute significance, feature selection)
  #Modeling

#3Original data dictionary.docx (from dataset as accessed from link above)

#4Updated data dictionary.docx (immediately before modeling)

#5Confusion matrices comparison (of the different models)

#6aWALA_2018 (original database)
#6bWALAclean (database post first round of cleaning, in advance of z score calculation)
#6cWALAclean_zscore (database post z score calculation)
