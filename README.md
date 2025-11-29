# 666TermProject

- processing.ipynb: Jupyter notebook for processing text data and extracting features.
- processed_text.csv: CSV file containing the cleaned and preprocessed text data.
- BookofMormon.txt: raw text file of the book of mormon, with some small manual tweaks.  Originally from https://archive.org/stream/thebookofmormon00017gut/mormon13.txt
- tfidf_features_100.csv: CSV file containing the TF-IDF features with 100 features extracted from the text data.  Useful for classification, potentially for clustering
- tfidf_features_5000.csv: CSV file containing the TF-IDF features with 5,000 features extracted from the text data.  Useful for NMF topic modeling
- wordcount_features.csv: CSV file containing the word count features extracted from the text data.  Useful for LDA topic modeling
- chapter_clustering.R: R script to analyze cluster groupings of Book of Mormon Chapters based on 100 TFIDF features
