Cop TV Classification
This repository contains R scripts for classifying transcripts from police TV shows using OpenAI's GPT API. The project analyzes patterns in how police and law enforcement are portrayed across various TV series.

📖 Overview
The analysis focuses on identifying positive and negative depictions of law enforcement, leveraging machine learning-based text classification. It includes:

Text classification via OpenAI's GPT API
Factor analysis and dimensionality reduction
Time-series analysis of portrayals over time
Data visualization using ggplot2
🛠️ Installation & Setup
Clone the Repository:
git clone https://github.com/EstebanMFernandez/CopTVClassification.git
cd CopTVClassification

Install Required R Packages:
install.packages(c("httr", "jsonlite", "ggplot2", "psych", "dplyr", "stringr", "openxlsx"))

API Key Setup (Environment Variable):
To run the classification, you need an OpenAI API key.

Open or create an .Renviron file in your home directory:
file.edit("~/.Renviron")

Add this line (replace with your actual key):
OPENAI_API_KEY=sk-your-api-key-here

Save and restart RStudio.

🚀 Usage
The primary script is GPTCopClassify112624.R, which processes and classifies transcripts.

Run the Analysis:
source("GPTCopClassify112624.R")

Expected Outputs:
Classification labels for each transcript (e.g., positive/negative portrayal).
Factor analysis results for thematic trends.
Time-series plots illustrating changes over time.
📊 Methodology
The project uses the OpenAI GPT API to classify text passages from police TV shows. The classification focuses on valence (positive/negative portrayals) and is validated through manual coding checks and factor analysis.

Key Analytical Steps:

Preprocessing: Cleaning transcripts (stopwords, punctuation removal).
Classification: GPT API for labeling sentences based on sentiment/valence.
Analysis: Factor analysis to identify underlying portrayal patterns.
Visualization: Time-series analysis to track changes across years.
🔍 Sample Insights
The classification reveals trends in how police portrayals shift across different TV shows and time periods, highlighting patterns such as increased positive portrayals during certain cultural moments.

⚠️ Security Notice
API keys are handled via environment variables. Please do not hardcode your key into the script.

🤝 Acknowledgments
Inspired by ongoing research into police portrayals in media.
Leveraged OpenAI’s GPT API for text classification.
