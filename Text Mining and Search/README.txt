Dataset:
- Main dataset = https://drive.google.com/file/d/1bbcSwSAftfbUr6o8zQHttFXoW82bjtXl/view?usp=sharing
- subtopic dataset = https://drive.google.com/file/d/1Z5cw9NFQ-CTrC42SyHbP01yJvxG28-zB/view?usp=sharing

Download the notebook and open it on colab or jupyter notebook.
In order to run the notebook you have 2 options:
1) download the dataset and upload it on drive, when running the code on colab it will connect to your drive, from there you can import the dataset by copying the path and paste it in the third cell, at the first line that is:
sp = pd.read_csv("/content/gdrive/MyDrive/PubMed Multi Label Text Classification Dataset Processed.csv", encoding="utf-8")
replace the path with yours.
In the subtopic part, we import the topic.csv that we generated for simplifying the process.
After that you can simply run all the code.

2) download the dataset, when running the code on jupyter notebook you can ignore and remove the lines in second cell that gives you problem, justo turn them into a comment or remove them, they are used to connect to drive on colab. 
Then you can import the dataset by copying its path from your download folder and paste it in the third cell, at the first line that is:
sp = pd.read_csv("/content/gdrive/MyDrive/PubMed Multi Label Text Classification Dataset Processed.csv", encoding="utf-8")
replace the path with yours.
In the subtopic part, we import the topic.csv that we generated for simplifying the process.
After that you can simply run all the code.