#### Preamble ####
# Purpose: Analyze comments via Google Perspective API
# Author: Inessa De Angelis
# Date: 14 February 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Libraries set up ####
from googleapiclient.discovery import build
import pandas as pd
import time  

#### API Key ####
API_KEY = 'ADD HERE' # Add your Perspective API key here

#### Analyze comments ####
## Google API Client ##
def get_client():
    return build(
        'commentanalyzer',
        'v1alpha1',  
        developerKey=API_KEY,
        discoveryServiceUrl="https://commentanalyzer.googleapis.com/$discovery/rest?version=v1alpha1",
        static_discovery=False,
    )

def analyze_comment(client, comment_text):
    analyze_request = {
        'comment': {'text': comment_text},
        'requestedAttributes': {
            'TOXICITY': {},
            'SEVERE_TOXICITY': {},
            'INSULT': {},
            'SEXUALLY_EXPLICIT': {},
            'PROFANITY': {},
            'THREAT': {},
            'FLIRTATION': {}
        }
    }
    try:
        response = client.comments().analyze(body=analyze_request).execute()
        return response
    except Exception as e:
        print(f"Error analyzing comment: {e}")
        return None

def process_comments_in_batches(csv_file_path, batch_size=59):
    ## Load comments from CSV file ##
    df = pd.read_csv(csv_file_path)
    
    ## Check CSV has a column named 'Comment' ##
    if 'Comment' not in df.columns:
        raise ValueError("CSV file must contain a column named 'Comment'")
    
    ## Create list to hold the scores ##
    scores = []

    ## Initialize the Google API client ##
    client = get_client()

    ## Process in batches ##
    num_rows = len(df)
    for start_idx in range(0, num_rows, batch_size):
        end_idx = min(start_idx + batch_size, num_rows)
        batch_df = df.iloc[start_idx:end_idx]  # Get the batch

        ## Process each comment in the batch ##
        for index, row in batch_df.iterrows():
            comment_text = row['Comment']
            
            try:
                response = analyze_comment(client, comment_text)
                
                if response:
                    toxicity_score = response.get('attributeScores', {}).get('TOXICITY', {}).get('summaryScore', {}).get('value', 'N/A')
                    severe_toxicity_score = response.get('attributeScores', {}).get('SEVERE_TOXICITY', {}).get('summaryScore', {}).get('value', 'N/A')
                    insult_score = response.get('attributeScores', {}).get('INSULT', {}).get('summaryScore', {}).get('value', 'N/A')
                    sexually_explicit_score = response.get('attributeScores', {}).get('SEXUALLY_EXPLICIT', {}).get('summaryScore', {}).get('value', 'N/A')
                    profanity_score = response.get('attributeScores', {}).get('PROFANITY', {}).get('summaryScore', {}).get('value', 'N/A')
                    threat_score = response.get('attributeScores', {}).get('THREAT', {}).get('summaryScore', {}).get('value', 'N/A')
                    flirtation_score = response.get('attributeScores', {}).get('FLIRTATION', {}).get('summaryScore', {}).get('value', 'N/A')

                    ## Add comment scores to list ##
                    scores.append({
                        'Comment': comment_text,
                        'toxicity_score': toxicity_score,
                        'severe_toxicity_score': severe_toxicity_score,
                        'insult_score': insult_score,
                        'sexually_explicit_score': sexually_explicit_score,
                        'profanity_score': profanity_score,
                        'threat_score': threat_score,
                        'flirtation_score': flirtation_score
                    })

            except Exception as e:
                print(f"Error processing comment at index {index}: {e}. Skipping this comment.")

        ## Sleep between processing batches ##
        print(f"Processed batch {start_idx // batch_size + 1}. Sleeping for 61 seconds...")
        time.sleep(61)

    ## Create DataFrame from the list of scores ##
    scores_df = pd.DataFrame(scores)
    
    ## Add back other columns from the original dataset ##
    final_df = pd.concat([df.drop(columns=['Comment']), scores_df], axis=1)

    ## Re-organize dataset to put 'Comment' column before the scores ##
    final_df = final_df[['Comment'] + [col for col in final_df.columns if col != 'Comment']]

    return final_df

def save_results_to_csv(final_df, output_file_path):
    ## Save results to CSV ##
    final_df.to_csv(output_file_path, index=False)

def main():
    ## Read in dataset ##
    input_csv_file_path = 'ADD PATH' # update
    ## Save analyzed comments ##
    output_csv_file_path = 'ADD PATH' # update
    
    final_df = process_comments_in_batches(input_csv_file_path)
    save_results_to_csv(final_df, output_csv_file_path)
    
    print(f"Results saved to {output_csv_file_path}")

if __name__ == "__main__":
    main()
