#### Preamble ####
# Purpose: Download download YouTube comments by MP username
# Author: Inessa De Angelis
# Date: 1 August 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Note: Example script used to download comments by MP username

#### Set up ####
import os
import csv
import googleapiclient.discovery
import googleapiclient.errors
from datetime import datetime

API_KEY = 'ADD KEY HERE'  # Add API key
CHANNEL_HANDLE = '@NAME'  # Replace with handle
OUTPUT_FILE = 'ADD FILE PATH'  # Add file name/path

#### Run ####
def get_youtube_service():
    """Returns an instance of the YouTube API client."""
    return googleapiclient.discovery.build('youtube', 'v3', developerKey=API_KEY)

def get_channel_id(youtube, channel_handle):
    """Fetches the channel ID for a given channel handle or custom URL."""
    request = youtube.search().list(
        part='snippet',
        q=channel_handle,
        type='channel',
        maxResults=1
    )
    response = request.execute()
    if 'items' in response and len(response['items']) > 0:
        return response['items'][0]['id']['channelId']
    return None

def get_video_ids(youtube, channel_id):
    """Fetches all video IDs from a given channel between specific dates."""
    video_ids = []
    next_page_token = None
    while True:
        request = youtube.search().list(
            part='id',
            channelId=channel_id,
            maxResults=50,
            type='video',
            publishedAfter='2024-12-01T00:00:00Z', # Update as needed 
            publishedBefore='2024-12-08T00:00:00Z', # Update as needed 
            pageToken=next_page_token
        )
        response = request.execute()
        for item in response.get('items', []):
            video_ids.append(item['id']['videoId'])
        next_page_token = response.get('nextPageToken')
        if not next_page_token:
            break
    return video_ids

def get_comments(youtube, video_id):
    """Fetches all comments from a given video ID, including additional details."""
    comments = []
    next_page_token = None
    while True:
        try:
            request = youtube.commentThreads().list(
                part='snippet,replies',
                videoId=video_id,
                textFormat='plainText',
                pageToken=next_page_token
            )
            response = request.execute()
            print(f"Fetched page for video ID {video_id}: {response}")  # Debug statement

            for item in response.get('items', []):
                snippet = item['snippet']['topLevelComment']['snippet']
                comment = {
                    'username': snippet['authorDisplayName'],
                    'comment': snippet['textDisplay'],
                    'authorDisplayName': snippet['authorDisplayName'],
                    'authorProfileImageUrl': snippet['authorProfileImageUrl'],
                    'authorChannelUrl': snippet['authorChannelUrl'],
                    'authorChannelId': snippet['authorChannelId']['value'],
                    'replyCount': item['snippet']['totalReplyCount'],
                    'likeCount': snippet['likeCount'],
                    'publishedAt': snippet['publishedAt'],
                    'updatedAt': snippet['updatedAt'],
                    'commentId': item['id'],
                    'parentId': item.get('snippet', {}).get('parentId', 'N/A'),
                    'videoId': video_id,
                    'videoDate': get_video_date(youtube, video_id)
                }
                comments.append(comment)
                
                # Handle replies if they exist
                if 'replies' in item:
                    for reply in item['replies']['comments']:
                        reply_snippet = reply['snippet']
                        reply_comment = {
                            'username': reply_snippet['authorDisplayName'],
                            'comment': reply_snippet['textDisplay'],
                            'authorDisplayName': reply_snippet['authorDisplayName'],
                            'authorProfileImageUrl': reply_snippet['authorProfileImageUrl'],
                            'authorChannelUrl': reply_snippet['authorChannelUrl'],
                            'authorChannelId': reply_snippet['authorChannelId']['value'],
                            'replyCount': 0,
                            'likeCount': reply_snippet['likeCount'],
                            'publishedAt': reply_snippet['publishedAt'],
                            'updatedAt': reply_snippet['updatedAt'],
                            'commentId': reply['id'],
                            'parentId': reply['snippet']['parentId'],
                            'videoId': video_id,
                            'videoDate': get_video_date(youtube, video_id)
                        }
                        comments.append(reply_comment)

            next_page_token = response.get('nextPageToken')
            if not next_page_token:
                break
        except googleapiclient.errors.HttpError as e:
            error_message = e._get_reason()  
            print(f"An error occurred: {error_message}")
            break  
    return comments

def get_video_date(youtube, video_id):
    """Fetches the video publish date for a given video ID."""
    try:
        request = youtube.videos().list(
            part='snippet',
            id=video_id
        )
        response = request.execute()
        if 'items' in response and len(response['items']) > 0:
            return response['items'][0]['snippet']['publishedAt']
        return 'N/A'
    except googleapiclient.errors.HttpError as e:
        print(f"An error occurred while fetching video date: {e}")
        return 'N/A'

def save_comments_to_csv(comments_data):
    """Saves comments data to a CSV file."""
    with open(OUTPUT_FILE, 'w', newline='', encoding='utf-8') as file:
        writer = csv.DictWriter(file, fieldnames=[
            'Username', 'Comment', 'AuthorDisplayName', 'AuthorProfileImageUrl',
            'AuthorChannelUrl', 'AuthorChannelId', 'ReplyCount', 'LikeCount',
            'PublishedAt', 'UpdatedAt', 'CommentId', 'ParentId', 'VideoId',
            'VideoDate'
        ])
        writer.writeheader()
        for comment in comments_data:
            writer.writerow(comment)

def main():
    youtube = get_youtube_service()
    comments_data = []

    ## Use the channel handle ##
    handle = CHANNEL_HANDLE
    channel_id = get_channel_id(youtube, handle)
    if not channel_id:
        print(f"Channel not found for handle: {handle}")
        return

    video_ids = get_video_ids(youtube, channel_id)
    for video_id in video_ids:
        print(f"Fetching comments for video ID: {video_id}")
        comments = get_comments(youtube, video_id)
        if comments:
            for comment in comments:
                comments_data.append({
                    'Username': handle,
                    'Comment': comment['comment'],
                    'AuthorDisplayName': comment['authorDisplayName'],
                    'AuthorProfileImageUrl': comment['authorProfileImageUrl'],
                    'AuthorChannelUrl': comment['authorChannelUrl'],
                    'AuthorChannelId': comment['authorChannelId'],
                    'ReplyCount': comment['replyCount'],
                    'LikeCount': comment['likeCount'],
                    'PublishedAt': comment['publishedAt'],
                    'UpdatedAt': comment['updatedAt'],
                    'CommentId': comment['commentId'],
                    'ParentId': comment['parentId'],
                    'VideoId': comment['videoId'],
                    'VideoDate': comment['videoDate']
                })
            print(f"Comments fetched for video ID: {video_id}")
        else:
            print(f"No comments found for video ID: {video_id}")

    save_comments_to_csv(comments_data)
    print(f"All comments saved to {OUTPUT_FILE}")

if __name__ == '__main__':
    main()
