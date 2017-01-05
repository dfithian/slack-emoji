docker stop slack_emoji
docker rm slack_emoji
docker pull dfithian/slack-emoji:2.1
docker run -d -p 3000:3000/tcp --name slack_emoji dfithian/slack-emoji:2.1
