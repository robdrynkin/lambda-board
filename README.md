# lambda-board

Локальная сборка:
`docker build -t robdrynkin/lambda-board:latest .`

На докерхабе оно само собирается из репозитория из ветки develop с тегом develop.
https://hub.docker.com/r/robdrynkin/lambda-board/builds


Чтобы перевыкатить надо:
`ssh root@161.35.17.177`
`docker pull robdrynkin/lambda-board:develop`
`docker run -d -p 80:3000 robdrynkin/lambda-board:develop`
