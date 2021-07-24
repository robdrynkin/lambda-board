CREATE TABLE comments (
    id         INTEGER PRIMARY KEY AUTOINCREMENT,
    threadName STRING,
    text       STRING,
    date       STRING,
    replyToId  INTEGER
);

CREATE TABLE threads (
    name       STRING NOT NULL PRIMARY KEY,
    ncomments  INTEGER
);
