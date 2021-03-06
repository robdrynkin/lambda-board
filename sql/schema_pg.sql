CREATE TABLE threads (
    name       TEXT NOT NULL PRIMARY KEY,
    ncomments  INTEGER
);


CREATE TABLE comments (
    id         SERIAL,
    threadName TEXT,
    text       TEXT,
    date       TEXT,
    replyToId  INTEGER
);

CREATE INDEX comments_thread_name_index ON comments USING hash (threadName);
