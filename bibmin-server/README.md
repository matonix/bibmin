# bibmin-server

## query
```
curl -X POST -d bibtex='@misc{ patashnik-bibtexing,
  author = "Oren Patashnik",
  title = "BIBTEXing",
  year = "1988" }' -D - http://localhost:8080/bibmin

```

## response
```
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Sat, 26 Jan 2019 13:58:47 GMT
Server: Warp/3.2.25
Content-Type: text/plain; charset=utf-8

@misc{patashnik-bibtexing,
author = "Oren Patashnik",
title = "BIBTEXing",
year = "1988"
}⏎
```