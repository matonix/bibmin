# bibmin-tonaserver

[Mattermost slash command](https://docs.mattermost.com/developer/slash-commands.html#custom-slash-command) server using [tonatona](https://hackage.haskell.org/package/tonatona).

## Sample

```
$ curl -X POST --data-urlencode 'text=@misc{ patashnik-bibtexing,
  author = "Oren Patashnik",
  title = "BIBTEXing",
  year = "1988" }' http://localhost:8000/bibmin | jq -r '.text'
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   285    0   117  100   168  17155  24633 --:--:-- --:--:-- --:--:-- 28000
@misc{patashnik-bibtexing,
  author = "Oren Patashnik",
  title = "BIBTEXing",
  year = "1988"
}
```