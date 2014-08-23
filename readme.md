This webservice provides a Twilio request URL for finding and returning book reviews from Goodreads via SMS. I made this because 3g connections can be patchy, especially in sheltered bookstores.

Deploying should be as simple as:

```shell
> cabal sandbox init
> cabal install -j
> ./.cabal-sandbox/bin/g2t
```
