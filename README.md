# slack-emoji

Web service providing text emojis by lookup term. Originally developed as a slash command in Slack.

TODO needs instructions for setting up Slack.

## Environment

### Haskell

If you haven't already, install [stack](http://docs.haskellstack.org/en/stable/install_and_upgrade.html).
You'll need to install GHC as well: `stack setup --install-ghc`.

### Docker (optional)

If you want to build locally for another architecture (like linux) you'll need to install
[docker](https://www.docker.com/products/docker-desktop) and then build with `--docker`.

## Building

```bash
# if you want to build for release you can remove `--fast`
# if you want to target linux for example (in the case you're deploying there you can specify `--docker`)
stack build --fast
```

## Running

```bash
stack exec slack-emoji
curl http://localhost:3000/?text=happy
```
