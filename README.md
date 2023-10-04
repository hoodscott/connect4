# Connect 4 made with Elm

<div align="middle">
  <img src="/img/setup.png" alt="screenshot of connect 4 game setup" width="270" />
  <img src="/img/game.png" alt="screenshot of connect 4 game in progress" width="270" />
  <img src="/img/win.png" alt="screenshot of connect 4 game win screen" width="270" />
</div>

## Can play:

- Human vs. Human
- Human vs. AI
- AI vs AI

## AIs implemented:

- Choose random column
- Simple minimax that looks for winning/losing games in the next n turns
- Choose middle column
- Choose least empty column (tries to stack pieces)
- Choose most empty columm (tries to spread pieces)

## Setup

Requires global install of Elm.  Then run following commands to setup elm-live server on [localhost:8000](http://localhost:8000/):

```
npm i
npm run live
```
