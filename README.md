# Stacker

Simple web app to facilitate stack based video meetings.

## Check It Out

You can try `stacker` out [over here](https://stacker-2je4.onrender.com/)

## Dev

### Server

This project uses [`cabal`](https://cabal.readthedocs.io/en/stable/) for builds
and deployments. Use the classic

```sh
cabal update
cabal install
env PORT=8080 cabal run stacker
```

to build and run locally on port 8080.

### Client

This project is now using tailwind for styling. Run the following to make changes and build the new css.

```
npx tailwindcss -i ./static/src/style.css -o ./static/style.css  --watch
```

## Docker

### Build

Build this image in the usual way:

```
docker build . -t stacker:<tag name>
```

### Run

Run the image with:

```
docker run --rm --name stacker -p 8080:8080 stacker:<tag name>
```

Note, if your hosting provider expects a specific port or host you can assign it
with:

```
docker run -e PORT=<new port> -e HOST=<new host> --rm ...  
```


## TODO

- ~Dockerize~
- ~Add support for stacking on behalf of other attendees~
- Add progressive stack / ~method to move people around the stack~
- Add support for context based stacking
- ~Add speak counter to attendees~
- ~Prettify UI~
- Consider separating the websocket app from the http app
