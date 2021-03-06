# Stacker

Simple web app to facilitate stack based video meetings.

## Dev

### Server

This project uses [`stack`](https://docs.haskellstack.org/en/stable/README/) for builds
and deployments. Use the classic

```sh
stack clean
stack build
stack exec stacker
```

to get clean, build, then run locally.

## Client

This project is now using tailwind for styling. Run the following to make
changes and build the new css.

```
npx tailwindcss -i ./static/src/style.css -o ./static/style.css  --watch
```

## Deploy to Heroku

I am currently hosting my instance of `stacker` on Heroku. To do the same you
will need to add a `stack` buildpack for your project. [This
one](https://github.com/mfine/heroku-buildpack-stack) has worked well for me.

### Heroku Free Tier

*NOTE* if you are on the Heroku free tier consider using the environment variable
`KEEPALIVE_URL` to create a new keepalive page in the application. This page is
useful to keep the application from being put to sleep after ~30 minutes when
web requests are no longer coming in but meetings are still ongoing. For example,
adding the variable `KEEAPLIVE_URL=/keepalive` will add a keepalive endpoint that
just refreshes the page every 10 minutes to keep the application running.

Check out an instance of the free tier at at https://chat-stacker.herokuapp.com


## Docker

### Build

Build this image in the usual way:

```
docker build . -t stacker:<tag name>
```

### Run

Run the image with:

```
docker run --rm --name stacker -p 80:80 stacker:<tag name>
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
