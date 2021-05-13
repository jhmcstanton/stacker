# Stacker

Simple web app to facilitate stack based video meetings.

## Dev

This project uses [`stack`](https://docs.haskellstack.org/en/stable/README/) for builds
and deployments. Use the classic

```sh
stack clean
stack build
stack exec stacker
```

to get clean, build, then run locally.

## Deploy to Heroku

I am currently hosting my instance of `stacker` on Heroku. To do the same you
will need to add a `stack` buildpack for your project. [This
one](https://github.com/mfine/heroku-buildpack-stack) has worked well for me.

## TODO

- Add support for stacking on behalf of other attendees
