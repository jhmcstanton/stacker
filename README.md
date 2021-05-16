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

### Heroku Free Tier

*NOTE* if you are on the Heroku free tier the app will sleep after ~30 minutes of
not being in use (in this case, not receiving new web requests). Be sure to
refresh the page periodically to ensure it does not do this for long running
meetings.

Check out an instance of the free tier at at https://chat-stacker.herokuapp.com

## TODO

- Dockerize
- Add support for stacking on behalf of other attendees
- Add progressive stack / method to move people around the stack
- Add speak counter to attendees
- Prettify UI
