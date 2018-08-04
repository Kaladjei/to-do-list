# to-do-list

Basic and advanced to do applications written with miso

### Apps

- Basic ([sources](apps/basic-to-do-list))

### How to build

```
stack setup
stack build
```

### How to run apps

To run an app use `run.sh` script:

```
./run.sh basic-to-do-list
```

When GHCJSi has loaded open your browser and point to
http://localhost:6401/ghcjsiClient.html
(this is a custom REPL page with script sources specified).

Now run (or reload) the app using

```
clearBody >> main
```

If you change the sources you can reload affected modules with

```
:reload
```

And reload the app again with `clearBody >> main`.

This script will build everything if needed and also replace `index.html` when necessary.
If build is complete the script will also `open` the example using default browser.
