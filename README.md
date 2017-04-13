This is my attempt to use `reflex` for both the front and the back end of a server.

All credit for the project layout / nix magic goes to the author of [this project](https://github.com/srhb/reflex-servant-scaffold).

You should be able to do
```
nix-build .
./result/bin/serve-serial
```
to get things started.

At the moment this is using git submodules, but I'll shuffle some of that into nix expressions soon enough.
