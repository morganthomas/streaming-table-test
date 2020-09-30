# Usage

Run client on port 8080:

```
$ nix-shell
nix-shell$ cabal repl client
Main> main
```

Run server on port 8081:

```
$ nix-shell
nix-shell$ cabal repl server
Main> main
```

Run worker server on port 8082:

```
$ nix-build --arg isJS true
$ ./static-server.sh
```

Open up http://localhost:8080/ in a browser instance with the CORS restriction disabled.
