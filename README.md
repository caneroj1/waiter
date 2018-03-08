# waiter

An easy-to-use CLI tool to set up a static file server in any directory.

## Usage

To serve files from the current directory, just run:

```
$ waiter
Waiter: Serving '.' on port 3000!
```

The only argument to the executable is an optional path to change the directory from where files are served:

```
$ waiter /my/server/files
Waiter: Serving '/my/server/files' on port 3000!
```

### Options

* By default, all requests are logged to stdout. To disable that, run with the flag `--no-log`.

* To change the port of the server, run with `-p <port>` or `--port <port>`

