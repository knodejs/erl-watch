erl-watch
=========


Erlang application to watch and reload changed application beam files.

Intended to be used for development only to avoid manually reloading
beam files after compile.

Pools all beam files on directories in the `code:get_path()` minus the
`code:lib_dir()`.


Using with rebar
---------------

Add as a dependency to application and ensure the
application is started.

Using directly
--------------

* compile using `make compile`
* add ebin directory to your code path
  e.g., `erl -pa .../erl-watch/ebin' -pa ...`
* application:start(watch)
