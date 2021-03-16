# prueba-s4n-haskell

## Problem
[backend-dev-technical-test](./backend-dev-technical-test.pdf)

## Description
In the solution, a modularized application is designed, using typeclasses and tagless-final, where:

1. **core-s4n**: is an internal DSL that represents the actions and evaluations of the drone.
2. **infrastructure/envs**: It is the layer that allows us to manage environment variables.
2. **infrastructure/logs**: It is the layer that allows us to manage environment logs.
2. **infrastructure/files**: Is the layer that allows us to write and read files.
3. **location**: Is the subdomain in charge of registering the positions of a drone.
4. **delivery**: Is the subdomain responsible for order delivery.

## Requirements
* Haskell (GHC)
* Haskell Stack
* Cabal
    
## Create ENV VARS
```shell
$ export IN_PATH={your_in_path} # required
$ export IN_FILE_PREFIX={your_in_file_prefix} # default: in
$ export OUT_PATH={your_out_path} # required
$ export OUT_FILE_PREFIX={your_out_file_prefix} # default: out
$ export EXT_FILE_PREFIX={your_ext_file_prefix} # default: .txt
$ export LOGS_DIR={your_logs_dir} # default: log
$ export LOGS_FILENAME={your_logs_filename} # default: prueba-s4n-haskell.log
$ export APP_NAME={your_app_name} # default: prueba-s4n-haskell
$ export APP_ENV={your_app_env} # default: local 
```

## Build
```shell
$ stack build prueba-s4n-haskell
```

## Run
```shell
$ stack exec prueba-s4n-haskell-exe
```
