# core

## Init
```shell
$ mkdir core-s4n
$ cd core-s4n
$ cabal init -p core-s4n
```

## Add in `stack.yml`
```yaml
packages:
- ./
- ./infrastructure/envs
- ./infrastructure/logs
- ./infrastructure/files
- ./core-s4n
```