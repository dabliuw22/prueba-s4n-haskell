# files

## Init
```shell
$ mkdir infrastructure/files
$ cd infrastructure/files
$ cabal init -p files
```

## Add in `stack.yml`
```yaml
packages:
- ./
- ./infrastructure/envs
- ./infrastructure/logs
- ./infrastructure/files
```