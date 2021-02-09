# logs

## Init
```shell
$ mkdir infrastructure/logs
$ cd infrastructure/logs
$ cabal init -p logs
```

## Add in `stack.yml`
```yaml
packages:
- ./
- ./infrastructure/envs
- ./infrastructure/logs
```