# envs

## Init
```shell
$ mkdir infrastructure/envs
$ cd infrastructure/envs
$ cabal init -p envs
```

## Add in `stack.yml`
```yaml
packages:
- ./
- ./infrastructure/envs
```