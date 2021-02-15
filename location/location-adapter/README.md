# location-adapter

## Init
```shell
$ mkdir location/location-adapter
$ cd location/location-adapter
$ cabal init -p location-adapter
```

## Add in `stack.yml`
```yaml
packages:
- ./
- ./infrastructure/envs
- ./infrastructure/logs
- ./infrastructure/files
- ./core-s4n
- ./location/location-domain
- ./location/location-adapter
```