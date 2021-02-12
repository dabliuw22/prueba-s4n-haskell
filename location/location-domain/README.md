# location-domain

## Init
```shell
$ mkdir location/location-domain
$ cd location/location-domain
$ cabal init -p location-domain
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
```