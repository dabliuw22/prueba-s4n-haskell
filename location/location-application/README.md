# location-application

## Init
```shell
$ mkdir location/location-application
$ cd location/location-application
$ cabal init -p location-application
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
- ./location/location-application
```