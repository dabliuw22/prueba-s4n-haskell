# delivery-application

## Init
```shell
$ mkdir delivery/delivery-application
$ cd delivery/delivery-application
$ cabal init -p delivery-application
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
- ./delivery/delivery-domain
- ./delivery/delivery-adapter
- ./delivery/delivery-application
```