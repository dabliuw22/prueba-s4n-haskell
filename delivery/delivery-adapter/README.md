# delivery-adapter

## Init
```shell
$ mkdir delivery/delivery-adapter
$ cd delivery/delivery-adapter
$ cabal init -p delivery-adapter
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
```