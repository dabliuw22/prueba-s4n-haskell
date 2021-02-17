# delivery-domain

## Init
```shell
$ mkdir delivery/delivery-domain
$ cd delivery/delivery-domain
$ cabal init -p delivery-domain
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
```