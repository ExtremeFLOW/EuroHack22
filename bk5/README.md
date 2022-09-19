## Matrix-vector product benchmark

To run the benchmark 
* Install `neko` and set `PKG_CONFIG_PATH` to point to `<installation prefix>/lib/pkgconfig`, e.g.:
```console
Using bash
> export PKG_CONFIG_PATH=/path/to/neko/installation/lib/pkgconfig
```
```console
Using tcsh
> setenv PKG_CONFIG_PATH /path/to/neko/installation/lib/pkgconfig
```
* Build bk5 using `make`
* Run with `./bk5. <neko mesh> <#GLL points>` 
  * Suitable meshes can be found under `data`)
  * `#GLL points` is defined as polynomial order + 1
