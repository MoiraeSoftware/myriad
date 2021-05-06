# Notes about MSBuild machinery

## How is a rebuild determined?

Rebuilding is determined by:
* the hash of the version of myriad
* reference paths
* `--inputfile`
* `--outputfile`
* `--configkey`

The MSBuild implementation relies on `_MyriadSdkCodeGenInputCache` property which is defined in [Myriad.Sdk.targets](./src/Myriad.Sdk/build/Myriad.Sdk.targets)
