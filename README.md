# myriad

<Insert funky picture!>

See [Applied Metaprogramming with Myriad And Falanx](https://7sharp9.github.io/2019/04/24/2019-04-24-applied-metaprogramming-with-myriad/) for details on myriad.  I'll add more details here soon.


### How to build

1. Make sure you have .Net Core SDK installed - check required version in [global.json](global.json)
2. Run `dotnet tool restore`
3. Run `dotnet build`

### How to test

1. Make sure you can build project
2. Run `dotnet test`
3. In case test failed check `TestOutput.xml` for test logs
