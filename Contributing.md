# How to build:
1. Make sure you have version of `dotnet` specified in [global.json](global.json)
2. Run `dotnet tool restore` to install required local tools
3. Run `dotnet build`

# How to debug:
- uncomment line     `<!-- <MyriadSdkWaitForDebugger>true</MyriadSdkWaitForDebugger> -->` in test\Myriad.IntegrationPluginTests\Myriad.IntegrationPluginTests.fsproj
- `dotnet build test\Myriad.IntegrationPluginTests -v n` the test project
- myriad will start but wait for a debugger to attach (so the dotnet build is not yet done)
- in VSCode/Rider, attach to the process that contains myriad.dll (I added a vscode task in debugger window, ready to use)
- debugger will stop at beginning of the main of myriad (edited)

With this, you can just modify a test, and attach in debugger to see it, no need to copy the cli args
