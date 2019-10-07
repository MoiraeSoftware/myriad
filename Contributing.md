how to debug:
- decomment line     <!-- <MyriadSdkWaitForDebugger>true</MyriadSdkWaitForDebugger> --> in test\Myriad.IntegrationPluginTests\Myriad.IntegrationPluginTests.fsproj
- dotnet build test\Myriad.IntegrationPluginTests -v n the test project
- myriad will start but wait for a debugger to attach (so the dotnet build is not yet done)
- in VSCode/Rider, attach to the process who contains myriad.dll (i added a vscode task in debugger window, ready to use)
- debugger will stop at beginning of the main of myriad (edited) 

like that, you can just modify a test, and attach in debugger to see it, no need to copy the cli args
