---
title: Debugging
category: tutorial
menu_order: 4
pre: "<i class='fas fa-bug'></i> "
---

# Debugging

![Generator](pexels-markus-spiske-965345.jpg)
To debug Myriad, you can use the following two command line options:

* `--verbose` — write diagnostic logs out to standard out
* `--wait-for-debugger` — causes Myriad to wait for a debugger to attach to the Myriad process

These can be triggered from msbuild by the `<MyriadSdkVerboseOutput>true</MyriadSdkVerboseOutput>` and `<MyriadSdkWaitForDebugger>true</MyriadSdkWaitForDebugger>` properties, respectively.