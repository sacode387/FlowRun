
## Prerequisites
- jdk 1.8, 11 or 17..
- sbt
- VSCode or IntelliJ (with respective plugins: Metals/Scala)

Easiest to install these is via SDKMAN.  
Or chocolatey on Windows.

## Developing

```shell
sbt
sbt:flowrun> ~demo/fastLinkJS
```

Open `demo/target/web/public/main/index.html` manually  
or use VSCode Live Server  
or any "live reload" server...

## Code organization
Project has 2 parts:
- core, which contains all of the logic for the editor/interpreter
- demo, contains demo code that uses core

Core can be production-optimized with `core/fullLinkJS`.  
The JS code will be generated in `core\target\scala-3.1.0\flowrun-opt\main.js`.



