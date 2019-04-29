# Relude Test Maker

This program reads the documentation strings for the Relude library, extracts
the example code (between ` ```re ` and ` ``` `), and encloses it in a 
`Js.log2();` call that displays the example code and its result.

# Build
```
npm run build
```

# Running the Program

```
node src/MakeTests.bs.js ModuleName.re ModuleName > OutputFile.re
```
