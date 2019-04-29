type scanStateType =
  | Scanning
  | InExample;
  
type stateType = {
  scanState: scanStateType,
  exampleLines: array(string),
  result: string
};

let escapeQuotes = (s: string): string => {
  Js.String.replaceByRe([%re "/\"/g"],"\\\"", s);
};

let processExampleLines = (arr: array(string)): string => {
  let endStmtPattern = [%re "/;\\s*$/"];

  let helper = ((result, stmt), item) => {
    let full_stmt = stmt ++ item
    if (Js.Re.test_(endStmtPattern, full_stmt)) {
      if (Js.Re.test_([%re "/^let\\s+/"], full_stmt)) {
        (result ++ full_stmt ++ "\n", "")
      } else {
        (result
          ++ "Js.log2(\""
          ++ escapeQuotes(full_stmt)
          ++ " \",\n  "
          ++ Js.String.replaceByRe(endStmtPattern, "", full_stmt)
          ++ ");\n",
          "")
      }
    } else {
      (result, stmt ++ item ++ "\n  ")
    }
  }
  let (result, _) = Belt.Array.reduce(arr, ("", ""), helper);
  result;
};

let lineReducer = (acc: stateType, line: string): stateType => {
  switch (acc.scanState) {
    | Scanning => {
        if (Js.Re.test_([%re "/```re/"], line)) {
          {...acc,
            scanState: InExample,
            exampleLines: [| |]
          }
        } else {
          acc;
        }
      }
    | InExample => {
        if (Js.Re.test_([%re "/```/"], line)) {
          {...acc,
            result: acc.result
              ++ "Js.log(\"================\");\n"
              ++ processExampleLines(acc.exampleLines) ++ "\n",
            scanState: Scanning
          }
        } else {
          {...acc,
            exampleLines: Belt.Array.concat(acc.exampleLines,
              [| Js.String.trim(line) |])
          }
        }
      }
  }
};

let processFile = (inFileName:string, moduleName: string): unit => {
  let fileContents = Node.Fs.readFileAsUtf8Sync(inFileName);
  let lines = Js.String.split("\n", fileContents);
  let init = {
    scanState: Scanning,
    exampleLines: [| |],
    result: "open " ++ moduleName ++ ";\n\n"
  };

  let finalResult =
    Belt.Array.reduce(lines, init, lineReducer);

  Js.log(finalResult.result);
};

let nodeArg = Belt.Array.getExn(Node.Process.argv, 0);
let progArg = Belt.Array.getExn(Node.Process.argv, 1);
let fileOpt = Belt.Array.get(Node.Process.argv, 2);
let moduleNameOpt = Belt.Array.get(Node.Process.argv, 3);

switch (fileOpt, moduleNameOpt) {
  | (Some(inFileName), Some(moduleName)) => processFile(inFileName, moduleName)
  | ( _, _) =>
    Js.log("Usage: " ++ nodeArg ++ " " ++ progArg ++ " ModuleName.re ModuleName")
};
