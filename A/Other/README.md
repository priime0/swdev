# xjson

`xjson` is a program for extracting string values as themselves and
number values as the string `"number"` from a well-formed JSON input.
The program consumes this input from STDIN, and outputs the collected
values as a comma-separated JSON string.

## Examples

Input:

``` json
[
  "abc",
  {
    "abc": 1
    "abb": "2",
  }
]
```

Output:

``` json
"abc, 2, number"
```

## Building the Executable

We have provided a `Makefile` for producing the `xjson` program. Run
`make` to install the dependencies and build the binary:

``` shell
$ make
raco pkg install --auto --skip-installed && raco exe -o xjson xjson.rkt
Linking current directory as a package
```

## Running the Executable

After the executable has been built, you can run the program as
follows:

``` shell
$ ./xjson < ./Tests/0-in.json
"some, json, value"
```

## Organization

From the top-level `A` directory:

- `xjson.rkt` contains the Racket program that reads JSON from STDIN,
  produces a JSON string containing the extracted values and sends it
  to STDOUT. It also contains unit tests for the logic of the `xjson`
  function.
- `info.rkt` specifies a Racket package representing the `xjson`
  binary, defining the dependencies consumed by the `xjson` program.
- `Tests/0-in.json` and `Tests/0-out.json` represent a sample input to
  pass into the `xjson` program and the sample expected output after
  the input has been consumed, respectively.
- `Makefile` is used to install the necessary Racket packages and
  compile the `xjson.rkt` program into a binary.
- `Other/test.sh` runs the sample tests against the produced `xjson`
  executable.

## Roadmap

No roadmap is necessary for this assignment.

## Testing

The test harness is runnable by running:

``` shell
$ chmod +x ./Tests/test.sh && ./Tests/test.sh
```

To run the unit tests, you can use `raco`:

``` shell
$ raco test xjson.rkt
```

