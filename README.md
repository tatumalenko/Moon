Moon
====
[![Build Status](https://github.com/tatumalenko/Moon/workflows/build/badge.svg)](https://github.com/tatumalenko/Moon/actions)
[![Coverage Status](https://coveralls.io/repos/github/tatumalenko/Moon/badge.svg?branch=master&t=Bx5cMI&service=github)](https://coveralls.io/github/tatumalenko/Moon?branch=master)
[![.Net Core Version](https://badge.fury.io/gh/dotnet%2Fcore.svg)](https://badge.fury.io/gh/dotnet%2Fcore)
[![Docker Image](https://dockeri.co/image/tatumalenko/moon)](https://hub.docker.com/r/tatumalenko/moon/)

> A Moon compiler written in F# using the .NET Core framework.

# Install
**Requirements:** Depending on the approach used to execute the program, either the .NET Core 3.1.1 SDK or Docker is required to run the program. The following sections outline how to use either approaches.

# Use
## Run
There are three ways to run the command line program.

### 1. Using Docker (Recommended):
```sh
$ docker run --rm tatumalenko/moon lex --text "$(cat ~/Desktop/input.src)"
```

OR by using the Make rule:

```sh
$ make dockerrun path="~/Desktop/input.src"
```

This will pull the latest image hosted on [Docker Hub](https://hub.docker.com/r/tatumalenko/moon/tags) and immediately run it using the contents of the file from the path supplied. This is the easiest way to get the program running without needing to install any platform specific SDKs and whatnot.

### 2. Using `dotnet core` SDK:

```sh
$ cd Moon
$ dotnet run -- lex --path "~/Desktop/input.src" --outdir "~/Desktop"
```

OR by using the Make rule:

```sh
$ make run path="~/Desktop/input.src" outdir="~/Desktop"
```

This will output `input.outlextokens` and `input.outlexerrors` files into the `~/Desktop` directory. This unfortunately to install the `dotnet core 3.1.1` SDK and associated dependencies, which may be a hassle. For this reason, approach #1 is the recommended one.

## Test
The unit tests can only be executed by using the source code and `dotnet core` SDK.

```sh
$ dotnet test
```

OR by using the Make rule:

```sh
$ make test
```