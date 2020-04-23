# Moon

[![Build Status](https://github.com/tatumalenko/Moon/workflows/build/badge.svg)](https://github.com/tatumalenko/Moon/actions)
[![Coverage Status](https://coveralls.io/repos/github/tatumalenko/Moon/badge.svg?branch=master&t=Bx5cMI&service=github)](https://coveralls.io/github/tatumalenko/Moon?branch=master)
[![.Net Core Version](https://badge.fury.io/gh/dotnet%2Fcore.svg)](https://badge.fury.io/gh/dotnet%2Fcore)
[![Docker Image](https://dockeri.co/image/tatumalenko/moon)](https://hub.docker.com/r/tatumalenko/moon/)

> A Moon compiler written in F# using the .NET Core framework.

# Assignments

## Assignment 1

You will find the assignment document located in `doc/a1/a1_40055122.pdf`.

## Assignment 2

You will find the assignment document located in `doc/a2/a2_40055122.pdf`.

## Assignment 3

You will find the assignment document located in `doc/a3/a3_40055122.pdf`.

## Assignment 4

You will find the assignment document located in `doc/a4/a4_40055122.pdf`.

# Install

**Requirements:** Depending on the approach used to execute the program, the .NET Core 3.1.1 SDK is required to run the program. The following sections outline how to use run the compiler.

# Use

## Run

```sh
$ cd /Path/to/root/directory/Moon
$ dotnet run --path "/path/to/input.src" --outdir "/path/to/some/outDir"
```

The relevant files will be written to the directory supplied by the `outdir` argument.

## Test

The unit tests can only be executed by using the source code and `dotnet core` SDK.

```sh
$ dotnet test
```
