# RemObjects Pascal Script for Delphi ( RPS )

![Sample](Articles/ps-128.png?raw=true)

- **Homepage:** https://www.remobjects.com/ps
- **Base Repository:** https://github.com/remobjects/pascalscript

![](https://tokei.rs/b1/github/pult/pascalscript?category=code) ![](https://tokei.rs/b1/github/pult/pascalscript?category=files)

 Pascal Script is a free scripting engine that allows you to use most of the Object Pascal language within your Delphi or Free Pascal projects at runtime. Written completely in Delphi, it is composed of a set of units that can be compiled into your executable, eliminating the need to distribute any external files. Pascal Script started out as a need for a good working script, when there were none available at the time.

Why use a scripting engine?

A scripting engine allows an end user to customize an application to his or her needs without having to recompile it. In addition, you can update your applications by just sending a new script file that could even be compiled to byte code, which cannot easily be transformed back to source code.

Pascal Script includes the following features:

    Variables, Constants
    Standard language constructs:
        Begin/End
        If/Then/Else
        For/To/Downto/Do
        Case x Of
        Repeat/Until
        While
        Uses
        Exit
        Continue
        Break
    Functions inside the script
    Calling any external DLL function (no special function headers required)
    Calling registered external methods
    All common types like Byte, Shortint, Char, Word, SmallInt, Cardinal, Longint, Integer, String, Real, Double, Single, Extended, Boolean, Array, Record, Enumerations, Variants
    Allows the importing and use of classes, with events, properties, methods and constructors
    Allows the importing and use of interfaces and their members
    Allows IDispatch dynamic method invocation through Variant
    Assignment of script functions to Delphi events
    Uses byte code as an intermediate format and allows the storing and reloading of compiled scripts
    Easy to use component version
    Support for include files
    Support for compiler defines
    Capability to call RemObjects SDK Services from within scripts
    Includes a tool to create headers for importing classes and interfaces
