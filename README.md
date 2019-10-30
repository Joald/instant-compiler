# instant-compiler

## Compilation and dependencies

The compiler is built using stack. 
Except for BNFC and hidden standard packages listed in `package.yaml`, it doesn't use any other tools or libraries. 

## Directory structure
- `app/Main.hs`: BNFC-generated `Main` module with very minor tweaks.
- `src/Abs.hs`: Definitions of custom types, their instances and small utility functions.
- `src/JVM.hs`, `src/LLVM.hs`: Compilation modules.
- `src/Processor.hs`: Source validation.
