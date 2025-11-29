# zrc-wasm: WebAssembly Bindings for `libzrc`

This crate provides `wasm-bindgen` WebAssembly bindings for `zrc`'s libraries, allowing you to
compile Zirco code in the browser or other WebAssembly environments.

## Wait, I just want to compile my code to WebAssembly!

If you want to compile Zirco code _to_ WebAssembly (i.e., generate `.wasm` files from Zirco source code), you do not need this crate. Instead, you can use the standard Zirco compiler with the `--target wasm32-unknown-unknown` flag:

```bash
zrc --target wasm32-unknown-unknown your_code.zrc -o your_code.wasm
```

This crate is specifically for running the Zirco compiler itself _as_ WebAssembly, not for generating WebAssembly output.

## Dependencies

The same as standard LLVM, plus a precompiled LLVM toolchain that you have already cross-compiled
for the `wasm32-unknown-unknown` target.
