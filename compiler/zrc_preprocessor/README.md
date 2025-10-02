# zrc_preprocessor

The Zirco preprocessor handles file inclusion and other preprocessing directives
before the source code is parsed.

## Features

- `#include "file.zr"` - Include other Zirco source files
- File path resolution relative to the including file
- Preserves source location information for error reporting
