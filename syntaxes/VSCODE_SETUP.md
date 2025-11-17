# Using Zirco Grammar in VS Code

This guide explains how to set up syntax highlighting for Zirco (`.zr`, `.zh`) files in Visual Studio Code.

## Quick Setup (5 minutes)

### Step 1: Create Extension Directory

Create a directory for your local Zirco VS Code extension:

```bash
# On Linux/Mac
mkdir -p ~/.vscode/extensions/zirco-language

# On Windows
mkdir %USERPROFILE%\.vscode\extensions\zirco-language
```

### Step 2: Copy Grammar File

Copy the TextMate grammar to your extension directory:

```bash
# On Linux/Mac (from the zrc repository root)
cp syntaxes/zirco.tmLanguage.json ~/.vscode/extensions/zirco-language/

# On Windows (from the zrc repository root)
copy syntaxes\zirco.tmLanguage.json %USERPROFILE%\.vscode\extensions\zirco-language\
```

### Step 3: Create Package Configuration

Create a `package.json` file in the extension directory:

**Linux/Mac:**
```bash
cat > ~/.vscode/extensions/zirco-language/package.json << 'EOF'
{
  "name": "zirco-language",
  "displayName": "Zirco Language Support",
  "description": "Syntax highlighting for Zirco programming language",
  "version": "0.1.0",
  "engines": {
    "vscode": "^1.50.0"
  },
  "categories": ["Programming Languages"],
  "contributes": {
    "languages": [{
      "id": "zirco",
      "aliases": ["Zirco", "zirco"],
      "extensions": [".zr", ".zh"],
      "configuration": "./language-configuration.json"
    }],
    "grammars": [{
      "language": "zirco",
      "scopeName": "source.zirco",
      "path": "./zirco.tmLanguage.json"
    }]
  }
}
EOF
```

**Windows (PowerShell):**
```powershell
@"
{
  "name": "zirco-language",
  "displayName": "Zirco Language Support",
  "description": "Syntax highlighting for Zirco programming language",
  "version": "0.1.0",
  "engines": {
    "vscode": "^1.50.0"
  },
  "categories": ["Programming Languages"],
  "contributes": {
    "languages": [{
      "id": "zirco",
      "aliases": ["Zirco", "zirco"],
      "extensions": [".zr", ".zh"],
      "configuration": "./language-configuration.json"
    }],
    "grammars": [{
      "language": "zirco",
      "scopeName": "source.zirco",
      "path": "./zirco.tmLanguage.json"
    }]
  }
}
"@ | Out-File -FilePath $env:USERPROFILE\.vscode\extensions\zirco-language\package.json -Encoding UTF8
```

### Step 4: Create Language Configuration (Optional)

Create a `language-configuration.json` file for better editor support (auto-closing brackets, comment toggling, etc.):

**Linux/Mac:**
```bash
cat > ~/.vscode/extensions/zirco-language/language-configuration.json << 'EOF'
{
  "comments": {
    "lineComment": "//"
  },
  "brackets": [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"]
  ],
  "autoClosingPairs": [
    { "open": "{", "close": "}" },
    { "open": "[", "close": "]" },
    { "open": "(", "close": ")" },
    { "open": "\"", "close": "\"", "notIn": ["string"] },
    { "open": "'", "close": "'", "notIn": ["string", "comment"] }
  ],
  "surroundingPairs": [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"],
    ["\"", "\""],
    ["'", "'"]
  ]
}
EOF
```

**Windows (PowerShell):**
```powershell
@"
{
  "comments": {
    "lineComment": "//"
  },
  "brackets": [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"]
  ],
  "autoClosingPairs": [
    { "open": "{", "close": "}" },
    { "open": "[", "close": "]" },
    { "open": "(", "close": ")" },
    { "open": "\"", "close": "\"", "notIn": ["string"] },
    { "open": "'", "close": "'", "notIn": ["string", "comment"] }
  ],
  "surroundingPairs": [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"],
    ["\"", "\""],
    ["'", "'"]
  ]
}
"@ | Out-File -FilePath $env:USERPROFILE\.vscode\extensions\zirco-language\language-configuration.json -Encoding UTF8
```

### Step 5: Reload VS Code

1. Open VS Code
2. Press `Ctrl+Shift+P` (or `Cmd+Shift+P` on Mac)
3. Type "Developer: Reload Window" and press Enter
4. Open any `.zr` or `.zh` file to see syntax highlighting

## Verifying Installation

1. Open a Zirco file (e.g., `examples/hello_world/main.zr`)
2. Check the bottom-right corner of VS Code - it should show "Zirco" as the language
3. Keywords, types, strings, and comments should be highlighted

## Manual Setup Script

For convenience, you can use this script to set up the extension:

**Linux/Mac:**
```bash
#!/bin/bash
# Save as setup-vscode.sh and run with: bash setup-vscode.sh

EXTENSION_DIR="$HOME/.vscode/extensions/zirco-language"
REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"

mkdir -p "$EXTENSION_DIR"
cp "$REPO_ROOT/syntaxes/zirco.tmLanguage.json" "$EXTENSION_DIR/"

cat > "$EXTENSION_DIR/package.json" << 'EOF'
{
  "name": "zirco-language",
  "displayName": "Zirco Language Support",
  "description": "Syntax highlighting for Zirco programming language",
  "version": "0.1.0",
  "engines": { "vscode": "^1.50.0" },
  "categories": ["Programming Languages"],
  "contributes": {
    "languages": [{"id": "zirco", "aliases": ["Zirco", "zirco"], "extensions": [".zr", ".zh"], "configuration": "./language-configuration.json"}],
    "grammars": [{"language": "zirco", "scopeName": "source.zirco", "path": "./zirco.tmLanguage.json"}]
  }
}
EOF

cat > "$EXTENSION_DIR/language-configuration.json" << 'EOF'
{
  "comments": { "lineComment": "//" },
  "brackets": [["{", "}"], ["[", "]"], ["(", ")"]],
  "autoClosingPairs": [
    { "open": "{", "close": "}" },
    { "open": "[", "close": "]" },
    { "open": "(", "close": ")" },
    { "open": "\"", "close": "\"", "notIn": ["string"] },
    { "open": "'", "close": "'", "notIn": ["string", "comment"] }
  ],
  "surroundingPairs": [["{", "}"], ["[", "]"], ["(", ")"], ["\"", "\""], ["'", "'"]]
}
EOF

echo "Zirco VS Code extension installed to: $EXTENSION_DIR"
echo "Please reload VS Code to activate the extension."
```

## Troubleshooting

### Extension Not Loading

1. Check that the extension directory exists:
   ```bash
   # Linux/Mac
   ls -la ~/.vscode/extensions/zirco-language/
   
   # Windows
   dir %USERPROFILE%\.vscode\extensions\zirco-language\
   ```

2. Verify all files are present:
   - `zirco.tmLanguage.json`
   - `package.json`
   - `language-configuration.json` (optional)

3. Check VS Code's extension list:
   - Press `Ctrl+Shift+X` (or `Cmd+Shift+X` on Mac)
   - Search for "Zirco"
   - The extension should appear in the list

### Syntax Highlighting Not Working

1. Verify the file has the correct extension (`.zr` or `.zh`)
2. Check the language mode in the bottom-right corner of VS Code
3. Try manually selecting the language:
   - Press `Ctrl+K M` (or `Cmd+K M` on Mac)
   - Type "Zirco" and press Enter

### Updating the Grammar

When the grammar is updated in the repository:

1. Copy the new grammar file:
   ```bash
   cp syntaxes/zirco.tmLanguage.json ~/.vscode/extensions/zirco-language/
   ```

2. Reload VS Code (Ctrl+Shift+P → "Developer: Reload Window")

## Alternative: Publishing to VS Code Marketplace

If you want to publish this as a proper VS Code extension:

1. Install `vsce`:
   ```bash
   npm install -g vsce
   ```

2. Package the extension:
   ```bash
   cd ~/.vscode/extensions/zirco-language
   vsce package
   ```

3. Publish to the marketplace (requires Microsoft account):
   ```bash
   vsce publish
   ```

See [VS Code Extension Publishing](https://code.visualstudio.com/api/working-with-extensions/publishing-extension) for more details.

## Additional Features

The language configuration provides:

- **Comment toggling**: Press `Ctrl+/` (or `Cmd+/` on Mac) to toggle line comments
- **Auto-closing brackets**: Automatically closes `{}`, `[]`, `()`, `""`, `''`
- **Surrounding pairs**: Select text and type `{`, `[`, `(`, `"`, or `'` to wrap it
- **Bracket matching**: Matching brackets are highlighted when cursor is adjacent

## Next Steps

After setting up syntax highlighting, you might want to:

1. Configure a custom color theme for Zirco in VS Code settings
2. Add snippets for common Zirco patterns
3. Set up the Zirco compiler as a build task in VS Code
4. Configure debugging support for Zirco programs

See the [VS Code documentation](https://code.visualstudio.com/docs) for more information on customizing your development environment.
