# ReScript MCP Server

A Model Context Protocol (MCP) server that provides comprehensive ReScript language support. This server exposes ReScript compiler analysis capabilities through MCP tools, enabling AI assistants to understand and work with ReScript code.

## Features

This MCP server provides the following tools:

### Core Analysis Tools

- **`context`** - Get type information and context for any position in a ReScript file
- **`bindings`** - Find available bindings and completions at a specific location
- **`find_definition`** - Navigate to the definition of a symbol
- **`find_type_definition`** - Navigate to the type definition of a symbol
- **`find_references`** - Find all references to a symbol

### Code Quality Tools

- **`document_symbols`** - Get all symbols declared in a ReScript file
- **`format_file`** - Format a ReScript file according to standard conventions
- **`syntax_diagnostics`** - Get syntax error diagnostics for a file
- **`inlay_hints`** - Get type hints for a range of code
- **`signature_help`** - Get function signature help at a specific location

## Prerequisites

- ReScript compiler and analysis tools must be installed and available in PATH
- The `rescript-editor-analysis.exe` binary must be accessible

## Installation

```bash
npm install
npm run build
```

## Usage

This is an MCP server designed to be used with MCP clients. To use it with an AI assistant:

1. Build the server: `npm run build`
2. Configure your MCP client to use this server
3. The server will be available at the built `dist/server.js`

## Tool Examples

### Get type information at a position

```json
{
  "tool": "context",
  "parameters": {
    "filePath": "src/MyComponent.res",
    "line": 10,
    "col": 5
  }
}
```

### Find definition of a symbol

```json
{
  "tool": "find_definition",
  "parameters": {
    "filePath": "src/MyComponent.res",
    "line": 15,
    "col": 12
  }
}
```

### Format a file

```json
{
  "tool": "format_file",
  "parameters": {
    "filePath": "src/MyComponent.res"
  }
}
```

### Get document symbols

```json
{
  "tool": "document_symbols",
  "parameters": {
    "filePath": "src/MyComponent.res"
  }
}
```

## Development

- `npm run dev` - Watch mode for TypeScript compilation
- `npm run build` - Build the server
- `npm start` - Run the built server

## Architecture

This MCP server acts as a bridge between MCP clients and the ReScript compiler's analysis engine. It spawns the `rescript-editor-analysis.exe` process to perform the actual language analysis and returns the results through the MCP protocol.

All tools use 0-indexed line and column numbers, following the Language Server Protocol (LSP) specification.
