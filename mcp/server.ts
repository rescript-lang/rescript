import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import { execFileSync, ExecFileSyncOptions } from "child_process";
import * as fs from "fs";
import * as path from "path";

// Create an MCP server
const server = new McpServer({
  name: "ReScript MCP",
  version: "1.0.0",
});

const {
  binPaths: { rescript_editor_analysis_exe, rescript_tools_exe },
} = await import(`@rescript/${process.platform}-${process.arch}`);

function execAnalysis(args: string[]): string | null {
  let options: ExecFileSyncOptions = {
    maxBuffer: Infinity,
    env: {
      // TODO: Do not hard code
      RESCRIPT_VERSION: "12.0.0-alpha.14",
    },
  };

  let stdout = "";
  try {
    stdout = execFileSync(
      rescript_editor_analysis_exe,
      args,
      options
    ).toString();
    return stdout;
  } catch (e) {
    console.error(e);
  }

  return null;
}

// Add type definition finder tool
server.tool(
  "find_type_definition",
  {
    filePath: z.string().describe("Absolute path to the ReScript file"),
    line: z.number().describe("Line number (0-indexed)"),
    col: z.number().describe("Column number (0-indexed)"),
  },
  {
    destructiveHint: false,
    idempotentHint: true,
    readOnlyHint: true,
    title: "Finds the type definition of a symbol at the given location",
  },
  ({ filePath, line, col }) => {
    try {
      const result = execAnalysis([
        "mcp",
        "loc-info",
        filePath,
        line.toString(),
        col.toString(),
      ]);
      return {
        content: [{ type: "text", text: result ?? "No result." }],
      };
    } catch (error) {
      return {
        content: [
          { type: "text", text: `Error finding type definition: ${error}` },
        ],
      };
    }
  }
);

server.tool(
  "find_type_of_global_identifier",
  {
    filePath: z.string().describe("Absolute path to the ReScript file"),
    identifier: z.string().describe("The identifier to find the type of"),
  },
  {
    destructiveHint: false,
    idempotentHint: true,
    readOnlyHint: true,
    title: `Finds the type of a global identifier, like \`SomeModule.SomeNestedModule.someValue\` or \`React.createElement\`.
      
      - **Always** use the fully qualified global path. Even if you are in \`SomeModule.res\` and are looking for \`x\` in that file, use the fully qualified path \`SomeModule.x\`.`,
  },
  ({ filePath, identifier }) => {
    try {
      const result = execAnalysis([
        "mcp",
        "identifier-info",
        filePath,
        identifier,
      ]);
      return {
        content: [{ type: "text", text: result ?? "No result." }],
      };
    } catch (error) {
      return {
        content: [
          { type: "text", text: `Error finding type definition: ${error}` },
        ],
      };
    }
  }
);

server.tool(
  "language_docs_file_path",
  {},
  {
    destructiveHint: false,
    idempotentHint: true,
    readOnlyHint: true,
    title: `Gets the path to a markdown file containing the full language docs for ReScript the language, including examples.
      
      - **Always** use this to grep or search as needed when you have questions about ReScript the language.`,
  },
  async () => {
    try {
      const cacheDir = path.resolve(__dirname, ".cache");
      const docsFileName = "rescript-lang-docs-12.0.0.md";
      const docsFilePath = path.join(cacheDir, docsFileName);

      if (!fs.existsSync(cacheDir)) {
        fs.mkdirSync(cacheDir, { recursive: true });
      }

      if (!fs.existsSync(docsFilePath)) {
        const res = await fetch(
          "https://rescript-lang.org/llms/manual/v12.0.0/llm-full.txt"
        );
        const text = await res.text();
        fs.writeFileSync(docsFilePath, text);
      }

      return {
        content: [{ type: "text", text: docsFilePath }],
      };
    } catch (e) {
      return {
        content: [
          { type: "text", text: `Error getting language docs file path: ${e}` },
        ],
      };
    }
  }
);

// Main function to start the server
async function main() {
  try {
    // Clear the cache directory
    fs.rmSync(path.resolve(__dirname, ".cache"), {
      recursive: true,
      force: true,
    });

    // Start receiving messages on stdin and sending messages on stdout
    const transport = new StdioServerTransport();
    await server.connect(transport);
  } catch (error) {
    console.error("Failed to start MCP server:", error);
    process.exit(1);
  }
}

// Start the server
main();
