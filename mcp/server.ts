import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import { execFileSync, ExecFileSyncOptions } from "child_process";

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

// Main function to start the server
async function main() {
  try {
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
