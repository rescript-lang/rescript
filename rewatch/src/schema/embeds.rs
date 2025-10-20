use schemars::{schema::RootSchema, schema_for};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(deny_unknown_fields)]
pub struct GeneratorSourceSchema {
    /// Absolute or project-relative path to the source file containing the embed
    pub path: String,
    /// Module name of the source file (e.g. Foo__Bar)
    pub module: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(deny_unknown_fields)]
pub struct GeneratorConfigSchema {
    /// Extra files the generator depends on (project-relative paths)
    #[serde(default)]
    pub extra_sources: Vec<String>,
    /// Reserved for future project-level options. Pass-through JSON.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub options: Option<serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(deny_unknown_fields)]
pub struct GeneratorRequestSchema {
    /// The embed tag that matched, e.g. "sql.one"
    pub tag: String,
    /// The embed data: either a string literal or a config object
    pub data: serde_json::Value,
    /// Source file path and module
    pub source: GeneratorSourceSchema,
    /// 1-based occurrence index of this embed in the file for this tag
    pub occurrence_index: u32,
    /// Generator configuration as derived from rescript.json
    pub config: GeneratorConfigSchema,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(deny_unknown_fields)]
#[schemars(example = "example_batch_input")]
pub struct BatchInputSchema {
    /// Requests to process in order
    pub requests: Vec<GeneratorRequestSchema>,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(deny_unknown_fields)]
pub struct GenDiagPosSchema {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(deny_unknown_fields)]
pub struct GenDiagItemSchema {
    /// Human-readable error message
    pub message: String,
    /// Optional severity ("error" | "warning" | "info"), defaults to "error"
    #[serde(default)]
    pub severity: Option<String>,
    /// Optional machine-readable code (e.g. "SQL001")
    #[serde(default)]
    pub code: Option<String>,
    /// Start position relative to the embed string (1-based)
    #[serde(default)]
    pub start: Option<GenDiagPosSchema>,
    /// End position relative to the embed string (1-based, inclusive)
    #[serde(default)]
    pub end: Option<GenDiagPosSchema>,
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "camelCase", tag = "status")]
#[schemars(example = "example_output_ok")]
pub enum GeneratorOutputSchema {
    #[serde(rename_all = "camelCase")]
    Ok {
        /// ReScript source code to write to generated module (.res)
        code: String,
    },
    #[serde(rename_all = "camelCase")]
    Error {
        /// Diagnostics mapped to the embed string
        errors: Vec<GenDiagItemSchema>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize, schemars::JsonSchema)]
#[serde(rename_all = "camelCase")]
#[schemars(deny_unknown_fields)]
pub struct BatchOutputSchema {
    /// Results for each request in the same order
    pub results: Vec<GeneratorOutputSchema>,
}

// Examples for schema docs
fn example_batch_input() -> BatchInputSchema {
    BatchInputSchema {
        requests: vec![GeneratorRequestSchema {
            tag: "sql.one".to_string(),
            data: serde_json::json!("/* @name GetUser */ select * from users where id = :id"),
            source: GeneratorSourceSchema { path: "src/Foo.res".to_string(), module: "Foo".to_string() },
            occurrence_index: 1,
            config: GeneratorConfigSchema { extra_sources: vec!["schema.graphql".to_string()], options: None },
        }],
    }
}

fn example_output_ok() -> GeneratorOutputSchema {
    GeneratorOutputSchema::Ok {
        code: "let default = \"...\"".to_string(),
    }
}

pub fn embedlang_input_schema() -> RootSchema { schema_for!(BatchInputSchema) }
pub fn embedlang_output_schema() -> RootSchema { schema_for!(BatchOutputSchema) }

pub fn openapi_document() -> serde_json::Value {
    // Build a minimal OpenAPI 3.1 document with components only.
    let input = embedlang_input_schema();
    let output = embedlang_output_schema();
    let mut components = serde_json::Map::new();
    components.insert("BatchInput".to_string(), serde_json::to_value(&input.schema).unwrap_or(serde_json::json!({})));
    // Inject discriminator for tagged union on `status` in OpenAPI doc
    let mut output_schema = serde_json::to_value(&output.schema).unwrap_or(serde_json::json!({}));
    if let serde_json::Value::Object(ref mut o) = output_schema {
        o.insert(
            "discriminator".to_string(),
            serde_json::json!({"propertyName": "status"}),
        );
    }
    components.insert("BatchOutput".to_string(), output_schema);
    // Merge definitions (if any) into components as inline schemas with stable keys
    for (k, v) in input.definitions {
        components.insert(k, serde_json::to_value(v).unwrap());
    }
    for (k, v) in output.definitions {
        components.insert(k, serde_json::to_value(v).unwrap());
    }

    serde_json::json!({
        "openapi": "3.1.0",
        "info": {
            "title": "Rewatch EmbedLang Protocol",
            "version": "1.0.0"
        },
        "paths": {},
        "components": { "schemas": components },
    })
}
