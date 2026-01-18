//! Compiler configuration types.
//!
//! This module provides immutable configuration for the compiler,
//! replacing the mutable refs in the OCaml `js_config.ml`.
//!
//! # Immutability
//!
//! Unlike the OCaml implementation which uses mutable refs, all configuration
//! is immutable after construction. This enables safe sharing across threads.
//!
//! # Example
//!
//! ```rust
//! use rescript_compiler::config::{CompilerConfig, JsxVersion, JsxModule};
//!
//! let config = CompilerConfig::builder()
//!     .jsx_version(JsxVersion::V4)
//!     .jsx_module(JsxModule::React)
//!     .debug(true)
//!     .build();
//!
//! assert!(config.debug);
//! assert_eq!(config.jsx_version, Some(JsxVersion::V4));
//! ```

use serde::{Deserialize, Serialize};

/// JSX version (currently only V4 is supported).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum JsxVersion {
    /// JSX version 4.
    #[default]
    V4,
}

impl JsxVersion {
    /// Convert to integer representation.
    pub fn to_int(self) -> i32 {
        match self {
            JsxVersion::V4 => 4,
        }
    }

    /// Try to create from integer.
    pub fn from_int(n: i32) -> Option<Self> {
        match n {
            4 => Some(JsxVersion::V4),
            _ => None,
        }
    }
}

/// JSX module to use for JSX transformation.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum JsxModule {
    /// React JSX runtime.
    #[default]
    React,
    /// Generic JSX with custom module name.
    Generic {
        /// The module name to use for JSX.
        module_name: String,
    },
}

impl JsxModule {
    /// Get the module name as a string.
    pub fn as_str(&self) -> &str {
        match self {
            JsxModule::React => "react",
            JsxModule::Generic { module_name } => module_name,
        }
    }
}

impl std::str::FromStr for JsxModule {
    type Err = std::convert::Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "react" => JsxModule::React,
            other => JsxModule::Generic {
                module_name: other.to_string(),
            },
        })
    }
}

/// Immutable compiler configuration.
///
/// This replaces all the mutable refs in `js_config.ml`. Once created,
/// a `CompilerConfig` cannot be modified, making it safe to share
/// across threads via `Arc<CompilerConfig>`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompilerConfig {
    // Output options
    /// Whether to include version header in output.
    pub no_version_header: bool,
    /// Directives to include at top of output files.
    pub directives: Vec<String>,

    // Optimization options
    /// Enable cross-module inlining.
    pub cross_module_inline: bool,
    /// Enable diagnostic output.
    pub diagnose: bool,
    /// Check for division by zero.
    pub check_div_by_zero: bool,

    // PPX options
    /// Disable built-in PPX.
    pub no_builtin_ppx: bool,

    // Compilation mode options
    /// Only parse syntax, don't compile.
    pub syntax_only: bool,
    /// Output binary AST.
    pub binary_ast: bool,
    /// Enable debug mode.
    pub debug: bool,
    /// Only generate .cmi files.
    pub cmi_only: bool,
    /// Only generate .cmj files.
    pub cmj_only: bool,
    /// Force regeneration of .cmi files.
    pub force_cmi: bool,
    /// Force regeneration of .cmj files.
    pub force_cmj: bool,

    // JSX options
    /// JSX version to use.
    pub jsx_version: Option<JsxVersion>,
    /// JSX module to use.
    pub jsx_module: JsxModule,
    /// Preserve JSX syntax in output.
    pub jsx_preserve: bool,

    // Output options
    /// Output JavaScript to stdout.
    pub js_stdout: bool,
    /// Treat all modules as aliases.
    pub all_module_aliases: bool,
    /// Don't link stdlib.
    pub no_stdlib: bool,
    /// Don't export module.
    pub no_export: bool,

    // PPX options
    /// Running as preprocessor.
    pub as_pp: bool,
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            no_version_header: false,
            directives: Vec::new(),
            cross_module_inline: false,
            diagnose: false,
            check_div_by_zero: true,
            no_builtin_ppx: false,
            syntax_only: false,
            binary_ast: false,
            debug: false,
            cmi_only: false,
            cmj_only: false,
            force_cmi: false,
            force_cmj: false,
            jsx_version: None,
            jsx_module: JsxModule::default(),
            jsx_preserve: false,
            js_stdout: true,
            all_module_aliases: false,
            no_stdlib: false,
            no_export: false,
            as_pp: false,
        }
    }
}

impl CompilerConfig {
    /// Create a new builder for `CompilerConfig`.
    pub fn builder() -> CompilerConfigBuilder {
        CompilerConfigBuilder::default()
    }

    /// Tool name for this compiler.
    pub const TOOL_NAME: &'static str = "ReScript";
}

/// Builder for `CompilerConfig`.
///
/// Provides a fluent API for constructing compiler configurations.
#[derive(Debug, Default)]
pub struct CompilerConfigBuilder {
    config: CompilerConfig,
}

impl CompilerConfigBuilder {
    /// Set whether to include version header.
    pub fn no_version_header(mut self, value: bool) -> Self {
        self.config.no_version_header = value;
        self
    }

    /// Set directives.
    pub fn directives(mut self, value: Vec<String>) -> Self {
        self.config.directives = value;
        self
    }

    /// Enable cross-module inlining.
    pub fn cross_module_inline(mut self, value: bool) -> Self {
        self.config.cross_module_inline = value;
        self
    }

    /// Enable diagnostic output.
    pub fn diagnose(mut self, value: bool) -> Self {
        self.config.diagnose = value;
        self
    }

    /// Check for division by zero.
    pub fn check_div_by_zero(mut self, value: bool) -> Self {
        self.config.check_div_by_zero = value;
        self
    }

    /// Disable built-in PPX.
    pub fn no_builtin_ppx(mut self, value: bool) -> Self {
        self.config.no_builtin_ppx = value;
        self
    }

    /// Only parse syntax.
    pub fn syntax_only(mut self, value: bool) -> Self {
        self.config.syntax_only = value;
        self
    }

    /// Output binary AST.
    pub fn binary_ast(mut self, value: bool) -> Self {
        self.config.binary_ast = value;
        self
    }

    /// Enable debug mode.
    pub fn debug(mut self, value: bool) -> Self {
        self.config.debug = value;
        self
    }

    /// Only generate .cmi files.
    pub fn cmi_only(mut self, value: bool) -> Self {
        self.config.cmi_only = value;
        self
    }

    /// Only generate .cmj files.
    pub fn cmj_only(mut self, value: bool) -> Self {
        self.config.cmj_only = value;
        self
    }

    /// Force regeneration of .cmi files.
    pub fn force_cmi(mut self, value: bool) -> Self {
        self.config.force_cmi = value;
        self
    }

    /// Force regeneration of .cmj files.
    pub fn force_cmj(mut self, value: bool) -> Self {
        self.config.force_cmj = value;
        self
    }

    /// Set JSX version.
    pub fn jsx_version(mut self, value: JsxVersion) -> Self {
        self.config.jsx_version = Some(value);
        self
    }

    /// Set JSX module.
    pub fn jsx_module(mut self, value: JsxModule) -> Self {
        self.config.jsx_module = value;
        self
    }

    /// Preserve JSX syntax.
    pub fn jsx_preserve(mut self, value: bool) -> Self {
        self.config.jsx_preserve = value;
        self
    }

    /// Output JavaScript to stdout.
    pub fn js_stdout(mut self, value: bool) -> Self {
        self.config.js_stdout = value;
        self
    }

    /// Treat all modules as aliases.
    pub fn all_module_aliases(mut self, value: bool) -> Self {
        self.config.all_module_aliases = value;
        self
    }

    /// Don't link stdlib.
    pub fn no_stdlib(mut self, value: bool) -> Self {
        self.config.no_stdlib = value;
        self
    }

    /// Don't export module.
    pub fn no_export(mut self, value: bool) -> Self {
        self.config.no_export = value;
        self
    }

    /// Running as preprocessor.
    pub fn as_pp(mut self, value: bool) -> Self {
        self.config.as_pp = value;
        self
    }

    /// Build the configuration.
    pub fn build(self) -> CompilerConfig {
        self.config
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = CompilerConfig::default();
        assert!(!config.debug);
        assert!(!config.syntax_only);
        assert!(config.check_div_by_zero);
        assert_eq!(config.jsx_module, JsxModule::React);
    }

    #[test]
    fn test_builder() {
        let config = CompilerConfig::builder()
            .debug(true)
            .jsx_version(JsxVersion::V4)
            .jsx_module(JsxModule::Generic {
                module_name: "preact".to_string(),
            })
            .build();

        assert!(config.debug);
        assert_eq!(config.jsx_version, Some(JsxVersion::V4));
        assert_eq!(config.jsx_module.as_str(), "preact");
    }

    #[test]
    fn test_jsx_version_conversion() {
        assert_eq!(JsxVersion::V4.to_int(), 4);
        assert_eq!(JsxVersion::from_int(4), Some(JsxVersion::V4));
        assert_eq!(JsxVersion::from_int(3), None);
    }

    #[test]
    fn test_jsx_module() {
        assert_eq!(JsxModule::React.as_str(), "react");
        assert_eq!("react".parse::<JsxModule>().unwrap(), JsxModule::React);
        assert_eq!(
            "preact".parse::<JsxModule>().unwrap(),
            JsxModule::Generic {
                module_name: "preact".to_string()
            }
        );
    }

    #[test]
    fn test_config_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<CompilerConfig>();
    }
}
