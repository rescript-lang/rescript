mod build;
mod clean;
mod compiler_args;
mod connection;
mod debug;
pub mod format;
mod output;
mod watch;

pub use build::run as build;
pub use clean::run as clean;
pub use compiler_args::get_compiler_args;
pub use connection::{
    connect, connect_or_start, find_project_root, is_daemon_running, start_daemon_if_needed,
};
pub use debug::run as debug;
pub use watch::run as watch;
