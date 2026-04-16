use crate::helpers::emojis::*;
use console::style;
use std::io::{BufRead, BufReader};
use std::process::{Command, Stdio};
use std::time::Instant;

pub fn run(command_string: String) {
    let start_subcommand = Instant::now();

    log::info!(
        "{} {}Running subcommand... \n{}\n",
        style("[...]").bold().dim(),
        COMMAND,
        style("────────"),
    );

    let parsed_command = command_string.split_whitespace().collect::<Vec<&str>>();
    let (command, params) = parsed_command.split_at(1);

    let mut cmd = Command::new(command[0])
        .args(params)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to execute process");

    {
        let stdout = cmd.stdout.as_mut().expect("TODO: handle error");
        let stderr = cmd.stderr.as_mut().expect("TODO: handle error");

        let stdout_reader = BufReader::new(stdout);
        let stderr_reader = BufReader::new(stderr);

        let stdout_lines = stdout_reader.lines();
        let std_err = stderr_reader.lines();

        for line in stdout_lines {
            println!("{}", line.expect("TODO: handle error"));
        }

        for line in std_err {
            eprintln!("{}", line.expect("TODO: handle error"));
        }

        let subcommand_duration = start_subcommand.elapsed();
        log::info!(
            "{}{} {}Ran subcommand in {:.2}s",
            LINE_CLEAR,
            style("[...]").bold().dim(),
            COMMAND,
            subcommand_duration.as_secs_f64(),
        );
    }

    cmd.wait().expect("TODO: handle error");
}
