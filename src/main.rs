#![feature(if_let_guard)]
#![feature(map_try_insert)]
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]
#![forbid(unsafe_code)]
#![deny(clippy::pedantic)]
#![deny(clippy::nursery)]
#![forbid(clippy::enum_glob_use)]
#![forbid(clippy::unwrap_used)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::cognitive_complexity)]
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_wrap)]

use std::path::PathBuf;

use badlang_parser as pa;
pub use pa::{report_error, report_warning};
use savefile::prelude::*;

#[macro_use]
extern crate savefile_derive;

mod microservices;
#[cfg(test)]
mod test;
mod tutorial;

use microservices as ms;

fn sillyness(save_data: &mut ms::SaveData) {
    // jod_thread::spawn(|| {
    //     tauri::Builder::default()
    //         .any_thread()
    //         .invoke_handler(tauri::generate_handler!(ms::tauri_handler))
    //         .build(tauri::generate_context!())
    //         .expect("error while building tauri application")
    //         .run(|_app_handle, event| {
    //             if let tauri::RunEvent::ExitRequested { api, .. } = event {
    //                 api.prevent_exit();
    //             }
    //         });
    // })
    // .detach();

    ms::has_internet();
    ms::server_outage();

    let dialogs_displayed = save_data.dialogs_displayed;
    #[cfg(not(all(target_os = "macos", not(debug_assertions))))] // The dialogs currently segfault on intel macs
    jod_thread::spawn(move || {
        if !dialogs_displayed {
            ms::show_dialogs();
        }
    })
    .detach();

    if !save_data.dialogs_displayed {
        save_data.dialogs_displayed = true;
    }

    ms::mailing_list_notification();
    ms::advertisement();
    ms::cookies();
    ms::login(&mut save_data.account);
    ms::self_promotion();
    ms::update(&mut save_data.last_update);
    ms::trial_message(&mut save_data.runs_so_far);
}

// The silly part is over. Thank god

fn main() {
    // Using this removes lots of fluff from panic messages
    std::panic::set_hook(Box::new(|panic_bundle| {
        eprintln!("{}", panic_bundle.payload().downcast_ref::<String>().map_or_else(|| panic_bundle.to_string(), std::clone::Clone::clone));
    }));

    let matches = clap::command!()
        .subcommands([
            clap::Command::new("run")
                .about("Run a program")
                .arg(clap::arg!(<file> "The file to run").value_parser(clap::value_parser!(PathBuf)))
                .arg(clap::arg!(--notroll).hide(true).required(false).value_parser(clap::value_parser!(bool))), /* This argument isn't really needed and potentially defeats the purpose of the
                                                                                                                 * program but it is here so I can keep my sanity. */
            clap::Command::new("tutorial")
                .about("Unlock new language features")
                .long_about("Unlock handy new features of the language in a playful sandbox where you learn to use them!"),
        ])
        .arg(
            clap::arg!(-s --subscribe "Open the subscription plans in a browser")
                .required(false)
                .value_parser(clap::value_parser!(bool)),
        )
        .get_matches();

    if let Some(subscribe) = matches.get_one::<bool>("subscribe")
        && *subscribe
    {
        let _ = open::that("https://www.youtube.com/watch?v=dQw4w9WgXcQ");
        report_error("I can't currently open a subscription page without doing some tax-evasion in-case somebody actually donates. Maybe later :/");
    }

    match matches.subcommand() {
        Some(("run", run_matches)) => {
            let out_of_free_runs = if let Some(no_troll) = run_matches.get_one::<bool>("notroll")
                && !(*no_troll)
            {
                let mut savefile_path = home::home_dir().expect("Couldn't locate your home directory, aborting");
                savefile_path.push(".config");
                savefile_path.push("badlang");
                savefile_path.push("badlang.bin");

                let mut save_data = match load_file::<ms::SaveData, &PathBuf>(&savefile_path, 0) {
                    Ok(sd) => {
                        if sd
                            .account
                            .as_ref()
                            .is_some_and(|acc| acc.version == semver::Version::parse(env!("CARGO_PKG_VERSION")).expect("WTF cargo").to_string())
                        {
                            sd
                        } else {
                            report_warning("Because the version your account was created on doesn't match your current version, your account was invalidated. Create a new one.");
                            ms::SaveData::default()
                        }
                    }
                    Err(_) => ms::SaveData::default(),
                };

                sillyness(&mut save_data);

                if let Some(parent_dir) = savefile_path.parent() && let Err(err) = std::fs::DirBuilder::new().recursive(true).create(parent_dir) {
                    report_error(format!("Couldn't create savefile because {err}").as_str());
                }

                save_file(savefile_path, 0, &save_data).expect("Couldn't save damn");

                save_data.runs_so_far >= ms::FREE_RUNS
            } else {
                false
            };

            let tokens = run_matches
                .get_one::<PathBuf>("file")
                .map_or_else(|| report_error("Please provide a path to run!"), pa::parse_file)
                .unwrap_or_else(|err| report_error(err.to_string().as_str()));

            pa::execute_tokens(&tokens, out_of_free_runs, &mut std::io::stdout(), None).unwrap_or_else(|err| report_error(err.to_string().as_str()));
        }

        Some(("tutorial", _)) => tutorial::tutorial(0),
        Some((_, _)) => report_error("Invalid subcommand!"),
        None => report_error("No subcommand"),
    }
}
