// This part of the program serves absolutely no reason and is just here
// because I find it incredibly funny. More about the motivation can be read here: https://github.com/gianzellweger/badlang/blob/release/MOTIVATION.md

// List of "features" and a roadmap can be found at https://github.com/gianzellweger/badlang/issues/3

use std::{
    collections::HashSet,
    io::Write,
    sync::Mutex,
    time::{Instant, SystemTime, UNIX_EPOCH},
};

use argon2::{
    password_hash::{rand_core::OsRng, PasswordHash, PasswordHasher, PasswordVerifier, SaltString},
    Argon2,
};
use badlang_parser as pa;
use colored::Colorize;
use inquire::{validator::Validation, CustomUserError};
use rand::distr::Distribution;
use strum::EnumCount;
use strum_macros::EnumCount as EnumCountMacro;

use crate::{report_error, report_warning};

#[derive(EnumCountMacro, Debug, PartialEq, Eq)]
pub enum Advertisement {
    Temu,
    Shein,
    BetterHelp,
    Nestle,
    JohnsonJohnson,
    CocaCola,
    McDonalds,
}

// This is satire, but mostly based in fact. I encourage you to correct me
// if any newer information surfaces. I also encourage you to include more
// examples if there are more current ones.
impl std::fmt::Display for Advertisement {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Temu => "You support slave labor and genocide? Buy from our sponsor Temu!",
            Self::Shein => "You support exploiting small designers, genocide and slave labor? Buy clothes from our sponsor Shein!",
            Self::BetterHelp => "You want Facebook to have your sensitive medical records? Get online therapy from our sponsor BetterHelp!",
            Self::Nestle => "You enjoy stealing the water of African villages, bottling it and then selling it to them? Buy from our sponsor Nestlé!",
            Self::JohnsonJohnson => "You enjoy getting even richer by letting people in third-world countries die? Buy drugs from our sponsor Johnson & Johnson!",
            Self::CocaCola => "You enjoy infesting our seas with microplastics? Buy a refreshing beverage from our sponsor Coca-Cola!",
            Self::McDonalds => "You support McDonalds? Go shop at McDonalds!",
        })
    }
}

impl Distribution<Advertisement> for rand::distr::StandardUniform {
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Advertisement {
        static_assertions::const_assert_eq!(Advertisement::COUNT, 7); // If the match below isn't updated when a new Advertisement is added, it
                                                                      // won't be added to the rotation
        match rng.random_range(0..Advertisement::COUNT) {
            0 => Advertisement::Temu,
            1 => Advertisement::Shein,
            2 => Advertisement::BetterHelp,
            3 => Advertisement::Nestle,
            4 => Advertisement::JohnsonJohnson,
            5 => Advertisement::CocaCola,
            6 => Advertisement::McDonalds,
            _ => unreachable!(),
        }
    }
}

// #[allow(dead_code)]
// #[allow(clippy::needless_pass_by_value)]
// #[tauri::command]
// pub fn tauri_handler<R: tauri::Runtime>(window: tauri::Window<R>) {
//     static VELOCITY: Mutex<(i32, i32)> = Mutex::new((4, 4));
//     static POSITION: Mutex<(i32, i32)> = Mutex::new((0, 0));

//     let (screen_x, screen_y) = window
//         .current_monitor()
//         .ok()
//         .flatten()
//         .map(|monitor| *monitor.size())
//         .map_or((1920, 1080), |pos| (pos.height as i32, pos.width as i32));

//     let mut position = POSITION.lock().expect("Unreachable");
//     let mut velocity = VELOCITY.lock().expect("Unreachable");
//     if position.0 > screen_x || position.0 < 0 {
//         velocity.0 = -velocity.0;
//     }
//     if position.1 > screen_y || position.1 < 0 {
//         velocity.1 = -velocity.1;
//     }

//     position.0 += velocity.0;
//     position.1 += velocity.1;
//     drop(velocity);

//     let _ = window.set_position(tauri::Position::Physical((*position).into()));
//     drop(position);

//     let _ = window.set_focus();
// }

#[derive(Debug, Savefile, Clone)]
pub struct EditorState {
    pub text:              Vec<String>,
    pub cursor:            (u16, u16),
    pub console:           Result<String, String>,
    pub level:             u8,
    pub unlocked_features: Vec<pa::Token>,
}

impl std::default::Default for EditorState {
    fn default() -> Self {
        Self {
            text:              vec![],
            cursor:            (0, 0),
            console:           Ok(String::new()),
            level:             0,
            unlocked_features: vec![pa::Token::StackValue(pa::StackValue::Integer(0)), pa::Token::Print, pa::Token::Println],
        }
    }
}

#[derive(Savefile, Clone, Debug)]
pub enum TwoFA {
    GoogleAuth(String),
    SecurityQuestion(Vec<String>),
    OS,
}

#[derive(Savefile, Clone, Debug)]
pub struct Account {
    pub name:          String,
    // Yes I am actually taking a programming account (that doesn't do anything) serious enough to actually use encryption.
    pub password_hash: String,
    pub version:       String,
    pub two_factor:    TwoFA,
}

#[derive(Savefile, Clone, Debug, Default)]
pub struct SaveData {
    pub account:           Option<Account>,
    pub runs_so_far:       usize,
    pub last_update:       u64,  // This is in seconds since UNIX_EPOCH
    pub dialogs_displayed: bool, // It makes sense to display them only once per device, as this is how it works in serious applications.
    pub tutorial_state:    EditorState,
}

// These files are used to measure download speed. There are multiple
// because there are actual server outages and I don't want people to miss
// out on this one just because of such a tiny problem
pub const TEST_FILE: [(&str, u64); 4] = [
    ("https://speed.hetzner.de/100MB.bin", 100_000_000),
    ("https://ash-speed.hetzner.com/100MB.bin", 100_000_000),
    ("https://hel1-speed.hetzner.com/100MB.bin", 100_000_000),
    ("https://fsn1-speed.hetzner.com/100MB.bin", 100_000_000),
];

// The cookies don't actually do anything. Yet.
pub const TYPES_OF_COOKIES: [&str; 25] = [
    "Authentication Cookies",
    "Personalization Cookies",
    "Shopping Cart Cookies",
    "Analytics Cookies",
    "Ad Targeting Cookies",
    "Session Management Cookies",
    "Security Cookies",
    "Performance Cookies",
    "Social Media Integration Cookies",
    "Localization Cookies",
    "Predictive Preference Cookies",
    "Enhanced Virtual Reality Cookies",
    "Celebrity Influence Cookies",
    "Historical Context Cookies",
    "Multiverse Browsing Cookies",
    "Visual Communication Cookies",
    "Extraterrestrial Inspiration Cookies",
    "Mood Enhancement Cookies",
    "Future Trend Cookies",
    "Personalized Insights Cookies",
    "Predictive Health Cookies",
    "Behavioral Employment Cookies",
    "Government Compliance Cookies",
    "Mood Manipulation Cookies",
    "Social Credit Cookies",
];

// These Terms of Service are not actually legally binding, refer to the
// license for the actual legalese, don't sue me, etc.
pub const TERMS_OF_SERVICE: &str = r#"Terms of Service

1. Acceptance of Terms
By continuing to read these musings, you agree to acknowledge the lighthearted nature of the content. If, however, you find yourself questioning the validity of our statements, consider this your cue to exit stage left.

2. Changes to Terms
We reserve the right to evolve these "Terms" at our discretion. Changes may occur without notice, and users will navigate the subtle shifts in the landscape unassisted.

3. User Accounts
Creating a user account involves a process known only to a select few. Your password is stored in a secure vault, accessible only to those who possess the secret passphrase.

4. Content
You can't post any content on BadLang™. Please exercise discretion, as some forms of expression may be subject to interpretation by our team of resident enigmatologists.

5. Intellectual Property
BadLang™ and its features belong to the collective imagination. Attempts to assert ownership may lead to a journey through the labyrinthine corridors of paradoxes and riddles.

6. Termination
We reserve the right to limit access to BadLang™. Banishment may lead you to discover other realms, where the unknown becomes the known.

7. Governing Law
These "Terms" are subject to the laws of Wakanda. In the event of any disputes, resolutions may be found in the concealed archives of ancient wisdom.

8. Contact Us
For inquiries about these enigmatic Terms, submit your questions through channels familiar to those initiated into the subtleties of subtle communication.

ChatGPT
BadLang™ Legal Team

To accept, please type: 
I solemnly declare that I've thoroughly read and understood the Terms of Service, and I'm committed to adhering to its provisions
"#;

// Note: this is distinct from the text above in one very important way:
// These spaces are actually spaces, while the ones above are the Unicode
// character U+2002, which looks identical. This is to prevent copying.
pub const ACCEPTANCE_PHRASE: &str = "I solemnly declare that I've thoroughly read and understood the Terms of Service, and I'm committed to adhering to its provisions";

pub const CHANCE_OF_SERVER_MAINTENANCE: f64 = 0.1; // Also known as 10%

pub const FREE_RUNS: usize = 5;

pub const UPDATE_SIZE: u64 = 1_000_000_000; // 1GB
pub const UPDATE_VARIATION: u64 = 300_000_000; // 300MB
pub const DOWNLOAD_SPEED_VARIATION: f64 = 0.8; // 80%
pub const DOWNLOAD_UPDATE_INTERVAL: f64 = 0.5; // This is in seconds

pub const FIRST_OPTION: &str = "Yes, proceed to login";
pub const SECOND_OPTION: &str = "No, proceed to signup";

// Yes this function was fully written by ChatGPT because I forgot how to
// this shit
pub fn fetch_data(url: &str) -> Option<Vec<String>> {
    // Make a blocking GET request
    let response = reqwest::blocking::get(url).ok()?;

    // Check if the request was successful (status code 200 OK)
    if response.status().is_success() {
        // Read the response body as a string
        let body = response.text().ok()?;

        // Split the data by new-line
        let lines: Vec<String> = body.lines().map(String::from).collect();

        Some(lines)
    } else {
        // If the request was not successful, return an error
        None
    }
}

#[allow(clippy::unnecessary_wraps)]
pub fn password_validator(password: &str) -> Result<Validation, CustomUserError> {
    static TOP_100_PASSWORDS: Mutex<Option<Vec<String>>> = Mutex::new(None);

    // Yes this is very much a stolen idea from the Password game. I thought
    // it's a nice nod to the game after basically copying half its
    // concept
    let todays_wordle_answer = reqwest::blocking::get(format!("https://www.nytimes.com/svc/wordle/v2/{}.json", chrono::offset::Local::now().date_naive().format("%Y-%m-%d"))).map_or(None, |res| {
        match res.json::<serde_json::Value>() {
            Ok(serde_json::Value::Object(map)) => match map.get("solution") {
                Some(serde_json::Value::String(solution)) => Some(solution.clone()),
                _ => None,
            },
            _ => None,
        }
    });

    let mut top_100_passwords = TOP_100_PASSWORDS.lock().expect("Unreachable");

    if top_100_passwords.is_none() {
        *top_100_passwords =
            Some(fetch_data("https://raw.githubusercontent.com/danielmiessler/SecLists/master/Passwords/Common-Credentials/10-million-password-list-top-10000.txt").unwrap_or_default());
    }

    let password_list = if top_100_passwords.as_ref().is_some_and(Vec::is_empty) {
        None
    } else {
        top_100_passwords.as_ref()
    };

    Ok(if password.is_empty() {
        Validation::Invalid("The password is required".into())
    } else if password.chars().count() < 18 {
        Validation::Invalid("Your password needs to be at least 18 characters long".into())
    } else if password.chars().count() > 26 {
        Validation::Invalid("Your password cannot exceed 26 characters".into())
    } else if password.chars().any(char::is_whitespace) {
        Validation::Invalid("Your password may not contain any whitespace".into())
    } else if !password.chars().any(char::is_uppercase) {
        Validation::Invalid("Your password must contain an uppercase letter".into())
    } else if !password.chars().any(char::is_lowercase) {
        Validation::Invalid("Your password must contain a lowercase letter".into())
    } else if !password.chars().any(|c| c.is_ascii_digit()) {
        Validation::Invalid("Your password must contain a number".into())
    } else if password.chars().all(char::is_alphanumeric) {
        Validation::Invalid("Your password must contain a special character".into())
    } else if !password.chars().any(|c| unic_emoji_char::is_emoji(c) && !c.is_ascii()) {
        Validation::Invalid("Your password must contain an emoji".into())
    } else if password.chars().collect::<HashSet<_>>().len() < password.chars().count() {
        Validation::Invalid("Your password may not contain duplicate characters".into())
    } else if let Some(wordle_answer) = todays_wordle_answer.as_ref()
        && !password.to_lowercase().contains(wordle_answer.to_lowercase().as_str())
        && wordle_answer.chars().collect::<HashSet<_>>().len() == wordle_answer.len()
    {
        Validation::Invalid("Your password must contain today's wordle answer".into())
    } else if let Some(passwords) = password_list
        && let Some(part) = passwords.iter().find(|&pw| password.contains(pw))
        && let Some(wordle_answer) = todays_wordle_answer
        && !passwords.contains(&wordle_answer)
    {
        Validation::Invalid(format!("Your password contains one of the top 10000 most common passwords, '{part}', making it insecure").into())
    } else {
        Validation::Valid
    })
}

pub fn has_internet() {
    let has_internet = reqwest::blocking::get("https://google.com").is_ok(); // Googles servers are always up so I'm using them
    if !has_internet {
        report_error("To use this programming language, you need an internet connection!");
    }
}

pub fn server_outage() {
    if fastrand::f64() <= CHANCE_OF_SERVER_MAINTENANCE {
        report_error("Our servers are currently experiencing outages, but we are working hard to get them back online!");
    }
}

pub fn show_dialogs() {
    let _ = native_dialog::DialogBuilder::message()
        .set_level(native_dialog::MessageLevel::Warning)
        .set_title("BadLang™")
        .set_text(r#""BadLang™" wants to access your contacts. Allow?"#)
        .confirm();
    let _ = native_dialog::DialogBuilder::message()
        .set_level(native_dialog::MessageLevel::Warning)
        .set_title("BadLang™")
        .set_text(r#""BadLang™" wants to access your location. Allow?"#)
        .confirm();
    let _ = native_dialog::DialogBuilder::message()
        .set_level(native_dialog::MessageLevel::Warning)
        .set_title("BadLang™")
        .set_text(r#""BadLang™" wants to make and receive phone calls on your behalf. Allow?"#)
        .confirm();
    let _ = native_dialog::DialogBuilder::message()
        .set_level(native_dialog::MessageLevel::Warning)
        .set_title("BadLang™")
        .set_text(r#""BadLang™" wants to manage incoming network connections. Allow?"#)
        .confirm();
    let _ = native_dialog::DialogBuilder::message()
        .set_level(native_dialog::MessageLevel::Warning)
        .set_title("BadLang™")
        .set_text(r#""BadLang™" wants to access your passwords. Allow?"#)
        .confirm();
    let _ = native_dialog::DialogBuilder::message()
        .set_level(native_dialog::MessageLevel::Warning)
        .set_title("BadLang™")
        .set_text(r#""BadLang™" wants to access your liver. Allow?"#)
        .confirm();
}

pub fn mailing_list_notification() {
    let _ = notify_rust::Notification::new()
        .summary("Do you want to subscribe to our mailing list?")
        .body("Shoot an email to mailinglist@badlang.dev and you will automatically be added to the mailing list!")
        .appname("Mailing List Subscriber")
        .auto_icon()
        .sound_name("alarm-clock-elapsed")
        .timeout(0)
        .show(); // This notification will not go away unless you dismiss
                 // it.
}

pub fn advertisement() {
    let random_advertisement: Advertisement = rand::random();

    println!("{}", random_advertisement.to_string().yellow().on_purple().bold());
}

pub fn cookies() {
    let accepted_cookies = match inquire::Select::new("This programming language uses cookies.", vec!["Accept all", "Customize"])
        .without_help_message()
        .prompt()
        .expect("What")
    {
        "Customize" => inquire::MultiSelect::new("Select the kind of cookies you want", TYPES_OF_COOKIES.to_vec())
            .without_help_message()
            .with_default((0..25).collect::<Vec<_>>().as_slice())
            .prompt()
            .expect("No"),
        _ => TYPES_OF_COOKIES.to_vec(),
    };

    if !accepted_cookies.is_empty() {
        println!("The types of cookies you accepted are:");
        for cookie in accepted_cookies {
            println!("- {cookie}");
        }
    }

    println!();
}

pub fn login(account: &mut Option<Account>) {
    let argon2 = Argon2::default();

    match inquire::Select::new("To use this programming language, you need a BadLang™ Account. Do you already have one?", vec![
        FIRST_OPTION,
        SECOND_OPTION,
    ])
    .without_help_message()
    .prompt()
    .expect("What")
    {
        SECOND_OPTION => {
            let name = inquire::Text::new("Enter your username: ")
                .with_validator(inquire::required!())
                .with_validator(inquire::min_length!(16))
                .with_validator(|name: &str| {
                    Ok(if name.chars().all(|c| c.is_lowercase() || c.is_ascii_digit() || c == '.' || c == '_') {
                        Validation::Valid
                    } else {
                        Validation::Invalid("Usernames may only contain lowercase letters, numbers, underscores and dots".into())
                    })
                })
                .prompt()
                .expect("Enter your username");

            let password = inquire::Password::new("Enter your password: ")
                .without_confirmation()
                .with_validator(password_validator)
                .prompt()
                .expect("Enter a password");
            let password_repetition = inquire::Password::new("Repeat your password: ").without_confirmation().prompt().expect("Enter a password");
            let password_repetition2 = inquire::Password::new("Repeat your password again: ").without_confirmation().prompt().expect("Enter a password");

            if !(password == password_repetition && password_repetition == password_repetition2 && password == password_repetition2) {
                report_error("Your passwords do not match!");
            }

            let salt = SaltString::generate(&mut OsRng);
            let password_hash = argon2.hash_password(password.as_bytes(), &salt).expect("What happened? Why did the hasher fail?").to_string();

            let two_factor = match inquire::Select::new(
                "It is recommended that you use Two Factor Authentification for your Badlang™ Account. Which type would you like to use?",
                vec!["Google Authenticator", "Security questions", "Biometrics"],
            )
            .without_help_message()
            .prompt()
            .expect("What the fuck?")
            {
                "Google Authenticator" => {
                    let ga = google_authenticator::GoogleAuthenticator::new();
                    let secret = ga.create_secret(32);
                    let scheme = format!(
                        "otpauth://totp/{}?secret={}&issuer={}",
                        percent_encoding::utf8_percent_encode(name.as_str(), percent_encoding::NON_ALPHANUMERIC),
                        secret,
                        percent_encoding::utf8_percent_encode("BadLang™", percent_encoding::NON_ALPHANUMERIC)
                    );
                    let qr_code = qrcode::QrCode::new(scheme.as_bytes()).expect("Wow why no QR-code?");

                    println!("{}", qr_code.render().light_color("  ").dark_color("██").build());

                    print!(
                        "{}",
                        "This is a 2FA code that secures your account. It is scannable using apps such as Google Authenticator. Do scan it, because it will never ever be available again! Press \
                         enter as soon as you're ready "
                            .bright_green()
                            .bold()
                    );

                    let _ = std::io::stdout().flush();
                    {
                        let mut buffer = String::new();
                        let stdin = std::io::stdin();
                        let _ = stdin.read_line(&mut buffer);
                    }
                    TwoFA::GoogleAuth(secret)
                }
                "Security questions" => {
                    let security_questions = vec![
                        "What's your mothers maiden name?",
                        "What's your favorite color?",
                        "What city were you born in?",
                        "How do you feel about geese?",
                        "What are you wearing right now?",
                        "Are you able to do a handstand?",
                        "Favorite toe?",
                        "What day is it today?",
                        "List all the funny numbers on your credit card separated by a slash.",
                        "Did you lie on any of our questions?",
                        "Now answer the last question honestly",
                    ];
                    let mut answers = vec![];
                    for security_question in security_questions {
                        answers.push(
                            inquire::Text::new(security_question)
                                .with_validator(inquire::min_length!(4))
                                .prompt()
                                .expect("Answer the question dammit!"),
                        );
                    }
                    TwoFA::SecurityQuestion(answers)
                }
                "Biometrics" => {
                    use robius_authentication as ro_au;

                    let policy = ro_au::PolicyBuilder::new()
                        .biometrics(Some(ro_au::BiometricStrength::Strong))
                        .password(true)
                        .watch(true)
                        .wrist_detection(true)
                        .build()
                        .unwrap();

                    let text = ro_au::Text {
                        android: ro_au::AndroidText {
                            title:       "Holy fuck!",
                            subtitle:    Some("You managed to make it run on android! You're probably the first and only person to ever see this! Here take a cookie 🍪"),
                            description: Some(
                                "Also if you somehow actually made this run on an android for whatever reason and are not just reading source code, please do send me a screenshot on Mail or \
                                 Instagram",
                            ),
                        },
                        apple:   "steal your biometrics in order to clone you once technology advances far enough",
                        windows: ro_au::WindowsText::new("BadLang™ needs you to authenticate yourself", "Data collected may be share with third or even fourth parties.").expect("Yurr"),
                    };

                    let auth_result = ro_au::Context::new(()).blocking_authenticate(text, &policy);
                    if auth_result.is_err() {
                        report_error("Your 2FA failed!");
                    }
                    TwoFA::OS
                }
                _ => report_error("What the hell?"),
            };

            inquire::Text::new(TERMS_OF_SERVICE)
                .with_validator(|v: &str| {
                    if v == ACCEPTANCE_PHRASE {
                        Ok(Validation::Valid)
                    } else if v == ACCEPTANCE_PHRASE.replace(' ', " ") {
                        Ok(Validation::Invalid("You can't just copy-paste and expect it to work!".into()))
                    } else {
                        Ok(Validation::Invalid("Incorrect text!".into()))
                    }
                })
                .prompt()
                .expect("Enter the phrase");

            *account = Some(Account {
                name,
                password_hash,
                version: semver::Version::parse(env!("CARGO_PKG_VERSION")).expect("WTF cargo").to_string(),
                two_factor,
            });
            println!("{}! Account saved!", "SUCCESS".green());
        }
        FIRST_OPTION => {
            let Some(account) = account.as_ref() else {
                report_error("You don't have an account yet!");
            };

            let name = inquire::Text::new("Enter your username: ")
                .with_validator(inquire::required!())
                .with_validator(inquire::min_length!(16))
                .prompt()
                .expect("Enter your username");

            let password = inquire::Password::new("Enter your password: ").without_confirmation().prompt().expect("Enter a password");

            let confirm_signs = ["✅", "✅", "✔️", "✓", "✔"];
            let cancel_signs = [
                "𝕏", "✗", "✘", "×", "Χ", "χ", "Х", "х", "╳", "☓", "✕", "✖", "❌", "❎", "⨉", "⨯", "🗙", "🗴", "🞨", "🞩", "🞪", "🞫", "🞬", "🞭", "🞮",
            ];
            let bool_formatter: inquire::formatter::BoolFormatter = &|boolean| if boolean { confirm_signs.as_slice().join("/") } else { cancel_signs.as_slice().join("/") };
            let bool_parser: inquire::parser::BoolParser = &|string| {
                if confirm_signs.as_slice().contains(&string) {
                    Ok(true)
                } else if cancel_signs.as_slice().contains(&string) {
                    Ok(false)
                } else {
                    Err(())
                }
            };
            // I want to draw your attention to the fact that this does in fact do
            // nothing at all, like so many checkboxes of this type
            inquire::Confirm::new("Remember password?")
                .with_formatter(bool_formatter)
                .with_parser(bool_parser)
                .with_error_message(
                    format!(
                        "Type one of {} to accept and one of {} to decline",
                        confirm_signs.as_slice().join("/"),
                        cancel_signs.as_slice().join("/")
                    )
                    .as_str(),
                )
                .prompt()
                .expect("Confirm or cancel");

            let parsed_hash = PasswordHash::new(&account.password_hash).expect("Oh no");
            if !(name == account.name && argon2.verify_password(password.as_bytes(), &parsed_hash).is_ok()) {
                report_error("Either your name or password were wrong. Try again!");
            }
            match &account.two_factor {
                TwoFA::GoogleAuth(secret) => {
                    let auth_code = inquire::Text::new("Enter your Google Authenticator code:")
                        .with_validator(inquire::required!())
                        .with_validator(inquire::length!(6))
                        .prompt()
                        .expect("Enter your auth code");

                    let ga = google_authenticator::GoogleAuthenticator::new();

                    if !ga.verify_code(secret.as_str(), auth_code.as_str(), 0, 0) {
                        report_error("Your auth code was wrong!");
                    }
                }
                TwoFA::SecurityQuestion(answers) => {
                    let answer = inquire::Text::new("Enter an answer to a security question:").prompt().expect("Enter the answer dammit!");
                    if !answers.contains(&answer) {
                        report_error("Your answer was wrong!");
                    }
                }
                TwoFA::OS => {
                    use robius_authentication as ro_au;

                    let policy = ro_au::PolicyBuilder::new()
                        .biometrics(Some(ro_au::BiometricStrength::Strong))
                        .password(true)
                        .watch(true)
                        .wrist_detection(true)
                        .build()
                        .unwrap();

                    let text = ro_au::Text {
                        android: ro_au::AndroidText {
                            title:       "Holy fuck!",
                            subtitle:    Some("You managed to make it run on android! You're probably the first and only person to ever see this! Here take a cookie 🍪"),
                            description: Some(
                                "Also if you somehow actually made this run on an android for whatever reason and are not just reading source code, please do send me a screenshot on Mail or \
                                 Instagram",
                            ),
                        },
                        apple:   "steal your biometrics in order to clone you once technology advances far enough",
                        windows: ro_au::WindowsText::new("BadLang™ needs you to authenticate yourself", "Data collected may be share with third or even fourth parties.").expect("Fuck why?"),
                    };

                    let auth_result = ro_au::Context::new(()).blocking_authenticate(text, &policy);
                    if auth_result.is_err() {
                        report_error("Your 2FA failed!");
                    }
                }
            }

            println!("{}! Logged into your account!", "SUCCESS".green().bold());
        }
        _ => unreachable!(),
    }
}

pub fn self_promotion() {
    println!(
        "{}",
        "Follow us on Instagram and Twitter @badlang_dev and be sure to also give us a Github star ⭐️⭐️⭐️⭐️⭐️"
            .bold()
            .green()
            .on_magenta()
    );
}

pub fn update(last_update: &mut u64) {
    if SystemTime::now().duration_since(UNIX_EPOCH).expect("Damn bro what kinda system you running").as_secs() - *last_update > (24 * 60 * 60) {
        // These are for testing because this part of the code likes to break.
        // if false {
        // if true {
        println!("Beginning update process...");
        let update_size = fastrand::u64((UPDATE_SIZE - UPDATE_VARIATION)..(UPDATE_SIZE + UPDATE_VARIATION));
        let (download_time, content_length) = {
            let start = Instant::now();
            let mut content_length = u64::MAX;
            for test_file in TEST_FILE {
                content_length = match reqwest::blocking::get(test_file.0) {
                    Ok(ok) => {
                        if ok.status().is_server_error() {
                            continue;
                        }
                        let temp = ok.content_length().unwrap_or(test_file.1);
                        let _ = std::hint::black_box(ok.text().unwrap_or_else(|_| String::new()));
                        // let _ = std::fs::write("/dev/null", ok.text().unwrap_or_else(|_|
                        // String::new())); // This write is stupid because it shouldn't need to be
                        // here. It just won't measure any download speed
                        // otherwise and since the file is "empty" it
                        // doesn't actually print anything. But I'm leaving it here because I feel
                        // it fits with the feel of the program
                        temp
                    }
                    Err(_err) => {
                        continue;
                    }
                };
                break;
            }
            (start.elapsed(), content_length)
        };
        if content_length == u64::MAX {
            report_error("There is an actual server error. For real this time");
        }
        let download_speed = content_length as f64 / download_time.as_secs_f64();
        let mut progress = 0;
        let progress_bar = indicatif::ProgressBar::new(update_size);
        let mut iteration: usize = 0;
        while progress < update_size {
            let increment = fastrand::u64(
                (((1.0 - DOWNLOAD_SPEED_VARIATION) * download_speed / DOWNLOAD_UPDATE_INTERVAL) as u64)..(((1.0 + DOWNLOAD_SPEED_VARIATION) * download_speed / DOWNLOAD_UPDATE_INTERVAL) as u64),
            );
            progress_bar.set_style(
                indicatif::ProgressStyle::with_template(
                    format!(
                        "Downloading update{:<4} {{wide_bar}} {{bytes}}/{{total_bytes}} [{}/s]",
                        ".".repeat((iteration % 3) + 1),
                        humansize::format_size(increment, humansize::BINARY)
                    )
                    .as_str(),
                )
                .expect("This shouldn't fail"),
            );
            iteration += 1;
            progress_bar.inc(increment);
            progress += increment;
            std::thread::sleep(std::time::Duration::from_secs_f64(DOWNLOAD_UPDATE_INTERVAL));
        }
        progress_bar.finish();
        println!("Applying update...");
        std::thread::sleep(std::time::Duration::from_secs(10)); // It's literally just constant.
        *last_update = SystemTime::now().duration_since(UNIX_EPOCH).expect("Damn bro what kinda system you running").as_secs();
    }
}

pub fn trial_message(runs_so_far: &mut usize) {
    let exec_name = std::env::args().next().expect("How did you manage this one again?");
    *runs_so_far += 1;
    if *runs_so_far < FREE_RUNS {
        report_warning(
            format!(
                "You have used {runs_so_far} out of your {FREE_RUNS} free runs. Afterwards, the program will run on our bronze tier server infrastructure, unless you subscribe to either our gold or \
                 platinum subscription tier. You can open the subscription plans using `{exec_name} -s` or `{exec_name} --subscribe`",
            )
            .as_str(),
        );
    } else {
        report_warning(
            format!(
                "You have used up all your free runs and your program will now run on our bronze tier server infrastructure. Subscribe to our gold or platinum tier to run your programs on your \
                 device or on our gold tier server infrastructure. You can open the subscription plans using `{exec_name} -s` or `{exec_name} --subscribe`"
            )
            .as_str(),
        );
    }
}
