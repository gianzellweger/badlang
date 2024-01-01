#[cfg(any(target_os = "linux", target_os = "windows"))]
fn main() {
    tauri_build::build()
}

#[cfg(not(any(target_os = "linux", target_os = "windows")))]
fn main() {}
