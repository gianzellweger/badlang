fn generate_quadratic_equation() -> (i32, i32, i32, i32, i32) {
    use fastrand as fr;
    let root1 = fr::choice([-1, 1]).unwrap() * fr::i32(1..=7);
    let root2 = fr::choice([-1, 1]).unwrap() * fr::i32(1..=7);

    let a = fr::i32(1..=10);
    let b = -a * (root1 + root2);
    let c = a * root1 * root2;

    (a, b, c, root1, root2)
}

fn main() {
    let (a1, b1, c1, x1, x2) = generate_quadratic_equation();
    let res = format!("{a1}x^2 {b1:+}x {c1:+} = 0");
    let opts = katex::Opts::builder()
        .display_mode(true)
        .output_type(katex::OutputType::Mathml)
        .build()
        .unwrap();
    let html = katex::render_with_opts(&res, &opts)
        .unwrap()
        .replace('−', "-");

    println!("{html}");
}
