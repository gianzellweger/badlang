use captcha::filters::*;
use rayon::prelude::*;
use std::io::Cursor;

fn main() {
    let png1 = captcha::gen(captcha::Difficulty::Easy).as_png().unwrap();
    std::fs::write("captcha1.png", png1).expect("Hehe");
    let png2 = captcha::gen(captcha::Difficulty::Hard).as_png().unwrap();
    std::fs::write("captcha2.png", png2).expect("Hehe");
    let png3 = captcha::Captcha::new()
        .add_chars(8)
        .apply_filter(Cow::new().circles(69))
        .apply_filter(Dots::new(69).min_radius(1).max_radius(3))
        .apply_filter(Grid::new(10, 10))
        .apply_filter(Noise::new(0.3))
        .apply_filter(Wave::new(5.0, 10.0).horizontal())
        .apply_filter(Wave::new(2.0, 20.0).vertical())
        .view(220, 120)
        .as_png()
        .unwrap();
    std::fs::write("captcha3.png", png3).expect("Hehe");
    let wavs = captcha::gen(captcha::Difficulty::Easy)
        .add_chars(3)
        .as_wav()
        .iter()
        .map(|x| x.as_ref().unwrap().clone())
        .collect::<Vec<Vec<u8>>>();
    let samples = wavs
        .iter()
        .skip(1)
        .flat_map(|wav| {
            hound::WavReader::new(&wav[..])
                .unwrap()
                .into_samples::<i16>()
        })
        .map(|x| x.unwrap())
        .collect::<Vec<_>>();
    let mut buf: Cursor<Vec<u8>> = Cursor::new(wavs[0].clone());
    let mut writer = hound::WavWriter::new_append(&mut buf).unwrap();
    samples
        .par_iter()
        .for_each(|sample| writer.write_sample(*sample).unwrap());
    writer.finalize().unwrap();
    std::fs::write("captcha.wav", buf.into_inner()).expect("Hehe");
}
