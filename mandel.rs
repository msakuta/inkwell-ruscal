fn printdensity(d: i32) {
    if d > 127 {
      print!("{}", ' ');
    } else if d > 8 {
      print!("{}", '.');
    } else if d > 4 {
      print!("{}", '+');
    } else {
      print!("{}", '*');
    }
}
  
fn newline() {
    println!("");
}
  
fn mandelconverge(creal: f64, cimag: f64) -> i32 {
    let mut r = creal;
    let mut i = cimag;
    const MAX_ITER: i32 = 255;
    for iter in 0..MAX_ITER {
        if r*r + i*i > 4. {
            return iter;
        }
        let next_r = r*r - i*i + creal;
        i = 2.*r*i + cimag;
        r = next_r;
    }
    MAX_ITER
}
  
fn mandel(xmin: f64, ymin: f64, xstep: f64, ystep: f64, xsteps: i32, ysteps: i32) {
    let xmax = xmin + xstep * xsteps as f64;
    let ymax = ymin + ystep * ysteps as f64;
    for iy in 0..ysteps {
        let y = iy as f64 * (ymax - ymin) * ystep as f64 + ymin;
        for ix in 0..xsteps {
            let x = ix as f64 * (xmax - xmin) * xstep as f64 + xmin;
            printdensity(mandelconverge(x,y));
        }
        newline();
    }
}
  
fn main() {
    mandel(-2.3, -2.0, 0.025 / 2., 0.05 / 2., 156, 80);
}