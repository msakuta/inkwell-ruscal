#include <stdio.h>

void printdensity(double d) {
  if (d > 127) {
    putchar(32);
  } else if (d > 8) {
    putchar(46);
  } else if (d > 4) {
    putchar(43);
  } else {
    putchar(42);
  }
}

void newline() {
    putchar(10);
}

double mandelconverge(double creal, double cimag) {
    double r = creal;
    double i = cimag;
    int iter = 0;
    for (; iter < 255; iter++) {
        if (r*r + i*i > 4) {
            return iter;
        }
        double next_r = r*r - i*i + creal;
        i = 2*r*i + cimag;
        r = next_r;
    }
    return iter;
}

void mandel(double xmin, double ymin, double xstep, double ystep, int xsteps, int ysteps) {
    double xmax = xmin + xstep * xsteps;
    double ymax = ymin + ystep * ysteps;
    for (int iy = 0; iy < ysteps; iy++) {
        double y = iy * (ymax - ymin) * ystep + ymin;
        for (int ix = 0; ix < xsteps; ix++) {
            double x = ix * (xmax - xmin) * xstep + xmin;
            printdensity(mandelconverge(x,y));
        }
        newline();
    }
}

void main() {
    // mandel(-2.3, -2.0, 0.025, 0.05, 78, 40);
    mandel(-2.3, -2.0, 0.025 / 2, 0.05 / 2, 156, 80);
}