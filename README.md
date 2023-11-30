# minc
A toy C compiler implemented in Rust.
This is a project for the course "Programming Language" at The University of Tokyo.
The compiler is validated by the test suites provided in the course.

## Example of code that can be compiled
```c
long fib(long n) {
  if (n < 2) {
    return 1;
  } else {
    long x;
    long y;
    x = fib(n - 1);
    y = fib(n - 2);
    return x + y;
  }
}
long f(long n) {
  long m;
  long y;
  m = 1 + n % 30;
  y = fib(m);
  return y;
}
```

```c
long f(long a0, long a1, long a2, long a3, long a4, long a5) {
  return (a0 + a1) * ((a2 + a3) / (a4 - a5));
}
```
