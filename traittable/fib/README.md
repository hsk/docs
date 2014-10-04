# Fib 40 bench

| language | fib | object | trait | trait 2d | trait opt |
| --- | ---- | ---- | --- | ---- |
| g++ -O3 | 474ms | 1,031ms | 1,220ms | 1399ms | 926ms |
| Java | 627ms | 1,110ms | 1,576ms |     | |
| Node | 2,085ms | 3,754ms |   |   | |
| Ruby | 23,380ms | 121,465ms | | | |
| Rust --opt-level=3| 548ms | 609ms | | | |
| Rust -O| 550ms | 1,032ms | | | |
| Rust | 1,355ms | 3,927ms | | | |
| ocamlopt | 771ms | 13,791ms | | | |
| ocamlc | 6,516ms | 49.043ms | | | |
| gcc fib.m -O3 -fobjc-arc| 474ms | 151,031ms | | | |