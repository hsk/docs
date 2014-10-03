# Fib 40 bench

| language | fib | object | trait | trait opt |
| --- | ---- | ---- | --- | ---- |
| g++ -O3 | 474ms | 1031ms | 1220ms | 926ms |
| Java | 627ms | 1110ms | 1576ms |     |
| Node | 2085ms | 3754ms |   |   |
| Ruby | 23380ms | 121465ms | | |
| Rust --opt-level=3| 548ms | 609ms | | |
| Rust -O| 550ms | 1032ms | | |
| Rust | 1355ms | 3927ms | | |
