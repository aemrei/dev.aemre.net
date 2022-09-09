# Types

```rust
let myString = format!("Hello, {}!", name);

let mut myFloat: f64 = 1.2;

let one: u32 = 1.99 as u32;

let myBoolean = if x > 5 { true } else { false }


let mut point: (i64, i64, i64) = (0, 0, 0);
point.0 = 17;


let mut array: [i32, 3] = [1995, 2000, 2005];


let myTuples: (i64, bool) = (1, true);

struct MyStruct {x: i64, y: bool} 


enum Color {
    Green,
    Red,
    Yellow,
    Custom {red: u8, green: u8, blue: u8},
    TupleCustom(u8, u8, u8),
}

let stop = Color::Red;
let purple = Color::Custom {red: 100, green: 0, blue: 250};
let purple2 = Color::TupleCustom (100, 0, 250);
```

## Vectors

```rust
let mut years: Vec<i32> = vec![1995, 2000, 2005];
years.push(2010); // Now `years` has 4 elements,
                  // ending in 2010
years.push(2015); // Now `years` has 5 elements,
                  // ending in 2015

println!("Number of years: {}", years.len());
```

## Iterations

```rust
for year in years.iter() {
    println!("Next year: {}", year + 1);
}
```

## Conditionals

```rust
match current_color {
    Color::Green => {
        println!("It was green!");
    }
    Color::Yellow => {
        println!("It was yellow!");
    }
    Color::Custom { red, green, blue } => {
        println!("{} {} {}", red, green, blue);
    }
}
```

```rust
let color_str = match current_color {
    Color::Green => {
        "It was green!"
    }
    Color::Yellow => {
        "It was yellow!"
    }
    _ => {
        "It was something else!"
    }
};
```
