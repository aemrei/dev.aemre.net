---
title: Go Language Sytax
---

## Basic Types

```go
func main() {
  var i, j int = 1, 2

  primes := [6]int{2, 3, 5, 7, 11, 13}

  var (
    ToBe   bool       = false
    MaxInt uint64     = 1<<64 - 1
    z      complex128 = cmplx.Sqrt(-5 + 12i)
  )
}

```

## Conditions and loops

```go
func test() {
  for i := 0; i < 10; i++ {
    sum += i
  }

  if v := math.Pow(x, n); v < lim {
    return v
  }

  switch time.Saturday {
  case today + 0:
    fmt.Println("Today.")
  case today + 1:
    fmt.Println("Tomorrow.")
  case today + 2:
    fmt.Println("In two days.")
  default:
    fmt.Println("Too far away.")
  }
}
```

## Structure

```go
type Vertex struct {
  X int
  Y int
}

func main() {
  v := Vertex{1, 2}
}
```
