# Microservices

## Very simple service

```go
package main

import "net/http"
import "log"


func main() {
  http.HandleFunc("/", func(http.ResponseWriter, *http.Request) {
    log.Println("Hello, world");
  })

  http.ListenAndServe(":9090", nil)
}
```

<[- The Go Programming Language](https://go.dev/src/net/http/server.go?s=61509%3A61556#L2378)>

## Read write data

```go
package main

import (
  "net/http"
  "fmt"
  "log"
  "io/ioutil"
)


func main() {
  http.HandleFunc("/", func(rw http.ResponseWriter, *http.Request) {
    log.Println("Hello, world");
    d, err := ioutil.ReadAll(r.Body)

    if err != nil {
      http.Error(rw, "Ooops", http.StatusBadRequest)
      return
    }

    fmt.Fprintf(rw, "Hello %s", d)
  })

  http.ListenAndServe(":9090", nil)
}
```
