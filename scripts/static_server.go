package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"path/filepath"
)

func main() {
	addr := flag.String("addr", ":8090", "listen address, e.g. :8090")
	dir := flag.String("dir", "static", "directory to serve as /static/")
	flag.Parse()

	abs, err := filepath.Abs(*dir)
	if err != nil {
		log.Fatalf("failed to resolve static dir: %v", err)
	}
	info, err := os.Stat(abs)
	if err != nil {
		log.Fatalf("static dir not accessible: %v", err)
	}
	if !info.IsDir() {
		log.Fatalf("static path is not a directory: %s", abs)
	}

	fs := http.FileServer(http.Dir(abs))
	http.Handle("/static/", http.StripPrefix("/static/", fs))

	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		fmt.Fprintf(w, "Static server OK. Try /static/js/at_risk.js\nRoot: %s\n", abs)
	})

	log.Printf("Serving static from %s at http://127.0.0.1%s/static/", abs, *addr)
	if err := http.ListenAndServe(*addr, nil); err != nil {
		log.Fatalf("server error: %v", err)
	}
}
