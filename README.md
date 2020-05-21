## SICP

This joins the other half-finished repositories for this tough book that you can find on GitHub.

## How do I run this code?

- Install [Dr Racket](https://racket-lang.org/)
- Get the [SICP Language module installed](https://docs.racket-lang.org/sicp-manual/Installation.html)
- If you're on emacs, you may want to install `racket-mode`.

For a nice experience, add this to your emacs config:

```scheme
  (global-prettify-symbols-mode 1)
  (add-hook 'racket-mode-hook
            (lambda ()
              "Beautify Lambdas"
              (setq prettify-symbols-alist '(("lambda" . 955)))))
```

## Any questions?

Write me, or create an issue!
