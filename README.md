# Coexp
# Installation
```bash
cargo install --path .
```
# Usage
## Repl
```
coexp
```
## Execute file
```
coexp file.coexp
```
# Example programm

```hs
hello = 1
variable = other
id = fn x => x
one = id 1
zero = case cofn x => x {
    | inl x => 0
    | inr f => f @ 1
}
unit = ()
function = fn parameter => ()
const = fn x => fn y => x
cofunction = cofn parameter => ()
two = case inl 2 {
    | inl x => x
    | inr x => x
}
left = inl 1
right = inr 1
debug!(case cofn x => x {
    | inl x => 0
    | inr f => f @ 1
})
```
