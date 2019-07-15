# LogicTGenerator

## Usage

#### Observe single result
```haskell
observe $ makeHeap 10 0 100
```

#### Observe multiple results
```haskell
observeMany 5 $ makeHeap 10 0 100
```

#### Observe all results
```haskell
observeAll $ makeHeap 10 0 100
```
