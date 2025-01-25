# TESC
# Tessera Compiler
### A Toy Programming Language

Tessera is a multi-paradigm programming Language
inspired by the Rust language.

This language is made for fun, and is by far not production-ready.
Language is under heavy development, nothing is set in stone.

Most items within the language are expressions.

#### Features (TBD)
 - Quasi Expression-based language
 - Lightweight QBE backend for fast compilation times
 - Multiple ways to loop/iterate.
 - Statically Typed
 - Potential Meta-programming using macros that operate on both Token Streams, and AST.
 - Planned runtime reflection library
 - Tagged Unions (WIP)

Example code 

More examples are in `examples/`

```rust
module main;
import std::core::mem;

func i64 int_add(i64: a, i64: b) {
  a + b
}

enum Enum {
    Variant,
    AnotherVariant,
    TupleVariant(bool, bool, char),
}

struct Stack<T> {
    public capacity: uint,
    size: uint,
    elements: *[T; 256]
}

func void Stack_push<T>(Stack<T> *self, T element) {
    if self.capacity == self.size {
        self.capacity *= 2;
        if self.capacity < 16 { self.capacity = 16; };

        self.elements = mem::realloc(self.elements, sizeof(Type) * self.capacity);
    }
    self.elements[self.size++] = element;
}

func T Stack_pop(Stack<T> *self) {
    std::assert(self.size > 0);

    self.elements[--self.size]
}

func bool Stack_empty(Stack<T> ¨self) {
    !self.size
}

pub func void main() {
    let stack: Stack = Stack {capacity: 16, size: 0, elements: &[] };

    stack.push(1);
    stack.push(2);

    // Prints pop: 2
    out "pop: %d\n" stack.pop();
    // Prints pop: 1
    out "pop: %d\n" stack.pop();
}


``` 

#### Milestones

- Lexer: ✓ (100%)
- Parser: ✓ (100%) 
- Type Resolver: ✓ (100%)
- AST to IR Lowering: ⏲  WIP, todo:
   - Imports (incl. resolver)
   - Modules (incl. resolver)
   - Index Expressions
   - Arrays (incl. Array Exprs, Array Init)
   - Structs (incl. Struct Exprs)
   - Pointers
   - Member Expressions
