# Oxide - A Modern Object-Oriented Language

Oxide is a modern, expressive, and object-oriented programming language written in Rust. Inspired by Python and Rust, Oxide treats everything as an object while supporting powerful features like classes, methods, and async operations.

## 🚀 Features

- **Object-Oriented**: Everything in Oxide is an object, including primitive types.
- **Classes & Methods**: Define reusable objects with methods and inheritance.
- **Async Support**: Write non-blocking asynchronous code.
- **Flexible Type System**: Supports dynamic and static typing.
- **Functional Constructs**: First-class functions, lambda expressions, and more.
- **REPL Mode**: Interactive programming with an Oxide shell.
- **Powerful Standard Library**: Built-in support for collections, string manipulation, and more.

## 📜 Installation

To build Oxide, ensure you have Rust installed and then run:

```sh
cargo build --release
```

## 🏃‍♂️ Getting Started

### Running a Script

Create an Oxide script file (e.g., `hello_world.ox`) and run it:

```sh
oxide hello_world.ox
```

### Using the REPL

Start an interactive Oxide session:

```sh
oxide
```

### Example Code

```oxide
class Animal {
    fn __init__(name) {
        this.name = name;
    }
    fn speak(self) {
        print(str(self));
    }
    fn __str__(self) {
        return f"{this.name} makes a noise.";
    }
}

animal = Animal("Rex");
animal.speak();
```

## 🛠️ Run Modes

Oxide can be executed in different modes:

### 1️⃣ Script Execution Mode

Run a script file by passing the filename:

```sh
oxide my_script.ox
```

This will read and execute the contents of `my_script.ox`.

### 2️⃣ Direct Code Execution Mode

Run a one-liner or small script directly from the command line using `-e`:

```sh
oxide -e "print('Hello, World!')"
```

This allows quick execution of Oxide code without creating a file.

### 3️⃣ Interactive REPL Mode

Start an interactive session to experiment with Oxide in real-time:

```sh
oxide
```

This mode lets you enter commands and see the output instantly.

## 🛠 Project Structure

```text
src/
  ├── interpreter.rs  # Executes Oxide programs
  ├── lexer.rs        # Tokenizes source code
  ├── parser.rs       # Parses tokens into an AST
  ├── repl.rs         # Interactive shell for Oxide
  ├── main.rs         # Entry point
Cargo.toml            # Project configuration
.gitignore            # Ignored files
```

## 🔥 Roadmap

- [ ] Implement build-in objects
- [ ] Implement non-const methods
- [ ] Implement object's access control
- [ ] Implement class and method
- [ ] Implement full async/await support
- [ ] Improve error handling and debugging tools
- [ ] Expand standard library
- [ ] Add more functional programming constructs
- [ ] Enhance REPL with better error messages and command history
- [ ] Optimize interpreter performance
- [ ] Implement standard collection types like HashMap and Set
- [ ] Add module and import system for better code organization
- [ ] Support pattern matching and destructuring
- [ ] Implement better garbage collection or memory management techniques
- [ ] Improve standard library with built-in utility functions
- [ ] Add bytecode compilation and virtual machine execution

## 📄 License

Oxide is released under the MIT License.

---
Made with ❤️ using Rust
