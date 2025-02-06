let counter = 0;
counter++;
for i : range(1, 10) {
  counter += i;
  print(counter);
}

fn greet(name) {
    return "Hello, " + name;
}
print(greet("Charlie"));

# let person = { name: "Charlie", age: 25, city: "New York" };
# let { name, age } = person;
# print(f"{name} is {age} years old.");

let numbers = [1, 2, 3, 4, 5];
print(numbers);
# let doubled = numbers.map(n => n * 2);
# let evens = numbers.filter(n => n % 2 === 0);
# let sum = numbers.reduce((acc, n) => acc + n, 0);
# print(doubled, evens, sum);

class Animal {
    fn __init__(name) {
        this.name = name;
    }
    fn speak() {
        print(f"{this.name} makes a noise.");
    }
}

class Dog extends Animal {
    fn speak() {
        print(f"{this.name} barks.");
    }
}

let dog = Dog("Rex");
dog.speak();
