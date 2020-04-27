# monkey-rs
- An implementation of Interpreter book in Rust.
https://www.oreilly.co.jp/books/9784873118222/

```
>> let add = fn(a, b) { a + b };
>> let sub = fn(a, b) { a - b };
>> let applyFunc = fn(a, b, func) { func(a, b) };
>> applyFunc(2, 2, add);
4
>> applyFunc(5, 2, sub);
3
```
