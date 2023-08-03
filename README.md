# Spark
Spark is an interpreted minimal proglang written in rust over the span of roughly a week. Spark supports:
- Functions
- Higher order functions
- Lists, Booleans, Numbers, Strings
- While loops

🚀🚀🚀🚀🚀 blazingly fast 🚀🚀🚀🚀🚀🚀🚀 (kill me)
## Prime Numbers
```
let is_prime = fn(x) {
    let i = 2;
    while i*i <= x {
        if x%i==0 {
            return false;
        }
        i = i+1;
    }
    return true;
};

let i = 1;
while i <= 100 {
    if is_prime(i) {
        println(str(i) + " is prime");
    } else {
        println(str(i) + " is not prime");
    }
    i = i+1;
}
// 1 is prime
// 2 is prime
// 3 is prime
// 4 is not prime
// ...
```
## Recursive Map
```
let map = fn(func, lst) {
    if (lst == []) {
        return lst;
    } else if (len(lst) == 1) {
        return [func(get(lst, 0))];
    } else {
        let head = get(lst, 0);
        let tail = sub(lst, 1, len(lst)-1);

        return [func(head)] + map(func, tail);
    }
};

let double  = fn(x) {
    return x*2;
};

println(map(double, [1,2,3]));
// [2,4,6]
```
