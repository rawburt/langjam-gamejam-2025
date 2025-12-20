# The Programming Language

The programming language is a statically typed programming language that compiles to JavaScript with specific knowledge of the JavaScript game engine it is compiling for.

## Basic Types

* `unit` (only used for function return types)
* `bool`
* `int`
* `color`
* `str`
* `image` (discussed below under "Images")

```
var b: bool = true
var i: int = 1
var c: color = #990099
car s: str = "hello, world!"
```

## Composite Types

### Lists

Lists of type `T` are typed like `[T]`. Examples:

```
--- declare a list
var items: [int] = [1,2,3,4]

--- add item to end of list
push(4, items)

--- remove last item of the list
var i: int = pop(items)

--- remove item at index 2 and shrink the list
delete(2, items)
```

### Records

Example

```
rec MyRecord do
  item: int
  other_item: str
end

var my_record: MyRecord = MyRecord(item: 1, other_item: "hello!")
```

## Enumerable Type

```
enum MyEnum do
  Item1
  Item2
  Item3
end

var item: MyEnum = MyEnum.Item2
```

## Variables

Variables must be declared with a type and an initial value. There is no such thing as `null` is the programming language.

Examples:
```
var i: int = 1
i += 2

var j: color = #99aa00
```

Constants can't be mutate and require all capital names:

```
const HELLO: str = "HELLO"
```

## Statements

Statements have no return value and they manipulate the control flow of a program.

### If

```
```

### Cond

The first matching `when` condition will execute.

```
cond do
  when 1 < 0 do
    debug("nope!")
  end
  when 4 == 4 do
    debug("yup!")
  end
  when 3 < 4 do
    debug("not executed")
  end
end
```

### For

```
var sum: int = 0

--- final loop i = 99. for loops in this sense are not inclusive of the end number.
for i = 0 to 100 do
  sum += i
end

var items: [int] = [1,2,3,4]

for i = 0 to len(items) do
  debug(str(items[i]))
end

--- alternative for loop for lists
for item in items do
  debug(str(item))
end
```

### Match

You can `match` on `enum` types. The `when`'s are checked for exhaustiveness.

```
enum Direction do
  Left
  Right
  Up
  Down
end

var dir: Direction = Direction.Left

var x: int = 0
var y: int = 0

match dir do
  when Direction.Left do
    x -= 1
  end
  when Direction.Right do
    x += 1
  end
  when Direction.Up do
    y -= 1
  end
  when Direction.Down do
    y += 1
  end
end
```

## Functions

## Images

## Built In Functions
