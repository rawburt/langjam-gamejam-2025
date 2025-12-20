# The Programming Language

The programming language is a statically typed programming language that compiles to JavaScript with specific knowledge of the JavaScript game engine it is compiling for.

Details about the compiler architecture and features can be found at [./COMPILER.md](./COMPILER.md).

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

Constants can only be declared in the top level.

## Statements

Statements have no return value and they manipulate the control flow of a program.

### If

```
if true do
  debug("HOORAY")
end

if false do
  debug("OOPS")
else
  debug("OK")
end
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

```
--- functions with no return do not have a return type hint
def do_something() do
  debug("i'm doing something")
end

def add_1(i: int) int do
  ret i + 1
end
```

Functions with returns must always return. The compiler checks that all flows return the required value type.

## Images

To allow the game engine to be aware of image assets, you must declare an image asset:

```
asset ASSET_NAME = "assets/file.png"
```

The type of the asset is `image`. The compiler will check that the file exists.

Assets can only be declared in the top level.

## Libraries

Libraries and other source files can be defined as depdencies to any given file:

```
use std
use other/helper
```

The compiler checks the directory of the source file for the included libraries. There compiler also checks a global `lib/` directory for source files.

Libraries can only be included at the top of a source file.

## Built In Functions

* `pset(x: int, y: int, c: color) unit`
  * Set pixel at `(x, y)` to color `c`
* `button(i: int) bool`
  * Check if button is being pressed
* `buttonp(i: int) bool`
  * Check if button was just pressed for the first time this frame
* `clear()`
  * Clear the screen
* `text(s: str, x: int, y: int, s: int, c: color) unit`
  * Render text `s` at `(x, y)` in size `s` and color `c`.
* `debug(s: str) unit`
  * Log string `s` to the JavaScript console. Provides source location of the debug call.
  * Example: `debug("hello")` will print `[file.lg:4] hello`.
* `render(i: image, sx: int, sy: int, sw: int, sh: int, dx: int, dy: int, dw: int, dh: int)`
  * Using image `i`, render source rectangle to destination rectangle.
  * Source rectangle is `(sx, sy)` with dimensions `sw x sh`.
  * Destination rectangle is `(dx, dy)` with dimensions `dw x dh`.
* `render_overlay(c: color, x: int, y: int, w: int, h: int)`
  * Blend color `c` on top of whatever is currently rendered at location `(x, y)` with dimension `w * h`.
* `rand(min: int, max: int) int`
  * Return a random number between `min` and `max` (inclusive).
* `forall a: str(a) str`
  * Turn any time `a` into `str`.
* `forall a: len([a] | str) int`
  * Return the length of a list or a string.
* `forall a: push(i: a, l: list[a]) unit`
  * Push and item to the back of a list.
* `forall a: pop(l: list[a]) a`
  * Remove the last item from a list and return it.
* `forall a: delete(i: int, l: list[a]) unit`
  * Delete an item at index `i` from list `l` (resizes list to remove item).
