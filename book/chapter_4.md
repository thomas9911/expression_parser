# User Functions

User functions can be defined as follows:

```
{{#include ../expression_parser/tests/book_test.rs:chapter_4_simple}}
```

You see that the calling of a function is different than built-ins functions. It uses the `.` operator to call the function.

Variables outside the function can be used inside the function.

```
{{#include ../expression_parser/tests/book_test.rs:chapter_4_globals}}
```

Even currying it implemented.

```
{{#include ../expression_parser/tests/book_test.rs:chapter_4_curry}}
```

Also simple recursion works:

```
{{#include ../expression_parser/tests/book_test.rs:chapter_4_recursion}}
```
