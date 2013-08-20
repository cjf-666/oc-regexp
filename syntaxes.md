The following constructs will be supported for version 0.1:

1. `*` Matches the preceding expression zero, one or several times.
2. `+` Matches the preceding expression one or several times.
3. `?` Matches the preceding expression once or not at all.
4. `|` Alternative between two expressions.
5. `()` Group expressions, but can not capture substrings for now.
5. `\` Escape special characters, for now they are `* + ? | \ ( ).


Todo

1. 将悬空的边单独放在一起，加速map。
