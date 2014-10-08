# Fasten

**Fasten** is a tool for finding more optimal static configurations of
programs. It takes a directory tree of annotated source code, generates a
population of variants of that tree, and evolves those variants over several
generations to find a configuration that is more optimal according to a given
fitness function.

## Example

### Annotating Source

Integers followed by `/* TYPE FASTENABLE */` are enabled for optimization, where
`TYPE` is one of:

 * `BOOL`: a boolean (`0` or `1`) to toggle
 * `INT`: an integer to increment or decrement
 * `POW`: a power of two to double or halve

```
#define BUFFER_SIZE 128 /* POW FASTENABLE */
```

### Testing Fitness

The fitness function should produce a single floating-point value, such as a
total time. A *lower* value is considered *better*.

```
#!/bin/bash
time -p ( bin/project --benchmark > output 2> errors ) 2>timing
perl -ne 'if (/real\s+([\d.]+)/) { printf "%f", $1; }' timing
```

### Running Fasten

Fasten takes a regex of file paths to load, a command for resetting the tree, a
build command, the fitness function, and the directories to search for
sources. Note that your file regex does not need to match all files in your
project, only those that you want to be searched for `FASTENABLE` directives.

```
fasten \
  --files "(\\.c|\\.h)$"
  --reset "git checkout ." \
  --build "make" \
  --fitness "fitness.sh" \
  /path/to/project
```

See `--help` for full usage.

## Interpreting Output

Fasten produces an array of fittest individuals, in descending order of
fitness. An individual is an array of changes made to the tree, such as:

```
path/to/file.c:100: change 512 to 1024.
```

You may achieve even better performance by judiciously combining the top few
individuals.
