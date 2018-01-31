# Spit Haskell Rules

Generate dummy rules for `rules_haskell` for Bazel. Designed for profiling
purposes.

```
Usage: spit-haskell-rules (-w|--width W) (-d|--depth D)

Available options:
  -h,--help                Show this help text
  -w,--width W             On how many libraries the top-level target should
                           depend
  -d,--depth D             Length of the chain of linear dependencies
```

The resulting Bazel repository will contain one top-level package which will
depend on `W` other packages, each of them will have a chain of `D`
dependencies forming a dependency graph similar to the following (`W=4`, `D=3`):

```
o  o  o  o
|  |  |  |
o  o  o  o
|  |  |  |
o  o  o  o
\  |  |  /
 \ |  | /
  \\  //
   \\//
   top
```

## License

Copyright © 2018 Mark Karpov

Distributed under BSD 3 clause license.
