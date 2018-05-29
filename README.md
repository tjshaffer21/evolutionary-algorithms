## Description

A collection of evolutionary algorithms for experimental purposes.

## Getting Started

### Prerequisites

* Common Lisp environment - Code has not been tested outside of CCL.
* ASDF or Quickload
* iterate package
* lisp-unit2 - Unit Testing

### Testing

To run the unit tests the **evolution_test.asd** system must be loaded up.

To do a simple tests on everything: ```(evolution.tests:run-all-tests) ```

To get a summary of the results: ```(lisp-unit2:print-summary (evolution.tests:run-all-tests)) ```

### Examples

Examples are in the **evolution-examples.asd** system.

### Running

The main system is in **evolution.asd**.

```lisp
(evolution:evolution :truncation :ascii-caps "HELLO" 5 100 1000 0.75)
```

## Limitations/Issues

* Ongoing and in the early stages so many functions are applicable to change drastically.
* Some algorithms are likely to be unoptimized.
* Currently, :ascii-caps is the only available representation type.
