## Description

A collection of evolutionary algorithms for experimental purposes.

## Getting Started

### Prerequisites

* Common Lisp environment - Code has not been tested outside of CCL.
* ASDF or Quickload
* iterate package
* lisp-unit and lisp-unit2 - If doing unit tests.

### Installation

At this moment there is no means of installation.

The simplest method is to add the project to Quickload's local-project subdirectory, and then use quickload to load the system.

### Testing

To run the unit tests the **evolution_test.asd** system must be loaded up.

To do a simple tests on everything: ```(evolution.tests:run-all-tests) ```

To get a summary of the results: ```(lisp-unit2:print-summary (evolution.tests:run-all-tests)) ```

### Examples

Examples are in the **evolution-examples.asd** system.

### Running

The main system is in **evolution.asd**.

The basic steps is to:

1. Create a fitness function
2. Run the system

```lisp
(defun fitness-hello (candidate)
  (reduce #'+ (iter
                (for ele in-string "HELLO")
                (for rep in (evolution::candidate-genotype candidate))
                (collecting (if (= (evolution::char- rep ele) 0) 1 0)))))
(evolution:evolution :truncation :ascii-caps (function fitness-hello) 5 100 100)
```

## Limitations/Issues

* Ongoing and in the early stages so many functions are applicable to change drastically.
* Some algorithms are likely to be unoptimized.
* Currently, :ascii-caps is the only available representation type.
