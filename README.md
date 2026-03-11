# cl-finite-fields

Pure Common Lisp finite field arithmetic for cryptographic applications.

## Features

- **Prime Field Operations (Fp)**: Addition, subtraction, multiplication, division, inversion
- **Extension Field Tower**: Fp -> Fp2 -> Fp6 -> Fp12 (for BLS12-381 pairings)
- **Montgomery Multiplication**: Efficient modular multiplication without division
- **Square Roots**: Tonelli-Shanks algorithm for finding square roots
- **Batch Inversion**: Montgomery's trick for O(n) inversions with single division
- **Constant-Time Operations**: Timing-attack resistant primitives
- **Field Serialization**: Byte and hex encoding/decoding

## Installation

Clone the repository and load via ASDF:

```lisp
(asdf:load-system :cl-finite-fields)
```

## Usage

```lisp
(use-package :cl-finite-fields)

;; Prime field arithmetic (defaults to BLS12-381)
(fp-add 5 7)          ; => 12
(fp-mul 3 4)          ; => 12
(fp-inv 7)            ; => multiplicative inverse of 7
(fp-sqrt 4)           ; => 2 (or p-2)

;; With custom modulus
(fp-add 5 7 17)       ; => 12
(fp-add 10 10 17)     ; => 3 (20 mod 17)

;; Batch inversion (efficient for many elements)
(fp-batch-inv '(2 3 5 7) 17)

;; Extension fields (Fp2)
(let ((a (make-fp2-element 3 5))
      (b (make-fp2-element 7 2)))
  (fp2-mul a b))

;; Montgomery multiplication
(let ((ctx (make-default-montgomery-context)))
  (mont-mul (mont-from-normal 5 ctx)
            (mont-from-normal 7 ctx)
            ctx))
```

## Field Constants

- `+bls12-381-p+`: BLS12-381 base field modulus (381 bits)
- `+secp256k1-p+`: secp256k1 field modulus (256 bits)

## Running Tests

```lisp
(asdf:test-system :cl-finite-fields)
```

## License

BSD-3-Clause. See LICENSE file.
