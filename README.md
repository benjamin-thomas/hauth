## Build the project

```bash
apt install libpcre3-dev
cabal build
```

---

## Run tests from one specific module:

```bash
cabal test --test-option=--match=Domain.Validation
```

---

## Run one specific test

```bash
# Full qualified path
cabal test --test-option='--match=/Domain.Validation/checks input with `lengthLessThan`'

# Test name
cabal test --test-option='--match=checks input with `lengthLessThan`'

# Partial match from the test name
cabal test --test-option=--match=lengthLessThan
```
