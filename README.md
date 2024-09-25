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

---

## DB setup

Setup

```sh
# Step 0: grant user privileges to create extensions
# postgres=# GRANT CREATE ON DATABASE hauth TO benjamin;

 # Step 1: create a local db
postgres@dev:~$ createdb hauth

# Step 2: if this DB user exists, my local linux account of the same name will
# be able to connect, already authenticated (that's "peer authentication")
# The local "peer" user can create tables, and will become the owner of the table.
postgres@dev:~$ createuser benjamin

# You can list the users and the users via the psql shell
postgres=# \du
postgres=# \l

# Step 3: now connect!
benjamin@dev:~$ PGDATABASE=hauth psql
```