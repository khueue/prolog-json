PROLOG = swipl -O

.PHONY: all
all: test

.PHONY: test
test:
	@ echo "--- Run tests and exit ..."
	time $(PROLOG) -s load -g json_test -t halt

.PHONY: cov
cov:
	@ echo "--- Run tests, print test coverage and exit ..."
	$(PROLOG) -s load -g json_cov -t halt

.PHONY: repl
repl:
	@ echo "--- Load and enter REPL ..."
	$(PROLOG) -s load -g json_repl
