all:
	@echo no such target
	@exit 1

.PHONY: clean
clean:
	touch dummy.fasl
	find . \( -name \*.fasl -o -name \*.x86f \) -print0 | xargs -0 rm
