GHC_VERSION := $$(ghc --numeric-version)
CABAL_STORE := ${HOME}/.cabal/store/ghc-${GHC_VERSION}
PACKAGE_DB  := ${CABAL_STORE}/package.db

# This avoids changing the global environment file.
#
# This file should never hang around, it is temporarily created and then deleted
# by the rules below.
ENV=.specialist.env

.PHONY: install
install:
	# avoid changing the default environment
	cabal install -j \
		--lib \
		--package-db=${PACKAGE_DB} \
		--package-env=${ENV} \
		--reinstall \
	    specialist
	rm ${ENV}
	ghc-pkg describe --package-db=${PACKAGE_DB} specialist | grep -A1 ^id
	@echo installed in package database ${PACKAGE_DB}

.PHONY: uninstall
uninstall:
	# Unfortunately the only reliable way I have found to "uninstall" is to
	# completely remove the entire store
	rm -rf ${CABAL_STORE}

.PHONY: prof-install
prof-install:
	# avoid changing the default environment
	cabal install -j \
		--lib \
		--package-db=${PACKAGE_DB} \
		--project-file=cabal.project.prof \
		--package-db=${PACKAGE_DB} \
		--package-env=${ENV} \
		specialist
	rm ${ENV}
	ghc-pkg describe --package-db=${PACKAGE_DB} specialist | grep -A1 ^id
	@echo installed in package database ${PACKAGE_DB}

.PHONY: exe-install
exe-install: install
	cabal install --package-db=${PACKAGE_DB} specialyze --overwrite-policy=always

.PHONY: prof-exe-install
prof-exe-install: prof-install
	cabal install --package-db=${PACKAGE_DB} --project-file=cabal.project.prof specialyze --overwrite-policy=always

.PHONY: reinstall
reinstall: uninstall install

.PHONY: list
list:
	ghc-pkg list --package-db=${PACKAGE_DB} | grep specialist

.PHONY: latest
latest:
	ghc-pkg latest --package-db=${PACKAGE_DB} specialist

.PHONY: recache
recache:
	ghc-pkg recache --package-db=${PACKAGE_DB}

.PHONY: check
check:
	ghc-pkg check --package-db=${PACKAGE_DB} 2>&1 | grep specialist

.PHONY: describe
describe:
	ghc-pkg describe --package-db=${PACKAGE_DB} specialist
